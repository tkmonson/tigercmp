signature FRAME =
sig

(*  The register for a temp is how that register is represented in assembly. So t123 might be $t0 *)
  type register = string
  type reg_info = Temp.temp * register


  datatype frame = makeFrame of {name: Temp.label, formals:access list, offset: int ref, moves: Tree.stm list}
  and      access =  InFrame of int
                   | InReg   of Temp.temp
  and       frag = PROC of {body:Tree.stm, frame:frame}
               | STRING of Temp.label * string

  val isLeaf : bool ref
  val FP : Temp.temp
  val wordsize : int
  val exp: access -> Tree.exp -> Tree.exp

  val newFrame : {name:Temp.label, formals:bool list} -> frame
  val name: frame -> Temp.label
  val formals : frame -> access list

  val allocLocal : frame -> bool -> access

  val externalCall : string*Tree.exp list -> Tree.exp

  val procEntryExit1 : frame * Tree.stm -> Tree.stm
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list

(*Label for the machine code of this function : Produced using Temp.newLabel()*)
  val funclabel : Temp.label

  val tempMap : register Symbol.table
  val getRegName : Temp.temp -> string

end

structure MipsFrame:FRAME =
struct

  type register = string
  type reg_info = Temp.temp * register

  (*name of the frame, access for each formal, AND a ref int that represents the offset for the next local variable*)
  datatype frame = makeFrame of {name: Temp.label, formals:access list, offset:int ref, moves:Tree.stm list}

  (*Represents the location (in register or in frame) of any formal or local variable*)
  and      access =  InFrame of int
                   | InReg   of Temp.temp

  and       frag = PROC of {body:Tree.stm, frame:frame}
                 | STRING of Temp.label * string

  val wordsize = 4

  (* Zero Register *)
  val RZ = Temp.newtemp()

  (* Result Registers *)
  val v0 = Temp.newtemp()
  val v1 = Temp.newtemp()

  (* Argument Registers *)
  val a0 = Temp.newtemp()
  val a1 = Temp.newtemp()
  val a2 = Temp.newtemp()
  val a3 = Temp.newtemp()

  (* Caller-Saved Registers *)
  val t0 = Temp.newtemp()
  val t1 = Temp.newtemp()
  val t2 = Temp.newtemp()
  val t3 = Temp.newtemp()
  val t4 = Temp.newtemp()
  val t5 = Temp.newtemp()
  val t6 = Temp.newtemp()
  val t7 = Temp.newtemp()

  (* Callee-Saved Registers *)
  val s0 = Temp.newtemp()
  val s1 = Temp.newtemp()
  val s2 = Temp.newtemp()
  val s3 = Temp.newtemp()
  val s4 = Temp.newtemp()
  val s5 = Temp.newtemp()
  val s6 = Temp.newtemp()
  val s7 = Temp.newtemp()

  (* More Caller-Saved Registers *)
  val t8 = Temp.newtemp()
  val t9 = Temp.newtemp()

  (* Kernel Registers  *)
  val k0 = Temp.newtemp()
  val k1 = Temp.newtemp()

  (* Special Registers -- Global, Stack, Frame, Return *)
  val GP = Temp.newtemp()
  val SP = Temp.newtemp()
  val FP = Temp.newtemp()
  val RA = Temp.newtemp()

  (*REGISTER LISTS : These are lists of temps that have special purposes in MIPS
                      These lists will be used by IR translation and insn selection phases to refer to special purpose registers*)

  val argRegs = [(a0, "a0"),(a1, "a1"),
                 (a2, "a2"),(a3, "a3")]

  val calleeSaves = [(s0, "s0"), (s0, "s1"), (s2, "s2"), (s3, "s3"),
                     (s4, "s4"), (s5, "s5"), (s6, "s6"), (s7, "s7")]

  val callerSaves = [(t0, "t0"), (t1, "t1"), (t2, "t2"), (t3, "t3"), (t4, "t4"),
                      (t5, "t5"), (t6, "t6"), (t7, "t7"), (t8, "t8"), (t9, "t9")]

  val specials = [(FP, "FP"), (SP, "SP"), (RZ, "R0"), (RA, "RA"), (GP, "GP"), (v0, "v0")]

  (*Register map : This table maps register labels (eg "t123") to friendly names (eg "SP") for temps that are used as special registers*)
  val tempMap = foldl (fn ((temp, tempName):reg_info, table) =>
                            Symbol.enter(table, Symbol.symbol (Temp.makestring temp), tempName))
                      Symbol.empty
                      (argRegs @ calleeSaves @ callerSaves @ specials)

  fun getTemp(temp, tempname) = temp

 (*This function returns a string for a temp. If that temp is used as a special register, we turn its friendly name (eg SP).
    Otherwise, we just return the temp in string form (eg 123)*)
  fun getRegName(temp) = case Symbol.look(tempMap, Symbol.symbol (Temp.makestring temp)) of
                        SOME(name) => name
                      | NONE       => Temp.makestring temp

(* Tells a caller where to put argument n, either in an arg reg or on the stack *)
(* Arguments are 0-indexed *)
  fun getCallerArgLoc(n) =
    if n < 4 then Tree.TEMP (getTemp (List.nth (argRegs, n)))
    else Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP SP, Tree.CONST (wordsize*(n-4))))

  fun newFrame ({name:Temp.label, formals:bool list}) =
    let val allFormals = true :: formals
        val oset = ref 0
        fun processFormals(accesses, viewshifts, count) =
            if count = List.length allFormals then (accesses, viewshifts)
            else let
                   val offset = !oset
                   val stackSlot = Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP FP, Tree.CONST(wordsize*offset)))
                   val acc = if List.nth(allFormals, count) then (oset:=(!oset)-1; InFrame offset)
                             else InReg(Temp.newtemp())

                   val viewshift = case acc of
                                  InReg(t)   => Tree.MOVE(Tree.TEMP t, getCallerArgLoc count)
                                | InFrame(a) => Tree.MOVE(stackSlot, getCallerArgLoc count)
                 in
                  processFormals(accesses@[acc], viewshifts@[viewshift], count+1)
                 end
        val (accesses, viewshifts) = processFormals([],[],0)

      in makeFrame({name=name, formals=accesses, offset=oset, moves=viewshifts})
      end

  fun name (makeFrame{name, formals, offset, moves}) = name
  fun formals (makeFrame{name, formals, offset, moves}) = formals
  val funclabel = Temp.newlabel()

  val isLeaf = ref false

  fun allocLocal (makeFrame{name, formals, offset, moves}) = fn(x:bool) => if x then (offset:=(!offset)-1; InFrame ((!offset)+1)) else InReg(Temp.newtemp())

  fun exp (a:access) = fn(e:Tree.exp) => case a of
                                         InReg(t:Temp.temp) => Tree.TEMP(t)
                                       | InFrame(offset)    => Tree.MEM(Tree.BINOP(Tree.PLUS, e, Tree.CONST(wordsize*offset)))

  (*This is part of the view shift*)
  (*From caller's perspective, args are in reg a0-a3. For the callee, they need to be moved from a0-a3 into various temps and frame slots. You
    can do this in this phase or in the next phase. *)
  fun procEntryExit1 (makeFrame{name, formals, offset, moves}, stat:Tree.stm) =
    let
    (* This let generates pre, which copies all calleesaves onto the stack, and post which copies them back *)
      val fr = makeFrame{name=name, formals=formals, offset=offset, moves=moves}
      val accesses = map (fn(x) => allocLocal(fr)(x)) (map (fn(x) => true) calleeSaves)
      val cstemps = map getTemp calleeSaves
      val pre = ListPair.map (fn(x,y) => case x of
                                          InReg(t) => Tree.MOVE(Tree.TEMP t, Tree.TEMP y)
                                        | InFrame(offset) => Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP FP, Tree.CONST (wordsize*offset))), Tree.TEMP y)) (accesses, cstemps)
      val post = ListPair.map (fn(x,y) => case x of
                                          InReg(t) => Tree.MOVE(Tree.TEMP y, Tree.TEMP t)
                                        | InFrame(offset) => Tree.MOVE(Tree.TEMP y, Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP FP, Tree.CONST (wordsize*offset))))) (accesses, cstemps)
      val stmList = pre@moves@[stat]@post
    in
      Tree.seq stmList
    end



 (*TODO:Why do we do jump some?*)
  fun procEntryExit2(frame, body) =
      body @
      [Assem.OPER{assem="",
              src=[RZ,RA,SP] @ (map getTemp calleeSaves),
              dst=[], jump=SOME[]}]

  fun externalCall (fname, argList) = Tree.CALL(Tree.NAME(Temp.namedlabel(fname)), argList)


end
