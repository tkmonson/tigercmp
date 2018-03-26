signature FRAME =
sig

(*  The register for a temp is how that register is represented in assembly. So t123 might be $t0 *)
  type register = string
  type reg_info = Temp.temp * register


  datatype frame = makeFrame of {name: Temp.label, formals:access list, offset: int ref}
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
  datatype frame = makeFrame of {name: Temp.label, formals:access list, offset:int ref}

  (*Represents the location (in register or in frame) of any formal or local variable*)
  and      access =  InFrame of int
                   | InReg   of Temp.temp

  and       frag = PROC of {body:Tree.stm, frame:frame}
                 | STRING of Temp.label * string

  val wordsize = 4

  val FP = Temp.newtemp()
  val SP = Temp.newtemp()
  val RZ = Temp.newtemp()
  val RA = Temp.newtemp()
  val GP = Temp.newtemp()


  (*REGISTER LISTS : These are lists of temps that have special purposes in MIPS
                      These lists will be used by IR translation and insn selection phases to refer to special purpose registers*)

  val argRegs = [(Temp.newtemp(), "a0"),(Temp.newtemp(), "a1"),
                 (Temp.newtemp(), "a2"),(Temp.newtemp(), "a3")]

  val calleeSaves = [(Temp.newtemp(), "s0"), (Temp.newtemp(), "s1"), (Temp.newtemp(), "s2"), (Temp.newtemp(), "s3"),
                     (Temp.newtemp(), "s4"), (Temp.newtemp(), "s5"), (Temp.newtemp(), "s6"), (Temp.newtemp(), "s7")]

  val callerSaves = [(Temp.newtemp(), "t0"), (Temp.newtemp(), "t1"), (Temp.newtemp(), "t2"), (Temp.newtemp(), "t3"), (Temp.newtemp(), "t4"),
                      (Temp.newtemp(), "t5"), (Temp.newtemp(), "t6"), (Temp.newtemp(), "t7"), (Temp.newtemp(), "t8"), (Temp.newtemp(), "t9")]

  val specials = [(FP, "FP"), (SP, "SP"), (RZ, "R0"), (RA, "RA"), (GP, "GP")]

  (*Register map : This table maps register labels (eg "t123") to friendly names (eg "SP") for temps that are used as special registers*)
  val tempMap = foldl (fn ((temp, tempName):reg_info, table) =>
                            Symbol.enter(table, Symbol.symbol (Temp.makestring temp), tempName))
                      Symbol.empty
                      (argRegs @ calleeSaves @ callerSaves @ specials)

 (*This function returns a string for a temp. If that temp is used as a special register, we turn its friendly name (eg SP).
    Otherwise, we just return the temp in string form (eg 123)*)
  fun getRegName(temp) = case Symbol.look(tempMap, Symbol.symbol (Temp.makestring temp)) of
                        SOME(name) => name
                      | NONE       => Temp.makestring temp

  fun newFrame ({name:Temp.label, formals:bool list}) =
      let val oset = ref 0
          val allFormals = true::formals
          fun createAccess (esc:bool):access = if esc then (oset:=(!oset)+1; InFrame ((!oset)-1)) else InReg (Temp.newtemp())
          val accessList = map createAccess formals
      in makeFrame({name=name, formals=accessList, offset=oset})
      end

  fun name (makeFrame{name, formals, offset}) = name
  fun formals (makeFrame{name, formals, offset}) = formals
  val funclabel = Temp.newlabel()

  val isLeaf = ref false

  fun allocLocal (makeFrame{name, formals, offset}) = fn(x:bool) => if x then (offset:=(!offset)+1; InFrame ((!offset)-1)) else InReg(Temp.newtemp())

  fun exp (a:access) = fn(e:Tree.exp) => case a of
                                         InReg(t:Temp.temp) => Tree.TEMP(t)
                                       | InFrame(offset)    => Tree.MEM(Tree.BINOP(Tree.PLUS, e, Tree.CONST(offset)))

  (*This is part of the view shift*)
  (*From caller's perspective, args are in reg a0-a3. For the callee, they need to be moved from a0-a3 into various temps and frame slots. You
    can do this in this phase or in the next phase. *)
    (*TODO: Make this actually do something*)
  fun procEntryExit1 (makeFrame{name, formals, offset}, stat:Tree.stm) = stat

  fun externalCall (fname, argList) = Tree.CALL(Tree.NAME(Temp.namedlabel(fname)), argList)


end
