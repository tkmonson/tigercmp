signature FRAME =
sig

  datatype frame = makeFrame of {name: Temp.label, formals:access list, offset: int ref}
  and      access =  InFrame of int
                   | InReg   of Temp.temp

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

end

structure MipsFrame:FRAME =
struct

  (*name of the frame, access for each formal, AND a ref int that represents the offset for the next local variable*)
  datatype frame = makeFrame of {name: Temp.label, formals:access list, offset:int ref}

  (*Represents the location (in register or in frame) of any formal or local variable*)
  and      access =  InFrame of int
                   | InReg   of Temp.temp

  and       frag = PROC of {body:Tree.stm, frame:frame}
                 | STRING of Temp.label * string 

  val FP = Temp.newtemp()
  val wordsize = 32

  fun newFrame({name:Temp.label, formals:bool list}) =
      let val oset = ref 0
          val allFormals = true::formals
          fun createAccess(esc:bool):access = if esc then (oset:=(!oset)+1; InFrame ((!oset)-1)) else InReg (Temp.newtemp())
          val accessList = map createAccess formals
      in makeFrame({name=name, formals=accessList, offset=oset})
      end

  fun name(makeFrame{name, formals, offset}) = name
  fun formals(makeFrame{name, formals, offset}) = formals
  val funclabel = Temp.newlabel()

  fun allocLocal(makeFrame{name, formals, offset}) = fn(x:bool) => if x then (offset:=(!offset)+1; InFrame ((!offset)-1)) else InReg(Temp.newtemp())

  fun exp(a:access) = fn(e:Tree.exp) => case a of
                                        InReg(t:Temp.temp) => Tree.TEMP(t)
                                      | InFrame(offset)    => Tree.MEM(Tree.BINOP(Tree.PLUS, e, Tree.CONST(offset)))

  (*This is part of the view shift*)
  (*From caller's perspective, args are in reg a0-a3. For the callee, they need to be moved from a0-a3 into various temps and frame slots. You
    can do this in this phase or in the next phase. *)
    (*TODO: Make this actually do something*)
  fun procEntryExit1(makeFrame{name, formals, offset}, stat:Tree.stm) = stat

  fun externalCall(fname, argList) = Tree.CALL(Tree.NAME(Temp.namedlabel(fname)), argList)


end
