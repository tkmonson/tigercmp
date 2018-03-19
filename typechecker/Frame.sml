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

(*This is part of the view shift*)
(*QUESTION: Do we need to worry about this for current phase?*)
  (*From caller's perspective, args are in reg a0-a3. For the callee, they need to be moved from a0-a3 into various temps and frame slots. You
  can do this in this phase or in the next phase. *)
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




end
