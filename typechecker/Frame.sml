signature FRAME =
sig

(*Details on page 156*)
  val FP : temp.temp
  val wordsize : int
  val exp: Frame.access -> Tree.exp -> Tree.exp

(*Holds info about formals and local variables*)
(*QUESTION: What information? My guess is name of the frame, and access for each formal, AND a ref int that represents the offset for the next local variable *)
  type frame

(*Represents the location (in register or in frame) of any formal or local variable*)
  type access  (* InFrame of int
                   |InReg of temp.temp *)

  val newFrame : {name:Temp.label, formals:bool list} -> frame
  val name: frame -> temp.label
  val formals : frame -> access list

  val allocLocal : frame -> bool -> access

(*This is part of the view shift*)
(*QUESTION: Do we need to worry about this for current phase?*)
  (*From caller's perspective, args are in reg a0-a3. For the callee, they need to be moved from a0-a3 into various temps and frame slots. You
  can do this in this phase or in the next phase. *)
  val procEntryExit1 : frame * Tree.stm -> Tree.stm

(*Label for the machine code of this function : Produced using Temp.newLabel()*)
  val funclabel : temp.label

end
