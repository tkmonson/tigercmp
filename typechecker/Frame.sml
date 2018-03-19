signature FRAME =
sig

(*Details on page 156*)
  val FP : temp.temp
  val wordsize : int
  (*QUESTION: Should FP and exp be associated with the type frame, or with the structure Frame?
         I think that an FP should be associated with a particular frame...
         In general, what functionality belongs in this structure and what belongs in the type?*)
  val exp: Frame.access -> Tree.exp -> Tree.exp

(*Holds info about formals and local variables*)
(*QUESTION: What information? My guess is name and access for each formal and each local variable*)
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
  val procEntryExit1 : frame * Tree.stm -> Tree.stm

(*Label for the machine code of this function : Produced using Temp.newLabel()*)
  val funclabel : temp.label

end
