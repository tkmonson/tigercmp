signature TRANSLATE =
sig

  (*QUESTION: What information is stored in a level?
  My guess is that it stores a Frame.frame option and a parent and a ref UNIT*)
  (*Answer: Keep the frame. Can use a unit REF or a depth counter. *)
  (*Associated with a function *)
  (*Static link: One example of static link following is MEM(MEM(FP))*)
  type level

  (*Different from Frame.access; Aware of static links*)
  (*Associated with a variable *)
  (*QUESTION: What's in this? My guess is a Frame.access along with information about static links, in the form of a Translate.level*)
  (*Answer: Correct.*)
  type access

  datatype exp = Ex of Tree.exp
                |Nx of Tree.stm
                |Cx of Temp.label*Temp.label -> Temp.stm

  val outermost : level

  (*This function calls Frame.newFrame to create a frame with the formals and a static link*)
  val newLevel : {parent:level, name:Temp.label, formals:bool list} -> level
  val formals : level -> access list
  val allocLocal : level -> bool -> access

  val simpleVar : access*level -> exp

end
