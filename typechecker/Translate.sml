signature TRANSLATE =
sig

  (*QUESTION: What information is stored in a level?*)
  (*Answer: Keep the frame and the parent. Can use a unit REF or a depth counter. *)
  (*Associated with a function *)
  (*Static link: One example of static link following is MEM(MEM(FP))*)
  type level

  (*Associated with a variable *)
  (* a Frame.access along with information about static links, in the form of a Translate.level*)
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
