signature TRANSLATE =
sig

  (*QUESTION: Can this just be an int and a UNIT ref? What information is stored in a level?
  My guess is that it stores a Frame.frame option and a parent*)
  (*Associated with a function *)
  type level

  (*Different from Frame.access; Aware of static links*)
  (*Associated with a variable *)
  (*QUESTION: What's in this? My guess is a Frame.access along with information about static links, in the form of a Frame.frame or a Translate.level*)
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
