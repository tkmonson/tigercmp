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

structure T = Tree

(* exp -> Tree.exp *)
fun unEx (Ex e) = e
  | unEx (Cx genstm) =
    let val r = Temp.newtemp()
	val t = Temp.newlabel() and f = Temp.newlabel()
    in T.ESEQ(seq[T.MOVE(T.TEMP r, T.CONST 1),
		  genstm(t,f),
		  T.LABEL f,
		  T.MOVE(T.TEMP r, T.CONST 0),
		  T.LABEL t],
	      T.TEMP r)
    end
  | unEx (Nx s) = T.ESEQ(s,T.CONST 0)

(* exp -> Tree.stm *)
fun unNx (Ex e) =
  | unNx (Nx n) = n
  | unNx (Cx x) = 

(* exp -> (Temp.label * Temp.label -> Tree.stm) *)
fun unCx (Ex e) = fn (t,f) => CJUMP(NEQ, e, CONST 0, t, f)
  | unCx (Ex (CONST 0)) = fn(t,f) => JUMP(NAME f, [f])
  | unCx (Ex (CONST 1)) = fn(t,f) => JUMP(NAME t, [t])
    (* Nx pattern match necessary? *)				 
  | unCx (Cx c) = c
	
