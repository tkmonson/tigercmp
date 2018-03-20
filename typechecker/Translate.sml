signature TRANSLATE =
sig

  (*Associated with a function *)
  (*Static link: One example of static link following is MEM(MEM(FP))*)
  (*See pg 143 for description of outermost level*)
  datatype level = makeLevel of {frame:MipsFrame.frame, parent:level, unq: unit ref}
                 | outermost

  (* Associated with a variable *)
  datatype access = makeAccess of {acc:MipsFrame.access, lev:level}

  datatype exp = Ex of Tree.exp
                |Nx of Tree.stm
                |Cx of Temp.label*Temp.label -> Tree.stm

  (*This function calls Frame.newFrame to create a frame with the formals and a static link*)
  val newLevel : {parent:level, name:Temp.label, formals:bool list} -> level

  val formals : level -> access list
  val allocLocal : level -> bool -> access
  (*val simpleVar : access*level -> exp*)

end

structure Translate:TRANSLATE =
struct
  (*Associated with a function *)
  (*Static link: One example of static link following is MEM(MEM(FP))*)
  (*See pg 143 for description of outermost level*)
  datatype level = makeLevel of {frame:MipsFrame.frame, parent:level, unq: unit ref}
                 | outermost

  (* Associated with a variable *)
  datatype access = makeAccess of {acc:MipsFrame.access, lev:level}

  datatype exp = Ex of Tree.exp
                |Nx of Tree.stm
                |Cx of Temp.label*Temp.label -> Tree.stm

  (*This function calls Frame.newFrame to create a frame with the formals and a static link*)
  fun newLevel({parent:level, name:Temp.label, formals:bool list}) =
    let
      val fr = MipsFrame.newFrame{formals=formals, name=Temp.newlabel()}
    in
    makeLevel{frame=fr, parent=parent,unq=ref ()}
    end

  fun formals(makeLevel{frame, parent, unq}) =
    let val accList = MipsFrame.formals(frame)
        val l = makeLevel{frame=frame, parent=parent, unq=unq}
        fun accWrapper(a:MipsFrame.access) = makeAccess{acc=a, lev=l}
    in map accWrapper accList
    end

  fun allocLocal(makeLevel{frame, parent, unq}) =
      fn(x:bool) => makeAccess{acc=MipsFrame.allocLocal(frame) (x),
                               lev=makeLevel{frame=frame,parent=parent,unq=unq}}

(*)  fun simpleVar(makeAccess{accessType, makeLevel{accessFrame, accessParent, accessUnq}},
                makeLevel{curFrame, curParent, curUnq}) =
        let
        (*TODO: Make this a recursive function that takes two levels as arguments*)
          fun createExp = case accessType of
                          InReg(t) => Tree.CONST 0
                          InFrame(offset) => if accessUnq = curUnq then Tree.MEM(MipsFrame.FP)
                                                                  else

        in
          MipsFrame.exp(accessType)(createExp)*)





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
	
