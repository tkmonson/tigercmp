signature TRANSLATE =
sig

  (*Associated with a function *)
  (*Static link: One example of static link following is MEM(MEM(FP))*)
  (*See pg 143 for description of outermost level*)
  datatype level = makeLevel of {frame:MipsFrame.frame, parent:level, unq: unit ref}
                 | outermost

  (* Associated with a variable: Keeps track of how to access that variable, and what level it was declared *)
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

  structure T = Tree

  (*Associated with a function *)
  (*Static link: One example of static link following is MEM(MEM(FP))*)
  (*See pg 143 for description of outermost level*)
  datatype level = makeLevel of {frame:MipsFrame.frame, parent:level, unq: unit ref}
                 | outermost

  (* Associated with a variable *)
  datatype access = makeAccess of {acc:MipsFrame.access, lev:level}

  datatype exp = Ex of T.exp
                |Nx of T.stm
                |Cx of Temp.label*Temp.label -> T.stm

  (*This function calls Frame.newFrame to create a frame with the formals and a static link*)
  fun newLevel ({parent:level, name:Temp.label, formals:bool list}) =
    let
      val fr = MipsFrame.newFrame{formals=formals, name=Temp.newlabel()}
    in
    makeLevel{frame=fr, parent=parent,unq=ref ()}
    end

 (*Given the level for some function, formals returns the Translate.access for each of its formals, except for the static link*)
  fun formals(makeLevel{frame=frame, parent=parent, unq=unq}) =
    (let val accList = MipsFrame.formals(frame)
        val l = makeLevel{frame=frame, parent=parent, unq=unq}
        fun accWrapper(a:MipsFrame.access) = makeAccess{acc=a, lev=l}
    in case accList of
      [] => []
    | a::l => map accWrapper l
    end)
   |  formals(outermost) = (ErrorMsg.error 0 ("Trying to access formals of outermost level!") ; [])

  fun allocLocal(makeLevel{frame=frame, parent=parent, unq=unq}) =
      (fn(x:bool) => makeAccess{acc=MipsFrame.allocLocal(frame) (x),
                               lev=makeLevel{frame=frame,parent=parent,unq=unq}})
    | allocLocal(outermost) = fn(x:bool) => (;makeAccess(MipsFrame., outermost))

  fun traverseLevels(makeLevel{frame=accessFrame, parent=accessParent, unq=accessUnq},
                     makeLevel{frame=curFrame, parent=curParent, unq=curUnq}) =
    if curUnq = accessUnq then T.TEMP(MipsFrame.FP)
                          else let val subexp = traverseLevels(makeLevel{frame=accessFrame, parent=accessParent, unq=accessUnq}, curParent)
                          in T.CONST(1)
                          end
(*First argument is the access representing where a variable was declared*)
(*Second argument is the level where the variable is being accessed*)
  fun simpleVar(makeAccess{acc=accessType, lev=accLevel as makeLevel{frame=accessFrame, parent=accessParent, unq=accessUnq}},
                makeLevel{frame=curFrame, parent=curParent, unq=curUnq}) =
        let
          val exp = case accessType of
                          MipsFrame.InReg(t) => T.CONST 0
                        | MipsFrame.InFrame(offset) => traverseLevels(makeLevel{frame=accessFrame, parent=accessParent, unq=accessUnq},
                                                            makeLevel{frame=curFrame, parent=curParent, unq=curUnq})
        in
          MipsFrame.exp(accessType)(exp)
        end

(* exp -> T.exp *)
  fun unEx (Ex e) = e
    | unEx (Nx s) = T.ESEQ(s,T.CONST 0)
    | unEx (Cx genstm) =
        let val r = Temp.newtemp()
            val t = Temp.newlabel() and f = Temp.newlabel()
        in T.ESEQ(T.seq[T.MOVE(T.TEMP r, T.CONST 1),
		        genstm(t,f),
		        T.LABEL f,
		        T.MOVE(T.TEMP r, T.CONST 0),
		        T.LABEL t],
	                T.TEMP r)
        end

(* exp -> Tree.stm *)
fun unNx (Ex e) = T.EXP e
  | unNx (Nx s) = s
  | unNx (Cx c) = let val t = Temp.newlabel() in c(t,t); T.LABEL t end

(* exp -> (Temp.label * Temp.label -> Tree.stm) *)
fun unCx (Ex (T.CONST 0)) = (fn(t,f) => T.JUMP(T.NAME f, [f]))
  | unCx (Ex (T.CONST 1)) = (fn(t,f) => T.JUMP(T.NAME t, [t]))
  | unCx (Ex e) = (fn (t,f) => T.CJUMP(T.NE, e, T.CONST 0, t, f))
  | unCx (Nx _) = raise ErrorMsg.Error
  | unCx (Cx c) = c

  (*Translation for an ArrayExp*)
  (*Assume that the external function initArray will initialize the array and return its base addr in a temp*)
  (*Stores array size in index -1 *)
  fun arrayCreate(size, initValue) =
  let
   val baseAddr = Temp.newtemp()
   val getBaseAddr = MipsFrame.externalCall("initArray", [T.CONST(size+1), T.CONST(initValue)])
   val storeBaseAddr = T.MOVE(T.TEMP(baseAddr), T.BINOP(T.PLUS, T.CONST 4, getBaseAddr))
   val storeArrSize = T.MOVE(T.MEM(T.BINOP(T.MINUS, T.TEMP(baseAddr), T.CONST 4)), T.CONST(size))
  in T.ESEQ(T.seq([storeBaseAddr, storeArrSize]), T.TEMP(baseAddr))
  end

 (*Translation for a RecordExp*)
 (*Assume that malloc returns base address in a temp*)
 (*WARNING: This function uses REFS for convenience...Saumya got lazy*)
  fun recCreate(fieldList, numFields) =
   let
    val baseAddr = Temp.newtemp()
    val getBaseAddr = MipsFrame.externalCall("malloc",[T.CONST(numFields*MipsFrame.wordsize)])
    val storeBaseAddr = T.MOVE(T.TEMP(baseAddr), getBaseAddr)
    val offset = ref 0
    fun genMove(offsetRef, field) = (offset := (!offset) + 1;
                                    T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP(baseAddr), T.CONST((!offset-1)*MipsFrame.wordsize))), field))
    val moveList = map genMove fieldList
    val statements = storeBaseAddr::moveList
  in
    T.ESEQ(T.seq(statements), T.TEMP(baseAddr))
  end

(*test and body are both Translate.exp*)
(*ldone is the done label for this loop, which is created in Semant
  because Semant needs it to be able to translate BreakExps*)
  fun whileLoop(test, body, ldone) =
  let
    val lbody = Temp.newlabel()
    val ltest = Temp.newlabel()
    val cond = unCx(test)
    val bodyNx = unNx(body)
    val jumpToTest = T.JUMP(T.NAME ltest, [ltest])
  in
    Tree.seq([T.LABEL ltest, cond(lbody, ldone), T.LABEL lbody, bodyNx, jumpToTest, T.LABEL ldone])
  end

  fun translateIfThenElse(test, thenExp, elseExp) =
  let val testCx = unCx(test)
      val thenEx = unEx(thenExp)
      val elseEx = unEx(elseExp)
      val tLabel = Temp.newlabel()
      val fLabel = Temp.newlabel()
      val retVal = Temp.newtemp()
      val joinLabel = Temp.newlabel()
      val join = T.JUMP(T.NAME(joinLabel),[joinLabel])
  in
      T.ESEQ(
              T.seq [testCx(tLabel, fLabel), T.LABEL(tLabel), T.MOVE(T.TEMP(retVal), thenEx), join,
                                             T.LABEL(fLabel), T.MOVE(T.TEMP(retVal), elseEx), join, T.LABEL(joinLabel)],
              T.TEMP(retVal))
  end

  fun translateIfThen(test, thenExp) =
  let val testCx = unCx(test)
      val thenEx = unEx(thenExp)
      val tLabel = Temp.newlabel()
      val fLabel = Temp.newlabel()
      val retVal = Temp.newtemp()
      val joinLabel = Temp.newlabel()
      val join = T.JUMP(T.NAME(joinLabel),[joinLabel])
  in
      T.ESEQ(
              T.seq [testCx(tLabel, fLabel), T.LABEL(tLabel), T.MOVE(T.TEMP(retVal), thenEx), join,
                                             T.LABEL(fLabel), join, T.LABEL(joinLabel)],
              T.TEMP(retVal))
  end


end
