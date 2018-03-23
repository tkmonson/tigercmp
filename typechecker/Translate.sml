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

structure Tr = Tree

  (*Associated with a function *)
  (*Static link: One example of static link following is MEM(MEM(FP))*)
  (*See pg 143 for description of outermost level*)
  datatype level = makeLevel of {frame:MipsFrame.frame, parent:level, unq: unit ref}
                 | outermost

  (* Associated with a variable *)
  datatype access = makeAccess of {acc:MipsFrame.access, lev:level}

  datatype exp = Ex of Tr.exp
                |Nx of Tr.stm
                |Cx of Temp.label*Temp.label -> Tr.stm

  (*This function calls Frame.newFrame to create a frame with the formals and a static link*)
  fun newLevel ({parent:level, name:Temp.label, formals:bool list}) =
    let
      val fr = MipsFrame.newFrame{formals=formals, name=Temp.newlabel()}
    in
    makeLevel{frame=fr, parent=parent,unq=ref ()}
    end

 (*Given the level for some function, formals returns the Translate.access for each of its formals, except for the static link*)
  fun formals (makeLevel{frame=frame, parent=parent, unq=unq}) =
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

 (*Give two levels, returns IR sequence that traverses from one level to another*)
  fun traverseLevels(makeLevel{frame=accessFrame, parent=accessParent, unq=accessUnq},
                     makeLevel{frame=curFrame, parent=curParent, unq=curUnq}) =
    if curUnq = accessUnq then Tr.TEMP(MipsFrame.FP)
                          else let val subexp = traverseLevels(makeLevel{frame=accessFrame, parent=accessParent, unq=accessUnq}, curParent)
                          in Tr.CONST(1)
                          end

  (* exp -> Tr.exp *)
    fun unEx (Ex e) = e
      | unEx (Nx s) = Tr.ESEQ(s,Tr.CONST 0)
      | unEx (Cx genstm) =
          let val r = Temp.newtemp()
              val t = Temp.newlabel() and f = Temp.newlabel()
          in Tr.ESEQ(Tr.seq[Tr.MOVE(Tr.TEMP r, Tr.CONST 1),
  		        genstm(t,f),
  		        Tr.LABEL f,
  		        Tr.MOVE(Tr.TEMP r, Tr.CONST 0),
  		        Tr.LABEL t],
  	                Tr.TEMP r)
          end

  (* exp -> Tree.stm *)
  fun unNx (Ex e) = Tr.EXP e
    | unNx (Nx s) = s
    | unNx (Cx c) = let val t = Temp.newlabel() in c(t,t); Tr.LABEL t end

  (* exp -> (Temp.label * Temp.label -> Tree.stm) *)
  fun unCx (Ex (T.CONST 0)) = (fn(t,f) => T.JUMP(T.NAME f, [f]))
    | unCx (Ex (T.CONST 1)) = (fn(t,f) => T.JUMP(T.NAME t, [t]))
    | unCx (Ex e) = (fn (t,f) => T.CJUMP(T.NE, e, T.CONST 0, t, f))
    | unCx (Nx _) = raise ErrorMsg.Error
    | unCx (Cx c) = c

  (* binop and relop handle OpExp *)
  fun binop (oper,lexp,rexp) : exp =
      let
	  val TreeOper =
	      case oper of
		  Absyn.PlusOp => Tr.PLUS
		| Absyn.MinusOp => Tr.MINUS
		| Absyn.TimesOp => Tr.MUL
		| Absyn.DivideOp => Tr.DIV
      in
	  Ex(Tr.BINOP(TreeOper,unEx(lexp),unEx(rexp)))
      end

  fun relop (oper,lexp,rexp) : exp =
      let
	  val TreeOper =
	      case oper of
		  Absyn.EqOp => Tr.EQ
		| Absyn.NeqOp => Tr.NE
		| Absyn.LtOp => Tr.LT
		| Absyn.LeOp => Tr.LE
		| Absyn.GtOp => Tr.GT
		| Absyn.GeOp => Tr.GE
      in
	  Cx(fn (t,f) => Tr.CJUMP(TreeOper,unEx(lexp),unEx(rexp),t,f))
      end

  (*First argument is the access representing where a variable was declared*)
  (*Second argument is the level where the variable is being accessed*)
    fun simpleVar(makeAccess{acc=accessType, lev=accLevel as makeLevel{frame=accessFrame, parent=accessParent, unq=accessUnq}},
                  makeLevel{frame=curFrame, parent=curParent, unq=curUnq}) =
          let
            val exp = case accessType of
                            MipsFrame.InReg(t) => Tr.CONST 0
                          | MipsFrame.InFrame(offset) => traverseLevels(makeLevel{frame=accessFrame, parent=accessParent, unq=accessUnq},
                                                              makeLevel{frame=curFrame, parent=curParent, unq=curUnq})
          in
            MipsFrame.exp(accessType)(exp)
          end

    val NilExp = Ex(Tr.CONST 0)

    fun IntExp (n:int) : exp = Ex(Tr.CONST n)

    fun StringExp (s:string) : unit = ()

  (*Translation for an ArrayExp*)
  (*Assume that the external function initArray will initialize the array and return its base addr in a temp*)
  (*Stores array size in index -1 *)
  fun arrayCreate(size, initValue) =
  let
   val baseAddr = Temp.newtemp()
   val getBaseAddr = MipsFrame.externalCall("initArray", [T.BINOP(T.PLUS, T.CONST 1, unEx(size)), unEx(initValue)])
   val storeBaseAddr = T.MOVE(T.TEMP(baseAddr), T.BINOP(T.PLUS, T.CONST 4, getBaseAddr))
   val storeArrSize = T.MOVE(T.MEM(T.BINOP(T.MINUS, T.TEMP(baseAddr), T.CONST 4)), unEx(size))
  in T.ESEQ(T.seq([storeBaseAddr, storeArrSize]), T.TEMP(baseAddr))
  end

 (*Translation for a RecordExp*)
 (*Assume that malloc returns base address in a temp*)
 (*WARNING: This function uses REFS for convenience...Saumya got lazy CLASSIC*)
  fun recCreate(fieldList, numFields) =
   let
    val baseAddr = Temp.newtemp()
    val getBaseAddr = MipsFrame.externalCall("malloc",[Tr.CONST(numFields*MipsFrame.wordsize)])
    val storeBaseAddr = Tr.MOVE(Tr.TEMP(baseAddr), getBaseAddr)
    val offset = ref 0
    fun genMove(offsetRef, field) = (offset := (!offset) + 1;
                                    Tr.MOVE(Tr.MEM(Tr.BINOP(Tr.PLUS, Tr.TEMP(baseAddr), Tr.CONST((!offset-1)*MipsFrame.wordsize))), field))
    val moveList = map genMove fieldList
    val statements = storeBaseAddr::moveList
  in
    Tr.ESEQ(Tr.seq(statements), Tr.TEMP(baseAddr))
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
      val jumpToTest = Tr.JUMP(Tr.NAME ltest, [ltest])
    in
      Tree.seq([Tr.LABEL ltest, cond(lbody, ldone), Tr.LABEL lbody, bodyNx, jumpToTest, Tr.LABEL ldone])
    end
  (*make eseq to turn everything but last one into statement and return last one as exp*)
  fun seqExp (elist) = Tr.ESEQ (Tr.seq (map unNx (List.take (elist, (List.length elist) - 1))), unEx (List.last(elist)))

  (*location comes from transvar call in semant, exp comes from recursivecall in semant*)
  fun assignExp (location, exp) =
      Tr.MOVE(Tr.MEM(location), unEx(exp))

  (*just go to label from while loop*)
  (* fun breakExp(label) = Tr.JUMP(label,[label])*)

  (*need level where fun was declared, then level where it was called*)
  (*Need level of f and level of fn calling f to compute static link*)
  fun callExp (makeLevel{frame=frame, parent=level, unq=_}, currLevel, label:Temp.label, argExpList) =
      Tr.CALL(Tr.NAME label, (traverseLevels(level, currLevel)::(map unEx argExpList)))

  (*return exp of assignment expression to initialize var
    do we call assignExp on the var...?*)
  (* fun varDec (translatedExp, ) = assignExp(translatedExp, ) *)
  (*translate.alloc local in Semant to create frame
    transvar called to accumulate list of exps*)

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



  (* If index < 0: Jump to ifBelowZer
     else:         Jump to ifAboveZero

     ifBelowZero: exit
     ifAboveZero: if index >= array size jump to ifOutOFBounds
                  else:                  jump to allGood
    ifOutOFBounds: exit
    allGood: Do nothing

  *)
  fun subscript(baseAddr, index) =
  let
    val arrSize = Tr.MEM(Tr.BINOP(Tr.MINUS, unEx(baseAddr), Tr.CONST 4))
    val ifBelowZero = Temp.newlabel()
    val ifAboveZero = Temp.newlabel()
    val ifOutOFBounds = Temp.newlabel()
    val allGood = Temp.newlabel()
    val checkOutOfBounds = Tr.CJUMP(Tr.GE, unEx(index), arrSize, ifOutOFBounds, allGood)
    val checkBelowZero = Tr.CJUMP(Tr.LT, unEx(index), Tr.CONST 0, ifBelowZero, ifAboveZero)
    val retVal = Tr.MEM(Tr.BINOP(Tr.PLUS, unEx(baseAddr), unEx(index)))
    val exit = Tr.EXP(MipsFrame.externalCall("exit", [Tr.CONST 1]))
  in
    Tr.ESEQ(Tr.seq [checkBelowZero, Tr.LABEL ifBelowZero, exit, Tr.LABEL ifAboveZero,
                    checkOutOfBounds, Tr.LABEL ifOutOFBounds, exit, Tr.LABEL allGood],
           retVal)
  end



end
