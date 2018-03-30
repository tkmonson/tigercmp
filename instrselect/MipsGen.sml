
structure MipsGen =
struct

structure As = Assem
structure Tr = Tree

  fun codegen(frame)(stm:Tree.stm) : Assem.instr list =
  let val ilist = ref nil
  fun emit x = ilist := x :: !ilist

	(* Check the minus here *)
	fun int2str n = if n >= 0
			then Int.toString(n)
			else ("-" ^ Int.toString(0-n))

  fun result(gen) = let val t = Temp.newtemp() in gen t; t end

	(* On a function call, these registers are trashed: caller-saves, return address, return value *)
	val codedefs = [MipsFrame.t0,MipsFrame.t1,MipsFrame.t2,MipsFrame.t3,MipsFrame.t4,MipsFrame.t5,
			MipsFrame.t6,MipsFrame.t7,MipsFrame.t8,MipsFrame.t9,MipsFrame.RA,MipsFrame.v0]

        (*This function handles insn selection for a Tree.exp
            It returns the result of the exp in a Temp, and emits MIPS as a side-effect. p. 205*)

  fun munchExp (Tr.CONST i) =
	         result (fn r => emit(As.OPER {
                                      assem="addi `d0, `r0, " ^ int2str i ^ "\n",
                                      src=[],
                                      dst=[r],
                                      jump=NONE}))

            (* TEMP *)
	    | munchExp (Tr.TEMP temp) = temp

	    (* NAME *)
	    | munchExp (Tr.NAME label) =
	      result (fn r => emit(As.OPER {
				      assem="la `d0, " ^ Symbol.name label,
  			      src=[],
				      dst=[r],
				      jump=NONE}))

            (* MEM *)
	    | munchExp (Tr.MEM(Tr.BINOP(Tr.PLUS,e,Tr.CONST i))) =
	      result (fn r => emit(As.OPER{
                                      assem="lw `d0, " ^ int2str i ^ "(`s0)\n",
                                      src=[munchExp e],
                                      dst=[r],
                                      jump=NONE}))

	    | munchExp (Tr.MEM(Tr.BINOP(Tr.PLUS,Tr.CONST i,e))) =
	      result (fn r => emit(As.OPER{
                                      assem="lw `d0, " ^ int2str i ^ "(`s0)\n",
                                      src=[munchExp e],
                                      dst=[r],
                                      jump=NONE}))

	    | munchExp (Tr.MEM(Tr.BINOP(Tr.MINUS,e,Tr.CONST i))) =
	      result (fn r => emit(As.OPER{
                                      assem="lw `d0, " ^ int2str (~i) ^ "(`s0)\n",
                                      src=[munchExp e],
                                      dst=[r],
                                      jump=NONE}))

	    | munchExp (Tr.MEM(Tr.BINOP(Tr.MINUS,Tr.CONST i,e))) =
	      result (fn r => emit(As.OPER{
                                      assem="lw `d0, " ^ int2str (~i) ^ "(`s0)\n",
                                      src=[munchExp e],
                                      dst=[r],
                                      jump=NONE}))
		     
            | munchExp (Tr.MEM e) =
              result (fn r => emit(As.OPER{
                                      assem="lw `d0, 0(`s0)\n",
                                      src=[munchExp e],
                                      dst=[r],
                                      jump=NONE}))
            (* BINOP *)
                       (* ADD *)
            | munchExp (Tr.BINOP(Tr.PLUS,e,Tr.CONST i)) =
	      result (fn r => emit(As.OPER{
				      assem="addi `d0, `s0, " ^ int2str i  ^ "\n",
		                      src=[munchExp e],
				      dst=[r],
		                      jump=NONE}))

	    | munchExp (Tr.BINOP(Tr.PLUS,Tr.CONST i,e)) =
	      result (fn r => emit(As.OPER{
			              assem="addi `d0, `s0, " ^ int2str i  ^ "\n",
		                      src=[munchExp e],
				      dst=[r],
		                      jump=NONE}))

	    | munchExp (Tr.BINOP(Tr.PLUS,e1,e2)) =
	      result (fn r => emit(As.OPER{
				      assem="add `d0, `s0, `s1\n",
		                      src=[munchExp e1, munchExp e2],
				      dst=[r],
		                      jump=NONE}))

	               (* SUB *)
	    | munchExp (Tr.BINOP(Tr.MINUS,e,Tr.CONST i)) =
	      result (fn r => emit(As.OPER{
			              assem="addi `d0, `s0, " ^ int2str (~i)  ^ "\n",
		                      src=[munchExp e],
				      dst=[r],
		                      jump=NONE}))

	    | munchExp (Tr.BINOP(Tr.MINUS,e1,e2)) =
	      result (fn r => emit(As.OPER{
			              assem="sub `d0, `s0, `s1\n",
		                      src=[munchExp e1, munchExp e2],
				      dst=[r],
		                      jump=NONE}))

	               (* MUL *)
	    | munchExp (Tr.BINOP(Tr.MUL,e,Tr.CONST i)) =
	      result (fn r => emit(As.OPER{
				      assem="mul `d0, `s0, " ^ int2str i  ^ "\n",
		                      src=[munchExp e],
				      dst=[r],
		                      jump=NONE}))

	    | munchExp (Tr.BINOP(Tr.MUL,Tr.CONST i,e)) =
	      result (fn r => emit(As.OPER{
				      assem="mul `d0, `s0, " ^ int2str i  ^ "\n",
		                      src=[munchExp e],
				      dst=[r],
		                      jump=NONE}))

	    | munchExp (Tr.BINOP(Tr.MUL,e1,e2)) =
	      result (fn r => emit(As.OPER{
				      assem="mul `d0, `s0, `s1\n",
		                      src=[munchExp e1, munchExp e2],
				      dst=[r],
		                      jump=NONE}))

	               (* DIV *)
	    | munchExp (Tr.BINOP(Tr.DIV,e,Tr.CONST i)) =
	      result (fn r => emit(As.OPER{
				      assem="div `d0, `s0, " ^ int2str i  ^ "\n",
		                      src=[munchExp e],
				      dst=[r],
		                      jump=NONE}))

	    | munchExp (Tr.BINOP(Tr.DIV,e1,e2)) =
	      result (fn r => emit(As.OPER{
				      assem="div `d0, `s0, `s1\n",
		                      src=[munchExp e1, munchExp e2],
				      dst=[r],
		                      jump=NONE}))

	               (* AND *)
	    | munchExp (Tr.BINOP(Tr.AND,e,Tr.CONST i)) =
	      result (fn r => emit(As.OPER{
				      assem="andi `d0, `s0, " ^ int2str i  ^ "\n",
		                      src=[munchExp e],
				      dst=[r],
		                      jump=NONE}))

	    | munchExp (Tr.BINOP(Tr.AND,Tr.CONST i,e)) =
	      result (fn r => emit(As.OPER{
				      assem="andi `d0, `s0, " ^ int2str i  ^ "\n",
		                      src=[munchExp e],
				      dst=[r],
		                      jump=NONE}))

	    | munchExp (Tr.BINOP(Tr.AND,e1,e2)) =
	      result (fn r => emit(As.OPER{
				      assem="and `d0, `s0, `s1\n",
		                      src=[munchExp e1, munchExp e2],
				      dst=[r],
		                      jump=NONE}))

	               (* OR *)
	    | munchExp (Tr.BINOP(Tr.OR,e,Tr.CONST i)) =
	      result (fn r => emit(As.OPER{
				      assem="ori `d0, `s0, " ^ int2str i  ^ "\n",
		                      src=[munchExp e],
				      dst=[r],
		                      jump=NONE}))

	    | munchExp (Tr.BINOP(Tr.OR,Tr.CONST i,e)) =
	      result (fn r => emit(As.OPER{
				      assem="ori `d0, `s0, " ^ int2str i  ^ "\n",
		                      src=[munchExp e],
				      dst=[r],
		                      jump=NONE}))

	    | munchExp (Tr.BINOP(Tr.OR,e1,e2)) =
	      result (fn r => emit(As.OPER{
				      assem="or `d0, `s0, `s1\n",
		                      src=[munchExp e1, munchExp e2],
				      dst=[r],
		                      jump=NONE}))

	               (* XOR *)
	    | munchExp (Tr.BINOP(Tr.XOR,e,Tr.CONST i)) =
	      result (fn r => emit(As.OPER{
				      assem="xori `d0, `s0, " ^ int2str i  ^ "\n",
		                      src=[munchExp e],
				      dst=[r],
		                      jump=NONE}))

	    | munchExp (Tr.BINOP(Tr.XOR,Tr.CONST i,e)) =
	      result (fn r => emit(As.OPER{
				      assem="xori `d0, `s0, " ^ int2str i  ^ "\n",
		                      src=[munchExp e],
				      dst=[r],
		                      jump=NONE}))

	    | munchExp (Tr.BINOP(Tr.XOR,e1,e2)) =
	      result (fn r => emit(As.OPER{
				      assem="xor `d0, `s0, `s1\n",
		                      src=[munchExp e1, munchExp e2],
				      dst=[r],
		                      jump=NONE}))

	               (* SLL *)
	    | munchExp (Tr.BINOP(Tr.LSHIFT,e,Tr.CONST i)) =
	      result (fn r => emit(As.OPER{
				      assem="sll `d0, `s0, " ^ int2str i  ^ "\n",
		                      src=[munchExp e],
				      dst=[r],
		                      jump=NONE}))

	    | munchExp (Tr.BINOP(Tr.LSHIFT,e1,e2)) =
	      result (fn r => emit(As.OPER{
				      assem="sllv `d0, `s0, `s1\n",
		                      src=[munchExp e1, munchExp e2],
				      dst=[r],
		                      jump=NONE}))

	               (* SRL *)
	    | munchExp (Tr.BINOP(Tr.RSHIFT,e,Tr.CONST i)) =
	      result (fn r => emit(As.OPER{
				      assem="srl `d0, `s0, " ^ int2str i  ^ "\n",
		                      src=[munchExp e],
				      dst=[r],
		                      jump=NONE}))

	    | munchExp (Tr.BINOP(Tr.RSHIFT,e1,e2)) =
	      result (fn r => emit(As.OPER{
				      assem="srlv `d0, `s0, `s1\n",
		                      src=[munchExp e1, munchExp e2],
				      dst=[r],
		                      jump=NONE}))

	               (* SRA *)
	    | munchExp (Tr.BINOP(Tr.ARSHIFT,e,Tr.CONST i)) =
	      result (fn r => emit(As.OPER{
				      assem="sra `d0, `s0, " ^ int2str i  ^ "\n",
		                      src=[munchExp e],
				      dst=[r]
		                      jump=NONE}))

	    | munchExp (Tr.BINOP(Tr.ARSHIFT,e1,e2)) =
	      result (fn r => emit(As.OPER{
				      assem="srav `d0, `s0, `s1\n",
		                      src=[munchExp e1, munchExp e2],
				      dst=[r],
		                      jump=NONE}))

      | munchExp(Tr.ESEQ(_,_)) = (Semant.printError("Encountered an ESEQ in insn sel, shouldn't happen.", 0); Temp.newtemp())

            (* TODO: CALL *)
            (*QUESTION: Do we need this, or will Ch 8 make sure that we never get to this point?*)
	    | munchExp (Tr.CALL(e,args)) =
	      let
		  val callerSaves = map MipsFrame.getTemp MipsFrame.callerSaves
		  val tempPairs = map (fn r => (Temp.newtemp(), r)) callerSaves
		  fun store t r = Tr.MOVE(Tr.TEMP t, Tr.TEMP r)
	      in
		  map (fn (t,r) => munchStm(store t r)) tempPairs;
                  result (fn r => emit(As.OPER{
				      assem="jalr `s0, `d0\n",
		                      src=munchExp(e) :: munchArgs(0,args),
				      dst=codedefs,
		                      jump=NONE}));
		  map (fn (t,r) => munchStm(store r t)) tempPairs;
		  MipsFrame.v0
	      end

        (*This function helps handle function arguments for a procedure call stm
          It emits code to move the args into arg registers and the stack
          It returns a list of all temps that will be passed to the CALL.
          These come from calling munchExp on the args. p. 204 *)
      and munchArgs (i, arg::rest) =
            let val dst = MipsFrame.getCallerArgLoc(i)
                val src = munchExp(arg)
            in
            munchStm(Tr.MOVE(dst, Tr.TEMP src));
            src :: munchArgs(i+i,rest)
            end
        | munchArgs(i,[]) = []

        (*This function emits MIPS for a Tree.stm as a side-effect. p. 204*)
        (*Returns unit*)
      and munchStm(Tr.SEQ(stmA, stmB)) = (munchStm(stmA); munchStm(stmB))
					     
        | munchStm(Tr.MOVE(Tr.MEM(Tr.BINOP(Tr.PLUS, exp1, Tr.CONST i)), exp2)) = emit (As.OPER{
                                                                                      assem="SW 's1 " ^ int2str i ^ "('s0')\n",
                                                                                      src=[munchExp exp1, munchExp exp2],
                                                                                      dst=[],
                                                                                      jump=NONE})
											  
        | munchStm(Tr.MOVE(exp1, Tr.MEM(Tr.BINOP(Tr.PLUS, exp2, Tr.CONST i)))) = emit (As.OPER{
                                                                                      assem="LW 's0 " ^ int2str i ^ "('s1')\n",
                                                                                      src=[munchExp exp1, munchExp exp2],
                                                                                      dst=[],
                                                                                      jump=NONE})
											  
        | munchStm(Tr.MOVE(Tr.MEM(Tr.CONST i), exp1)) = emit (As.OPER{
                                                   assem="SW 's0 " ^ int2str i ^ "('s1)\n",
                                                   src=[munchExp exp1, MipsFrame.RZ],
                                                   dst=[],
                                                   jump=NONE})
								 
        (*TODO: make sure left argument of Tr.MOVE can only be a Tr.MEM or temp*)
        | munchStm(Tr.MOVE(Tr.MEM (exp1), exp2)) = emit (As.OPER{
                                                 assem="SW 's0 's1\n",
                                                 src=[munchExp exp2, munchExp exp1],
                                                 dst=[],
                                                 jump=NONE})
							    
        | munchStm(Tr.MOVE(Tr.TEMP t, Tr.CALL(Tr.NAME(l), argList))) = (munchStm (Tr.EXP(Tr.CALL(Tr.NAME(l), argList)));
                                                                       emit(As.MOVE {
                                                                               assem="move 'd0 's0  \n",
                                                                               src=munchExp(Tr.CALL(Tr.NAME(l), argList))),
                                                                               dst=t}))
									       
        | munchStm(Tr.MOVE(Tr.TEMP temp, exp1)) = emit (As.MOVE{
                                                               assem="move 's0 's1\n",
                                                               src=munchExp exp1,
                                                               dst=temp})
							   
        | munchStm(Tr.MOVE(_,_)) = Semant.printError("Trying to move into some exp that's not a temp or mem. Should never happen in well-typed code.",0)
        | munchStm(Tr.LABEL(label)) = emit (As.LABEL{
                                                   assem = Symbol.name label ^ ":\n",
                                                   lab = label})
					       
        | munchStm(Tr.EXP(Tr.CALL(Tr.NAME(l), argList))) = munchExp(Tr.CALL(Tr.NAME(l), argList))); ()
								   
        | munchStm(Tr.EXP(e)) = (munchExp(e); ())
    in
        munchStm stm; rev(!ilist)

    end

    fun printTemp t =
    let
      val tempSymbol = Symbol.symbol (Temp.makestring t)
      val nameOpt = Symbol.look(MipsFrame.tempMap, tempSymbol)
    in
      case nameOpt of
        SOME(name) => name
      | NONE       => Temp.makestring t
    end

    fun emitproc out (MipsFrame.PROC{body,frame}) =
    let val stms   = Canon.linearize body
        val stms'  = Canon.traceSchedule(Canon.basicBlocks stms)
        val instrs = List.concat(map (codegen frame) stms')
        val format0 = Assem.format(printTemp)
    in
      app (fn i => TextIO.output(out,format0 i)) instrs
    end
    |   emitproc out (MipsFrame.STRING(lab,s)) =  ()

    fun transProg filename =
    let val mainLevel = R.newLevel({parent=R.outermost, name=Symbol.symbol "tig_main", formals=[]})
        val prog = (Parse.parse filename)
        val findEscapes = FindEscape.findEscape prog
        val {ty=progTy, exp=progIR} = Semant.transExp(Env.base_venv, Env.base_tenv, mainLevel, false, Temp.newlabel()) (prog)
        val makeFrag = R.makeFunction(progIR, mainLevel)
        val fragList = R.getResult()
    in
      app (emitproc TextIO.stdOut) fragList
    end



end
