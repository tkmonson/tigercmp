structure MipsGen :> CODEGEN =
struct

structure As = Assem

  fun codegen(frame)(stm:Tree.stm) : Assem.instr list =
    let val ilist = ref (nil:Assem.instr)
        fun emit x = ilist := x :: !ilist
				       
	(* Check the minus here *)
	fun int2str n = if n >= 0
			then Int.toString(n)
			else ("-" ^ Int.toString(0-n))

        fun result(gen) = let val t = Temp.newtemp() in gen t; t end

	(* On a function call, these registers are trashed: caller-saves, return address, return value *)
	val codedefs = [Frame.t0,Frame.t1,Frame.t2,Frame.t3,Frame.t4,Frame.t5,
			Frame.t6,Frame.t7,Frame.t8,Frame.t9,Frame.RA,Frame.v0]

        (*This function emits MIPS for a Tree.stm as a side-effect. p. 204*)
        (*Returns unit*)
        fun munchStm(Tr.SEQ(stmA, stmB)) = (munchStm(stmA); munchStm(stmB))
            | munchStm(Tr.MOVE(Tr.MEM(Tr.BINOP(Tr.PLUS, exp1, Tr.CONST i)), exp2)) = emit (As.OPER{
                                                                                      assem="SW 's1 " ^ int i ^ "('s0')\n",
                                                                                      src=[munchExp exp1, munchExp exp2],
                                                                                      dst=[],
                                                                                      jump=NONE})
            | munchStm(Tr.MOVE(exp1, Tr.MEM(Tr.BINOP(Tr.PLUS, exp2, Tr.CONST i)))) = emit (As.OPER{
                                                                                      assem="LW 's0 " ^ int i ^ "('s1')\n",
                                                                                      src=[munchExp exp1, munchExp exp2],
                                                                                      dst=[],
                                                                                      jump=NONE})
            | munchStm(Tr.MOVE(Tr.MEM(Tr.CONST i), exp1)) = emit (As.OPER{
                                                   assem="SW 's0 " ^ int i + "('s1)\n",
                                                   src=[munchExp exp1, MipsFrame.RZ],
                                                   dst=[]
                                                   jump=NONE})
            (*TODO: make sure left argument of Tr.MOVE can only be a Tr.MEM or temp*)
            | munchStm(Tr.MOVE(Tr.MEM (exp1), exp2)) = emit (As.OPER{
                                                 assem="SW 's0 's1\n",
                                                 src=[munchExp exp2, munchExp exp1],
                                                 dst=[]
                                                 jump=NONE})
            | munchStm(Tr.MOVE(Tr.TEMP temp, exp1)) = emit (As.MOVE{
                                                   assem="move 's0 's1\n",
                                                   src=[temp, munchExp exp1],
                                                   dst=[]
                                                   jump=NONE})
            | munchStm(Tr.MOVE(_,_)) = Semant.printError("Trying to move into some exp that's not a temp or mem.
                                                          Should never happen in well-typed code.",0)
            | munchStm(Tr.LABEL(label)) = emit (As.LABEL{
                                            assem = label ^ ":\n",
                                            lab = label})
            | munchStm(Tr.EXP(e)) = (munchExp(e); ())
            | munchStm(Tr.LABEL(label)) = emit (As.LABEL{
                                            assem = label ^ ":\n",
                                            lab = label})

            | munchStm(Tr.EXP(Tr.CALL(Tr.LABEL(l), argList))) = emit(As.OPER {
                                                               assem="jal" ^ Symbol.name l ^ "\n",
                                                               src=munchArgs argList,
                                                               dst=MipsFrame.calldefs,
                                                               jump=SOME([l])})

            | munchStm(Tr.MOVE(Tr.TEMP t, Tr.CALL(Tr.LABEL(l), argList))) = (munchStm (Tr.EXP(Tr.CALL(Tr.LABEL(l), argList)));
                                                                        emit(As.MOVE {
                                                                          assem="move 'd0 's0  \n",
                                                                          src=MipsFrame.RA,
                                                                          dst=t,
                                                                          jump=[l]}))

          (*This function handles insn selection for a Tree.exp
            It returns the result of the exp in a Temp, and emits MIPS as a side-effect. p. 205*)

	    (* CONST *)
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
				      assem="la `d0, " ^ Symbol.name label
  			              src=[]
				      dst=[r]
				      jump=NONE}))	     
		
            (* MEM *)
            | munchExp (Tr.MEM e) =
	      result (fn r => emit(As.OPER{
                                      assem="lw `d0, 0(`s0)\n",
                                      src=[munchExp e],
                                      dst=[],
                                      jump=NONE}))
		     
	    | munchExp (Tr.MEM(Tr.BINOP(Tr.PLUS,e,Tr.CONST i))) =
	      result (fn r => emit(As.OPER{
                                      assem="lw `d0, " ^ int2str i ^ "(`s0)\n",
                                      src=[munchExp e],
                                      dst=[],
                                      jump=NONE}))

	    | munchExp (Tr.MEM(Tr.BINOP(Tr.PLUS,Tr.CONST i,e))) =
	      result (fn r => emit(As.OPER{
                                      assem="lw `d0, " ^ int2str i ^ "(`s0)\n",
                                      src=[munchExp e],
                                      dst=[],
                                      jump=NONE}))

	    | munchExp (Tr.MEM(Tr.BINOP(Tr.MINUS,e,Tr.CONST i))) =
	      result (fn r => emit(As.OPER{
                                      assem="lw `d0, " ^ int2str (~i) ^ "(`s0)\n",
                                      src=[munchExp e],
                                      dst=[],
                                      jump=NONE}))

	    | munchExp (Tr.MEM(Tr.BINOP(Tr.MINUS,Tr.CONST i,e))) =
	      result (fn r => emit(As.OPER{
                                      assem="lw `d0, " ^ int2str (~i) ^ "(`s0)\n",
                                      src=[munchExp e],
                                      dst=[],
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
				      dst=[r],
		                      jump=NONE}))
		     
	    | munchExp (Tr.BINOP(Tr.ARSHIFT,e1,e2)) =
	      result (fn r => emit(As.OPER{
				      assem="srav `d0, `s0, `s1\n",
		                      src=[munchExp e1, munchExp e2],
				      dst=[r],
		                      jump=NONE}))
		     
            (* TODO: CALL *)
	    | munchExp (Tr.CALL(e,args)) =
	      let
                  val callerSaves = [Frame.t0,Frame.t1,Frame.t2,Frame.t3,Frame.t4,
				     Frame.t5,Frame.t6,Frame.t7,Frame.t8,Frame.t9]
		  val tempPairs = map (fn r => (Temp.newtemp(), r)) callerSaves
		  fun store t r = Tr.MOVE(Tr.TEMP t, Tr.TEMP r)
	      in
		  map (fn (t,r) => munchStm(store t r)) tempPairs;
                  result (fn r => emit(As.OPER{
				      assem="jalr `s0, `d0\n",
		                      src=munchExp(e) :: munchArgs(0,args),
				      dst=calldefs,
		                      jump=NONE}));
		  map (fn (t,r) => munchStm(store r t)) tempPairs;
		  Frame.v0
	      end

        (*This function helps handle function arguments for a procedure call stm
          It emits code to move the args into arg registers and the stack
          It returns a list of all temps that will be passed to the CALL.
          These come from calling munchExp on the args. p. 204 *)
        fun munchArgs (i, arg::rest) =
            let val dst = MipsFrame.getCallerArgLoc(i)
                val src = munchExp(arg)
            in
  	        munchStm(Tr.MOVE(dst, T.TEMP src));
  	        src :: munchArgs(i+i,rest)
	    end


    in
        munchStm stm; rev(!ilist)

    end
