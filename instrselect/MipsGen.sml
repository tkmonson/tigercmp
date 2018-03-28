structure MipsGen :> CODEGEN =
struct

  fun codegen(frame)(stm:Tree.stm) : Assem.instr list =
    let val ilist = ref (nil:Assem.instr)
        fun emit x = ilist := x :: !ilist
	fun int2str n = if n >= 0
			then Int.toString(n)
			else ("-" ^ Int.toString(n))
        fun result(gen) = let val t = Temp.newtemp() in gen t; t end

        (*This function emits MIPS for a Tree.stm as a side-effect. p. 204*)
        (*Returns unit*)
        fun munchStm(Tr.SEQ(stmA, stmB)) = (munchStm(stmA); munchStm(stmB))
            | munchStm(Tr.EXP(e)) = (munchExp(e); ())
            | munchStm(Tr.LABEL(label)) = emit (A.LABEL{
                                            assem = label ^ ":\n",
                                            lab = label})
            (*TODO: Handle reg-mem, mem-reg, reg-reg moves as special cases*)
            | munchStm(Tr.MOVE(T.MEM exp1, exp2) = emit (A.OPER{
                                                     src=[munchExp exp1, munchExp exp2],
                                                     dst=[]
                                                     jump=[NONE]})

            | munchStm(Tr.EXP(T.CALL(T.LABEL(l), argList))) = emit(A.OPER {
                                                               assem="jal" ^ Symbol.name l ^ "\n",
                                                               src=munchArgs argList,
                                                               dst=MipsFrame.calldefs,
                                                               jump=[l]})

        (*This function handles insn selection for a Tree.exp
            It returns the result of the exp in a Temp, and emits MIPS as a side-effect. p. 205*)
        fun munchExp (Tr.CONST i) = result (fn r => emit(A.OPER {
                                                           assem="ADDI 'd0 <- 'r0+" ^ int i ^ "\n",
                                                           src=[],
                                                           dst=[r],
                                                           jump=NONE}))
            (*TODO: Add special cases for MEM where exp1 is reg +- const or const +- reg*)
            | munchExp (Tr.MEM(exp1)) = result (fn r => emit(A.OPER{
                                                             assem="LW 'do <- 0('s0)\n",
                                                             src=[munchExp exp1],
                                                             dst=[],
                                                             jump=NONE}))
              }))
	    | munchExp (Tr.TEMP temp) = temp

            (* binop -- add *)
            | munchExp (Tr.BINOP(Tr.PLUS,e1,Tr.CONST i)) =
	      result (fn r => emit(A.OPER
				       {assem="addi `d0,`s0," ^ int2str i  ^ "\n",
		                        src=[munchExp e1], dst=[r],
		                        jump=NONE}))
					    
	    | munchExp (Tr.BINOP(Tr.PLUS,Tr.CONST i,e1)) =
	      result (fn r => emit(A.OPER
				       {assem="addi `d0,`s0," ^ int2str i  ^ "\n",
		                        src=[munchExp e1], dst=[r],
		                        jump=NONE}))
					    
	    | munchExp (Tr.BINOP(Tr.PLUS,e1,e2)) =
	      result (fn r => emit(A.OPER
				       {assem="add `d0,`s0,`s1\n",
		                        src=[munchExp e1, munchExp e2], dst=[r],
		                        jump=NONE}))
					    
	    (* binop -- sub *)
	    | munchExp (Tr.BINOP(Tr.MINUS,e1,Tr.CONST i)) =
	      result (fn r => emit(A.OPER
				       {assem="addi `d0,`s0," ^ int2str (-i)  ^ "\n",
		                        src=[munchExp e1], dst=[r],
		                        jump=NONE}))
					    
	    | munchExp (Tr.BINOP(Tr.MINUS,e1,e2)) =
	      result (fn r => emit(A.OPER
				       {assem="sub `d0,`s0,`s1\n",
		                        src=[munchExp e1, munchExp e2], dst=[r],
		                        jump=NONE}))

	    (* binop -- mul *)
	    | munchExp (Tr.BINOP(Tr.MUL,e1,Tr.CONST i)) =
	      result (fn r => emit(A.OPER
				       {assem="mul `d0,`s0," ^ int2str i  ^ "\n",
		                        src=[munchExp e1], dst=[r],
		                        jump=NONE}))
		     
	    | munchExp (Tr.BINOP(Tr.MUL,Tr.CONST i,e1)) =
	      result (fn r => emit(A.OPER
				       {assem="mul `d0,`s0," ^ int2str i  ^ "\n",
		                        src=[munchExp e1], dst=[r],
		                        jump=NONE}))
					    
	    | munchExp (Tr.BINOP(Tr.MUL,e1,e2)) =
	      result (fn r => emit(A.OPER
				       {assem="mul `d0,`s0,`s1\n",
		                        src=[munchExp e1, munchExp e2], dst=[r],
		                        jump=NONE}))

	    (* binop -- div *)
	    | munchExp (Tr.BINOP(Tr.DIV,e1,Tr.CONST i)) =
	      result (fn r => emit(A.OPER
				       {assem="div `d0,`s0," ^ int2str i  ^ "\n",
		                        src=[munchExp e1], dst=[r],
		                        jump=NONE}))
					    
	    | munchExp (Tr.BINOP(Tr.DIV,e1,e2)) =
	      result (fn r => emit(A.OPER
				       {assem="div `d0,`s0,`s1\n",
		                        src=[munchExp e1, munchExp e2], dst=[r],
		                        jump=NONE}))

	    (* binop -- and *)
	    | munchExp (Tr.BINOP(Tr.AND,e1,Tr.CONST i)) =
	      result (fn r => emit(A.OPER
				       {assem="andi `d0,`s0," ^ int2str i  ^ "\n",
		                        src=[munchExp e1], dst=[r],
		                        jump=NONE}))
		     
	    | munchExp (Tr.BINOP(Tr.AND,Tr.CONST i,e1)) =
	      result (fn r => emit(A.OPER
				       {assem="andi `d0,`s0," ^ int2str i  ^ "\n",
		                        src=[munchExp e1], dst=[r],
		                        jump=NONE}))
					    
	    | munchExp (Tr.BINOP(Tr.AND,e1,e2)) =
	      result (fn r => emit(A.OPER
				       {assem="and `d0,`s0,`s1\n",
		                        src=[munchExp e1, munchExp e2], dst=[r],
		                        jump=NONE}))

	    (* binop -- or *)
	    | munchExp (Tr.BINOP(Tr.OR,e1,Tr.CONST i)) =
	      result (fn r => emit(A.OPER
				       {assem="ori `d0,`s0," ^ int2str i  ^ "\n",
		                        src=[munchExp e1], dst=[r],
		                        jump=NONE}))
		     
	    | munchExp (Tr.BINOP(Tr.OR,Tr.CONST i,e1)) =
	      result (fn r => emit(A.OPER
				       {assem="ori `d0,`s0," ^ int2str i  ^ "\n",
		                        src=[munchExp e1], dst=[r],
		                        jump=NONE}))
					    
	    | munchExp (Tr.BINOP(Tr.OR,e1,e2)) =
	      result (fn r => emit(A.OPER
				       {assem="or `d0,`s0,`s1\n",
		                        src=[munchExp e1, munchExp e2], dst=[r],
		                        jump=NONE}))

					    
	    (*BINOP of binop * exp * exp
                | MEM of exp
                | TEMP of Temp.temp
                | ESEQ of stm * exp
                | NAME of label
                | CONST of int
 	       | CALL of exp * exp list*)

        (*This function helps handle function arguments for a procedure call stm
          It emits code to move the args into arg registers and the stack
          It returns a list of all temps that will be passed to the CALL.
          These come from calling munchExp on the args. p. 204 *)
        fun munchArgs (i, arg::rest) =
        let val dst = MipsFrame.getCallerArgLoc(i)
            val src = munchExp(arg)
        in
  			     munchStm(Tr.MOVE(dst, T.TEMP src));
             (*I (Saumya) think that munchArgs should return the src registers based on book p. 204*)
             (*Tommy thinks it should be the dst registers*)
  			     src :: munchArgs(i+i,rest)
	    end



     in
      munchStm stm; rev(!ilist)
end
