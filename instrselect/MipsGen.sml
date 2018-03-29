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
	    | munchExp (Tr.CALL(e,args)) = e

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
