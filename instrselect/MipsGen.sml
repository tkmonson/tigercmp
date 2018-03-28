structure MipsGen :> CODEGEN =
struct

structure Tr = Tree

  fun codegen(frame)(stm:Tree.stm) : Assem.instr list =
    let val ilist = ref (nil:Assem.instr)
        fun emit x = ilist := x :: !ilist
        fun result(gen) = let val t = Temp.newtemp() in gen t; t end

        (*This function emits MIPS for a Tree.stm as a side-effect. p. 204*)
        (*Returns unit*)
        fun munchStm(T.SEQ(stmA, stmB)) = (munchStm(stmA); munchStm(stmB))
            | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, exp1, T.CONST i)), exp2)) = emit (A.OPER{
                                                                                      assem="SW 's1 " ^ int i ^ "('s0')\n",
                                                                                      src=[munchExp exp1, munchExp exp2],
                                                                                      dst=[],
                                                                                      jump=NONE})
            | munchStm(T.MOVE(T.MEM(exp1), T.MEM(exp2))) = emit (A.OPER{
                                                   assem="SW 's0 's1\n",
                                                   src=[munchExp T.MEM(exp2), munchExp (exp1)],
                                                   dst=[]
                                                   jump=[NONE
            (*TODO: make sure left argument of T.MOVE can only be a T.MEM or temp*)
            | munchStm(T.MOVE(T.MEM (exp1), exp2) = emit (A.OPER{
                                                   assem="SW 's0 's1\n",
                                                   src=[munchExp exp2, munchExp exp1],
                                                   dst=[]
                                                   jump=[NONE]})
            | munchStm(T.MOVE(T.MEM(T.CONST i), exp1) = emit (A.OPER{
                                                   assem="SW 's0 " ^ int i + "('s1)\n",
                                                   src=[munchExp exp1, MipsFrame.RZ],
                                                   dst=[]
                                                   jump=[NONE]})
            | munchStm(T.MOVE(T.TEMP temp, exp1)) = emit (A.OPER{
                                                   assem="SW 's0 's1\n",
                                                   src=[munchExp exp1, munchExp exp2],
                                                   dst=[]
                                                   jump=[NONE]})
            | munchStm(T.LABEL(label)) = emit (A.LABEL{
                                            assem = label ^ ":\n",
                                            lab = label})
            | munchStm(T.EXP(e)) = (munchExp(e); ())
            | munchStm(T.LABEL(label)) = emit (A.LABEL{
                                            assem = label ^ ":\n",
                                            lab = label})
            (*TODO: Handle reg-mem, mem-reg, reg-reg moves as special cases*)
            | munchStm(T.MOVE(T.MEM exp1, exp2)) = emit (A.OPER{
                                                     src=[munchExp exp1, munchExp exp2],
                                                     dst=[]
                                                     jump=NONE})

            | munchStm(T.EXP(T.CALL(T.LABEL(l), argList))) = emit(A.OPER {
                                                               assem="jal" ^ Symbol.name l ^ "\n",
                                                               src=munchArgs argList,
                                                               dst=MipsFrame.calldefs,
                                                               jump=[l]})

            | munchStm(T.MOVE(T.TEMP t, T.CALL(T.LABEL(l), argList))) = munchStm T.EXP T.CALL(T.LABEL(l), argList);
                                                                        emit(A.MOVE {
                                                                          assem="move 'd0 's0  \n",
                                                                          src=MipsFrame.RA,
                                                                          dst=t,
                                                                          jump=[l]})

        (*This function handles insn selection for a Tree.exp
            It returns the result of the exp in a Temp, and emits MIPS as a side-effect. p. 205*)
        fun munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST i, exp1) = result (fn r => emit(A.OPER{
                                                                                    assem="LW 'd0 " ^ int i ^ "('s0)\n",
                                                                                    src=[munchExp exp1],
                                                                                    dst=[r],
                                                                                    jump=NONE}))
            | munchExp(T.MEM(T.BINOP(T.PLUS, exp1, T.CONST i) = result (fn r => emit(A.OPER{
                                                                                    assem="LW 'd0 " ^ int i ^ "('s0)\n",
                                                                                    src=[munchExp exp1],
                                                                                    dst=[r],
                                                                                    jump=NONE}))
            | munchExp(T.MEM(T.CONST i)) = result(fn r => emit(A.OPER{
                                                                   assem="LA 'd0 " ^ int i ^ "\n",
                                                                   src=[],
                                                                   dst=[r],
                                                                   jump=NONE}))
            (*TODO: Add special cases for MEM where exp1 is reg +- const or const +- reg*)
            | munchExp(T.MEM(exp1)) = result (fn r => emit(A.OPER{
                                                           assem="LW 'do 0('s0)\n",
                                                           src=[munchExp exp1],
                                                           dst=[r],
                                                           jump=NONE}))
            | munchExp (T.BINOP(T.PLUS, exp1, T.CONST i)) = result(fn r => emit(A.OPER{
                                                                                assem="ADDI 'd0 's0 " ^ int i ^ "\n",
                                                                                src=[munchExp exp1],
                                                                                dst=[r],
                                                                                jump=NONE}))
            | munchExp (T.BINOP(T.PLUS, T.CONST i. exp1)) = result(fn r => emit(A.OPER{
                                                                                assem="ADDI 'd0 's0 " ^ int i ^ "\n",
                                                                                src=[munchExp exp1],
                                                                                dst=[r],
                                                                                jump=NONE}))
            | munchExp(T.CONST i) = result (fn r => emit(A.OPER {
                                                         assem = "ADDI 'd0 'r0 " ^ int i ^ "\n",
                                                         (* assem="ADDI 'd0 <- 'r0+" ^ int i ^ "\n", *)
                                                         src=[],
                                                         dst=[r],
                                                         jump=NONE}))
            | munchExp(T.BINOP(T.PLUS, exp1, exp2)) = result (fun r => emit(A.OPER
                                                                            assem="ADD 'd0 's0 's1\n",
                                                                            src=[munchExp exp1, munchExp exp2],
                                                                            dst=[r],
                                                                            jump=NONE}))
            | munchExp(T.TEMP temp) = temp

            (*
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
  			        (munchStm(Tr.MOVE(dst, T.TEMP src)); src :: munchArgs(i+i,rest))
                 (*I (Saumya) think that munchArgs should return the src registers based on book p. 204*)
                 (*Tommy thinks it should be the dst registers*)

	        end



     in
      munchStm stm; rev(!ilist)
      end
end
