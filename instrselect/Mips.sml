structure codegen =
struct

  fun codegen(frame)(stm:Tree.stm) : Assem.instr list =
    let val ilist = ref (nil:Assem.instr)
        fun emit x = ilist := x :: !ilist
        fun result(gen) = let val t = Temp.newtemp() in gen t; t end

        (*This function helps handle function arguments for a procedure call stm
          It emits code to move the args into arg registers and the stack
          It returns a list of all temps that will be passed to the CALL. These come from calling munchExp on the args.
          p204 *)
        fun munchArgs(argNum, args) =

        (*This function emits MIPS for a Tree.stm as a side-effect
            p204*)
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

        (*SEQ of stm * stm
                     | LABEL of label
                     | JUMP of exp * label list
                     | CJUMP of relop * exp * exp * label * label
      	       | MOVE of exp * exp
                     | EXP of exp*)

        (*This function handles insn selection for a Tree.exp
            It returns the result of the exp in a Temp, and emits MIPS as a side-effect
            p205*)
        fun  munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST i, exp1) = result (fn r => emit(A.OPER{
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
        (*BINOP of binop * exp * exp
                | MEM of exp
                | TEMP of Temp.temp
                | ESEQ of stm * exp
                | NAME of label
                | CONST of int
 	       | CALL of exp * exp list*)
     in
      munchStm stm; rev(!ilist)
end
