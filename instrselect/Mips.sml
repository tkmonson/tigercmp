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
            | munchStm(T.LABEL(label)) = emit (A.LABEL{
                                            assem = label ^ ":\n",
                                            lab = label})
            | munchStm(T.MOVE(T.MEM exp1, exp2) = emit (A.OPER{
                                                     src=[munchExp exp1, munchExp exp2],
                                                     dst=[]
                                                     jump=[NONE]})
        (*SEQ of stm * stm
                     | LABEL of label
                     | JUMP of exp * label list
                     | CJUMP of relop * exp * exp * label * label
      	       | MOVE of exp * exp
                     | EXP of exp*)

        (*This function handles insn selection for a Tree.exp
            It returns the result of the exp in a Temp, and emits MIPS as a side-effect
            p205*)
        fun munchExp(T.CONST i) = result (fn r => emit(A.OPER {
                                                           assem="ADDI 'd0 <- 'r0+" ^ int i ^ "\n",
                                                           src=[],
                                                           dst=[r],
                                                           jump=NONE}))
            | munchExp(T.MEM(exp1)) = result (fn r => emit(A.OPER{
                                                             assem="LW 'do <- 0('s0)\n",
                                                             src=[munchExp exp1],
                                                             dst=[],
                                                             jump=NONE}))
              }))
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
