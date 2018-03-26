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
        fun munchStm(a) =

        (*This function handles insn selection for a Tree.exp
            It returns the result of the exp in a Temp, and emits MIPS as a side-effect
            p205*)
        fun munchExp(a) =
     in
      munchStm stm; rev(!ilist)
end
