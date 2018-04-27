structure MipsGen =
struct

structure As = Assem
structure Tr = Tree

  (* Check the minus here *)
  fun int2str n = if n >= 0
      then Int.toString(n)
      else ("-" ^ Int.toString(0-n))

  fun codegen(frame)(stm:Tree.stm) : Assem.instr list =
  let val ilist = ref nil
  fun emit x = ilist := x :: !ilist
  fun result(gen) = let val t = Temp.newtemp() in gen t; t end

	(* On a function call, these registers are trashed: caller-saves, return address, return value *)
	val codedefs = [MipsFrame.t0,MipsFrame.t1,MipsFrame.t2,MipsFrame.t3,MipsFrame.t4,MipsFrame.t5,
			MipsFrame.t6,MipsFrame.t7,MipsFrame.t8,MipsFrame.t9,MipsFrame.RA,MipsFrame.v0]

            (*This function handles insn selection for a Tree.exp
                It returns the result of the exp in a Temp, and emits MIPS as a side-effect. p. 205*)
  fun munchExp(Tr.MEM(Tr.BINOP(Tr.PLUS, Tr.CONST i, exp1))) =
          result (fn r => emit(As.OPER{
                                 assem="lw `d0, " ^ int2str i ^ "(`s0)\n",
                                 src=[munchExp exp1],
                                 dst=[r],
                                 jump=NONE}))

      | munchExp(Tr.MEM(Tr.BINOP(Tr.PLUS, exp1, Tr.CONST i))) =
          result (fn r => emit(As.OPER{
                                 assem="lw `d0, " ^ int2str i ^ "(`s0)\n",
                                 src=[munchExp exp1],
                                 dst=[r],
                                 jump=NONE}))

      | munchExp(Tr.MEM(Tr.BINOP(Tr.MINUS, exp1, Tr.CONST i))) =
      result (fn r => emit(As.OPER{
                              assem="lw `d0, " ^ int2str (~i) ^ "(`s0)\n",
                              src=[munchExp exp1],
                              dst=[r],
                              jump=NONE}))

      | munchExp(Tr.MEM(Tr.BINOP(Tr.MINUS, Tr.CONST i, exp1))) =
      result (fn r => emit(As.OPER{
                              assem="lw `d0, " ^ int2str (~i) ^ "(`s0)\n",
                              src=[munchExp exp1],
                              dst=[r],
                              jump=NONE}))

      | munchExp(Tr.MEM(Tr.CONST i)) =
          result(fn r => emit(As.OPER{
                                assem="lw `d0 " ^ int2str i ^ "\n",
                                src=[],
                                dst=[r],
                                jump=NONE}))
      | munchExp(Tr.MEM(exp1)) =
          result (fn r => emit(As.OPER{
                                 assem="lw `d0, 0(`s0)\n",
                                 src=[munchExp exp1],
                                 dst=[r],
                                 jump=NONE}))

      | munchExp (Tr.BINOP(Tr.PLUS, exp1, Tr.CONST i)) =
          result(fn r => emit(As.OPER{
                                assem="addi `d0, `s0 " ^ int2str i ^ "\n",
                                src=[munchExp exp1],
                                dst=[r],
                                jump=NONE}))

      | munchExp (Tr.BINOP(Tr.PLUS, Tr.CONST i, exp1)) =
          result(fn r => emit(As.OPER{
                                assem="addi `d0, `s0 " ^ int2str i ^ "\n",
                                src=[munchExp exp1],
                                dst=[r],
                                jump=NONE}))

      | munchExp(Tr.BINOP(Tr.PLUS, exp1, exp2)) =
            result (fn r => emit(As.OPER{
                                    assem="add `d0, `s0 `s1\n",
                                    src=[munchExp exp1, munchExp exp2],
                                    dst=[r],
                                    jump=NONE}))

                 (* SUB *)
      | munchExp(Tr.BINOP(Tr.MINUS, exp1, Tr.CONST i)) =
        result (fn r => emit(As.OPER{
                                assem="addi `d0, `s0 " ^ int2str (~i)  ^ "\n",
                                src=[munchExp exp1],
                                dst=[r],
                                jump=NONE}))

      | munchExp(Tr.BINOP(Tr.MINUS, exp1, exp2)) =
        result (fn r => emit(As.OPER{
                                assem="sub `d0, `s0 `s1\n",
                                src=[munchExp exp1, munchExp exp2],
                                dst=[r],
                                jump=NONE}))

                 (* MUL *)
      | munchExp(Tr.BINOP(Tr.MUL, exp1, Tr.CONST i)) =
        result (fn r => emit(As.OPER{
                                assem="mul `d0, `s0 " ^ int2str i  ^ "\n",
                                src=[munchExp exp1],
                                dst=[r],
                                jump=NONE}))

      | munchExp(Tr.BINOP(Tr.MUL, Tr.CONST i, exp1)) =
            result (fn r => emit(As.OPER{
                                    assem="mul `d0, `s0 " ^ int2str i  ^ "\n",
                                    src=[munchExp exp1],
                                    dst=[r],
                                    jump=NONE}))

      | munchExp(Tr.BINOP(Tr.MUL, exp1, exp2)) =
            result (fn r => emit(As.OPER{
                                    assem="mul `d0, `s0 `s1\n",
                                    src=[munchExp exp1, munchExp exp2],
                                    dst=[r],
                                    jump=NONE}))

                 (* DIV *)
      | munchExp(Tr.BINOP(Tr.DIV, exp1, Tr.CONST i)) =
            result (fn r => emit(As.OPER{
                                    assem="div `d0, `s0 " ^ int2str i  ^ "\n",
                                    src=[munchExp exp1],
                                    dst=[r],
                                    jump=NONE}))

      | munchExp(Tr.BINOP(Tr.DIV, exp1, exp2)) =
            result (fn r => emit(As.OPER{
                                    assem="div `d0, `s0 `s1\n",
                                    src=[munchExp exp1, munchExp exp2],
                                    dst=[r],
                                    jump=NONE}))

                 (* AND *)
      | munchExp(Tr.BINOP(Tr.AND, exp1, Tr.CONST i)) =
            result (fn r => emit(As.OPER{
                                    assem="andi `d0, `s0, " ^ int2str i  ^ "\n",
                                    src=[munchExp exp1],
                                    dst=[r],
                                    jump=NONE}))

      | munchExp(Tr.BINOP(Tr.AND, Tr.CONST i, exp1)) =
            result (fn r => emit(As.OPER{
                                    assem="andi `d0, `s0 " ^ int2str i  ^ "\n",
                                    src=[munchExp exp1],
                                    dst=[r],
                                    jump=NONE}))

      | munchExp(Tr.BINOP(Tr.AND, exp1, exp2)) =
            result (fn r => emit(As.OPER{
                                    assem="and `d0, `s0 `s1\n",
                                    src=[munchExp exp1, munchExp exp2],
                                    dst=[r],
                                    jump=NONE}))

                 (* OR *)
      | munchExp(Tr.BINOP(Tr.OR, exp1, Tr.CONST i)) =
            result (fn r => emit(As.OPER{
                                    assem="ori `d0, `s0 " ^ int2str i  ^ "\n",
                                    src=[munchExp exp1],
                                    dst=[r],
                                    jump=NONE}))

      | munchExp(Tr.BINOP(Tr.OR, Tr.CONST i, exp1)) =
            result (fn r => emit(As.OPER{
                                    assem="ori `d0, `s0 " ^ int2str i  ^ "\n",
                                    src=[munchExp exp1],
                                    dst=[r],
                                    jump=NONE}))

      | munchExp(Tr.BINOP(Tr.OR, exp1, exp2)) =
            result (fn r => emit(As.OPER{
                                    assem="or `d0, `s0 `s1\n",
                                    src=[munchExp exp1, munchExp exp2],
                                    dst=[r],
                                    jump=NONE}))

                 (* XOR *)
      | munchExp(Tr.BINOP(Tr.XOR, exp, Tr.CONST i)) =
            result (fn r => emit(As.OPER{
                              assem="xori `d0, `s0 " ^ int2str i  ^ "\n",
                              src=[munchExp exp],
                              dst=[r],
                              jump=NONE}))

      | munchExp(Tr.BINOP(Tr.XOR, Tr.CONST i, exp1)) =
            result (fn r => emit(As.OPER{
                              assem="xori `d0, `s0 " ^ int2str i  ^ "\n",
                              src=[munchExp exp1],
                              dst=[r],
                              jump=NONE}))

      | munchExp(Tr.BINOP(Tr.XOR, exp1, exp2)) =
            result (fn r => emit(As.OPER{
                            assem="xor `d0, `s0 `s1\n",
                            src=[munchExp exp1, munchExp exp2],
                            dst=[r],
                            jump=NONE}))

                 (* SLL *)
      | munchExp(Tr.BINOP(Tr.LSHIFT, exp1, Tr.CONST i)) =
            result (fn r => emit(As.OPER{
                              assem="sll `d0, `s0 " ^ int2str i  ^ "\n",
                              src=[munchExp exp1],
                              dst=[r],
                              jump=NONE}))

      | munchExp(Tr.BINOP(Tr.LSHIFT, exp1, exp2)) =
            result (fn r => emit(As.OPER{
                              assem="sllv `d0, `s0, `s1\n",
                              src=[munchExp exp1, munchExp exp2],
                              dst=[r],
                              jump=NONE}))

                 (* SRL *)
      | munchExp(Tr.BINOP(Tr.RSHIFT, exp1, Tr.CONST i)) =
            result (fn r => emit(As.OPER{
                              assem="srl `d0, `s0 " ^ int2str i  ^ "\n",
                              src=[munchExp exp1],
                              dst=[r],
                              jump=NONE}))

      | munchExp(Tr.BINOP(Tr.RSHIFT, exp1, exp2)) =
            result (fn r => emit(As.OPER{
                              assem="srlv `d0, `s0 `s1\n",
                              src=[munchExp exp1, munchExp exp2],
                              dst=[r],
                              jump=NONE}))

                 (* SRA *)
      | munchExp(Tr.BINOP(Tr.ARSHIFT, exp1, Tr.CONST i)) =
            result (fn r => emit(As.OPER{
                                    assem="sra `d0, `s0 " ^ int2str i  ^ "\n",
                                    src=[munchExp exp1],
                                    dst=[r],
                                    jump=NONE}))

      | munchExp(Tr.BINOP(Tr.ARSHIFT, exp1, exp2)) =
            result (fn r => emit(As.OPER{
                                    assem="srav `d0, `s0 `s1\n",
                                    src=[munchExp exp1, munchExp exp2],
                                    dst=[r],
                                    jump=NONE}))

      | munchExp(Tr.TEMP temp) = temp

      (* NAME *)
      | munchExp(Tr.NAME label) =
            result (fn r => emit(As.OPER {
                                    assem="la `d0, " ^ Symbol.name label ^ "\n",
                                    src=[],
                                    dst=[r],
                                    jump=NONE}))

      | munchExp(Tr.CONST i) =
            result (fn r => emit(As.OPER {
                                 assem = "li `d0, " ^ int2str i ^ "\n",
                                 src=[],
                                 dst=[r],
                                 jump=NONE}))

      | munchExp(Tr.ESEQ(_,_)) = (Semant.printError("Encountered an ESEQ in insn sel, shouldn't happen.", 0); Temp.newtemp())

      | munchExp (Tr.CALL(exp1, args)) =
        (let
            val callerSaves = map MipsFrame.getTemp MipsFrame.callerSaves
            val tempPairs = map (fn r => (Temp.newtemp(), r)) callerSaves
            val () = MipsFrame.setLeaf(frame, false)
            val () = print "Not a leaf!\n"
            val () = MipsFrame.setOutgoingArgs(frame, List.length args)
            fun store t r = Tr.MOVE(Tr.TEMP t, Tr.TEMP r)
        in
            (* Since we're not doing spilling, we don't want to save the caller-saves! (map (fn (t,r) => munchStm(store t r)) tempPairs; *)
            (result (fn r => emit(As.OPER{
                                    assem="jal `s0\n",
                                    src=munchExp(exp1) :: munchArgs(0,args),
                                    dst=codedefs,
                                    jump=NONE}));
            (* map (fn (t,r) => munchStm(store r t)) tempPairs; *)
            MipsFrame.v0)
        end)

        (*This function emits MIPS for a Tree.stm as a side-effect. p. 204*)
        (*Returns unit*)
        and munchStm(Tr.SEQ(stmA, stmB)) = (munchStm(stmA); munchStm(stmB))

            (*All moves into memory*)
            | munchStm(Tr.MOVE(Tr.MEM(Tr.BINOP(Tr.PLUS, exp1, Tr.CONST i)), exp2)) = emit (As.OPER{
                                                                                              assem="sw `s1, " ^ int2str i ^ "(`s0)\n",
                                                                                              src=[munchExp exp1, munchExp exp2],
                                                                                              dst=[],
                                                                                              jump=NONE})
            | munchStm(Tr.MOVE(Tr.MEM(Tr.BINOP(Tr.PLUS, Tr.CONST i, exp1)), exp2)) = emit (As.OPER{
                                                                                              assem="sw `s1, " ^ int2str i ^ "(`s0)\n",
                                                                                              src=[munchExp exp1, munchExp exp2],
                                                                                              dst=[],
                                                                                              jump=NONE})
            | munchStm(Tr.MOVE(Tr.MEM(Tr.BINOP(Tr.MINUS, exp1, Tr.CONST i)), exp2)) = emit (As.OPER{
                                                                                              assem="sw `s1, " ^ int2str(~i) ^ "(`s0)\n",
                                                                                              src=[munchExp exp1, munchExp exp2],
                                                                                              dst=[],
                                                                                              jump=NONE})
            | munchStm(Tr.MOVE(Tr.MEM(exp1), Tr.MEM(exp2))) = emit (As.OPER{
                                                                    assem="sw `s0, `s1\n",
                                                                    src=[munchExp (Tr.MEM(exp2)), munchExp (exp1)],
                                                                    dst=[],
                                                                    jump=NONE})

            | munchStm(Tr.MOVE(Tr.MEM(Tr.CONST i), exp1)) = emit (As.OPER{
                                                                  assem="sw `s0, " ^ int2str i ^ "(`s1)\n",
                                                                  src=[munchExp exp1, MipsFrame.RZ],
                                                                  dst=[],
                                                                  jump=NONE})

            | munchStm(Tr.MOVE(Tr.MEM(exp1), exp2)) = emit (As.OPER{
                                                           assem="sw `s0, `s1\n",
                                                           src=[munchExp exp2, munchExp exp1],
                                                           dst=[],
                                                           jump=NONE})

            (*All moves from memory*)
            | munchStm(Tr.MOVE(Tr.TEMP t, Tr.MEM(Tr.BINOP(Tr.PLUS, exp1, Tr.CONST i)))) = emit (As.OPER{
                                                                                          assem="lw `d0, " ^ int2str i ^ "(`s0)\n",
                                                                                          src=[munchExp exp1],
                                                                                          dst=[t],
                                                                                          jump=NONE})

            | munchStm(Tr.MOVE(Tr.TEMP t, Tr.MEM(Tr.BINOP(Tr.PLUS, Tr.CONST i, exp1)))) = emit (As.OPER{
                                                                                          assem="lw `d0, " ^ int2str i ^ "(`s0)\n",
                                                                                          src=[munchExp exp1],
                                                                                          dst=[t],
                                                                                          jump=NONE})

            | munchStm(Tr.MOVE(Tr.TEMP t, Tr.MEM(Tr.BINOP(Tr.MINUS, exp1, Tr.CONST i)))) = emit (As.OPER{
                                                                                          assem="lw `d0, " ^ int2str (~i) ^ "(`s0)\n",
                                                                                          src=[munchExp exp1],
                                                                                          dst=[t],
                                                                                          jump=NONE})

            | munchStm(Tr.MOVE(Tr.TEMP t, Tr.MEM(Tr.CONST i))) = emit (As.OPER{
                                                                          assem="lw `d0, " ^ int2str (i) ^ "(`s0)\n",
                                                                          src=[MipsFrame.RZ],
                                                                          dst=[t],
                                                                          jump=NONE})

            | munchStm(Tr.MOVE(Tr.TEMP t, Tr.MEM exp1)) = emit (As.OPER{
                                                                          assem="lw `d0, (`s0)\n",
                                                                          src=[munchExp exp1],
                                                                          dst=[t],
                                                                          jump=NONE})

            (*Moves that don't involve memory*)
            | munchStm(Tr.MOVE(Tr.TEMP t, Tr.CONST i)) =  emit (As.OPER{
                                                              assem="li `d0, " ^ int2str i ^ "\n",
                                                              src=[],
                                                              dst=[t],
                                                              jump=NONE})

            | munchStm(Tr.MOVE(Tr.TEMP t, Tr.NAME l)) =  emit (As.OPER{
                                                             assem="la `d0, " ^ Symbol.name l ^ "\n",
                                                             src=[],
                                                             dst=[t],
                                                             jump=NONE})

            | munchStm(Tr.MOVE(Tr.TEMP t, Tr.CALL(Tr.NAME(l), argList))) = munchStm(Tr.MOVE(Tr.TEMP t, Tr.TEMP (munchExp(Tr.CALL(Tr.NAME(l), argList)))))

            | munchStm(Tr.MOVE(Tr.TEMP temp, exp1)) = emit(As.MOVE {
                                                              assem="move `d0, `s0  \n",
                                                              src=munchExp exp1,
                                                              dst=temp})

            | munchStm(Tr.MOVE(_,_)) = Semant.printError("Trying to move into some exp that's not a temp or mem. Should never happen in well-typed code.",0)

            | munchStm(Tr.EXP(Tr.CALL(Tr.NAME(l), argList))) = (munchExp(Tr.CALL(Tr.NAME(l), argList)); ())

            | munchStm(Tr.CJUMP(relop, exp1, exp2, label1, label2)) =
                  (let val instr =
                      case relop of
                                 Tr.EQ => "beq"
                               | Tr.NE => "bne"
                               | Tr.LT => "blt"
                               | Tr.GT => "bgt"
                               | Tr.LE => "ble"
                               | Tr.GE => "bge"
                               | Tr.ULT => "bltu"
                               | Tr.ULE => "bleu"
                               | Tr.UGT => "bgtu"
                               | Tr.UGE => "bgeu"
                 in emit (As.OPER{
                          assem = instr ^ " `s0, `s1 " ^ S.name label1 ^"\n",
                          src=[munchExp exp1, munchExp exp2],
                          dst=[],
                          jump=SOME[label1, label2]})
                 end)

            | munchStm(Tr.JUMP(Tr.TEMP t, labels)) = emit(As.OPER {
                                                    assem="jr `s0 \n",
                                                    src=[t],
                                                    dst=[],
                                                    jump=SOME(labels)})

            | munchStm(Tr.JUMP(exp, labels)) = emit(As.OPER {
                                                    assem="j `s0 \n",
                                                    src=[munchExp exp],
                                                    dst=[],
                                                    jump=SOME(labels)})

            | munchStm(Tr.LABEL(label)) = emit (As.LABEL{
                                              assem = Symbol.name label ^ ":\n",
                                              lab = label})

            | munchStm(Tr.EXP(e)) = (munchExp(e); ())

        (*This function helps handle function arguments for a procedure call stm
          It emits code to move the args into arg registers and the stack
          It returns a list of all temps that will be passed to the CALL.
          These come from calling munchExp on the args. p. 204 *)
      and munchArgs (i, arg::rest) =
            let val dst = MipsFrame.getCallerArgLoc(i)
                val src = munchExp(arg)
            in
            munchStm(Tr.MOVE(dst, Tr.TEMP src));

            case dst of
            Tree.TEMP(t) => t :: munchArgs(i+1,rest)
            |_           => munchArgs(i+1, rest)
            end
          | munchArgs(i,[]) = []

      in (munchStm(stm); rev(!ilist)) end

    fun printTemp t =
        let
          val nameOpt = Temp.Map.find(MipsFrame.tempMap, t)
        in
          case nameOpt of
            SOME(name) => name
            | NONE     => Temp.makestring t
        end

    (*Save/restore RA*)
    fun handleRA(insns, frame) =
    let
      val RAlocation = MipsFrame.allocRA(frame)
      val saveRA = [Assem.OPER{assem="sw `s0 " ^ int2str RAlocation ^ "(`s1)\n",
                              src=[MipsFrame.RA, MipsFrame.FP],
                              dst=[],
                              jump=NONE}]
      val restoreRA = [Assem.OPER{assem="lw `s0 " ^ int2str RAlocation ^ "(`s1)\n",
                              src=[MipsFrame.RA, MipsFrame.FP],
                              dst=[],
                              jump=NONE}]
    in
      saveRA @ insns @ restoreRA
    end

    (*Save/restore FP, SP*)
    fun handleFP(insns, frame) =
    let val FPlocation = MipsFrame.allocFP(frame)
        val frameSize = MipsFrame.allocOutgoingArgs(frame)
        val saveFP = [Assem.OPER{assem="sw `s0 " ^ int2str FPlocation ^ "(`s1)\n",
                                src=[MipsFrame.FP, MipsFrame.SP],
                                dst=[],
                                jump=NONE}]
        val setFP = [Assem.OPER{assem="move `d0, `s0\n",
                                src=[MipsFrame.SP],
                                dst=[MipsFrame.FP],
                                jump=NONE}]
        val setSP = [Assem.OPER{assem="addi `d0, `s0 " ^ int2str(frameSize) ^ "\n",
                                src=[MipsFrame.FP],
                                dst=[MipsFrame.SP],
                                jump=NONE}]
        val restoreSP = [Assem.OPER{assem="move `d0, `s0\n",
                                src=[MipsFrame.FP],
                                dst=[MipsFrame.SP],
                                jump=NONE}]
        val restoreFP = [Assem.OPER{assem="lw `d0 " ^ int2str FPlocation ^ "(`s0)\n",
                                src=[MipsFrame.SP],
                                dst=[MipsFrame.FP],
                                jump=NONE}]
    in
      saveFP @ setFP @ setSP @ insns @ restoreSP @ restoreFP
    end

    (*Prints assembly for a single fragment*)
    fun emitproc out (MipsFrame.PROC{body,frame}) =
            let val stms   = Canon.linearize body
                val () = Printtree.printtree(TextIO.stdOut, body)
                val stms'  = Canon.traceSchedule(Canon.basicBlocks stms)
                val instrs = List.concat(map (codegen frame) stms')
                val format0 = Assem.format(printTemp)
                (* val () = Printtree.printtree(out,body) *)
            in
              app (fn i => TextIO.output(out,format0 i)) instrs
            end
        | emitproc out (MipsFrame.STRING(lab,s)) =  ()

    (*Prints assembly for a list of fragments*)
    (*Actual top level*)
    fun transFrags fraglist = app (emitproc TextIO.stdOut) fraglist

    (*Prints assembly for all fragments of a Tiger file*)
    (*For testing only*)
    fun transProg filename =
        let val mainLevel = R.newLevel({parent=R.outermost, name=Symbol.symbol "tig_main", formals=[]})
            val prog = (Parse.parse filename)
            val findEscapes = FindEscape.findEscape prog
            val {ty=progTy, exp=progIR} = (Semant.transExp(Env.base_venv, Env.base_tenv, mainLevel, false, Temp.newlabel()) (prog))
            val makeFrag = R.makeTopLevelFrag(progIR, mainLevel)
            val (procfrags, stringfrags) = R.getResult()
            val () = Translate.procfrags := nil
            val () = Translate.stringfrags := nil
        in
          transFrags procfrags
        end

   (*Returns a list of Assem.instr list for a fragment*)
   (*Adds prolog and epilog*)
   fun getInstrList (MipsFrame.PROC{body,frame}) =
           let val stms   = Canon.linearize body
               val stms'  = Canon.traceSchedule(Canon.basicBlocks stms)
               val instrs = List.concat(map (codegen frame) stms')
               val lab = [List.hd(instrs)]
               val code = List.tl(instrs)
               val code' = handleRA(code, frame)
               val code'' = handleFP(code', frame)
               val jumpToReturn = [Assem.OPER{assem="jr `s0\n",
                                       src=[MipsFrame.RA],
                                       dst=[],
                                       jump=NONE}]
           in
             MipsFrame.procEntryExit2(frame, lab @ code'' @ jumpToReturn)
           end
       | getInstrList (MipsFrame.STRING(lab,s)) =  []

    (*Returns an Assem.instr list list*)
    (*One list per fragment in a Tiger program*)
    (*Also returns all the string fragments*)
    fun transFrags filename =
      let val mainLevel = R.newLevel({parent=R.outermost, name=Symbol.symbol "tig_main", formals=[]})
          val prog = (Parse.parse filename)
          val findEscapes = FindEscape.findEscape prog
          val {ty=progTy, exp=progIR} = (Semant.transExp(Env.base_venv, Env.base_tenv, mainLevel, false, Temp.newlabel()) (prog))
          val () = R.makeTopLevelFrag(progIR, mainLevel)
          val (procfrags, stringfrags) = R.getResult()
          val () = Translate.procfrags := nil
          val () = Translate.stringfrags := nil
          val fragInstrs = map getInstrList procfrags
       in
          (fragInstrs, stringfrags)
       end
end
