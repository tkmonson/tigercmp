signature TREE =
sig
  type label = Temp.label
  type size

  datatype stm = SEQ of stm * stm
               | LABEL of label
               | JUMP of exp * label list
               | CJUMP of relop * exp * exp * label * label
	       | MOVE of exp * exp
               | EXP of exp

       and exp = BINOP of binop * exp * exp
               | MEM of exp
               | TEMP of Temp.temp
               | ESEQ of stm * exp
               | NAME of label
               | CONST of int
	       | CALL of exp * exp list

       and binop = PLUS | MINUS | MUL | DIV
                   | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

       and relop = EQ | NE | LT | GT | LE | GE
	           | ULT | ULE | UGT | UGE

  val seq: stm list -> stm
  val notRel : relop -> relop
  (*val commute: exp * exp -> bool*)
end

structure Tree : TREE =
struct
  type label=Temp.label
  type size = int

  datatype stm = SEQ of stm * stm
               | LABEL of label
               | JUMP of exp * label list
               | CJUMP of relop * exp * exp * label * label
	       | MOVE of exp * exp
               | EXP of exp

       and exp = BINOP of binop * exp * exp
               | MEM of exp
               | TEMP of Temp.temp
               | ESEQ of stm * exp
               | NAME of label
               | CONST of int
	       | CALL of exp * exp list

       and binop = PLUS | MINUS | MUL | DIV
                   | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

       and relop = EQ | NE | LT | GT | LE | GE
	           | ULT | ULE | UGT | UGE

       (*WARNING: Nonexhaustive because we should never have a seq of < 2 stms TODO: throw error*)
       fun seq(s1::s2::[]) = SEQ(s1, s2)
          |seq(s1::s2::l)  = seq(SEQ(s1, s2)::l)

       (* Is this correct?  *)
       fun notRel (r:relop) : relop =
           case r of
	       EQ => NE
	      | NE => EQ
	      | LT => GE
	      | GT => LE
	      | LE => GT
	      | GE => LT
	      | ULT => UGE
        |       UGT => ULE
        |       ULE => UGT
	      | UGE => ULT

       (*fun commute (r1:relop,r2:relop) : relop = EQ*)
end
