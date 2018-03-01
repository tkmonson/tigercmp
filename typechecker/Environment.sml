structure S = Symbol

signature ENV =
sig
  type access
  type ty
  datatype enventry = VarEntry of {ty:ty}
                    | FunEntry of {formals: ty list, result:ty}

  val base_tenv : ty S.table
  val base_venv : enventry S.table

end

structure Env :> ENV =
struct

  type access = bool
  type ty = Types.ty

  datatype enventry = VarEntry of {ty:ty}
                    | FunEntry of {formals: ty list, result:ty}

  (*Basic type environment contains int and string*)
  val base_tenv = S.enter(S.enter(S.enter(S.empty, S.symbol "int", Types.INT), S.symbol "string", Types.STRING),S.symbol "nil", Types.NIL)

  val v1 = S.enter(S.empty, S.symbol "print", FunEntry {formals=[Types.STRING], result=Types.UNIT});
  val v1 = S.enter(v1, S.symbol "flush", FunEntry {formals=[], result=Types.UNIT});
  val v1 = S.enter(v1, S.symbol "getchar", FunEntry {formals=[], result=Types.STRING});
  val v1 = S.enter(v1, S.symbol "ord", FunEntry {formals=[Types.STRING], result=Types.INT});
  val v1 = S.enter(v1, S.symbol "chr", FunEntry {formals=[Types.INT], result=Types.STRING});
  val v1 = S.enter(v1, S.symbol "substring", FunEntry {formals=[Types.STRING, Types.INT, Types.INT], result=Types.STRING});
  val v1 = S.enter(v1, S.symbol "concat", FunEntry {formals=[Types.STRING, Types.STRING], result=Types.STRING});
  val v1 = S.enter(v1, S.symbol "not", FunEntry {formals=[Types.INT], result=Types.INT});
  val v1 = S.enter(v1, S.symbol "exit", FunEntry {formals=[Types.INT], result=Types.UNIT});

  val base_venv = v1

end
