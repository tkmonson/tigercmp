structure A = Absyn
structure S = Symbol
structure E = Environment
structure T = Types

(*A dummy Translate structure to use for this step*)
structure Translate = struct type exp = unit end

(*A defintion of expty that uses the dummy Translate for now*)
type expty = {exp: Translate.exp, ty:Types.ty}

(*Top level function: Calls side-effecting function transExp, and then returns unit*)
fun transProg exp = transExp (E.base_venv, E.base_tenv) exp; ()

(*
* transExp is side-effecting: It prints error messages, and returns trexp
* transExp: (venv*tenv) -> (A.exp -> expty)
* trexp: A.exp -> expty
* trvar: A.var -> expty
*)
fun transExp (venv, tenv) =
  let fun trexp (e: A.exp) = {exp=(), ty=Types.INT}
  and trvar (v:A.var) = ()
  in
  trexp
  end

fun transDecs (venv, tenv, d:A.dec list) = ()
  (*For each item in d*)
    (*Pattern match on the 3 different types of A.dec*)
    (*CASE:
          vardec: augment venv with new variable)
          FunctionDec: Call helper processFunDec that augments environments and then typecheck functions with the augmented envs
          TypeDec: Call helper processTypeDec that augments environments and then typecheck with the augmented envs *)

fun processFunDecHead (venv, tenv, f(flist):A.FunctionDec) =
(*for each in flist*)
(*take header, represent as funentry, add to venv*)

(*AFTER all are processed, call processFunDecBody*)

(****Takes entire list of fundecs to process bodies****)
(***Property of Tommy****)
fun processFunDecBody (venv, tenv, f(flist):A.FunctionDec) =

(*call transexp on exp in fundec body passed (v/t)env*)


(***Explained on page 120***)
(***Property of Anita****)
fun transTy (tenv, t:A.ty) = ()
(*call processTypeDecHead, add 'tenv to tenv*)

(*Somewhere processes body, revisit helper function needs*)

(*take header, represent as ty, add to tenv'*)
fun processTypeDecHead (tenv, []) = tenv
  | processTypeDecHead (tenv, ({n, t, p}::l):A.TypeDec) = A.NameTy(s,pos) => (S.enter (tenv, n, Types.NAME(n, ref NONE), l)

(*Question for TA: Can we have a TypeDec like

type a = {l:list}
type list = ...

In this case, our existing code would break

*)
fun recTyFromFlist (tenv, []) = []
    | recTyFromFlist (tenv, {n, e, t, p}:A.field::l) = (n, S.look tenv)::recTyFromFlist(l)


(*Converts a Absyn.ty to a Types.ty*)
 fun translateTypeDec t:A.ty

(***Thnk about functional implementation of name***)
fun processTypeDecBody (tenv, t(tlist{name: symbol, ty: ty, pos: pos}):A.TypeDec) = ()
(*Turn ty record from absyn into ty.RECORD from types.sml*)
case t of
    A.NameTy(s,pos) => (S.enter (tenv, n, Types.NAME(n, ref NONE), l)
    A.RecordTy(flist) => (S.enter (tenv, n, Types.RECORD ([], ref ()), l)
    A.ArrayTy(s,pos) => (S.enter (tenv, n, Types.ARRAY(T.UNIT, ref ()), l)

(***Property of Saums****)
(*  venv*tenv*A.var -> Types.ty *)
(*Tells you the type of a variable*)
fun transVar (venv, tenv, A.SubscriptVar(v,e,p)) = actualType (tenv, lookupArrayType (transVar v))
  | transVar (venv, tenv, A.FieldVar(v,s,p)) =   actualType (tenv, lookupFieldType (transVar v, s))
  | transVar (venv, tenv, A.SimpleVar(s,p)) = actualType (tenv, S.look (venv, s))

(* tenv*Types.ty -> Types.ty*)
(*For named types, this function looks up the "actual" type*)
(*TODO: Add error message to this in NONE case*)
fun actualType (tenv:Types.ty Symbol.table, Types.NAME(s,t)) = (case Symbol.look (tenv, s) of
                                                               SOME x => actualType (tenv, x)
                                                             | NONE => Types.UNIT)
  | actualType (tenv:Types.ty Symbol.table, t:Types.ty) = t

(*Types.ty -> Types.ty*)
(*Simple helper that tells you the type of object stored in an array*)
(*TODO: Error message when argument is not of type Types.ARRAY*)
fun lookupArrayType Types.ARRAY (ty, u) = ty
  | lookupArrayType a:Types.ty = Types.UNIT

fun lookupFieldType (Types.RECORD(fieldlist, u), s) = traverseFieldList (flist, s)
  | lookupFieldType (a:Types.ty, s) = Types.UNIT

  fun traverseFieldList ([], s:S.symbol) = Types.UNIT
    | traverseFieldList ((s1:S.symbol, t:Types.ty)::l, s2:S.symbol) = if s1=s2 then t else traverseFieldList (l, s2)
