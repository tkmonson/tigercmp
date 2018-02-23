structure A = Absyn

(*A dummy Translate structure to use for this step*)
structure Translate = struct type exp = unit end

(*A defintion of expty that uses the dummy Translate for now*)
type expty = {exp: Translate.exp, ty:Types.ty}

(*Top level function: Calls side-effecting function transExp, and then returns unit*)
fun transProg exp = transExp (Env.base_venv, Env.base_tenv) exp; ()

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

fun processTypeDecHead (tenv, t(tlist{name: symbol, ty: ty, pos: pos}):A.TypeDec) = ()
(*for each tydec*)

(*take header, represent as ty, add to tenv'*)


(***Thnk about functional implementation of name***)
fun processTypeDecBody (tenv, t(tlist{name: symbol, ty: ty, pos: pos}):A.TypeDec) = ()
(*Turn ty record from absyn into ty.RECORD from types.sml*)

(***Property of Saums****)
fun transVar (venv, tenv, v:A.var) = ()
