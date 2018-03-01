structure A = Absyn
structure S = Symbol
structure E = Environment
structure T = Types

(*A dummy Translate structure to use for this step*)
structure Translate = struct type exp = unit end

(*A defintion of expty that uses the dummy Translate for now*)
type expty = {exp: Translate.exp, ty:Types.ty}

(*Top level function: Calls side-effecting function transExp, and then returns unit*)
(**)
fun transProg exp = transExp (E.base_venv, E.base_tenv) exp; ()

(*
* transExp is side-effecting: It prints error messages, and returns trexp
* transExp: (venv*tenv) -> (A.exp -> expty)
* trexp: A.exp -> expty
* trvar: A.var -> T.ty
*)
fun transExp (venv, tenv) =
  let fun trexp A.NilExp = {exp = (), ty = T.Nil}
        | trexp A.IntExp(num) = {exp = (), ty = T.INT}
        | trexp A.StringExp(s,p) = {exp = (), ty = T.STRING}
        | trexp A.VarExp(v) = {exp = (), ty = trvar v}
        | trexp A.OpExp{left=l, oper=o, right=r, pos=p} = (checkInt(trexp l, p); checkInt(trexp r, p); {exp = (), ty = T.INT})
        | trexp A.LetExp{decs=d, body=b, pos=p} =
                let val {venv', tenv'} = transDecs (venv, tenv, d)
                in  transExp(venv', tenv') b
        | trexp A.SeqExp(elist) = trseq elist
        | trexp A.RecordExp{fields=flist, typ=t, pos=p} = (*Just lookup t and make sure it's a Types.RECORD*)
        | trexp A.AssignExp{var=v,exp=e,pos=p} = (*if v is in venv then check it against e
                                                  else add it to venv with type of e*)

        (*Below this is property of ANITA. No trespassing.*)                                          
        | trexp A.IfExp {test=t, then'=thencase, else'=elsecase, pos=p} = (*Check that t is an int, thencase and elsecase have the same type*)
        | trexp A.WhileExp{test=t, body=b, pos=p} =
        | trexp A.ForExp{var=v, escape=e, lo=l, hi=h, body=b, pos=p} =
        | trexp A.ArrayExp{typ=t, size=s, init=i, pos=p} = (*Just lookup t and make sure it's a Types.ARRAY*)
  and trvar v:A.var = (transVar (venv, tenv, v))
  and trseq [] = {exp=(), ty=T.UNIT}
    | trseq a::[] = trexp a
    | trseq a::l::[] = (trexp a; trseq l) (*Call trexp on a for side effects*)
  and checkInt({exp=e, ty=t}, pos) = if t = T.INT then () else (*Add error message here*)()
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

fun processTypeDecHead (tenv, []) = tenv
  | processTypeDecHead (tenv, ({n, t, p}::l):A.TypeDec) = S.enter (tenv, n, translateTypeDec t)
(*for each tydec*)

(*take header, represent as ty, add to tenv'*)

(*Converts a Absyn.ty to a Types.ty*)
 fun translateTypeDec t:A.ty

(***Thnk about functional implementation of name***)
fun processTypeDecBody (tenv, t(tlist{name: symbol, ty: ty, pos: pos}):A.TypeDec) = ()
(*Turn ty record from absyn into ty.RECORD from types.sml*)


(* tenv*Types.ty -> Types.ty*)
(*For named types, this function looks up the "actual" type*)
(*TODO: Add error message to this in NONE case*)
fun actualType (tenv:Types.ty Symbol.table, Types.NAME(s,t)) = actualType (tenv, t)
  | actualType (tenv:Types.ty Symbol.table, t:Types.ty) = t

(*Types.ty -> Types.ty*)
(*Simple helper that tells you the type of object stored in an array*)
(*TODO: Error message when argument is not of type Types.ARRAY*)
fun lookupArrayType (Types.ARRAY(ty, u)) = ty
  | lookupArrayType (a:Types.ty) = Types.UNIT

(*Types.ty -> Types.ty*)
fun lookupFieldType (Types.RECORD(fieldlist, u), s) = traverseFieldList (fieldlist, s)
  | lookupFieldType (a:Types.ty, s) = Types.UNIT

fun traverseFieldList ([], s:S.symbol) = Types.UNIT
  | traverseFieldList ((s1:S.symbol, t:Types.ty)::l, s2:S.symbol) = if s1=s2 then t else traverseFieldList (l, s2)


  (***Property of Saums****)
  (*  venv*tenv*A.var -> Types.ty *)
  (*Tells you the type of a variable*)
fun transVar (venv, tenv, A.SubscriptVar(v,e,p)) = actualType (tenv, lookupArrayType (transVar (venv, tenv, v)))
  | transVar (venv, tenv, A.FieldVar(v,s,p)) =   actualType (tenv, lookupFieldType ((transVar (venv, tenv, v)),s))
  | transVar (venv, tenv, A.SimpleVar(s,p)) = actualType (tenv, case S.look (venv, s) of
                                                                SOME x => x
                                                                | NONE => Types.UNIT)
