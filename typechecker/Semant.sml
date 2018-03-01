structure A = Absyn
structure S = Symbol
structure E = Environment
structure T = Types

(*A dummy Translate structure to use for this step*)
structure Translate = struct type exp = unit end

(*A defintion of expty that uses the dummy Translate for now*)
type expty = {exp: Translate.exp, ty:Types.ty}

fun checkdups (nil, nil) = ()
  | checkdups (name::others, pos::poss) =
    if (List.all (fn (x) = (name <> x)) others) then checkdup(others, poss)
    else (* error: duplicate definition *)
	

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

<<<<<<< HEAD
fun transDec (venv, tenv, A.VarDec{}) =

    
  | transDec (venv, tenv, A.TypeDec) =


    
(* Function Declaration

   1. Check return type
   2. Check param types
   3. No duplicate params
   4. Check body types*)
    
  | transDec (venv, tenv, f(fundecs):A.FunctionDec)  =
    let
	(* Function Header Checking  *)
	fun transfun ((* fundec *) {name, params, result, body, pos}, env) = 
 	    let
		(* 1. Check return type *)
		val rt =
		    case result of
		      NONE => T.UNIT
		    | SOME (typ, pos) =>
		        (case S.look(tenv,typ) of
			   SOME typ => typ
		         | NONE   => (*error typ not in tenv*))

		(* 2. Check param types ??? *)
		fun transparam(params:A.field) =
		    case S.look(tenv, typ) of
		      SOME t => t
		      NONE   => (* error *)

		val params' = map transparam params
		    
	    in
		(* 3. No duplicate params *)
		checkdups(map name params, map pos params);
		(* 4. Put function header into overall environment ???  *)
		S.enter(env, name, E.FunEntry{params', rt});
	    end
    in
	let
	    val venv' = foldl transfun venv fundecs
	    (* Function Body Checking *)
	    (* 1. Type checking
               2. Enter VarEntry in venv
               3. Check body *)
	    fun transbody ((* fundec *) {name, params, result, body, pos},{tenv,venv}) =
		let
		    
		in
		    
		end
		    

	in

	end
    end
	
		
	
=======
fun transDecs (venv, tenv, d:A.dec list) = ()
>>>>>>> 59f22271dacba712bee24488abd2e2acaa585f44
  (*For each item in d*)
    (*Pattern match on the 3 different types of A.dec*)
    (*CASE:
          vardec: augment venv with new variable)
          FunctionDec: Call helper processFunDecHead that adds headers to venv
                       Call helper processFunDecBody to fill out fields in venv
                       - allows for recursive functions
                      typecheck functions with final venvs
                      - handle arguments
                      - calls transExp on body
          TypeDec: Call helper processTypeDecHead that adds headers to tenv
                       Call helper processTypeDecBody to fill out fields in tenv
                       - allows for recursive functions
                       - three cases:
                        * name
                        * record - process fields with recTyFromFlist
                        * array
                      typecheck functions with final tenvs *)

fun processFunDecHead (venv, tenv, f(flist):A.FunctionDec) =
(*for each in flist*)
(*take header, represent as funentry, add to venv*)

(*AFTER all are processed, call processFunDecBody*)

(****Takes entire list of fundecs to process bodies****)
(***Property of Tommy****)
fun processFunDecBody (venv, tenv, f(flist):A.FunctionDec) =

(*call transexp on exp in fundec body passed (v/t)env*)


(***Explained on page 120***)
fun transTy (tenv, t:A.ty) = ()
(*call processTypeDecHead, add 'tenv to tenv*)

(*Somewhere processes body, revisit helper function needs*)

(*take header, represent as ty, add to tenv'*)
fun processTypeDecHead (tenv, []) = tenv
  | processTypeDecHead (tenv, ({n, t, p}::l):A.TypeDec) = (S.enter (tenv, n, Types.NAME(n, ref NONE), l)

(*Creates (S.symbol * ty) list from field list*)
fun recTyFromFlist (tenv, []) = []
    | recTyFromFlist (tenv, {n, e, t, p}:A.field::l) = (n, S.look(tenv, n))::recTyFromFlist(l)

(***Thnk about functional implementation of name***)
fun processTypeDecBody (tenv, {n, t, pos}::l:A.TypeDec) =
let
   val envTy = S.look(tenv, n)
in
(*Turn ty record from absyn into ty.RECORD from types.sml*)
case t of
    A.NameTy(s,pos) => #2(envTy) :=
                       SOME NAME(n, S.look (tenv, s))
    A.RecordTy(flist) => #2(envTy) :=
                        SOME RECORD (recTyFromFlist flist, ref ())
    A.ArrayTy(s,pos) => #2(envTy) :=
                        SOME ARRAY(S.look (tevn, s), ref ())

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
