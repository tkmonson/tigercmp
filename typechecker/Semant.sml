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

fun transDec (venv, tenv, vd:A.VarDec) = transVarDec(tenv, venv, vd)


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

fun transDecs (venv, tenv, d:A.dec list) = ()
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
fun transTy (tenv, t(tylist):A.TypeDec) =
    processTypeDecBody (processTypeDecHead(tenv, tylist), tylist)

(*take header, represent as ty, add to tenv'*)
fun processTypeDecHead (tenv, []) = tenv
  | processTypeDecHead (tenv, ({name=n, ty=t, pos=p}::l):A.TypeDec) = processTypeDecHead (S.enter (tenv, n, Types.NAME(n, ref NONE)), l)

(*Creates (S.symbol * ty) list from field list*)
fun recTyFromFlist (tenv, []) = []
    | recTyFromFlist (tenv, {name=n, escape=e, typ=t, pos=p}:A.field::l) = (n, tenvLookUp(tenv, n))::recTyFromFlist(l)

fun processTypeDecBodies (tenv, []) = tenv
    | processTypeDecBodies (tenv, a::l:A.TypeDec) = processTypeDecBodies (processTypeDecBody (tenv, a), l)

(***Thnk about functional implementation of name***)
fun processTypeDecBody (tenv, {name=n, ty=t, pos=p}::l:A.TypeDec) =
let
   val envTy = tenvLookUp (tevn, n)
in
(*Turn ty record from absyn into ty.RECORD from types.sml*)
case t of
    A.NameTy(s,pos) => #2(envTy) :=
                       SOME NAME(n, tenvLookUp(tenv, s))
    | A.RecordTy(flist) => #2(envTy) :=
                        SOME RECORD (recTyFromFlist flist, ref ())
    | A.ArrayTy(s,pos) => #2(envTy) :=
                        SOME ARRAY(tenvLookUp(tevn, s), ref ())
end

fun tenvLookup (tenv, n) = case S.look(tenv, n) of
                 SOME x => x
                 | NONE => Types.UNIT



(**SAUMYA**)


(*tenv*Types.ty -> Types.ty*)
(*TODO: Error message for case where the named type points to NONE*)
fun actualType (tenv:Types.ty Symbol.table, Types.NAME(s,t)) =
  (let val storedTy = case !t of
  SOME(x) => x
  | NONE => T.UNIT
  in
  actualType storedTy
  end)
  | actualType (tenv:Types.ty Symbol.table, t:Types.ty) = t

(*Types.ty -> Types.ty*)
(*Simple helper that tells you the type of object stored in an array*)
(*TODO: Error message when argument is not of type Types.ARRAY*)
fun lookupArrayType (Types.ARRAY(ty, u)) = ty
  | lookupArrayType (a:Types.ty) = Types.UNIT

(*Types.ty*symbol -> Types.ty*)
(*TODO: Error message when field list is empty, since that means that we didn't find that field*)
fun traverseFieldList ([], s:S.symbol) = Types.UNIT
  | traverseFieldList ((s1:S.symbol, t:Types.ty)::l, s2:S.symbol) = if s1=s2 then t else traverseFieldList (l, s2)

(*Types.ty*symbol -> Types.ty*)
(*TODO: Error message for when we try and access a field of something that's not a record*)
fun lookupFieldType (Types.RECORD(fieldlist, u), s) = traverseFieldList (fieldlist, s)
  | lookupFieldType (a:Types.ty, s) = Types.UNIT

(*  venv*tenv*A.var -> Types.ty *)
(*Tells you the type of a variable*)
(*TODO: Error message for when we have a simplevar that's not in the venv*)
fun transVar (venv, tenv, A.SubscriptVar(v,e,p)) = actualType (tenv, lookupArrayType (transVar(venv, tenv, v)))
  | transVar (venv, tenv, A.FieldVar(v,s,p)) =   actualType (tenv, lookupFieldType ((transVar (venv, tenv, v),s)))
  | transVar (venv, tenv, A.SimpleVar(s,p)) = case S.look (venv, s) of
                                              SOME x => actualType(tenv, x)
                                            | NONE => T.UNIT

(*Checks whether a is the same type as, or a subtype of, b*)
fun isCompatible(a:T.ty, b:T.ty) = (a=b) orelse (a=T.UNIT)

(*TODO: Add error messages if types are not equal or if expected type doens't exist in tenv*)
fun transVarDec(tenv:T.ty S.table, venv: E.enventry S.table, A.VarDec{name=varname, escape=esc, typ=vartype, init=i, pos=p}) =
      let val {a=exp, b=exptype} = transExp(tenv, venv) i
          val venv' = S.enter(venv, varname, E.VarEntry{ty=exptype})
          val venv'' = S.enter(venv, varname, E.VarEntry{ty=T.UNIT})
      in
         case vartype of
         SOME(vartypename,varpos) => (case S.look(tenv, vartypename) of
                    SOME(expectedtype) => if isCompatible(exptype,expectedtype) then venv' else venv''
                  | NONE => venv'')
       | NONE => venv'
      end
