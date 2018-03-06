structure A = Absyn
structure S = Symbol
structure E = Env
structure T = Types


(*A dummy Translate structure to use for this step*)
structure Translate = struct type exp = unit end

(*A defintion of expty that uses the dummy Translate for now*)
type expty = {exp: Translate.exp, ty:Types.ty}

fun tenvLookUp (tenv, n) = case S.look(tenv, n) of
                 SOME x => x
                 | NONE => (*TODO: Throw error*)Types.UNIT

fun checkdups (nil, nil) = ()
  | checkdups (name::others, pos::poss) =
    if (List.all (fn (x) => (name <> x)) others) then checkdups(others, poss)
    else ()(* error: duplicate definition *)

(*Checks whether a is the same type as, or a subtype of, b*)
fun isCompatible(a:T.ty, b:T.ty) = (a=b) orelse (a=T.UNIT)

(*
* transExp is side-effecting: It prints error messages, and returns trexp
* transExp: (venv*tenv) -> (A.exp -> expty)
* trexp: A.exp -> expty
* trvar: A.var -> T.ty
*)
fun transExp (venv, tenv) =
  (*fn(e:Absyn.exp) => {exp=(), ty=Types.UNIT}*)
  let fun trexp A.NilExp = {exp = (), ty = T.Nil}
        | trexp A.IntExp(num) = {exp = (), ty = T.INT}
        | trexp A.StringExp(s,p) = {exp = (), ty = T.STRING}
        | trexp A.VarExp(v) = {exp = (), ty = trvar v}
        | trexp A.OpExp{left=l, oper=o, right=r, pos=p} = (checkInt(trexp l, p); checkInt(trexp r, p); {exp = (), ty = T.INT})
        | trexp A.LetExp{decs=d, body=b, pos=p} =
                let val {venv', tenv'} = transDecs (venv, tenv, d)
                in  transExp(venv', tenv') b
                end
        | trexp A.SeqExp(elist) = trseq elist
        | trexp A.RecordExp{fields=flist, typ=t, pos=p} = {exp=(), ty=T.UNIT}(*Just lookup t and make sure it's a Types.RECORD*)
        | trexp A.AssignExp{var=v,exp=e,pos=p} = {exp=(), ty=T.UNIT}(*if v is in venv then check it against e
                                                  else add it to venv with type of e*)
        | trexp A.IfExp {test=t, then'=thencase, else'=elsecase, pos=p} = (checkInt(t, p);
                let val {exp=thenexp, ty=thenty} = trexp thencase;
                    val {exp=elseexp, ty=elsety} = case elsecase of
                                                       SOME(e) => trexp e
                                                       | NONE => {exp=(), ty=T.UNIT}
                in
                    if thenty = elsety
                    then {exp = (), ty = thenty}
                    else (*TODO:PRINT ERROR*){exp = (), ty = T.UNIT}
                end) (*Check that t is an int, thencase and elsecase have the same type*)
        | trexp A.WhileExp{test=t, body=b, pos=p} = (checkInt(t, p);
                                                    checkBody(b, p);
                                                    {exp = (), ty = T.UNIT})
        | trexp A.ForExp{var=v, escape=e, lo=l, hi=h, body=b, pos=p} = (checkInt(v, p);
                                                                       checkInt(l, p);
                                                                       checkInt(h, p);
                                                                       checkBody(b, p);
                                                                       {exp = (), ty = T.UNIT})
        | trexp A.ArrayExp{typ=t, size=s, init=i, pos=p} =
        (*Just lookup t and make sure it's a Types.ARRAY*)
            (let
                val aTy = tenvLookUp(t)
                val {exp=_,ty=iTy} = trexp i
            in
                case aTy of
                ARRAY(ty,u) => if  isCompatible(iTy, ty)
                              then {exp = (), ty = aTy}
                              else (*TODO:PRINT ERROR*){exp = (), ty = T.UNIT}
                | UNIT => (*TODO:Throw error, type does not exist*){exp = (), ty = T.UNIT}
            end)

  and trvar v:A.var = (transVar (venv, tenv, v))
  and trseq [] = {exp=(), ty=T.UNIT}
    | trseq a::[] = trexp a
    | trseq a::l::[] = (trexp a; trseq l) (*Call trexp on a for side effects*)
  and checkInt({exp=e, ty=t}, pos) = if isCompatible(trexp e, T.INT) then () else (*Add error message here*)()
  and checkBody b:A.exp = if trexp b <> T.UNIT then () else (*TODO: THROW ERROR*)()
  in
  trexp

  end

(*Top level function: Calls side-effecting function transExp, and then returns unit*)
(**)
fun transProg exp = (transExp (Env.base_venv, Env.base_tenv) exp; ())

(*TODO: Add error messages if types are not equal or if expected type doens't exist in tenv*)
fun transVarDec(tenv:T.ty S.table, venv: Env.enventry S.table, Absyn.VarDec{name=varname, escape=esc, typ=vartype, init=i, pos=p}) =
      let val {exp=exp, ty=exptype} = transExp(tenv, venv) i
          val venv' = S.enter(venv, varname, Env.VarEntry{ty=exptype})
          val venv'' = S.enter(venv, varname, Env.VarEntry{ty=T.UNIT})
      in
         case vartype of
         SOME(vartypename,varpos) => (case S.look(tenv, vartypename) of
                    SOME(expectedtype) => if isCompatible(exptype,expectedtype) then venv' else venv''
                  | NONE => venv'')
       | NONE => venv'
      end

fun transDecs (venv, tenv, d:Absyn.dec list) = ()
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
          TypeDec: Call helper processTypeDecHeads that adds headers to tenv
                       Call helper processTypeDecBody to fill out fields in tenv
                       - allows for recursive functions
                       - three cases:
                        * name
                        * record - process fields with recTyFromFlist
                        * array
                      typecheck functions with final tenvs *)
(*)
fun processFunDecHead (venv, tenv, f(flist):A.FunctionDec) =
(*for each in flist*)
(*take header, represent as funentry, add to venv*)

(*AFTER all are processed, call processFunDecBody*)

(****Takes entire list of fundecs to process bodies****)
(***Property of Tommy****)
fun processFunDecBody (venv, tenv, f(flist):A.FunctionDec) =

(*call transexp on exp in fundec body passed (v/t)env*)
*)



(*take header, represent as ty, add to tenv'*)
fun processTypeDecHeads (tenv, []) = tenv
  | processTypeDecHeads (tenv, {name=n, ty=t, pos=p}::l) = processTypeDecHeads (S.enter (tenv, n, Types.NAME(n, ref NONE)), l)

(*Creates (S.symbol * ty) list from field list*)
fun recTyFromFlist (tenv, []) = []
    | recTyFromFlist (tenv, {name=n, escape=e, typ=t, pos=p}::l) = (n, tenvLookUp(tenv, n))::recTyFromFlist(tenv, l)

(***Thnk about functional implementation of name***)
fun processTypeDecBody (tenv, {name=n, ty=t, pos=p}) =
let
   val envTy = tenvLookUp (tenv, n)
in
(*Turn ty record from absyn into ty.RECORD from types.sml*)
case envTy of
    Types.NAME (_, ty) =>
        (case t of
            Absyn.NameTy(s,pos) => (ty :=
                               SOME (Types.NAME(n, ref (SOME (tenvLookUp(tenv, s))))); tenv)
            | Absyn.RecordTy(flist) => (ty :=
                                SOME (Types.RECORD (recTyFromFlist (tenv, flist), ref ())); tenv)
            | Absyn.ArrayTy(s,pos) => (ty :=
                                SOME (Types.ARRAY(tenvLookUp(tenv, s), ref ())); tenv))
    | _ => tenv(*error*)
end

fun processTypeDecBodies (tenv, []) = tenv
    | processTypeDecBodies (tenv, a::l) = processTypeDecBodies (processTypeDecBody (tenv, a), l)

(***Explained on page 120***)
fun transTy (tenv, Absyn.TypeDec(tylist)) = processTypeDecBodies (processTypeDecHeads(tenv, tylist), tylist)

fun transDec (venv, tenv, Absyn.VarDec(vd)) = (transVarDec(tenv, venv, Absyn.VarDec vd), tenv)
  | transDec (venv, tenv, Absyn.TypeDec(td)) = (venv, transTy(tenv, Absyn.TypeDec td))
  | transDec (venv, tenv, Absyn.FunctionDec(fundecs))  =
  let
	    (* Check the header *)
	    fun transfun ((* fundec *) {name, params, result, body, pos}, env) =
 	    let
		     (* 1. Check return type *)
		     val rt =
		     case result of
		       NONE => T.UNIT
		     | SOME (typ, pos) =>
		            (case S.look(tenv,typ) of
			                SOME t => t
		                | NONE   => ((*error typ not in tenv*)))

		      (* 2. Check param types *)
		      fun transparam({typ,...}:Absyn.field) =
		          case S.look(tenv, typ) of
		            SOME t => t
		          | NONE => ((* error *))
		      val params' = map transparam params

	    in
		      (* 3. No duplicate params -- does map make sense here? *)
		      checkdups(map name params, map pos params);
		      (* Put function header into environment *)
		      S.enter(env, name, Env.FunEntry{formals = params', result = rt})
	    end

  in
	    (* 4. Check function body *)
	    let
	        val venv' = foldl transfun venv fundecs

	        fun transbody ((* fundec *) {name, params, result, body, pos},{tenv,venv}) =
		          let
		              (* Check that an identifier in the function body is in the value env (in scope) *)
		              val SOME(Env.FunEntry{formals, result}) = S.look(venv',name)

		              (* Check that an identifier actually has a valid type *)
		              fun transparam({name, escape, typ, pos}, access) =
		          	      case S.look(tenv,typ) of
		          	           SOME t => {access=access,name=name,ty=t}
		          	         | NONE   => ((* ERROR - identifier in code has wrong type *))
		              val params' = map transparam params


		              (* Enter the first field in params' into venv'
                                 Update venv' identifier so it now includes that field
                                 Repeat for all fields in params'
                                 Result: all variables in the body are now in the value environment *)
	                val venv'' = (foldl
		          		             (fn ({access,name,ty},env) =>
		          		                 S.enter(env,name,Env.VarEntry{access=access,ty=ty}))
		          		                 venv'
		          		                 params')

		             (* Find the body's result type *)
		             val {exp,ty} = transExp(venv'', tenv) body
		         in
		             (* Check that the body's result type matches the header's result type *)
		             if (result <> ty) then ((* type mismatch error *)) else ()
		         end
	    in
	        (* Check that there are no identical function declarations *)
	        checkdups(map name fundecs, map pos fundecs);
          let
		          val {venv, tenv} = foldl transbody {tenv=tenv,venv=venv} fundecs
	        in
		          {venv=venv, tenv=tenv}
	        end
	    end
  end


(**SAUMYA**)


(*tenv*Types.ty -> Types.ty*)
(*TODO: Error message for case where the named type points to NONE*)
fun actualType (tenv:Types.ty Symbol.table, Types.NAME(s,t)) =
  (let val storedTy = case !t of
  SOME(x) => x
  | NONE => T.UNIT
  in
  actualType (tenv, storedTy)
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

(*  venv*tenv*Absyn.var -> Types.ty *)
(*Tells you the type of a variable*)
(*TODO: Error message for when we have a simplevar that's not in the venv*)
fun transVar (venv, tenv, Absyn.SubscriptVar(v,e,p)) = actualType (tenv, lookupArrayType (transVar(venv, tenv, v)))
  | transVar (venv, tenv, Absyn.FieldVar(v,s,p)) =   actualType (tenv, lookupFieldType ((transVar (venv, tenv, v),s)))
  | transVar (venv, tenv, Absyn.SimpleVar(s,p)) = case S.look (venv, s) of
                                              SOME x => actualType(tenv, x)
                                            | NONE => T.UNIT
