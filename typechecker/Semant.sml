structure A = Absyn
structure S = Symbol
structure E = Env
structure T = Types


(*A dummy Translate structure to use for this step*)
structure Translate = struct type exp = unit end

(*A defintion of expty that uses the dummy Translate for now*)
type expty = {exp: Translate.exp, ty:Types.ty}

fun printError(msg, pos) = ErrorMsg.error pos (msg)

fun tenvLookUp (tenv, n, pos) = case S.look(tenv, n) of
                 SOME x => x
	       | NONE => (printError("Type does not exist in type environment.", pos); Types.BOTTOM)

fun makeVarEntry (typ:Types.ty) = E.VarEntry {ty=typ}

fun checkdups (nil, nil) = ()
  | checkdups (name::others, pos::poss) =
    if (List.all (fn (x) => (name <> x)) others) then checkdups(others, poss)
    else ()(* error: duplicate definition *)

(*tenv*Types.ty -> Types.ty*)
(*TODO: Error message for case where the named type points to NONE*)
fun actualType (tenv:Types.ty Symbol.table, Types.NAME(s,t), pos) =
  (let val storedTy = case !t of
  SOME(x) => x
  | NONE => (printError("Tried to call actualType on a NameTy with ref NONE...this should never happen!",pos); T.BOTTOM)
  in
  actualType (tenv, storedTy, pos)
  end)
  | actualType (tenv:Types.ty Symbol.table, t:Types.ty, pos) = t

(*Types.ty -> Types.ty*)
(*Simple helper that tells you the type of object stored in an array*)
fun lookupArrayType (Types.ARRAY(ty, u), pos) = ty
  | lookupArrayType (Types.BOTTOM, pos) = Types.BOTTOM
  | lookupArrayType (a:Types.ty, pos) = (printError("Trying to access subscript of a variable that is not an array", pos); Types.BOTTOM)

(*Types.ty*symbol -> Types.ty*)
fun traverseFieldList ([], s:S.symbol, pos) = (printError("Record variable does not have requested field", pos); Types.BOTTOM)
  | traverseFieldList ((s1:S.symbol, t:Types.ty)::l, s2:S.symbol, pos) = if s1=s2 then t else traverseFieldList (l, s2, pos)

(*Types.ty*symbol -> Types.ty*)
fun lookupFieldType (Types.RECORD(fieldlist, u), s, pos) = traverseFieldList (fieldlist, s, pos)
  | lookupFieldType(Types.BOTTOM, s, pos) = Types.BOTTOM
  | lookupFieldType (a:Types.ty, s, pos) = (printError("Trying to access field of a variable that is not a record", pos); Types.BOTTOM)

(*  venv*tenv*Absyn.var -> Types.ty *)
(*Tells you the type of a variable*)
(*TODO: Error message for when we have a simplevar that's not in the venv*)
fun transVar (venv:E.enventry S.table, tenv:T.ty S.table, Absyn.SubscriptVar(v,e,p)) = actualType (tenv, lookupArrayType ((transVar(venv, tenv, v),p)), p)
  | transVar (venv:E.enventry S.table, tenv:T.ty S.table, Absyn.FieldVar(v,s,p)) =   actualType (tenv, lookupFieldType ((transVar (venv, tenv, v),s,p)), p)
  | transVar (venv:E.enventry S.table, tenv:T.ty S.table, Absyn.SimpleVar(s,p)) = case S.look (venv, s) of
                                              SOME(E.VarEntry{ty=vartype}) => actualType(tenv, vartype, p)
                                            | NONE => (printError("Could not find this variable in the current scope", p);T.BOTTOM)

(*Checks whether a is the same type as, or a subtype of, b*)
fun isCompatible (T.BOTTOM, b:Types.ty) = true
  | isCompatible (a:T.ty, T.UNIT) = true
  | isCompatible(T.NIL, T.RECORD(arg1,arg2)) = true
  | isCompatible(a:T.ty, b:T.ty) = a=b

fun listCompatible ([]:T.ty list,[]:T.ty list) = true
  | listCompatible (a::[]:T.ty list,b::[]:T.ty list) = isCompatible(a,b)
  | listCompatible (a::(aa::(aTail::[])):T.ty list, b::(bb::(bTail::[])):T.ty list) =
    if isCompatible(a,b) then listCompatible(aa::(aTail::[]),bb::(bTail::[])) else false (*error*)
  | listCompatible (_,_) = false (*error*) 

			       
(*)
fun checkRecordFields(tenv, venv, [], []) = ()
|   checkRecordFields(tenv, venv, (rectypename, rectype)::l, (recname, recval, pos)::l) =
    if rectypename <> recname then ()
    else (if isCompatible(trexp recval, rectypename) then () else ())

fun checkRecordExp(A.RecordExp({fields=fieldlist, typ=typename, pos=p}), tenv, venv) =
let val recordType = S.look(tenv, typename)
in case recordType of
SOME(Types.RECORD(fieldtypelist, unq)) => (checkRecordFields(tenv, fieldtypelist, fieldlist);Types.RECORD(fieldtypelist, unq))
_    => Types.UNIT
*)

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


(*
* transExp is side-effecting: It prints error messages, and returns trexp
* transExp: (venv*tenv) -> (A.exp -> expty)
* trexp: A.exp -> expty
* trvar: A.var -> T.ty
*)

(*take header, represent as ty, add to tenv'*)
fun processTypeDecHeads (tenv, []) = tenv
  | processTypeDecHeads (tenv, {name=n, ty=t, pos=p}::l) = processTypeDecHeads (S.enter (tenv, n, Types.NAME(n, ref NONE)), l)

(*Creates (S.symbol * ty) list from field list*)
fun recTyFromFlist (tenv, []) = []
    | recTyFromFlist (tenv, {name=n, escape=e, typ=t, pos=p}::l) = (n, tenvLookUp(tenv, n, p))::recTyFromFlist(tenv, l)

(***Thnk about functional implementation of name***)
fun processTypeDecBody (tenv, {name=n, ty=t, pos=p}) =
    let
       val envTy = tenvLookUp (tenv, n, p)
    in
    (*Turn ty record from absyn into ty.RECORD from types.sml*)
    case envTy of
        Types.NAME (_, ty) =>
            (case t of
                Absyn.NameTy(s,pos) => (ty :=
                                   SOME (Types.NAME(n, ref (SOME (tenvLookUp(tenv, s, pos))))); tenv)
                | Absyn.RecordTy(flist) => (ty :=
                                    SOME (Types.RECORD (recTyFromFlist (tenv, flist), ref ())); tenv)
                | Absyn.ArrayTy(s,pos) => (ty :=
                                    SOME (Types.ARRAY(tenvLookUp(tenv, s, pos), ref ())); tenv))
                | _ => tenv(*error*)
    end

fun processTypeDecBodies (tenv, []) = tenv
    | processTypeDecBodies (tenv, a::l) = processTypeDecBodies (processTypeDecBody (tenv, a), l)

(***Explained on page 120***)
fun transTy (tenv, Absyn.TypeDec(tylist)) = processTypeDecBodies (processTypeDecHeads(tenv, tylist), tylist)

fun getNameFromField ({name, escape, typ, pos}:A.field) = name
fun getPosFromField  ({name, escape, typ, pos}:A.field) = pos
fun getNameFromFunDec ({name, params, result, body, pos}:A.fundec) = name
fun getPosFromFunDec  ({name, params, result, body, pos}:A.fundec) = pos

  (*
  * transExp is side-effecting: It prints error messages, and returns trexp
  * transExp: (venv*tenv) -> (A.exp -> expty)
  * trexp: A.exp -> expty
  * trvar: A.var -> T.ty
  *)

fun transExp (venv:Env.enventry S.table, tenv:T.ty S.table) =
  (*fn(e:Absyn.exp) => {exp=(), ty=T.UNIT}*)
  let fun trexp (A.NilExp) = {exp = (), ty = T.NIL}
        | trexp (A.IntExp(num)) = {exp = (), ty = T.INT}
        | trexp (A.StringExp(s,p)) = {exp = (), ty = T.STRING}
        | trexp (A.VarExp(v)) = {exp = (), ty = trvar v}
        | trexp (A.OpExp{left=l, oper=_, right=r, pos=p}) = (checkInt(l, p);
                                                          checkInt(r, p);
                                                          {exp = (), ty = T.INT})
        | trexp (A.LetExp{decs=d, body=b, pos=p}) =
                let val (venv', tenv') = transDecs (venv, tenv, d)
                in  transExp(venv', tenv') b
                end
        | trexp (A.RecordExp(rexp)) = {exp=(), ty=checkRecordExp(A.RecordExp(rexp))}
        | trexp (A.AssignExp{var=v,exp=e,pos=p}) =
        let val {exp=e, ty=exptype} = trexp(e)
            val vartype = trvar(v)
            val compat = isCompatible(exptype, vartype)
        in if compat then {exp=(), ty=T.UNIT} else (printError("Assign statement type incompatible", p); {exp=(), ty=T.UNIT})
        end
        | trexp (A.SeqExp(elist)) = trseq elist
        | trexp (A.IfExp{test=t, then'=thencase, else'=elsecase, pos=p}) = (checkInt(t, p);
                let val {exp=thenexp, ty=thenty} = trexp thencase;
                    val {exp=elseexp, ty=elsety} = case elsecase of
                                                       SOME(e) => trexp e
                                                       | NONE => {exp=(), ty=T.UNIT}
                in
                    if elsety <> T.UNIT
                    then
                        if thenty = elsety
                        then {exp = (), ty = thenty}
                        else (printError("Type mismatch in then and else statements", p); {exp = (), ty = T.BOTTOM})
                    else {exp = (), ty = T.UNIT}
                end) (*Check that t is an int, thencase and elsecase have the same type*)
        | trexp (A.WhileExp{test=t, body=b, pos=p}) = (checkInt(t, p);
                                                      checkUnitTy(b, p);
                                                      {exp = (), ty = T.UNIT})
        | trexp (A.ForExp{var=v, escape=e, lo=l, hi=h, body=b, pos=p}) = ((*What do we do with the var?
                                                                       create new scope for variable
                                                                       use it in ex3 in book ONLY then take out*)
                                                                       checkInt(l, p);
                                                                       checkInt(h, p);
                                                                       checkBody(S.enter (venv, v, Env.VarEntry{ty=T.INT}), b, v, p);
                                                                       {exp = (), ty = T.UNIT})
        | trexp (A.ArrayExp{typ=t, size=s, init=i, pos=p}) =
        (*Just lookup t and make sure it's a Types.ARRAY*)
            let
                val aTy = tenvLookUp(tenv, t, p)
                val {exp=_,ty=iTy} = trexp i
            in
                case aTy of
                T.ARRAY(ty,u) => if  isCompatible(iTy, ty)
                              then {exp = (), ty = aTy}
                              else (printError("Type mismatch between array and entry", p); {exp = (), ty = T.BOTTOM})
                | T.BOTTOM => (printError("Array's type does not exist", p); {exp = (), ty = T.BOTTOM})
            end

	| trexp (A.CallExp{func:A.symbol, args: A.exp list, pos:A.pos}) =
	  let
	      val fs = case S.look(venv:E.enventry S.table,func) of
		            SOME(E.FunEntry{formals=fs, result=rt}) => fs
		          | NONE => T.UNIT::[]
	      val rt = case S.look(venv,func) of
		            SOME(E.FunEntry{formals=fs, result=rt}) => rt
		          | NONE => T.UNIT
	  in
	      listCompatible(map (fn {exp,ty} => ty) (map trexp args),fs);
	      {exp = (), ty = rt}
	  end


  and trvar (v:A.var) = (transVar (venv, tenv, v))
  and trseq [] = {exp=(), ty=T.UNIT} (*This should be unit and not bottom!*)
    | trseq ((a,p)::[]) = trexp a
    | trseq ((a,p)::l) = (trexp a; trseq l) (*Call trexp on a for side effects*)
  and checkInt(e:A.exp, pos) =
      let val {exp=_, ty=eTy} = trexp e
      in
          if isCompatible(eTy, T.INT) then () else (printError("Expression is not an int!!", pos))
      end
  and checkBody (venv':E.enventry S.table, b:A.exp, v, pos:A.pos) =
      let val {exp=_, ty=bTy} = transExp (venv', tenv) b
      in
          if bTy <> T.UNIT then () else (printError("Unit return type expected", pos);())
      end
  and checkUnitTy (e:A.exp, pos:A.pos) =
      let val {exp=_, ty=eTy} = trexp e
      in
          if eTy <> T.UNIT then () else (printError("Unit return type expected", pos);())
      end

  and checkRecordFields([], []) = ()
    | checkRecordFields((rectypename, rectype)::l1, (recname, recval, pos)::l2) =
    let val {exp=e, ty=recvaltype} = trexp recval
    in
          if rectypename <> recname then ()
          else (if isCompatible(recvaltype, rectype) then checkRecordFields(l1, l2) else printError("Record fields are not compatible", pos))
    end
    | checkRecordFields(_,_) = ()

  and checkRecordExp(A.RecordExp({fields=fieldlist, typ=typename, pos=p})) =
      let val recordType = S.look(tenv, typename)
      in case recordType of
      SOME(Types.RECORD(fieldtypelist, unq)) => (checkRecordFields(fieldtypelist, fieldlist);Types.RECORD(fieldtypelist, unq))
    | _    => (printError("Trying to set fields of something that is not a record type", p);T.BOTTOM)
    end

  and transDecs (venv:E.enventry S.table, tenv:T.ty S.table, []) = (venv, tenv)
      | transDecs (venv:E.enventry S.table, tenv:T.ty S.table, a::l:Absyn.dec list) =
          let val (v', t') = transDec (venv, tenv, a)
          in transDecs(v', t', l) end

  and transDec (venv, tenv, Absyn.VarDec(vd)) = (transVarDec(venv, tenv, Absyn.VarDec vd), tenv)
    | transDec (venv, tenv, Absyn.TypeDec(td)) = (venv, transTy(tenv, Absyn.TypeDec td))
    | transDec (venv, tenv, Absyn.FunctionDec(fundecs))  = (transFunDec (venv, tenv, Absyn.FunctionDec fundecs), tenv)

  and processFunDecHead ((* fundec *) {name, params, result, body, pos}:A.fundec, (venv, tenv)) =
      let
          (* 1. Check return type *)
          val rt = case result of
	                NONE => T.UNIT
                      | SOME (typ, pos) => tenvLookUp(tenv, typ)

	  (* 2. Check param types *)
          fun transparam ({typ,...}:Absyn.field) = tenvLookUp(tenv, typ)
          val params' = map transparam params
	  val vEntries = map makeVarEntry params'
	  fun enterVars (v:E.enventry, venv:E.enventry S.table) =
	      S.enter(venv,name,v)
	  val venv' = foldr enterVars venv vEntries;
      in
          (* 3. No duplicate params -- does map make sense here? *)
          checkdups(map getNameFromField params, map getPosFromField params);
	  (* Put function header and params into (value) environment *)
          (S.enter(venv', name, E.FunEntry{formals = params', result = rt}), tenv)
      end
							       
  and processFunDecBody ((* fundec *) {name, params, result, body, pos}:A.fundec,(venv,tenv)) =
      let
          val {exp,ty} = transExp(venv, tenv) body
      in
          (* Check that the body's result type matches the header's result type *)
          if
  	    isCompatible (ty, case result of SOME(rSym,rPos) => tenvLookUp (tenv, rSym, rPos)
  					   | NONE            => T.UNIT)
  	then ()

  	else ((*error*));
  	(venv,tenv)
      end

  and transFunDec (venv: Env.enventry S.table, tenv:T.ty S.table, Absyn.FunctionDec fundecs) =
      (* Goal: Update the venv to include value entries declared in a function *)
      let
  	(* Include parameters from the function header in the new venv *)
        val (venv',tenv') = foldl processFunDecHead (venv,tenv) fundecs
  	(* Include values that are declared in the body of the function *)
  	val (venv'',tenv'') = foldl processFunDecBody (venv',tenv') fundecs
      in
  	(* Check that there are no identical function headers *)
  	checkdups(map getNameFromFunDec fundecs, map getPosFromFunDec fundecs);
        venv''
      end

  (*TODO: Add error messages if types are not equal or if expected type doens't exist in tenv*)
  and transVarDec (venv: Env.enventry S.table, tenv:T.ty S.table, Absyn.VarDec{name=varname, escape=esc, typ=vartype, init=i, pos=p}) =
        let val {exp=exp, ty=exptype} = transExp(venv, tenv) i
            val venv' = S.enter(venv, varname, Env.VarEntry{ty=exptype})
            val venv'' = S.enter(venv, varname, Env.VarEntry{ty=T.BOTTOM})
        in
           case vartype of
           SOME(vartypename,varpos) => (case S.look(tenv, vartypename) of
                      SOME(expectedtype) => if isCompatible(exptype, actualType(tenv, expectedtype, p)) then venv' else (printError("Variable type does not meet expected type", p);venv'')
                    | NONE => (printError("Could not find type in type environment",p);venv''))
         | NONE => venv'
        end

  in
  trexp
  end

fun transProg exp = (transExp (Env.base_venv, Env.base_tenv) exp; ())
