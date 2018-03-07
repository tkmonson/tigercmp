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
                 | NONE => (*TODO: Throw error*)Types.BOTTOM

fun checkdups (nil, nil) = ()
  | checkdups (name::others, pos::poss) =
    if (List.all (fn (x) => (name <> x)) others) then checkdups(others, poss)
    else ()(* error: duplicate definition *)

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
  | lookupArrayType (a:Types.ty) = T.UNIT

(*Types.ty*symbol -> Types.ty*)
(*TODO: Error message when field list is empty, since that means that we didn't find that field*)
fun traverseFieldList ([], s:S.symbol) = T.UNIT
  | traverseFieldList ((s1:S.symbol, t:Types.ty)::l, s2:S.symbol) = if s1=s2 then t else traverseFieldList (l, s2)

(*Types.ty*symbol -> Types.ty*)
(*TODO: Error message for when we try and access a field of something that's not a record*)
fun lookupFieldType (Types.RECORD(fieldlist, u), s) = traverseFieldList (fieldlist, s)
  | lookupFieldType (a:Types.ty, s) = T.UNIT

(*  venv*tenv*Absyn.var -> Types.ty *)
(*Tells you the type of a variable*)
(*TODO: Error message for when we have a simplevar that's not in the venv*)
fun transVar (venv:E.enventry S.table, tenv:T.ty S.table, Absyn.SubscriptVar(v,e,p)) = actualType (tenv, lookupArrayType (transVar(venv, tenv, v)))
  | transVar (venv:E.enventry S.table, tenv:T.ty S.table, Absyn.FieldVar(v,s,p)) =   actualType (tenv, lookupFieldType ((transVar (venv, tenv, v),s)))
  | transVar (venv:E.enventry S.table, tenv:T.ty S.table, Absyn.SimpleVar(s,p)) = case S.look (venv, s) of
                                              SOME(E.VarEntry{ty=vartype}) => actualType(tenv, vartype)
                                            | NONE => T.UNIT

(*Checks whether a is the same type as, or a subtype of, b*)
fun isCompatible(a:T.ty, b:T.ty) = (a=b) orelse (a=T.UNIT)

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

fun getNameFromField ({name, escape, typ, pos}:A.field) = name
fun getPosFromField  ({name, escape, typ, pos}:A.field) = pos
fun getNameFromFunDec ({name, params, result, body, pos}:A.fundec) = name
fun getPosFromFunDec  ({name, params, result, body, pos}:A.fundec) = pos


fun processFunDecHead ((* fundec *) {name, params, result, body, pos}:A.fundec, (venv, tenv)) =
    let
        (* 1. Check return type *)
        val rt = case result of
	               NONE => T.UNIT
                 | SOME (typ, pos) => tenvLookUp(tenv, typ)

	(* 2. Check param types *)
        fun transparam ({typ,...}:Absyn.field) = tenvLookUp(tenv, typ)
        val params' = map transparam params

    in
        (* 3. No duplicate params -- does map make sense here? *)
        checkdups(map getNameFromField params, map getPosFromField params);
	(* Put function header into (value) environment *)
        (S.enter(venv, name, E.FunEntry{formals = params', result = rt}), tenv)
    end

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
        in if compat then {exp=(), ty=T.UNIT} else (*TODO: ERROR MESSAGE*) {exp=(), ty=T.UNIT}
        end
        | trexp (A.SeqExp(elist)) = trseq elist
        | trexp (A.IfExp{test=t, then'=thencase, else'=elsecase, pos=p}) = (checkInt(t, p);
                let val {exp=thenexp, ty=thenty} = trexp thencase;
                    val {exp=elseexp, ty=elsety} = case elsecase of
                                                       SOME(e) => trexp e
                                                       | NONE => {exp=(), ty=T.UNIT}
                in
                    if elsety = T.UNIT
                    then
                        if thenty = elsety
                        then {exp = (), ty = thenty}
                        else (*TODO:PRINT ERROR*){exp = (), ty = T.BOTTOM}
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
                val aTy = tenvLookUp(tenv, t)
                val {exp=_,ty=iTy} = trexp i
            in
                case aTy of
                T.ARRAY(ty,u) => if  isCompatible(iTy, ty)
                              then {exp = (), ty = aTy}
                              else (*TODO:PRINT ERROR*){exp = (), ty = T.UNIT}
                | T.BOTTOM => (*TODO:Throw error, type does not exist*){exp = (), ty = T.BOTTOM}
            end

  and trvar (v:A.var) = (transVar (venv, tenv, v))
  and trseq [] = {exp=(), ty=T.UNIT}
    | trseq ((a,p)::[]) = trexp a
    | trseq ((a,p)::l) = (trexp a; trseq l) (*Call trexp on a for side effects*)
  and checkInt(e:A.exp, pos) =
      let val {exp=_, ty=eTy} = trexp e
      in
          if isCompatible(eTy, T.INT) then () else (*Add error message here*)()
      end
  and checkBody (venv':E.enventry S.table, b:A.exp, v, pos:A.pos) =
      let val {exp=_, ty=bTy} = transExp (venv', tenv) b
      in
          if bTy <> T.UNIT then () else (*TODO: THROW ERROR*)()
      end
  and checkUnitTy (e:A.exp, pos:A.pos) =
      let val {exp=_, ty=eTy} = trexp e
      in
          if eTy <> T.UNIT then () else (*TODO: THROW ERROR*)()
      end

  and checkRecordFields([], []) = ()
    | checkRecordFields((rectypename, rectype)::l1, (recname, recval, pos)::l2) =
    let val {exp=e, ty=recvaltype} = trexp recval
    in
          if rectypename <> recname then ()
          else (if isCompatible(recvaltype, rectype) then checkRecordFields(l1, l2) else ())
    end
    | checkRecordFields(_,_) = ()

  and checkRecordExp(A.RecordExp({fields=fieldlist, typ=typename, pos=p})) =
      let val recordType = S.look(tenv, typename)
      in case recordType of
      SOME(Types.RECORD(fieldtypelist, unq)) => (checkRecordFields(fieldtypelist, fieldlist);Types.RECORD(fieldtypelist, unq))
    | _    => T.UNIT
    end

  and transDecs (venv:E.enventry S.table, tenv:T.ty S.table, []) = (venv, tenv)
      | transDecs (venv:E.enventry S.table, tenv:T.ty S.table, a::l:Absyn.dec list) =
          let val (v', t') = transDec (venv, tenv, a)
          in transDecs(v', t', l) end

  and transDec (venv, tenv, Absyn.VarDec(vd)) = (transVarDec(venv, tenv, Absyn.VarDec vd), tenv)
    | transDec (venv, tenv, Absyn.TypeDec(td)) = (venv, transTy(tenv, Absyn.TypeDec td))
    | transDec (venv, tenv, Absyn.FunctionDec(fundecs))  = (transFunDec (venv, tenv, Absyn.FunctionDec fundecs), tenv)

  and processFunDecBody ((* fundec *) {name, params, result, body, pos}:A.fundec,(venv,tenv)) =
      let
          val {exp,ty} = transExp(venv, tenv) body
      in
          (* Check that the body's result type matches the header's result type *)
          if
  	    isCompatible (ty, case result of SOME(rSym,rPos) => tenvLookUp (tenv, rSym)
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
  and transVarDec(venv: Env.enventry S.table, tenv:T.ty S.table, Absyn.VarDec{name=varname, escape=esc, typ=vartype, init=i, pos=p}) =
        let val {exp=exp, ty=exptype} = transExp(venv, tenv) i
            val venv' = S.enter(venv, varname, Env.VarEntry{ty=exptype})
            val venv'' = S.enter(venv, varname, Env.VarEntry{ty=T.UNIT})
        in
           case vartype of
           SOME(vartypename,varpos) => (case S.look(tenv, vartypename) of
                      SOME(expectedtype) => if isCompatible(exptype,expectedtype) then venv' else venv''
                    | NONE => venv'')
         | NONE => venv'
        end

  in
  trexp
  end

fun transProg exp = (transExp (Env.base_venv, Env.base_tenv) exp; ())
