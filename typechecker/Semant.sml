structure Semant : SEMANT =
struct

structure A = Absyn
structure E = Env
structure T = Types
structure S = Symbol

(*A dummy Translate structure to use for this step*)
structure Translate = struct type exp = unit end

(*A defintion of expty that uses the dummy Translate for now*)
type expty = {exp: Translate.exp, ty:Types.ty}

fun getNameFromField   ({name, escape, typ, pos}:A.field) = name
fun getPosFromField    ({name, escape, typ, pos}:A.field) = pos
fun getNameFromFunDec  ({name, params, result, body, pos}:A.fundec) = name
fun getPosFromFunDec   ({name, params, result, body, pos}:A.fundec) = pos
fun getNameFromTypeDec ({name, ty, pos}) = name
fun getPosFromTypeDec  ({name, ty, pos}) = pos

fun printError(msg, pos) = ErrorMsg.error pos (msg)

fun checkLoopCounter(venv, A.SimpleVar(s,p)) = (case S.look (venv, s) of
                                            SOME(E.VarEntry{ty=vartype, isCounter=c}) =>
                                                if c then printError("Error: Can not assign value to for loop counter", p) else ()
                                          | NONE => ())
  | checkLoopCounter(venv, v:A.var) = ()

fun printType ty = case ty of
    T.RECORD(_,_) =>  "record"
  | T.NIL => "nil"
  | T.INT => "int"
  | T.STRING => "string"
  | T.ARRAY(t,_) => "array of " ^ printType(ty)
  | T.NAME(s,_) => "name of " ^ S.name(s)
  | T.UNIT => "unit"
  | T.BOTTOM => "bottom"

fun tenvLookUp (tenv, n, pos) = case S.look(tenv, n) of
                 SOME x => x
	       | NONE => (printError("Type does not exist in type environment.", pos); T.BOTTOM)

fun makeVarEntry (typ:Types.ty) = E.VarEntry {ty=typ, isCounter=false}

fun checkDups (nil, nil) = ()
  | checkDups (name::others, pos::poss) =
    if (List.all (fn (x) => (name <> x)) others) then checkDups(others, poss)
    else printError("Multiple functions or values or types share a name.", pos)

(*Types.ty -> Types.ty*)
fun actualType (ty:T.ty, pos) =
    case ty of
	T.NAME(sym,tyref) =>
	    (case !tyref of
                SOME(ty) => actualType(ty, pos)
              | NONE => (printError("Tried to call actualType on a NameTy with ref NONE... "
                                    ^ "this should never happen!", pos); T.BOTTOM))
      | T.ARRAY(t,u) => T.ARRAY(actualType(t,pos),u)
      | _ => ty

fun checkType (t1:T.ty, t2:T.ty, pos) =
    let
	val t = actualType(t1,pos)
	val tt = actualType(t2,pos)
    in
	if (t <> tt)
	then case (t,tt) of
		 (T.RECORD(_,_),T.NIL) => ()
	       | (T.NIL,T.RECORD(_,_)) => ()
	       | (_,_) => printError("Expected "   ^ printType(t) ^
				     ", received " ^ printType(tt) , pos)
	else ()
    end

(*Types.ty -> Types.ty*)
(*Simple helper that tells you the type of object stored in an array*)
fun lookupArrayType (Types.ARRAY(ty, u), pos) = ty
  | lookupArrayType (Types.BOTTOM, pos) = Types.BOTTOM
  | lookupArrayType (a:Types.ty, pos) = (printError("Trying to access subscript of a variable that has type " ^ printType a, pos); Types.BOTTOM)

(*Types.ty*symbol -> Types.ty*)
fun traverseFieldList ([], s:S.symbol, pos) = (printError("Record variable does not have field " ^ Symbol.name s, pos); Types.BOTTOM)
  | traverseFieldList ((s1:S.symbol, t:Types.ty)::l, s2:S.symbol, pos) = if s1=s2 then t else traverseFieldList (l, s2, pos)

(*Types.ty*symbol -> Types.ty*)
fun lookupFieldType (Types.RECORD(fieldlist, u), s, pos) = traverseFieldList (fieldlist, s, pos)
  | lookupFieldType(Types.BOTTOM, s, pos) = Types.BOTTOM
  | lookupFieldType (a:Types.ty, s, pos) = (printError("Trying to access field of a variable that has type " ^ printType a ^
                                                        ". Fields can only be accessed from records.", pos);
                                            Types.BOTTOM)

(* venv*tenv*Absyn.var -> Types.ty *)
(* Tells you the type of a variable*)
fun transVar (venv:E.enventry S.table, tenv:T.ty S.table, Absyn.SubscriptVar(v,e,p)) = actualType (lookupArrayType ((transVar(venv, tenv, v),p)), p)
  | transVar (venv:E.enventry S.table, tenv:T.ty S.table, Absyn.FieldVar(v,s,p)) =   actualType (lookupFieldType ((transVar (venv, tenv, v),s,p)), p)
  | transVar (venv:E.enventry S.table, tenv:T.ty S.table, Absyn.SimpleVar(s,p)) = case S.look (venv, s) of
                                              SOME(E.VarEntry{ty=vartype, isCounter=c}) => actualType(vartype, p)
                                            | NONE => (printError("Could not find variable " ^ S.name(s) ^ " in the current scope", p);T.BOTTOM)

(*Checks whether a is the same type as, or a subtype of, b*)
fun isCompatible (T.BOTTOM, b:Types.ty) = true
  | isCompatible (a:T.ty, T.UNIT) = true
  | isCompatible(T.NIL, T.RECORD(arg1,arg2)) = true
  | isCompatible(a:T.ty, b:T.ty) = a=b

(*take header, represent as ty, add to tenv'*)
fun processTypeDecHeads (tenv, []) = tenv
  | processTypeDecHeads (tenv, {name=n, ty=t, pos=p}::l) =
      let
      val nameList = (map getNameFromTypeDec) ({name=n,ty=t,pos=p}::l)
      val posList = (map getPosFromTypeDec) ({name=n,ty=t,pos=p}::l)
      val dup = checkDups(nameList, posList)
      in  processTypeDecHeads (S.enter (tenv, n, Types.NAME(n, ref NONE)), l)
      end

(*Creates (S.symbol * ty) list from field list*)
fun recTyFromFlist (tenv, []) = []
    | recTyFromFlist (tenv, {name=n, escape=e, typ=t, pos=p}::l) = (n, tenvLookUp(tenv, t, p))::recTyFromFlist(tenv, l)

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


fun listContains (element, []) = false
    | listContains (element, a::l) = if (a = element) then true else listContains (element, l)

(* call this function in trans ty after processing a group*)
fun checkTypeCycle (tvisited, tcurr, pos) =
    if listContains(tcurr, tvisited) then (printError("Cyclic type declaration", pos); ())(*we have a loop! throw error*)
    else
        case tcurr of
            T.RECORD(a,b) => ()
            | T.STRING => ()
            | T.INT => ()
            | T.NIL => ()
            | T.NAME(s,tref) => (case !tref of
                           SOME(child) => checkTypeCycle (tcurr::tvisited, child, pos)
                           | NONE => ())
            | T.ARRAY(arrTy,u) => checkTypeCycle (tcurr::tvisited, arrTy, pos)
            | _ => ()


fun checkTypeGroupCycle (tenv, []) = ()
    | checkTypeGroupCycle (tenv, {name=n, ty=t, pos=p}::l) = let val tyDec = tenvLookUp(tenv, n, p)
                                                            in case tyDec of
                                                                T.NAME(s,tref) => (case !tref of
                                                                                  SOME(child) => (checkTypeCycle([tyDec], child, p);
                                                                                                 checkTypeGroupCycle(tenv, l))
                                                                                 | NONE => checkTypeGroupCycle(tenv, l))
                                                                | _ => ()
                                                            end

(***Explained on page 120***)
fun transTy (tenv, Absyn.TypeDec(tylist)) = let val newTenv = processTypeDecBodies (processTypeDecHeads(tenv, tylist), tylist)
                                            in (checkTypeGroupCycle (newTenv, tylist); newTenv) end

  (*
  * transExp is side-effecting: It prints error messages, and returns trexp
  * transExp: (venv*tenv) -> (A.exp -> expty)
  * trexp: A.exp -> expty
  * trvar: A.var -> T.ty
  *)

fun transExp (venv:Env.enventry S.table, tenv:T.ty S.table, isLoop) =
  (*fn(e:Absyn.exp) => {exp=(), ty=T.UNIT}*)
    let fun trexp (A.NilExp) = {exp = (), ty = T.NIL}

          | trexp (A.IntExp(num)) = {exp = (), ty = T.INT}

          | trexp (A.BreakExp(p)) =
	    if isLoop
	    then {exp = (), ty = T.UNIT}
            else (printError("Can't call break outside of a loop body", p); {exp=(), ty=T.UNIT})

          | trexp (A.StringExp(s,p)) = {exp = (), ty = T.STRING}

          | trexp (A.VarExp(v)) = {exp = (), ty = trvar v}

          | trexp (A.OpExp{left=l, oper=oper, right=r, pos=p}) =

	    let
		val {exp=lexp, ty=lty} = trexp l
                val {exp=rexp, ty=rty} = trexp r

	        datatype CLASS = ARITH | COMP | EQ

  	        fun classify (oper) : CLASS =
		    case oper of
		        A.PlusOp   => ARITH
		      | A.MinusOp  => ARITH
		      | A.TimesOp  => ARITH
		      | A.DivideOp => ARITH
		      | A.LtOp     => COMP
		      | A.LeOp     => COMP
		      | A.GtOp     => COMP
		      | A.GeOp     => COMP
		      | A.EqOp     => EQ
		      | A.NeqOp    => EQ

	        fun checkArith () = (checkInt(l,p); checkInt(r,p))

	        fun checkComp () =
		    case lty of
		        T.INT    => checkType(T.INT,rty,p)
		      | T.STRING => checkType(T.STRING,rty,p)
		      | _ => printError("Can only compare int or string types, received "
				        ^ printType lty, p)

	        fun checkEq () =
                    case lty of
		        T.INT          => checkType(T.INT,rty,p)
		      | T.STRING       => checkType(T.STRING,rty,p)
		      | T.ARRAY(t,u)   => checkType(T.ARRAY(t,u),rty,p)
		      | T.RECORD(fs,u) => checkType(T.RECORD(fs,u),rty,p)
                      | T.NIL          => checkType(T.NIL, rty, p)
		      | _ => printError("Can only check equality of int, string, "
					^ "array, or record types, received "
				        ^ printType lty, p)

	    in
		case classify(oper) of
		    ARITH => (checkArith(); {exp=(), ty=T.INT})
		  | COMP  => (checkComp();  {exp=(), ty=T.INT})
		  | EQ    => (checkEq();    {exp=(), ty=T.INT})
	    end



          | trexp (A.LetExp{decs=d, body=b, pos=p}) =
            let val (venv', tenv') = transDecs (venv, tenv, d)
            in  transExp(venv', tenv', false) b
            end

          | trexp (A.RecordExp(rexp)) = {exp=(), ty=checkRecordExp(A.RecordExp(rexp))}

          | trexp (A.AssignExp{var=v,exp=e,pos=p}) =
            let val {exp=e, ty=exptype} = trexp(e)
		            val vartype = trvar(v)
		            val compat = isCompatible(actualType(exptype, p), actualType(vartype, p))
            in checkLoopCounter(venv,v); if compat then {exp=(), ty=T.UNIT} else (printError("Assign statement type incompatible", p); {exp=(), ty=T.UNIT})
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

     (*If elsety is UNIT, thenty must also be unit*)
     else (if thenty <> T.UNIT then printError("Then clause of an if/then statement can not return a value", p) else (); {exp = (), ty = T.UNIT})
             end)

          | trexp (A.WhileExp{test=t, body=b, pos=p}) = (
	      checkInt(t, p);
	      checkUnitTy(b, p);
	      {exp = (), ty = T.UNIT})

	  (*What do we do with the var? Create new scope for variable, use it in ex3 in book ONLY then take out*)
          | trexp (A.ForExp{var=v, escape=e, lo=l, hi=h, body=b, pos=p}) = (
              checkInt(l, p);
              checkInt(h, p);
              checkBody(S.enter (venv, v, Env.VarEntry{ty=T.INT, isCounter=true}), b, v, p);
              {exp = (), ty = T.UNIT})

          | trexp (A.ArrayExp{typ=t, size=s, init=i, pos=p}) =
            (checkInt(s, p);
             let
                 val aTy = actualType(tenvLookUp(tenv, t, p), p)
                 val {exp=_,ty=iTy} = trexp i
             in
                 case aTy of
                     T.ARRAY(ty,u) => if  isCompatible(actualType(iTy, p), actualType(ty, p))
				      then {exp = (), ty = aTy}
				      else (printError("Type mismatch between array and entry", p); {exp = (), ty = T.BOTTOM})
                   | T.BOTTOM => (printError("Array's type does not exist", p); {exp = (), ty = T.BOTTOM})
                   | _ => (printError("This should never happen in arrays", p); {exp = (), ty = T.BOTTOM})
             end)

	  | trexp (A.CallExp{func:A.symbol, args: A.exp list, pos:A.pos}) =
	    let
		val fs = case S.look(venv:E.enventry S.table,func) of
		             SOME(E.FunEntry{formals=fs, result=rt}) => fs
		           | NONE => T.UNIT::[]
		val rt = case S.look(venv,func) of
		             SOME(E.FunEntry{formals=fs, result=rt}) => rt
		           | NONE => (printError("Function " ^ Symbol.name func ^ " is not accessible in current scope", pos);T.BOTTOM)

		fun length l  = foldr (fn(x,y) => 1+y) 0 l
					 
		fun listCompatible ([], [], pos:int) = true
		  | listCompatible (a::aTail, b::bTail, pos:int) =
		    if isCompatible(a,b)
		    then listCompatible(aTail, bTail, pos)
		    else (printError("Argument type does not match parameter type in function declaration.", pos); false)
		  | listCompatible (_,_,pos) = (printError("Number of arguments does not equal number of parameters, expected " ^
							   Int.toString(length fs) ^ " arguments, recieved " ^
							   Int.toString(length args)  ^ " arguments.", pos); false)

		fun actualTypeWrapper typ = actualType(typ, pos)
	    in
      (*Arg1: Actual type of every argument
        Arg2: Actual type of every formal*)
		listCompatible(map actualTypeWrapper (map (fn {exp,ty} => ty) (map trexp args)), map actualTypeWrapper fs, pos);
		{exp = (), ty = rt}
	    end


	and trvar (v:A.var) = (transVar (venv, tenv, v))

	and trseq [] = {exp=(), ty=T.UNIT} (*This should be unit and not bottom!*)
          | trseq ((a,p)::[]) = trexp a
          | trseq ((a,p)::l) = (trexp a; trseq l) (*Call trexp on a for side effects*)

	and checkInt (e:A.exp, pos) =
	    let val {exp=_, ty=eTy} = trexp e
	    in
		if isCompatible(actualType(eTy, pos), T.INT) then () else (printError("Expression is not an int!!", pos))
	    end

	and checkBody (venv':E.enventry S.table, b:A.exp, v, pos:A.pos) =
	    let val {exp=_, ty=bTy} = transExp (venv', tenv, true) b
	    in
		if bTy = T.UNIT then () else (printError("Unit return type expected; received " ^ printType bTy, pos);())
	    end

	and checkUnitTy (e:A.exp, pos:A.pos) =
	    let val {exp=_, ty=eTy} = trexp e
	    in
		if eTy = T.UNIT then () else (printError("Unit return type expected; received " ^ printType eTy, pos);())
	    end

	and checkRecordFields ([], []) = ()
	  | checkRecordFields ((rectypename, rectype)::l1, (recname, recval, pos)::l2) =
	    let val {exp=e, ty=recvaltype} = trexp recval
	    in
		if rectypename <> recname then (printError("Expected field " ^ S.name(recname) ^ " but received " ^ S.name(rectypename), pos))
		else (if isCompatible(actualType(recvaltype,pos), actualType(rectype,pos))
		      then checkRecordFields(l1, l2)
		      else printError("Record fields are not compatible for field " ^ S.name(recname) ^
				      ". Expected " ^ printType rectype ^ " and received " ^ printType recvaltype , pos))
	    end
	  | checkRecordFields (_,_) = ()

	and checkRecordExp (A.RecordExp({fields=fieldlist, typ=typename, pos=p})) =
	    let val recordType = actualType(tenvLookUp(tenv, typename, p), p)
	    in case recordType of
		   Types.RECORD (fieldtypelist, unq) => (checkRecordFields(fieldtypelist, fieldlist);Types.RECORD(fieldtypelist, unq))
		 | _    => (printError("Trying to set fields of something that is not a record type", p);T.BOTTOM)
	    end

	and transDecs (venv:E.enventry S.table, tenv:T.ty S.table, []) = (venv, tenv)
	  | transDecs (venv:E.enventry S.table, tenv:T.ty S.table, a::l:Absyn.dec list) =
            let val (v', t') = transDec (venv, tenv, a)
            in transDecs(v', t', l) end

	and transDec (venv, tenv, Absyn.VarDec(vd)) = (transVarDec(venv, tenv, Absyn.VarDec vd), tenv)
	  | transDec (venv, tenv, Absyn.TypeDec(td)) = (venv, transTy(tenv, Absyn.TypeDec td))
	  | transDec (venv, tenv, Absyn.FunctionDec(fundecs))  = (transFunDec (venv, tenv, Absyn.FunctionDec fundecs), tenv)

	and processFunDecHead ({name, params, result, body, pos}:A.fundec, (venv, tenv)) =
	    let
		(* 1. Check that result has valid type *)
		val rt = case result of
	                     NONE => T.UNIT
			   | SOME (typ, pos) => tenvLookUp(tenv, typ, pos)

		(* 2. Check that params have valid types *)
		fun transparam ({typ,pos,...}:Absyn.field) = tenvLookUp(tenv, typ, pos)
		val names = map getNameFromField params
		val types = map transparam params

	    in

  		(* 3. Check that no params share a name *)
		checkDups(map getNameFromField params, map getPosFromField params);

		(* 4. Return venv with FunEntry *)
                (S.enter(venv, name, E.FunEntry{formals = types, result = rt}),tenv)
	    end

	and processFunDecBody ({name, params, result, body, pos}:A.fundec,(venv,tenv)) =
	    let
		(* 1. Make sure parameters have valid types  *)
		fun transparam ({typ,pos,...}:Absyn.field) = tenvLookUp(tenv, typ, pos)
  		val names = map getNameFromField params
  		val types = map transparam params

		(* 2. Declare the parameters to be in scope within the body of the function *)
  		val vEntries = map makeVarEntry types
  		fun enterVars ((name:S.symbol, vEntry:E.enventry), venv: E.enventry S.table) = S.enter(venv,name,vEntry)
  		fun combineLists ([], []) = []
		  | combineLists(a::aTail, b::bTail) = (a,b) :: combineLists(aTail, bTail)

  		val venv' = foldr enterVars venv (combineLists(names, vEntries));

		(* 3. Make sure body variables are in scope and evaluate the overall result type of the function's body *)
		val {exp,ty} = transExp(venv', tenv, false) body

		(* 4. Make sure that type of body matches expected return type of function *)
		val expectedReturnType = case result of SOME(rSym,rPos) => actualType(tenvLookUp (tenv, rSym, rPos), pos)
					 																| NONE            => T.UNIT
	  val actualReturnType = actualType(ty, pos)

    val checkBodyType =
		case expectedReturnType of
		  T.UNIT   => if actualReturnType = T.UNIT then () else printError("Void function should not be returning a value", pos)
		| _        =>	if isCompatible(actualReturnType, expectedReturnType) then () else printError("Result type of function header does not match result type of function body.", pos)

		in
		(* 5. Result is unimportant, this function is strictly side-effecting *)
		(venv,tenv)
	    end

	and transFunDec (venv: Env.enventry S.table, tenv:T.ty S.table, Absyn.FunctionDec fundecs) =
	    let
  		(* 1. processFunDecHead takes a function header and updates the venv to include the function *)
		val (venv',tenv) = foldl processFunDecHead (venv,tenv) fundecs
	    in
		(*2.  Make sure body variables have valid type and are in scope, compare body and header result types
	              This function is strictly side-effecting for the purpose of returning error messages *)
		foldl processFunDecBody (venv',tenv) fundecs;

  		(* 3. Check that there are no identical function names *)
  		checkDups(map getNameFromFunDec fundecs, map getPosFromFunDec fundecs);

		(* 4. Return the venv outside of the function body's scope *)
		venv'
	    end

	and transVarDec (venv: Env.enventry S.table, tenv:T.ty S.table, Absyn.VarDec{name=varname, escape=esc, typ=vartype, init=i, pos=p}) =
            let val {exp=exp, ty=exptype} = transExp(venv, tenv, false) i
		val venv' = S.enter(venv, varname, Env.VarEntry{ty=exptype, isCounter=false})
		val venv'' = S.enter(venv, varname, Env.VarEntry{ty=T.BOTTOM, isCounter=false})
            in
		case vartype of
		    SOME(vartypename,varpos) => (case S.look(tenv, vartypename) of
		                                     SOME(expectedtype) => if isCompatible(exptype, actualType(expectedtype, p))
									   then venv'
									   else (printError("Variable type does not meet expected type", p);venv'')
						   | NONE => (printError("Could not find type in type environment",p);venv''))
		  | NONE => if exptype = T.NIL then (printError("Implicitly typed variables can not be declared as NIL", p); venv'') else venv'
            end
    in
	trexp
    end

fun transProg exp = (transExp (Env.base_venv, Env.base_tenv, false) exp; ())

structure Main =
struct
  fun translate filename =
    transExp(Env.base_venv, Env.base_tenv, false) (Parse.parse filename);
end

end
