structure T = Types
structure R = Translate
structure S = Symbol

signature ENV =
sig

    datatype enventry = VarEntry of {access: Translate.access, ty:T.ty, isCounter:bool}
                      | FunEntry of {level: Translate.level, label: Temp.label, formals: T.ty list, result:T.ty}

    val base_tenv : T.ty S.table
    val base_venv : enventry S.table

end

structure Env :> ENV =
struct

    datatype enventry = VarEntry of {access: R.access, ty:T.ty, isCounter:bool}
                      | FunEntry of {level: R.level, label: Temp.label, formals: T.ty list, result:T.ty}

    (*Basic type environment contains int and string -- DOES THIS NEED NIL?*)
    val base_tenv = S.enter(S.enter(S.enter(S.empty, S.symbol "int", T.INT), S.symbol "string", T.STRING),S.symbol "nil", T.NIL)

    type fun_info = string * T.ty list * T.ty

    val base_funs : fun_info list =
	[("print",[T.STRING],T.UNIT),
         ("flush",[],T.UNIT),
	 ("getchar",[],T.STRING),
	 ("ord",[T.STRING],T.INT),
	 ("chr",[T.INT],T.INT),
	 ("substring",[T.STRING,T.INT,T.INT],T.STRING),
	 ("concat",[T.STRING,T.STRING],T.STRING),
	 ("not",[T.INT],T.INT),
	 ("exit",[T.INT],T.UNIT)]

    val base_venv = List.foldr (fn ((name,formals,result),env) =>
				   S.enter(env,
					   S.symbol name,
					   FunEntry{level   = R.newLevel{parent  = R.outermost,
									 name    = Temp.namedlabel(name),
									 formals = map (fn _ => false) formals},
						    label   = Temp.namedlabel(name),
						    formals = formals,
						    result  = result})
			       ) S.empty base_funs
end
