signature ENV =
sig
	     
    datatype enventry = VarEntry of {access: Translate.access, ty:Types.ty, isCounter:bool}
                      | FunEntry of {level: Translate.level, label: Temp.label, formals: Types.ty list, result:Types.ty}

    val base_tenv : Types.ty S.table
    val base_venv : enventry S.table

end

structure Env :> ENV =
struct

    structure S = Symbol
    structure Ty = Types
    structure R = Translate

    datatype enventry = VarEntry of {access: R.access, ty:Ty.ty, isCounter:bool}
                      | FunEntry of {level: R.level, label: Temp.label, formals: Ty.ty list, result:Ty.ty}

    (*Basic type environment contains int and string -- DOES THIS NEED NIL?*)
    val base_tenv = S.enter(S.enter(S.enter(S.empty, S.symbol "int", Ty.INT), S.symbol "string", Ty.STRING),S.symbol "nil", Ty.NIL)

    type fun_info = string * Ty.ty list * Ty.ty

    val base_funs : fun_info list =
	[("print",[Ty.STRING],Ty.UNIT),
         ("flush",[],Ty.UNIT),
	 ("getchar",[],Ty.STRING),
	 ("ord",[Ty.STRING],Ty.INT),
	 ("chr",[Ty.INT],Ty.INT),
	 ("substring",[Ty.STRING,Ty.INT,Ty.INT],Ty.STRING),
	 ("concat",[Ty.STRING,Ty.STRING],Ty.STRING),
	 ("not",[Ty.INT],Ty.INT),
	 ("exit",[Ty.INT],Ty.UNIT)]

    val base_venv = List.foldr (fn ((name,formals,result),env) =>
				   S.enter(env,
					   S.symbol name,
					   FunEntry{level   = R.newlevel{parent  = R.outermost,
									 name    = Temp.namedlabel(name),
									 formals = map (fn _ => false) formals},
						    label   = Temp.namedlabel(name),
						    formals = formals,
						    result  = result})
			       ) S.empty base_funs
end
