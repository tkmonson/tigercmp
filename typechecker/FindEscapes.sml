(*Top level like transexp*)
structure FindEscape: sig val findEscape: Absyn.exp -> unit
                      end =
struct
    type depth = int
    type escEnv = (depth * bool ref) Symbol.table

(*Use of var that's been declared*)
    fun traverseVar(env:escEnv, d:depth, s:Absyn.var): unit =
    (*Pattern matches like trexp in semant, check for lets, forexp, vardec, and field (fundec params and record ty)*)
    and traverseExp(env:escEnv, d:depth, s:Absyn.exp): unit =
(*Call on every dec within structure of traverseExp*)
    and traverseDecs(env, d, s: Absyn.dec list): escEnv =

(*Top level function*)
    fun findEscape(prog: Absyn.exp): unit =
    (**)
end

fun1 =
  let
    var a := 5
    fun nest =
      let
        var a := 6
      in
      a
      end
  in a
  end
