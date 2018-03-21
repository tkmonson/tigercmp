(*Top level like transexp*)
structure FindEscape: sig val findEscape: Absyn.exp -> unit
                      end =
struct
    type depth = int
    type escEnv = (depth * bool ref) Symbol.table

    (*Use of var that's been declared
      does this just check stuff against escEnv and update boolean
      simple var looks at the Symbol
      field ignore symbol, recurse on var
      subscript recurse on exp AND recurse on var*)
    fun traverseVar(env:escEnv, d:depth, s:Absyn.var): unit = let var esc = S.look (env, s)
                                                              in case esc of SOME ('d, r) => if 'd = d then () else (r:=true;())
                                                                             | NONE => ()

    (*Pattern matches like trexp in semant, check for lets, forexp, vardec, and field (fundec params and record ty)
     Side affects absyn tree to set boolean
     adds to escenv*)
    and traverseExp(env:escEnv, d:depth, s:Absyn.exp): unit =
        case s of
            NilExp
            | A.IntExp(i) = ()
            | A.StringExp(s,p) = ()
            | A.CallExp{func=s, args=expList, pos=p} = let function travList [] = ()
                                                                    | travList exp::l = (traverseExp(env, d, exp); travList(l))
            | A.OpExp{left=lexp, oper=oper, right=rexp, pos=pos} = (traverseExp(env, d, lexp);
                                                                    traverseExp(env, d, rexp))
            | A.RecordExp{fields=flist, typ=symbol, pos=pos} = let function checkFlist [] = ()
                                                                            | checkFlist ('symbol, 'exp, 'pos)::l = (traverseExp('exp); checkFlist(l))
                                                               in checkFlist(flist) end
            | A.SeqExp expList = let function travExpList [] = ()
                                              | travExpList ('exp, 'pos)::l = (traverseExp(env, d, 'exp); travExpList(l))
                                 in travExpList(expList) end
            | A.AssignExp{var=var, exp=exp, pos=pos} = (traverseVar(env, d, var); traverseExp(env, d, exp))
            | A.IfExp{test=exp, then'=texp, else'=elseOpt, pos=pos} = (traverseExp(env, d, exp);
                                                                      traverseExp(env, d, texp);
                                                                      case elseOpt of SOME(opt) = traverseExp(env, d, opt)
                                                                                      | NONE = ())
            | A.WhileExp{test=exp, body=bodExp, pos=pos} = (traverseExp(env, d, exp);
                                                            traverseExp(env, d, bodExp))
            | A.ForExp{var=symbol, escape=escRef, lo=lexp, hi=hexp, body=bodExp, pos=pos} = let val newEnv = S.enter (env, var, (d, escRef))
                                                                                               traverseExp(newEnv, d, lexp);
                                                                                               traverseExp(newEnv, d, hexp);
                                                                                               traverseExp(newEnv, d, bodExp)
                                                                                            end
            | A.BreakExp(pos) = ()
            | A.LetExp{decs=dlist, body=exp, pos=pos} = (traverseDecs(env, d, dlist);
                                                         traverseExp(env, d, exp))
            | A.ArrayExp{typ=symbol, size=exp, init=initExp, pos=pos} = (traverseExp(env, d, exp);
                                                                         traverseExp(env, d, initExp))

    and traverseDec(env, d, s: Absyn.dec): escEnv =
        case s of
            A.FunctionDec (fundec list {name: symbol, params: field list,result: (symbol * pos) option, body: exp, pos: pos} (*TODO (d+1)*)
            | A.VarDec{name: symbol, escape: bool ref, typ: (symbol * pos) option, init: exp, pos: pos}
            | A.TypeDec of {name: symbol, ty: ty, pos: pos} listS.enter(env, ())
        (*Adds s |-> d, (t/f) to env and rturns new escEnv*)

    (*Call on every dec within structure of traverseExp
      Does this just add entries to the escEnv and then return for traverseExp to use recursively?*)
    and traverseDecs(env, d, []): escEnv = env
        | traverseDecs(env, d, s::l: Absyn.dec list): escEnv = traverseDecs(traverseDec(env, d, s), d, l)

(*Top level function*)
    fun findEscape(prog: Absyn.exp): unit = traverseExp(newEnv, 0, prog)
    (**)
end
