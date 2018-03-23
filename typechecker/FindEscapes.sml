structure A = Absyn
structure E = Env

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
    fun traverseVar(env:escEnv, d:depth, s:Absyn.var): unit =
        case s of
        A.SimpleVar(s, pos) => let val esc = S.look (env, s)
                             in case esc of SOME (d', r) => if d' = d then () else (r:=true;())
                                          | NONE => ()
                             end
        (*call recursively on var*)
        | A.FieldVar(var, symbol, pos) => traverseVar(env, d, var)
        (*call recursively on var, call traverseexp on exp*)
        | A.SubscriptVar(var, exp, pos) => (traverseVar(env, d, var); traverseExp(env, d, exp))

    (*Pattern matches like trexp in semant, check for lets, forexp, vardec, and field (fundec params and record ty)
     Side affects absyn tree to set boolean
     adds to escenv*)
    and traverseExp(env:escEnv, d:depth, s:Absyn.exp): unit =
        case s of
            A.VarExp(var) => traverseVar(env, d, var)
            | A.NilExp => ()
            | A.IntExp(i) => ()
            | A.StringExp(s,p) => ()
            | A.CallExp{func=s, args=expList, pos=p} => (let fun travList [] = ()
                                                                 | travList (exp::l) = (traverseExp(env, d, exp); travList(l))
                                                        in travList(expList) end)
            | A.OpExp{left=lexp, oper=oper, right=rexp, pos=pos} => (traverseExp(env, d, lexp);
                                                                    traverseExp(env, d, rexp))
            | A.RecordExp{fields=flist, typ=symbol, pos=pos} => (let fun checkFlist [] = ()
                                                                         | checkFlist ((symbol', exp', pos')::l) = (traverseExp(env, d, exp'); checkFlist(l))
                                                               in checkFlist(flist) end)
            | A.SeqExp(expList) => (let fun travExpList [] = ()
                                          | travExpList ((exp, _)::l) = (traverseExp(env, d, exp); travExpList(l))
                                 in travExpList(expList) end)
            | A.AssignExp{var=var, exp=exp, pos=pos} => (traverseVar(env, d, var); traverseExp(env, d, exp))
            | A.IfExp{test=exp, then'=texp, else'=elseOpt, pos=pos} => (traverseExp(env, d, exp);
                                                                       traverseExp(env, d, texp);
                                                                       case elseOpt of SOME(opt) => traverseExp(env, d, opt)
                                                                                       | NONE => ())
            | A.WhileExp{test=exp, body=bodExp, pos=pos} => (traverseExp(env, d, exp);
                                                            traverseExp(env, d, bodExp))
            | A.ForExp{var=symbol, escape=escRef, lo=lexp, hi=hexp, body=bodExp, pos=pos} => (let val newEnv = S.enter (env, symbol, (d, escRef))
                                                                                             in
                                                                                               traverseExp(newEnv, d, lexp);
                                                                                               traverseExp(newEnv, d, hexp);
                                                                                               traverseExp(newEnv, d, bodExp)
                                                                                            end)
            | A.BreakExp(pos) => ()
            | A.LetExp{decs=dlist, body=exp, pos=pos} => (traverseDecs(env, d, dlist);
                                                         traverseExp(env, d, exp))
            | A.ArrayExp{typ=symbol, size=exp, init=initExp, pos=pos} => (traverseExp(env, d, exp);
                                                                         traverseExp(env, d, initExp))

    and traverseDec(env, d, s:Absyn.dec):escEnv =
        case s of
           (*For every field in the function create env that has all fields in field list with d' = d+1
             call traverseExp on body with new environment retun env*)
            A.FunctionDec (fdecList) => (let fun traverseFieldDecs (env, []) = env
                                                 | traverseFieldDecs (env, {name=fSym, escape=fBoolRef, typ=_, pos=_}::l) = traverseFieldDecs(S.enter(env, fSym, (d+1, fBoolRef)), l)
                                             fun processfundec ({name=symbol, params=flist, result=_, body=exp, pos=_}) = traverseExp(traverseFieldDecs(env, flist),d+1,exp)
                                        in (map processfundec fdecList; env) end)
            (*Add symbol to escEnv, return new escEnv*)
            | A.VarDec{name=s, escape=boolRef, typ=tyOpt, init=exp, pos=pos} => Symbol.enter (env, s, (d, boolRef))
            (*do nothing*)
            | A.TypeDec(td) => env
        (*Adds s |-> d, (t/f) to env and rturns new escEnv*)

    (*Call on every dec within structure of traverseExp
      Does this just add entries to the escEnv and then return for traverseExp to use recursively?*)
    and traverseDecs(env, d, []): escEnv = env
        | traverseDecs(env, d, s::l: Absyn.dec list): escEnv = traverseDecs(traverseDec(env, d, s), d, l)

(*Top level function*)
    fun findEscape(prog: Absyn.exp): unit = traverseExp(S.empty, 0, prog)
    (**)
end
