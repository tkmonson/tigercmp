functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\095\000\009\000\095\000\015\000\020\000\016\000\019\000\
\\017\000\018\000\018\000\017\000\019\000\016\000\020\000\015\000\
\\021\000\014\000\022\000\013\000\023\000\012\000\024\000\011\000\
\\030\000\095\000\031\000\095\000\037\000\095\000\038\000\095\000\000\000\
\\001\000\001\000\102\000\007\000\102\000\009\000\102\000\015\000\020\000\
\\016\000\019\000\017\000\018\000\018\000\017\000\030\000\102\000\
\\031\000\102\000\037\000\102\000\038\000\102\000\000\000\
\\001\000\001\000\103\000\007\000\103\000\009\000\103\000\015\000\020\000\
\\016\000\019\000\017\000\018\000\018\000\017\000\030\000\103\000\
\\031\000\103\000\037\000\103\000\038\000\103\000\000\000\
\\001\000\001\000\104\000\007\000\104\000\009\000\104\000\015\000\020\000\
\\016\000\019\000\017\000\018\000\018\000\017\000\030\000\104\000\
\\031\000\104\000\037\000\104\000\038\000\104\000\000\000\
\\001\000\001\000\105\000\007\000\105\000\009\000\105\000\015\000\020\000\
\\016\000\019\000\017\000\018\000\018\000\017\000\030\000\105\000\
\\031\000\105\000\037\000\105\000\038\000\105\000\000\000\
\\001\000\001\000\106\000\007\000\106\000\009\000\106\000\015\000\020\000\
\\016\000\019\000\017\000\018\000\018\000\017\000\030\000\106\000\
\\031\000\106\000\037\000\106\000\038\000\106\000\000\000\
\\001\000\001\000\107\000\007\000\107\000\009\000\107\000\015\000\020\000\
\\016\000\019\000\017\000\018\000\018\000\017\000\030\000\107\000\
\\031\000\107\000\037\000\107\000\038\000\107\000\000\000\
\\001\000\002\000\044\000\000\000\
\\001\000\002\000\044\000\012\000\061\000\028\000\060\000\000\000\
\\001\000\002\000\045\000\000\000\
\\001\000\002\000\046\000\000\000\
\\001\000\002\000\066\000\000\000\
\\001\000\002\000\066\000\009\000\065\000\000\000\
\\001\000\003\000\010\000\004\000\009\000\008\000\008\000\029\000\007\000\
\\036\000\006\000\041\000\005\000\000\000\
\\001\000\005\000\072\000\009\000\071\000\000\000\
\\001\000\005\000\072\000\013\000\078\000\000\000\
\\001\000\006\000\052\000\027\000\051\000\000\000\
\\001\000\006\000\074\000\019\000\073\000\000\000\
\\001\000\006\000\075\000\000\000\
\\001\000\006\000\081\000\019\000\080\000\000\000\
\\001\000\007\000\021\000\009\000\048\000\015\000\020\000\016\000\019\000\
\\017\000\018\000\018\000\017\000\019\000\016\000\020\000\015\000\
\\021\000\014\000\022\000\013\000\023\000\012\000\024\000\011\000\000\000\
\\001\000\007\000\021\000\015\000\020\000\016\000\019\000\017\000\018\000\
\\018\000\017\000\019\000\016\000\020\000\015\000\021\000\014\000\
\\022\000\013\000\023\000\012\000\024\000\011\000\030\000\047\000\000\000\
\\001\000\007\000\021\000\015\000\020\000\016\000\019\000\017\000\018\000\
\\018\000\017\000\019\000\016\000\020\000\015\000\021\000\014\000\
\\022\000\013\000\023\000\012\000\024\000\011\000\038\000\055\000\000\000\
\\001\000\008\000\053\000\000\000\
\\001\000\019\000\050\000\000\000\
\\001\000\019\000\088\000\000\000\
\\001\000\019\000\089\000\000\000\
\\001\000\027\000\070\000\000\000\
\\001\000\037\000\042\000\000\000\
\\001\000\039\000\068\000\000\000\
\\001\000\042\000\028\000\043\000\027\000\044\000\026\000\000\000\
\\093\000\007\000\021\000\015\000\020\000\016\000\019\000\017\000\018\000\
\\018\000\017\000\019\000\016\000\020\000\015\000\021\000\014\000\
\\022\000\013\000\023\000\012\000\024\000\011\000\000\000\
\\094\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\017\000\018\000\018\000\017\000\000\000\
\\099\000\017\000\018\000\018\000\017\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\108\000\007\000\021\000\015\000\020\000\016\000\019\000\017\000\018\000\
\\018\000\017\000\019\000\016\000\020\000\015\000\021\000\014\000\
\\022\000\013\000\023\000\012\000\024\000\011\000\000\000\
\\109\000\007\000\021\000\015\000\020\000\016\000\019\000\017\000\018\000\
\\018\000\017\000\019\000\016\000\020\000\015\000\021\000\014\000\
\\022\000\013\000\023\000\012\000\024\000\011\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\007\000\021\000\015\000\020\000\016\000\019\000\017\000\018\000\
\\018\000\017\000\019\000\016\000\020\000\015\000\021\000\014\000\
\\022\000\013\000\023\000\012\000\024\000\011\000\000\000\
\\126\000\007\000\021\000\015\000\020\000\016\000\019\000\017\000\018\000\
\\018\000\017\000\019\000\016\000\020\000\015\000\021\000\014\000\
\\022\000\013\000\023\000\012\000\024\000\011\000\000\000\
\\127\000\007\000\021\000\015\000\020\000\016\000\019\000\017\000\018\000\
\\018\000\017\000\019\000\016\000\020\000\015\000\021\000\014\000\
\\022\000\013\000\023\000\012\000\024\000\011\000\000\000\
\\128\000\007\000\021\000\015\000\020\000\016\000\019\000\017\000\018\000\
\\018\000\017\000\019\000\016\000\020\000\015\000\021\000\014\000\
\\022\000\013\000\023\000\012\000\024\000\011\000\000\000\
\\129\000\007\000\021\000\015\000\020\000\016\000\019\000\017\000\018\000\
\\018\000\017\000\019\000\016\000\020\000\015\000\021\000\014\000\
\\022\000\013\000\023\000\012\000\024\000\011\000\000\000\
\\130\000\007\000\021\000\015\000\020\000\016\000\019\000\017\000\018\000\
\\018\000\017\000\019\000\016\000\020\000\015\000\021\000\014\000\
\\022\000\013\000\023\000\012\000\024\000\011\000\000\000\
\"
val actionRowNumbers =
"\014\000\043\000\032\000\042\000\
\\031\000\014\000\014\000\035\000\
\\034\000\014\000\014\000\014\000\
\\014\000\014\000\014\000\014\000\
\\014\000\014\000\014\000\014\000\
\\046\000\045\000\029\000\047\000\
\\008\000\010\000\011\000\022\000\
\\021\000\007\000\006\000\005\000\
\\004\000\003\000\002\000\039\000\
\\038\000\037\000\036\000\001\000\
\\014\000\025\000\048\000\017\000\
\\024\000\014\000\033\000\023\000\
\\009\000\014\000\008\000\013\000\
\\040\000\044\000\052\000\051\000\
\\050\000\049\000\030\000\012\000\
\\058\000\028\000\015\000\018\000\
\\019\000\014\000\008\000\016\000\
\\014\000\020\000\012\000\014\000\
\\008\000\008\000\041\000\053\000\
\\054\000\057\000\014\000\008\000\
\\055\000\062\000\026\000\056\000\
\\061\000\027\000\014\000\014\000\
\\060\000\059\000\000\000"
val gotoT =
"\
\\001\000\002\000\002\000\090\000\012\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\023\000\004\000\022\000\005\000\021\000\006\000\020\000\000\000\
\\001\000\027\000\012\000\001\000\000\000\
\\001\000\028\000\012\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\029\000\012\000\001\000\000\000\
\\001\000\030\000\012\000\001\000\000\000\
\\001\000\031\000\012\000\001\000\000\000\
\\001\000\032\000\012\000\001\000\000\000\
\\001\000\033\000\012\000\001\000\000\000\
\\001\000\034\000\012\000\001\000\000\000\
\\001\000\035\000\012\000\001\000\000\000\
\\001\000\036\000\012\000\001\000\000\000\
\\001\000\037\000\012\000\001\000\000\000\
\\001\000\038\000\012\000\001\000\000\000\
\\001\000\039\000\012\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\041\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\047\000\012\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\052\000\012\000\001\000\000\000\
\\000\000\
\\000\000\
\\007\000\057\000\008\000\056\000\009\000\055\000\010\000\054\000\000\000\
\\001\000\060\000\012\000\001\000\000\000\
\\008\000\061\000\000\000\
\\011\000\062\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\067\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\074\000\012\000\001\000\000\000\
\\008\000\075\000\000\000\
\\000\000\
\\001\000\077\000\012\000\001\000\000\000\
\\000\000\
\\011\000\080\000\000\000\
\\001\000\081\000\012\000\001\000\000\000\
\\008\000\082\000\000\000\
\\008\000\083\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\084\000\012\000\001\000\000\000\
\\008\000\085\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\088\000\012\000\001\000\000\000\
\\001\000\089\000\012\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 91
val numrules = 38
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 31) => true | (T 32) => true | (T 33) => true | (T 39) => true
 | (T 35) => true | (T 36) => true | (T 37) => true | (T 41) => true
 | (T 42) => true | (T 43) => true | (T 27) => true | (T 28) => true
 | (T 29) => true | (T 30) => true | (T 34) => true | (T 38) => true
 | (T 40) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 29))::
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "AND"
  | (T 25) => "OR"
  | (T 26) => "ASSIGN"
  | (T 27) => "ARRAY"
  | (T 28) => "IF"
  | (T 29) => "THEN"
  | (T 30) => "ELSE"
  | (T 31) => "WHILE"
  | (T 32) => "FOR"
  | (T 33) => "TO"
  | (T 34) => "DO"
  | (T 35) => "LET"
  | (T 36) => "IN"
  | (T 37) => "END"
  | (T 38) => "OF"
  | (T 39) => "BREAK"
  | (T 40) => "NIL"
  | (T 41) => "FUNCTION"
  | (T 42) => "VAR"
  | (T 43) => "TYPE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37)
 $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30)
 $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23)
 $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID exp1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.ntVOID (fn _ => ( let val  INT1 = INT1
 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 STRING1 = STRING1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, STRING1left, STRING1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, IF1left, exp2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ntVOID exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _
)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, IF1left, exp3right), rest671)
end
|  ( 17, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.ntVOID let1, let1left, let1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
let1 = let1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, let1left, let1right), rest671)
end
|  ( 19, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.ntVOID exp1, _,
 _)) :: _ :: ( _, ( MlyValue.ntVOID dec1, _, _)) :: ( _, ( _, LET1left
, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  dec1 = dec1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 11, ( result, LET1left, END1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.ntVOID tydec1, tydec1left, tydec1right)) ::
 rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
tydec1 = tydec1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, tydec1left, tydec1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.ntVOID vardec1, vardec1left, vardec1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 vardec1 = vardec1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, vardec1left, vardec1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.ntVOID fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 fundec1 = fundec1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, fundec1left, fundec1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, ID1left, ID1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.ntVOID ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ntVOID tyid1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671))
 => let val  result = MlyValue.ntVOID (fn _ => ( let val  tyid1 = 
tyid1 ()
 val  ty1 = ty1 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, TYPE1left, ty1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.ntVOID tyid1, tyid1left, tyid1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
tyid1 = tyid1 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, tyid1left, tyid1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.ntVOID arrty1, arrty1left, arrty1right)) ::
 rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
arrty1 = arrty1 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, arrty1left, arrty1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.ntVOID recty1, recty1left, recty1right)) ::
 rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
recty1 = recty1 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, recty1left, recty1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ntVOID tyid1, _, tyid1right)) :: _ :: ( _, 
( _, ARRAY1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  tyid1 = tyid1 ()
 in ()
end; ()))
 in ( LrTable.NT 8, ( result, ARRAY1left, tyid1right), rest671)
end
|  ( 29, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID 
fieldec1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  fieldec1 = fieldec1
 ()
 in ()
end; ()))
 in ( LrTable.NT 9, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.ntVOID fieldec2, _, fieldec2right)) :: _ ::
 ( _, ( MlyValue.ntVOID fieldec1, fieldec1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  fieldec1 = 
fieldec1 ()
 val  fieldec2 = fieldec2 ()
 in ()
end; ()))
 in ( LrTable.NT 10, ( result, fieldec1left, fieldec2right), rest671)

end
|  ( 31, ( ( _, ( MlyValue.ntVOID tyid1, _, tyid1right)) :: _ :: ( _, 
( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  tyid1 = tyid1 ()
 in ()
end; ()))
 in ( LrTable.NT 10, ( result, ID1left, tyid1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ntVOID tyid1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _))
 :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  tyid1 = tyid1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, VAR1left, exp1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, VAR1left, exp1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ntVOID tyid1, _, _)) :: _ :: _ :: ( _, ( MlyValue.ntVOID 
fieldec1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  ID1 = ID1 ()
 val  fieldec1 = fieldec1 ()
 val  tyid1 = tyid1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ntVOID tyid1, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.ID ID1
, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  tyid1 = tyid1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: _ :: (
 _, ( MlyValue.ntVOID fieldec1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1,
 _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  fieldec1 = fieldec1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: _ :: _
 :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1
 = ID1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, FUNCTION1left, exp1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
end
end
