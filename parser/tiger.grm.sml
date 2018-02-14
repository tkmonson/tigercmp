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
structure A = Absyn


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\167\000\005\000\167\000\007\000\167\000\009\000\167\000\
\\011\000\167\000\013\000\167\000\015\000\028\000\016\000\027\000\
\\018\000\026\000\019\000\025\000\020\000\024\000\021\000\023\000\
\\022\000\022\000\023\000\021\000\024\000\020\000\025\000\019\000\
\\031\000\167\000\032\000\167\000\035\000\167\000\036\000\167\000\
\\038\000\167\000\039\000\167\000\043\000\167\000\044\000\167\000\
\\045\000\167\000\000\000\
\\001\000\001\000\168\000\005\000\168\000\007\000\168\000\009\000\168\000\
\\011\000\168\000\013\000\168\000\015\000\028\000\016\000\027\000\
\\018\000\026\000\019\000\025\000\020\000\024\000\021\000\023\000\
\\022\000\022\000\023\000\021\000\024\000\020\000\025\000\019\000\
\\031\000\168\000\032\000\168\000\035\000\168\000\036\000\168\000\
\\038\000\168\000\039\000\168\000\043\000\168\000\044\000\168\000\
\\045\000\168\000\000\000\
\\001\000\001\000\169\000\005\000\169\000\007\000\169\000\009\000\169\000\
\\011\000\169\000\013\000\169\000\015\000\028\000\016\000\027\000\
\\018\000\026\000\019\000\025\000\026\000\169\000\027\000\169\000\
\\031\000\169\000\032\000\169\000\035\000\169\000\036\000\169\000\
\\038\000\169\000\039\000\169\000\043\000\169\000\044\000\169\000\
\\045\000\169\000\000\000\
\\001\000\001\000\170\000\005\000\170\000\007\000\170\000\009\000\170\000\
\\011\000\170\000\013\000\170\000\015\000\028\000\016\000\027\000\
\\018\000\026\000\019\000\025\000\026\000\170\000\027\000\170\000\
\\031\000\170\000\032\000\170\000\035\000\170\000\036\000\170\000\
\\038\000\170\000\039\000\170\000\043\000\170\000\044\000\170\000\
\\045\000\170\000\000\000\
\\001\000\001\000\171\000\005\000\171\000\007\000\171\000\009\000\171\000\
\\011\000\171\000\013\000\171\000\015\000\028\000\016\000\027\000\
\\018\000\026\000\019\000\025\000\026\000\171\000\027\000\171\000\
\\031\000\171\000\032\000\171\000\035\000\171\000\036\000\171\000\
\\038\000\171\000\039\000\171\000\043\000\171\000\044\000\171\000\
\\045\000\171\000\000\000\
\\001\000\001\000\172\000\005\000\172\000\007\000\172\000\009\000\172\000\
\\011\000\172\000\013\000\172\000\015\000\028\000\016\000\027\000\
\\018\000\026\000\019\000\025\000\026\000\172\000\027\000\172\000\
\\031\000\172\000\032\000\172\000\035\000\172\000\036\000\172\000\
\\038\000\172\000\039\000\172\000\043\000\172\000\044\000\172\000\
\\045\000\172\000\000\000\
\\001\000\001\000\173\000\005\000\173\000\007\000\173\000\009\000\173\000\
\\011\000\173\000\013\000\173\000\015\000\028\000\016\000\027\000\
\\018\000\026\000\019\000\025\000\026\000\173\000\027\000\173\000\
\\031\000\173\000\032\000\173\000\035\000\173\000\036\000\173\000\
\\038\000\173\000\039\000\173\000\043\000\173\000\044\000\173\000\
\\045\000\173\000\000\000\
\\001\000\001\000\174\000\005\000\174\000\007\000\174\000\009\000\174\000\
\\011\000\174\000\013\000\174\000\015\000\028\000\016\000\027\000\
\\018\000\026\000\019\000\025\000\026\000\174\000\027\000\174\000\
\\031\000\174\000\032\000\174\000\035\000\174\000\036\000\174\000\
\\038\000\174\000\039\000\174\000\043\000\174\000\044\000\174\000\
\\045\000\174\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\009\000\043\000\016\000\011\000\030\000\010\000\033\000\009\000\
\\034\000\008\000\037\000\007\000\041\000\006\000\042\000\005\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\016\000\011\000\030\000\010\000\033\000\009\000\034\000\008\000\
\\037\000\007\000\039\000\081\000\041\000\006\000\042\000\005\000\000\000\
\\001\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\016\000\011\000\030\000\010\000\033\000\009\000\034\000\008\000\
\\037\000\007\000\041\000\006\000\042\000\005\000\000\000\
\\001\000\002\000\037\000\000\000\
\\001\000\002\000\064\000\000\000\
\\001\000\002\000\065\000\000\000\
\\001\000\002\000\066\000\000\000\
\\001\000\002\000\074\000\000\000\
\\001\000\002\000\105\000\012\000\104\000\029\000\103\000\000\000\
\\001\000\002\000\107\000\000\000\
\\001\000\002\000\110\000\000\000\
\\001\000\002\000\110\000\009\000\109\000\000\000\
\\001\000\002\000\110\000\013\000\120\000\000\000\
\\001\000\002\000\134\000\000\000\
\\001\000\002\000\141\000\000\000\
\\001\000\002\000\142\000\000\000\
\\001\000\002\000\144\000\000\000\
\\001\000\002\000\146\000\000\000\
\\001\000\005\000\123\000\009\000\122\000\000\000\
\\001\000\005\000\123\000\013\000\135\000\000\000\
\\001\000\006\000\084\000\028\000\083\000\000\000\
\\001\000\006\000\125\000\020\000\124\000\000\000\
\\001\000\006\000\126\000\000\000\
\\001\000\006\000\138\000\020\000\137\000\000\000\
\\001\000\007\000\071\000\009\000\070\000\000\000\
\\001\000\007\000\073\000\009\000\072\000\015\000\028\000\016\000\027\000\
\\018\000\026\000\019\000\025\000\020\000\024\000\021\000\023\000\
\\022\000\022\000\023\000\021\000\024\000\020\000\025\000\019\000\
\\026\000\018\000\027\000\017\000\000\000\
\\001\000\008\000\085\000\000\000\
\\001\000\009\000\096\000\000\000\
\\001\000\011\000\095\000\015\000\028\000\016\000\027\000\018\000\026\000\
\\019\000\025\000\020\000\024\000\021\000\023\000\022\000\022\000\
\\023\000\021\000\024\000\020\000\025\000\019\000\026\000\018\000\
\\027\000\017\000\000\000\
\\001\000\011\000\129\000\015\000\028\000\016\000\027\000\018\000\026\000\
\\019\000\025\000\020\000\024\000\021\000\023\000\022\000\022\000\
\\023\000\021\000\024\000\020\000\025\000\019\000\026\000\018\000\
\\027\000\017\000\000\000\
\\001\000\013\000\093\000\000\000\
\\001\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\
\\031\000\069\000\000\000\
\\001\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\
\\035\000\111\000\000\000\
\\001\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\
\\036\000\068\000\000\000\
\\001\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\
\\036\000\143\000\000\000\
\\001\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\
\\039\000\099\000\000\000\
\\001\000\020\000\082\000\000\000\
\\001\000\020\000\094\000\000\000\
\\001\000\020\000\147\000\000\000\
\\001\000\020\000\149\000\000\000\
\\001\000\020\000\150\000\000\000\
\\001\000\028\000\067\000\000\000\
\\001\000\028\000\121\000\000\000\
\\001\000\038\000\062\000\000\000\
\\001\000\040\000\118\000\000\000\
\\156\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\157\000\000\000\
\\158\000\000\000\
\\159\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\160\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\163\000\018\000\026\000\019\000\025\000\000\000\
\\164\000\018\000\026\000\019\000\025\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\175\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\176\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\180\000\000\000\
\\181\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\182\000\000\000\
\\183\000\028\000\016\000\000\000\
\\184\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\043\000\036\000\044\000\035\000\045\000\034\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\197\000\000\000\
\\198\000\000\000\
\\199\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\202\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\203\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\204\000\005\000\123\000\000\000\
\\205\000\000\000\
\\206\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\207\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\208\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\209\000\015\000\028\000\016\000\027\000\018\000\026\000\019\000\025\000\
\\020\000\024\000\021\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\026\000\018\000\027\000\017\000\000\000\
\\212\000\000\000\
\\213\000\002\000\076\000\000\000\
\\214\000\000\000\
\\215\000\005\000\131\000\015\000\028\000\016\000\027\000\018\000\026\000\
\\019\000\025\000\020\000\024\000\021\000\023\000\022\000\022\000\
\\023\000\021\000\024\000\020\000\025\000\019\000\026\000\018\000\
\\027\000\017\000\000\000\
\\216\000\000\000\
\\217\000\002\000\015\000\003\000\014\000\004\000\013\000\008\000\012\000\
\\016\000\011\000\030\000\010\000\033\000\009\000\034\000\008\000\
\\037\000\007\000\041\000\006\000\042\000\005\000\000\000\
\\218\000\000\000\
\\219\000\005\000\098\000\015\000\028\000\016\000\027\000\018\000\026\000\
\\019\000\025\000\020\000\024\000\021\000\023\000\022\000\022\000\
\\023\000\021\000\024\000\020\000\025\000\019\000\026\000\018\000\
\\027\000\017\000\000\000\
\\220\000\000\000\
\\221\000\000\000\
\\222\000\000\000\
\\223\000\008\000\048\000\010\000\047\000\012\000\046\000\014\000\045\000\000\000\
\\223\000\010\000\092\000\014\000\045\000\000\000\
\\223\000\010\000\092\000\014\000\045\000\040\000\116\000\000\000\
\"
val actionRowNumbers =
"\011\000\073\000\054\000\067\000\
\\070\000\084\000\012\000\011\000\
\\011\000\011\000\009\000\060\000\
\\059\000\111\000\011\000\011\000\
\\011\000\011\000\011\000\011\000\
\\011\000\011\000\011\000\011\000\
\\011\000\011\000\011\000\081\000\
\\082\000\080\000\052\000\084\000\
\\013\000\014\000\015\000\050\000\
\\042\000\040\000\072\000\033\000\
\\034\000\068\000\108\000\016\000\
\\101\000\011\000\105\000\069\000\
\\002\000\001\000\008\000\007\000\
\\006\000\005\000\004\000\003\000\
\\064\000\063\000\062\000\061\000\
\\010\000\083\000\045\000\029\000\
\\035\000\011\000\011\000\011\000\
\\056\000\011\000\055\000\011\000\
\\112\000\039\000\046\000\037\000\
\\036\000\107\000\044\000\079\000\
\\017\000\011\000\018\000\020\000\
\\041\000\071\000\065\000\058\000\
\\057\000\110\000\011\000\075\000\
\\011\000\113\000\076\000\104\000\
\\011\000\078\000\088\000\087\000\
\\085\000\053\000\021\000\086\000\
\\093\000\051\000\027\000\030\000\
\\031\000\011\000\011\000\038\000\
\\103\000\109\000\011\000\107\000\
\\022\000\028\000\091\000\011\000\
\\032\000\019\000\011\000\023\000\
\\024\000\043\000\066\000\112\000\
\\100\000\025\000\074\000\106\000\
\\089\000\090\000\092\000\011\000\
\\026\000\094\000\099\000\047\000\
\\095\000\011\000\048\000\098\000\
\\049\000\011\000\077\000\011\000\
\\011\000\097\000\103\000\096\000\
\\102\000\000\000"
val gotoT =
"\
\\001\000\153\000\002\000\002\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\031\000\005\000\030\000\006\000\029\000\010\000\028\000\
\\012\000\027\000\000\000\
\\000\000\
\\002\000\036\000\018\000\001\000\000\000\
\\002\000\037\000\018\000\001\000\000\000\
\\002\000\038\000\018\000\001\000\000\000\
\\002\000\040\000\003\000\039\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\019\000\042\000\000\000\
\\002\000\047\000\018\000\001\000\000\000\
\\002\000\048\000\018\000\001\000\000\000\
\\002\000\049\000\018\000\001\000\000\000\
\\002\000\050\000\018\000\001\000\000\000\
\\002\000\051\000\018\000\001\000\000\000\
\\002\000\052\000\018\000\001\000\000\000\
\\002\000\053\000\018\000\001\000\000\000\
\\002\000\054\000\018\000\001\000\000\000\
\\002\000\055\000\018\000\001\000\000\000\
\\002\000\056\000\018\000\001\000\000\000\
\\002\000\057\000\018\000\001\000\000\000\
\\002\000\058\000\018\000\001\000\000\000\
\\002\000\059\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\031\000\005\000\061\000\006\000\029\000\010\000\028\000\
\\012\000\027\000\000\000\
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
\\014\000\073\000\000\000\
\\002\000\075\000\018\000\001\000\000\000\
\\002\000\077\000\016\000\076\000\018\000\001\000\000\000\
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
\\002\000\078\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\084\000\018\000\001\000\000\000\
\\002\000\085\000\018\000\001\000\000\000\
\\002\000\086\000\018\000\001\000\000\000\
\\000\000\
\\002\000\087\000\018\000\001\000\000\000\
\\000\000\
\\002\000\088\000\018\000\001\000\000\000\
\\019\000\089\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\095\000\000\000\
\\000\000\
\\000\000\
\\007\000\100\000\008\000\099\000\009\000\098\000\000\000\
\\002\000\104\000\018\000\001\000\000\000\
\\000\000\
\\011\000\106\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\111\000\018\000\001\000\000\000\
\\000\000\
\\002\000\112\000\018\000\001\000\000\000\
\\019\000\113\000\000\000\
\\000\000\
\\000\000\
\\002\000\115\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\117\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\125\000\018\000\001\000\000\000\
\\002\000\126\000\018\000\001\000\000\000\
\\000\000\
\\015\000\128\000\000\000\
\\000\000\
\\002\000\130\000\018\000\001\000\000\000\
\\017\000\131\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\134\000\018\000\001\000\000\000\
\\000\000\
\\011\000\137\000\000\000\
\\002\000\138\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\019\000\113\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\143\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\146\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\149\000\018\000\001\000\000\000\
\\000\000\
\\002\000\150\000\018\000\001\000\000\000\
\\002\000\151\000\018\000\001\000\000\000\
\\000\000\
\\015\000\152\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 154
val numrules = 68
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
 | ID of unit ->  (string) | ltail of unit ->  (A.var)
 | lvalue of unit ->  (A.var) | funArgsTail of unit ->  (A.exp list)
 | funArgs of unit ->  (A.exp list)
 | flistTail of unit ->  (A.field list)
 | flist of unit ->  (A.field list) | field of unit ->  (A.field)
 | funDec of unit ->  (A.dec list) | fieldDec of unit ->  (A.field)
 | vardec of unit ->  (A.dec) | recty of unit ->  (A.ty)
 | arrty of unit ->  (A.ty) | ty of unit ->  (A.ty)
 | tyDec of unit ->  (A.dec list) | decList of unit ->  (A.dec list)
 | dec of unit ->  (A.dec)
 | expList of unit ->  ( ( A.exp * pos )  list)
 | exp of unit ->  (A.exp) | program of unit ->  (A.exp)
end
type svalue = MlyValue.svalue
type result = A.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 32) => true | (T 33) => true | (T 34) => true | (T 40) => true
 | (T 36) => true | (T 37) => true | (T 38) => true | (T 42) => true
 | (T 43) => true | (T 44) => true | (T 28) => true | (T 29) => true
 | (T 30) => true | (T 31) => true | (T 35) => true | (T 39) => true
 | (T 41) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 31))::
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
  | (T 16) => "UMINUS"
  | (T 17) => "TIMES"
  | (T 18) => "DIVIDE"
  | (T 19) => "EQ"
  | (T 20) => "NEQ"
  | (T 21) => "LT"
  | (T 22) => "LE"
  | (T 23) => "GT"
  | (T 24) => "GE"
  | (T 25) => "AND"
  | (T 26) => "OR"
  | (T 27) => "ASSIGN"
  | (T 28) => "ARRAY"
  | (T 29) => "IF"
  | (T 30) => "THEN"
  | (T 31) => "ELSE"
  | (T 32) => "WHILE"
  | (T 33) => "FOR"
  | (T 34) => "TO"
  | (T 35) => "DO"
  | (T 36) => "LET"
  | (T 37) => "IN"
  | (T 38) => "END"
  | (T 39) => "OF"
  | (T 40) => "BREAK"
  | (T 41) => "NIL"
  | (T 42) => "FUNCTION"
  | (T 43) => "VAR"
  | (T 44) => "TYPE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.program (fn _ => let val  exp1 = exp1
 ()
 in (exp1)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 in (exp1)
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 2, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expList 
expList1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.exp (fn _ => let val  expList1 = expList1 ()
 in (A.SeqExp expList1)
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.exp exp2, exp2left, exp2right)) :: _ :: ( _,
 ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.expList (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((exp1,exp1left:pos)::(exp2,exp2left:pos)::[])
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: _ :: ( _,
 ( MlyValue.expList expList1, expList1left, _)) :: rest671)) => let
 val  result = MlyValue.expList (fn _ => let val  expList1 = expList1
 ()
 val  exp1 = exp1 ()
 in ((exp1,exp1left:pos)::(expList1))
end)
 in ( LrTable.NT 2, ( result, expList1left, exp1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (A.IntExp INT)
end)
 in ( LrTable.NT 1, ( result, INT1left, INT1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left),
 STRING1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (STRING as STRING1) = STRING1 ()
 in (A.StringExp(STRING, STRINGleft))
end)
 in ( LrTable.NT 1, ( result, STRING1left, STRING1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.PlusOp, right=exp2, pos=exp1left:pos})

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.MinusOp, right=exp2, pos=exp1left:pos})

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.TimesOp, right=exp2, pos=exp1left:pos})

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.DivideOp, right=exp2, pos=exp1left:pos}
)
end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp{test=exp1, then'=exp2, else'=SOME (A.IntExp 0), pos=exp1left})

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp{test=exp1, then'=A.IntExp 1, else'=SOME exp2, pos=exp1left})

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.EqOp, right=exp2, pos=exp1left:pos})

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.NeqOp, right=exp2, pos=exp1left:pos})

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.LtOp, right=exp2, pos=exp1left:pos})

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.LeOp, right=exp2, pos=exp1left:pos})

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.GtOp, right=exp2, pos=exp1left:pos})

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp{left=exp1, oper=A.GeOp, right=exp2, pos=exp1left:pos})

end)
 in ( LrTable.NT 1, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: ( _, ( _, IF1left, _)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.IfExp{test=exp1, then'=exp2, else'=NONE, pos=exp1left})
end)
 in ( LrTable.NT 1, ( result, IF1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left,
 _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (A.IfExp{test=exp1, then'=exp2, else'=SOME(exp3), pos=exp1left})

end)
 in ( LrTable.NT 1, ( result, IF1left, exp3right), rest671)
end
|  ( 21, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => (A.NilExp))
 in ( LrTable.NT 1, ( result, NIL1left, NIL1right), rest671)
end
|  ( 22, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => (A.NilExp))
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.lvalue lvalue1, lvalue1left, _)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => let val  lvalue1 = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in (A.AssignExp{var=lvalue1, exp=exp1, pos=lvalue1left})
end)
 in ( LrTable.NT 1, ( result, lvalue1left, exp1right), rest671)
end
|  ( 24, ( ( _, ( _, (BREAKleft as BREAK1left), BREAK1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => (
A.BreakExp BREAKleft))
 in ( LrTable.NT 1, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: ( _, ( _, WHILE1left, _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = 
exp1 ()
 val  exp2 = exp2 ()
 in (A.WhileExp{test=exp1, body=exp2,pos=exp1left})
end)
 in ( LrTable.NT 1, ( result, WHILE1left, exp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: ( _, ( _
, MINUS1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  exp1 = exp1 ()
 in (
A.OpExp{left=(A.IntExp 0), oper=A.MinusOp, right=exp1, pos=exp1left})

end)
 in ( LrTable.NT 1, ( result, MINUS1left, exp1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.lvalue lvalue1, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  
lvalue1 = lvalue1 ()
 in (A.var lvalue1)
end)
 in ( LrTable.NT 1, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: _ :: ( _, 
( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft
 as ID1left), _)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.ArrayExp {typ=ID1, size=exp1, init=exp2, pos=IDleft})
end)
 in ( LrTable.NT 1, ( result, ID1left, exp2right), rest671)
end
|  ( 29, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.flist flist1
, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  ID1 = ID1 ()
 val  flist1 = flist1 ()
 in ()
end)
 in ( LrTable.NT 1, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 30, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.funArgs 
funArgs1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left),
 _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val 
 (ID as ID1) = ID1 ()
 val  (funArgs as funArgs1) = funArgs1 ()
 in (A.CallExp{func=ID, args=funArgs,pos=IDleft})
end)
 in ( LrTable.NT 1, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FOR1left, _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  ID1 = 
ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in ()
end)
 in ( LrTable.NT 1, ( result, FOR1left, exp3right), rest671)
end
|  ( 32, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.exp exp1, _, _)
) :: _ :: ( _, ( MlyValue.decList decList1, _, _)) :: ( _, ( _, 
LET1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  decList1 = decList1 ()
 val  exp1 = exp1 ()
 in ()
end)
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 33, ( ( _, ( _, _, END1right)) :: _ :: ( _, ( MlyValue.decList 
decList1, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => let val  decList1 = decList1 ()
 in ()
end)
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.tyDec tyDec1, tyDec1left, tyDec1right)) :: 
rest671)) => let val  result = MlyValue.dec (fn _ => let val  tyDec1 =
 tyDec1 ()
 in ()
end)
 in ( LrTable.NT 3, ( result, tyDec1left, tyDec1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.funDec funDec1, funDec1left, funDec1right))
 :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  
funDec1 = funDec1 ()
 in ()
end)
 in ( LrTable.NT 3, ( result, funDec1left, funDec1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.vardec vardec1, vardec1left, vardec1right))
 :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  
vardec1 = vardec1 ()
 in ()
end)
 in ( LrTable.NT 3, ( result, vardec1left, vardec1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.decList decList1, _, decList1right)) :: ( _
, ( MlyValue.dec dec1, dec1left, _)) :: rest671)) => let val  result =
 MlyValue.decList (fn _ => let val  dec1 = dec1 ()
 val  decList1 = decList1 ()
 in ()
end)
 in ( LrTable.NT 4, ( result, dec1left, decList1right), rest671)
end
|  ( 38, ( rest671)) => let val  result = MlyValue.decList (fn _ => ()
)
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 39, ( ( _, ( MlyValue.ty ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) =>
 let val  result = MlyValue.tyDec (fn _ => let val  ID1 = ID1 ()
 val  ty1 = ty1 ()
 in ()
end)
 in ( LrTable.NT 5, ( result, TYPE1left, ty1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ty (fn _ => let val  ID1 = ID1 ()
 in ()
end)
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.arrty arrty1, arrty1left, arrty1right)) :: 
rest671)) => let val  result = MlyValue.ty (fn _ => let val  arrty1 = 
arrty1 ()
 in ()
end)
 in ( LrTable.NT 6, ( result, arrty1left, arrty1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.recty recty1, recty1left, recty1right)) :: 
rest671)) => let val  result = MlyValue.ty (fn _ => let val  recty1 = 
recty1 ()
 in ()
end)
 in ( LrTable.NT 6, ( result, recty1left, recty1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, 
ARRAY1left, _)) :: rest671)) => let val  result = MlyValue.arrty (fn _
 => let val  ID1 = ID1 ()
 in ()
end)
 in ( LrTable.NT 7, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 44, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.fieldDec 
fieldDec1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.recty (fn _ => let val  fieldDec1 = fieldDec1
 ()
 in ()
end)
 in ( LrTable.NT 8, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 45, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( _, LBRACE1left, _))
 :: rest671)) => let val  result = MlyValue.recty (fn _ => ())
 in ( LrTable.NT 8, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _,
 ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.vardec
 (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  exp1 = exp1 ()
 in ()
end)
 in ( LrTable.NT 9, ( result, VAR1left, exp1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let
 val  result = MlyValue.vardec (fn _ => let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 in ()
end)
 in ( LrTable.NT 9, ( result, VAR1left, exp1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.fieldDec fieldDec2, _, fieldDec2right)) ::
 _ :: ( _, ( MlyValue.fieldDec fieldDec1, fieldDec1left, _)) :: 
rest671)) => let val  result = MlyValue.fieldDec (fn _ => let val  
fieldDec1 = fieldDec1 ()
 val  fieldDec2 = fieldDec2 ()
 in ()
end)
 in ( LrTable.NT 10, ( result, fieldDec1left, fieldDec2right), rest671
)
end
|  ( 49, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.fieldDec (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in ()
end)
 in ( LrTable.NT 10, ( result, ID1left, ID2right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: _ :: ( _, ( MlyValue.fieldDec 
fieldDec1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.funDec
 (fn _ => let val  ID1 = ID1 ()
 val  fieldDec1 = fieldDec1 ()
 val  ID2 = ID2 ()
 val  exp1 = exp1 ()
 in ()
end)
 in ( LrTable.NT 11, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.ID ID1, _, _
)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result = 
MlyValue.funDec (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  exp1 = exp1 ()
 in ()
end)
 in ( LrTable.NT 11, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: _ :: ( _, 
( MlyValue.fieldDec fieldDec1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1,
 _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  
result = MlyValue.funDec (fn _ => let val  ID1 = ID1 ()
 val  fieldDec1 = fieldDec1 ()
 val  exp1 = exp1 ()
 in ()
end)
 in ( LrTable.NT 11, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: _ :: _ :: 
( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: 
rest671)) => let val  result = MlyValue.funDec (fn _ => let val  ID1 =
 ID1 ()
 val  exp1 = exp1 ()
 in ()
end)
 in ( LrTable.NT 11, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.field (fn _ => let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 in ()
end)
 in ( LrTable.NT 12, ( result, ID1left, exp1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.field field2, _, field2right)) :: _ :: ( _,
 ( MlyValue.field field1, field1left, _)) :: rest671)) => let val  
result = MlyValue.field (fn _ => let val  field1 = field1 ()
 val  field2 = field2 ()
 in ()
end)
 in ( LrTable.NT 12, ( result, field1left, field2right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.flistTail flistTail1, _, flistTail1right))
 :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, 
ID1left, _)) :: rest671)) => let val  result = MlyValue.flist (fn _ =>
 let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  flistTail1 = flistTail1 ()
 in ()
end)
 in ( LrTable.NT 13, ( result, ID1left, flistTail1right), rest671)
end
|  ( 57, ( rest671)) => let val  result = MlyValue.flist (fn _ => ())
 in ( LrTable.NT 13, ( result, defaultPos, defaultPos), rest671)
end
|  ( 58, ( ( _, ( MlyValue.flistTail flistTail1, _, flistTail1right))
 :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _
, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = 
MlyValue.flistTail (fn _ => let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  flistTail1 = flistTail1 ()
 in ()
end)
 in ( LrTable.NT 14, ( result, COMMA1left, flistTail1right), rest671)

end
|  ( 59, ( rest671)) => let val  result = MlyValue.flistTail (fn _ =>
 ())
 in ( LrTable.NT 14, ( result, defaultPos, defaultPos), rest671)
end
|  ( 60, ( ( _, ( MlyValue.funArgsTail funArgsTail1, _, 
funArgsTail1right)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: 
rest671)) => let val  result = MlyValue.funArgs (fn _ => let val  (exp
 as exp1) = exp1 ()
 val  (funArgsTail as funArgsTail1) = funArgsTail1 ()
 in (exp::funArgsTail)
end)
 in ( LrTable.NT 15, ( result, exp1left, funArgsTail1right), rest671)

end
|  ( 61, ( rest671)) => let val  result = MlyValue.funArgs (fn _ => ()
)
 in ( LrTable.NT 15, ( result, defaultPos, defaultPos), rest671)
end
|  ( 62, ( ( _, ( MlyValue.funArgsTail funArgsTail1, _, 
funArgsTail1right)) :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, 
COMMA1left, _)) :: rest671)) => let val  result = MlyValue.funArgsTail
 (fn _ => let val  (exp as exp1) = exp1 ()
 val  (funArgsTail as funArgsTail1) = funArgsTail1 ()
 in (exp::funArgsTail)
end)
 in ( LrTable.NT 16, ( result, COMMA1left, funArgsTail1right), rest671
)
end
|  ( 63, ( rest671)) => let val  result = MlyValue.funArgsTail (fn _
 => ())
 in ( LrTable.NT 16, ( result, defaultPos, defaultPos), rest671)
end
|  ( 64, ( ( _, ( MlyValue.ltail ltail1, _, ltail1right)) :: ( _, ( 
MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  
result = MlyValue.lvalue (fn _ => let val  (ID as ID1) = ID1 ()
 val  (ltail as ltail1) = ltail1 ()
 in (A.SimpleVar(symbol ID, IDleft)::ltail)
end)
 in ( LrTable.NT 17, ( result, ID1left, ltail1right), rest671)
end
|  ( 65, ( ( _, ( MlyValue.ltail ltail1, _, ltail1right)) :: _ :: ( _,
 ( MlyValue.exp exp1, _, _)) :: ( _, ( _, LBRACK1left, _)) :: rest671)
) => let val  result = MlyValue.ltail (fn _ => let val  (exp as exp1)
 = exp1 ()
 val  ltail1 = ltail1 ()
 in (fn x => A.SubscriptVar(x, exp, IDleft))
end)
 in ( LrTable.NT 18, ( result, LBRACK1left, ltail1right), rest671)
end
|  ( 66, ( ( _, ( MlyValue.ltail ltail1, _, ltail1right)) :: ( _, ( 
MlyValue.ID ID1, IDleft, _)) :: ( _, ( _, DOT1left, _)) :: rest671))
 => let val  result = MlyValue.ltail (fn _ => let val  (ID as ID1) = 
ID1 ()
 val  ltail1 = ltail1 ()
 in (fn x => A.FieldVar(x, symbol ID, IDleft))
end)
 in ( LrTable.NT 18, ( result, DOT1left, ltail1right), rest671)
end
|  ( 67, ( rest671)) => let val  result = MlyValue.ltail (fn _ => ())
 in ( LrTable.NT 18, ( result, defaultPos, defaultPos), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
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
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
end
end
