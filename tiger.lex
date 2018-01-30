type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val commentDepth = ref 0
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%%
%%
<INITIAL> \n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> ","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL> var  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL> type => (Tokens.TYPE(yypos, yypos+4));
<INITIAL> function => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL> break => (Tokens.BREAK(yypos, yypos+5));
<INITIAL> of => (Tokens.OF(yypos, yypos+2));
<INITIAL> end => (Tokens.END(yypos, yypos+3));
<INITIAL> in => (Tokens.IN(yypos, yypos+2));
<INITIAL> nil => (Tokens.NIL(yypos, yypos+3));
<INITIAL> let => (Tokens.LET(yypos, yypos+3));
<INITIAL> do => (Tokens.DO(yypos, yypos+2));
<INITIAL> to => (Tokens.TO(yypos, yypos+2));
<INITIAL> for => (Tokens.FOR(yypos, yypos+3));
<INITIAL> while => (Tokens.WHILE(yypos, yypos+5));
<INITIAL> else => (Tokens.ELSE(yypos, yypos+4));
<INITIAL> then => (Tokens.THEN(yypos, yypos+4));
[" "\t]+ => (continue());
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
