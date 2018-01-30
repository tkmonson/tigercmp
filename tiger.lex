type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%%
%%
\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
","	=> (Tokens.COMMA(yypos,yypos+1));
var  	=> (Tokens.VAR(yypos,yypos+3));
"123"	=> (Tokens.INT(123,yypos,yypos+3));
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

<INITIAL>if => (Tokens.IF(yypos,yypos+2));
<INITIAL>array => (Tokens.ARRAY(yypos,yypos+5));
<INITIAL>:= => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>| => (Tokens.OR(yypos,yypos+1));
<INITIAL>& => (Tokens.AND(yypos,yypos+1));
<INITIAL>>= => (Tokens.GE(yypos,yypos+1));
<INITIAL>> => (Tokens.GT(yypos,yypos+1));
<INITIAL><= => (Tokens.LE(yypos,yypos+1));
<INITIAL>< => (Tokens.LT(yypos,yypos+1));
<INITIAL><> => (Tokens.NEQ(yypos,yypos+1));
<INITIAL>= => (Tokens.EQ(yypos,yypos+1));
<INITIAL>/ => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>* => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>- => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>+ => (Tokens.PLUS(yypos,yypos+1));
