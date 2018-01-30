type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val commentDepth = ref 0
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%%
%s COMMENT;
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
<INITIAL> if => (Tokens.IF(yypos,yypos+2));
<INITIAL> array => (Tokens.ARRAY(yypos,yypos+5));
<INITIAL> ":=" => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL> "|" => (Tokens.OR(yypos,yypos+1));
<INITIAL> "&" => (Tokens.AND(yypos,yypos+1));
<INITIAL> ">=" => (Tokens.GE(yypos,yypos+1));
<INITIAL> ">" => (Tokens.GT(yypos,yypos+1));
<INITIAL> "<=" => (Tokens.LE(yypos,yypos+1));
<INITIAL> "<" => (Tokens.LT(yypos,yypos+1));
<INITIAL> "<>" => (Tokens.NEQ(yypos,yypos+1));
<INITIAL> "=" => (Tokens.EQ(yypos,yypos+1));
<INITIAL> "/" => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL> "*" => (Tokens.TIMES(yypos,yypos+1));
<INITIAL> "-" => (Tokens.MINUS(yypos,yypos+1));
<INITIAL> "+" => (Tokens.PLUS(yypos,yypos+1));
<INITIAL> "/*" => (commentDepth := 1; YYBEGIN COMMENT; continue());

<COMMENT> [^/*]* => (continue());
<COMMENT> "/*" => ( commentDepth := !commentDepth+1; continue());
<COMMENT> "*/" => (commentDepth := !commentDepth-1; if !commentDepth=0
                                                    then YYBEGIN INITIAL else (); continue());
<COMMENT> "/" => (continue());
<COMMENT> "*" => (continue());


[" "\t]+ => (continue());
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
