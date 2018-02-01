type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val commentDepth = ref 0
val stringStart = ref 0
val stringBuffer = "" : string
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%%
%s COMMENT;
%%

<INITIAL> \n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> type => (Tokens.TYPE(yypos, yypos+4));
<INITIAL> var  	=> (Tokens.VAR(yypos,yypos+3));
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
<INITIAL> "." => (Tokens.DOT(yypos,yypos+1));
<INITIAL> "]" => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL> "[" => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL> "}" => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL> "{" => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL> ")" => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL> "(" => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL> ";" => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL> ":" => (Tokens.COLON(yypos,yypos+1));
<INITIAL> "," => (Tokens.COMMA(yypos,yypos+1));

<INITIAL> [0-9]+ => (Tokens.INT(case Int.fromString(yytext) of SOME y => y | NONE => ~1, yypos,yypos+size(yytext)));

<INITIAL> \" => (YYBEGIN STRING; stringStart := yypos; continue());
<STRING> [ -~\ \\n\t\^c\ddd\"\\]* => (stringBuffer ^ yytext; continue());
<STRING> \\[\f\n\t\ \\13]*\\ => (continue());
<STRING> \" => (YYBEGIN INITIAL; Tokens.STRING(stringBuffer,!stringStart,yypos+1); stringStart :=0; stringBuffer = "");

<INITIAL> [A-Za-z][A-Za-z0-9_]* => (Tokens.ID(yytext,yypos,yypos+size(yytext)));


<INITIAL> "/*" => (commentDepth := 1; YYBEGIN COMMENT; continue());
<COMMENT> [^/*]* => (continue());
<COMMENT> "/*" => ( commentDepth := !commentDepth+1; continue());
<COMMENT> "*/" => (commentDepth := !commentDepth-1; if !commentDepth=0
                                                    then YYBEGIN INITIAL else (); continue());
<COMMENT> "/" => (continue());
<COMMENT> "*" => (continue());


[" "\t]+ => (continue());
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
