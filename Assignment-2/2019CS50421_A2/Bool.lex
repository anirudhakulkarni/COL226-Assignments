structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token  
type lexresult = (svalue, pos) token

val pos = ref 0
val lineno = ref 1
val row = ref 1
val eof = fn () => Tokens.EOF(!pos, !pos)


%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));
%count
alpha=[A-Za-z]+;
ws = [\ \t];
remaining = [^];

%%
{ws}+   => (row:=(!row)+size yytext;lex());
"\n"   => (lineno:=(!lineno)+1;row:=1;lex());
";"     => (row:=(!row)+size yytext;print("TERM \""^yytext^"\", ");Tokens.TERM(yytext, (!row), (!lineno)));
"TRUE"  => (row:=(!row)+size yytext;print("CONST \""^yytext^"\", ");Tokens.CONST(yytext, (!row), (!lineno)));
"FALSE" => (row:=(!row)+size yytext;print("CONST \""^yytext^"\", ");Tokens.CONST(yytext, (!row), (!lineno)));
"AND"   => (row:=(!row)+size yytext;print("AND \""^yytext^"\", ");Tokens.AND(yytext, (!row), (!lineno)));
"OR"    => (row:=(!row)+size yytext;print("OR \""^yytext^"\", ");Tokens.OR(yytext, (!row), (!lineno)));
"XOR"   => (row:=(!row)+size yytext;print("XOR \""^yytext^"\", ");Tokens.XOR(yytext, (!row), (!lineno)));
"EQUALS"=> (row:=(!row)+size yytext;print("EQUALS \""^yytext^"\", ");Tokens.EQUALS(yytext, (!row), (!lineno)));
"NOT"   => (row:=(!row)+size yytext;print("NOT \""^yytext^"\", ");Tokens.NOT(yytext, (!row), (!lineno)));
"IMPLIES"=> (row:=(!row)+size yytext;print("IMPLIES \""^yytext^"\", ");Tokens.IMPLIES(yytext, (!row), (!lineno)));
"IF"    => (row:=(!row)+size yytext;print("IF \""^yytext^"\", ");Tokens.IF(yytext, (!row), (!lineno)));
"THEN"  => (row:=(!row)+size yytext;print("THEN \""^yytext^"\", ");Tokens.THEN(yytext, (!row), (!lineno)));
"ELSE"  => (row:=(!row)+size yytext;print("ELSE \""^yytext^"\", ");Tokens.ELSE(yytext, (!row), (!lineno)));
"("     => (row:=(!row)+size yytext;print("LPAREN \""^yytext^"\", ");Tokens.LPAREN(yytext, (!row), (!lineno)));
")"     => (row:=(!row)+size yytext;print("RPAREN \""^yytext^"\", ");Tokens.RPAREN(yytext, (!row), (!lineno)));
{alpha} => (row:=(!row)+size yytext;print("ID \""^yytext^"\", ");Tokens.ID(yytext, (!row), (!lineno)));
{remaining}=> (row:=(!row)+size yytext;print("Unknown token:"^Int.toString((!lineno))^":"^Int.toString((!row))^":"^yytext^", ");raise LexError;lex());