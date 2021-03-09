structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token  
type lexresult = (svalue, pos) token

val pos = ref 0
val lineno = ref 0
val row = ref 1
val eof = fn () => Tokens.EOF(!pos, !pos)
val error = fn (e, pos, lineno) => TextIO.output(TextIO.stdOut, "FUCK Error, line " ^ (Int.toString lineno)^" at letter number "^(Int.toString pos) ^ "," ^ e ^ "\n")


%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));
%count
alpha=[A-Za-z];
ws = [\ \t];

%%
{ws}+   => (row:=(!row)+1;lex());
"\n"   => (lineno:=(!lineno)+1;row:=1;lex());
";"     => (print("TERM \""^yytext^"\", ");Tokens.TERM(yytext, (!row), (!lineno)));
"TRUE"  => (print("CONST \""^yytext^"\", ");Tokens.CONST(yytext, (!row), (!lineno)));
"FALSE" => (print("CONST \""^yytext^"\", ");Tokens.CONST(yytext, (!row), (!lineno)));
"AND"   => (print("AND \""^yytext^"\", ");Tokens.AND(yytext, (!row), (!lineno)));
"OR"    => (print("OR \""^yytext^"\", ");Tokens.OR(yytext, (!row), (!lineno)));
"XOR"   => (print("XOR \""^yytext^"\", ");Tokens.XOR(yytext, (!row), (!lineno)));
"EQUALS"=> (print("EQUALS \""^yytext^"\", ");Tokens.EQUALS(yytext, (!row), (!lineno)));
"NOT"   => (print("NOT \""^yytext^"\", ");Tokens.NOT(yytext, (!row), (!lineno)));
"IMPLIES"=> (print("IMPLIES \""^yytext^"\", ");Tokens.IMPLIES(yytext, (!row), (!lineno)));
"IF"    => (print("IF \""^yytext^"\", ");Tokens.IF(yytext, (!row), (!lineno)));
"THEN"  => (print("THEN \""^yytext^"\", ");Tokens.THEN(yytext, (!row), (!lineno)));
"ELSE"  => (print("ELSE \""^yytext^"\", ");Tokens.ELSE(yytext, (!row), (!lineno)));
"("     => (print("LPAREN \""^yytext^"\", ");Tokens.LPAREN(yytext, (!row), (!lineno)));
")"     => (print("RPAREN \""^yytext^"\", ");Tokens.RPAREN(yytext, (!row), (!lineno)));
{alpha}+ => (print("ID \""^yytext^"\", ");Tokens.ID(yytext, (!row), (!lineno)));
