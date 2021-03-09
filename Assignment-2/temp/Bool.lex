structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token  
type lexresult = (svalue, pos) token

val pos = ref 0
val lin = ref 1;
val col = ref 0;
val eof = fn () => Tokens.EOF(!pos, !pos)
val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")


%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));
%count
alpha=[A-Za-z];
ws = [\ \t];

%%
"\n"      => (pos := (!pos) + 1; lex());
{ws}+   => (lex());
";"     => (print("TERM \""^yytext^"\", ");Tokens.TERM(yytext, yypos, yypos+size yytext));
"TRUE"  => (print("CONST \""^yytext^"\", ");Tokens.CONST(yytext, yypos, yypos+size yytext));
"FALSE" => (print("CONST \""^yytext^"\", ");Tokens.CONST(yytext, yypos, yypos+size yytext));
"AND"   => (print("AND \""^yytext^"\", ");Tokens.AND(yytext, yypos, yypos+size yytext));
"OR"    => (print("OR \""^yytext^"\", ");Tokens.OR(yytext, yypos, yypos+size yytext));
"XOR"   => (print("XOR \""^yytext^"\", ");Tokens.XOR(yytext, yypos, yypos+size yytext));
"EQUALS"=> (print("EQUALS \""^yytext^"\", ");Tokens.EQUALS(yytext, yypos, yypos+size yytext));
"NOT"   => (print("NOT \""^yytext^"\", ");Tokens.NOT(yytext, yypos, yypos+size yytext));
"IMPLIES"=> (print("IMPLIES \""^yytext^"\", ");Tokens.IMPLIES(yytext, yypos, yypos+size yytext));
"IF"    => (print("IF \""^yytext^"\", ");Tokens.IF(yytext, yypos, yypos+size yytext));
"THEN"  => (print("THEN \""^yytext^"\", ");Tokens.THEN(yytext, yypos, yypos+size yytext));
"ELSE"  => (print("ELSE \""^yytext^"\", ");Tokens.ELSE(yytext, yypos, yypos+size yytext));
"("     => (print("LPAREN \""^yytext^"\", ");Tokens.LPAREN(yytext, yypos, yypos+size yytext));
")"     => (print("RPAREN \""^yytext^"\", ");Tokens.RPAREN(yytext, yypos, yypos+size yytext));
{alpha}+ => (print("ID \""^yytext^"\", ");Tokens.ID(yytext, yypos, yypos+size yytext));
