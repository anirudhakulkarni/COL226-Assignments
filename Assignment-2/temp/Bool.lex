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

alpha=[A-Za-z];
ws = [\ \t];

%%
\n      => (pos := (!pos) + 1; lex());
{ws}+   => (lex());
";"     => (Tokens.TERM(yytext, yypos, yypos+size yytext));
"TRUE"  => (Tokens.CONST(yytext, yypos, yypos+size yytext));
"FALSE" => (Tokens.CONST(yytext, yypos, yypos+size yytext));
"AND"   => (Tokens.AND(yytext, yypos, yypos+size yytext));
"OR"    => (Tokens.OR(yytext, yypos, yypos+size yytext));
"XOR"   => (Tokens.XOR(yytext, yypos, yypos+size yytext));
"EQUALS"=> (Tokens.EQUALS(yytext, yypos, yypos+size yytext));
"NOT"   => (Tokens.NOT(yytext, yypos, yypos+size yytext));
"IMPLIES"=> (Tokens.IMPLIES(yytext, yypos, yypos+size yytext));
"IF"    => (Tokens.IF(yytext, yypos, yypos+size yytext));
"THEN"  => (Tokens.THEN(yytext, yypos, yypos+size yytext));
"ELSE"  => (Tokens.ELSE(yytext, yypos, yypos+size yytext));
"("     => (Tokens.LPAREN(yytext, yypos, yypos+size yytext));
")"     => (Tokens.RPAREN(yytext, yypos, yypos+size yytext));
{alpha}+ => (Tokens.ID(yytext, yypos, yypos+size yytext));
