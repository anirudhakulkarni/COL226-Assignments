structure BTokens = Tokens
    exception LexError
    type pos = int
    type svalue = BTokens.svalue
    type ('a,'b) token = ('a,'b) BTokens.token  
    type lexresult = (svalue, pos) token
    type lexarg = string
    type arg = lexarg

    val col= ref 0; 
    (* Column position in file *)
    val eolpos = ref 0;
    (* end of previous line position in file *)
    val lineno = ref 1;
    (* current line number in file *)
    val TokenList=ref ["_"];
    val eof = fn () => 
            let
                fun revAndPrint nil = print("[")
                |	revAndPrint (h::t) = (revAndPrint t; print (h ^ ", "))
                
                val _ = (revAndPrint (List.take((!TokenList),length(!TokenList)-1)); print("EOF]\n"));
                
            in
                TokenList := ["_"];Tokens.EOF(!lineno, !col)
            end
	val error = fn (e, lineno, col) => TextIO.output(TextIO.stdErr, "Unknown token:" ^ (Int.toString lineno) ^ ":" ^ (Int.toString col) ^ ":" ^ e ^ "\n")

    fun revfold _ nil b = b
        | revfold f (hd::tl) b = revfold f tl (f(hd,b))

    (* val keywords =
    [
    (* ("div",  Tokens.DIV), *)
    ("end",  Tokens.END),
    ("in",  Tokens.IN),
    ("let",  Tokens.LET)
    (* ("val",  Tokens.VAL) *)
    ]

    fun findKeywords (str:string, pos1:pos, pos2:pos) =
    case List.find (fn (s, _) => s = str )  keywords of 
    SOME (_, tk) => tk(pos1, pos2) 
    | NONE => Tokens.ID (str, pos1, pos2) *)



%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));
%count
alpha=[A-Za-z]+;
ws = [\ \t];
num=[0-9]+;
remaining = [^];

%%
{ws}+   => (lex());
"\n"   => (lineno:=(!lineno)+1;eolpos:=yypos+size yytext;lex());
";"     => (col:=yypos - (!eolpos);TokenList := "TERM \";\""::(!TokenList);BTokens.TERM( (!lineno), (!col)));
"TRUE"  => (col:=yypos - (!eolpos);TokenList := "CONST \""^yytext^"\""::(!TokenList);BTokens.CONST(yytext, (!lineno), (!col)));
"FALSE" => (col:=yypos - (!eolpos);TokenList := "CONST \""^yytext^"\""::(!TokenList);BTokens.CONST(yytext, (!lineno), (!col)));
"AND"   => (col:=yypos - (!eolpos);TokenList := "AND \""^yytext^"\""::(!TokenList);BTokens.AND( (!lineno), (!col)));
"OR"    => (col:=yypos - (!eolpos);TokenList := "OR \""^yytext^"\""::(!TokenList);BTokens.OR( (!lineno), (!col)));
"XOR"   => (col:=yypos - (!eolpos);TokenList := "XOR \""^yytext^"\""::(!TokenList);BTokens.XOR( (!lineno), (!col)));
"EQUALS"=> (col:=yypos - (!eolpos);TokenList := "EQUALS \""^yytext^"\""::(!TokenList);BTokens.EQUALS( (!lineno), (!col)));
"+"   => (col:=yypos - (!eolpos);TokenList := "PLUS \""^yytext^"\""::(!TokenList);BTokens.PLUS( (!lineno), (!col)));
"-"   => (col:=yypos - (!eolpos);TokenList := "MINUS \""^yytext^"\""::(!TokenList);BTokens.MINUS( (!lineno), (!col)));
"*"   => (col:=yypos - (!eolpos);TokenList := "TIMES \""^yytext^"\""::(!TokenList);BTokens.TIMES( (!lineno), (!col)));
"/"   => (col:=yypos - (!eolpos);TokenList := "DIV \""^yytext^"\""::(!TokenList);BTokens.DIV( (!lineno), (!col)));
"="   => (col:=yypos - (!eolpos);TokenList := "EQ \""^yytext^"\""::(!TokenList);BTokens.EQ( (!lineno), (!col)));
":="   => (col:=yypos - (!eolpos);TokenList := "ASSIGN \""^yytext^"\""::(!TokenList);BTokens.ASSIGN( (!lineno), (!col)));
":"   => (col:=yypos - (!eolpos);TokenList := "COLON \""^yytext^"\""::(!TokenList);BTokens.COLON( (!lineno), (!col)));
"fn"   => (col:=yypos - (!eolpos);TokenList := "FN \""^yytext^"\""::(!TokenList);BTokens.FN( (!lineno), (!col)));
"fun"   => (col:=yypos - (!eolpos);TokenList := "FUN \""^yytext^"\""::(!TokenList);BTokens.FUN( (!lineno), (!col)));
"int"   => (col:=yypos - (!eolpos);TokenList := "INT \""^yytext^"\""::(!TokenList);BTokens.INT( (!lineno), (!col)));
"bool"   => (col:=yypos - (!eolpos);TokenList := "BOOL \""^yytext^"\""::(!TokenList);BTokens.BOOL( (!lineno), (!col)));
"->"   => (col:=yypos - (!eolpos);TokenList := "ARROW \""^yytext^"\""::(!TokenList);BTokens.ARROW( (!lineno), (!col)));
"=>"   => (col:=yypos - (!eolpos);TokenList := "DARROW \""^yytext^"\""::(!TokenList);BTokens.DARROW( (!lineno), (!col)));
"<>"   => (col:=yypos - (!eolpos);TokenList := "NEQ \""^yytext^"\""::(!TokenList);BTokens.NEQ( (!lineno), (!col)));
"if"    => (col:=yypos - (!eolpos);TokenList := "if \""^yytext^"\""::(!TokenList);BTokens.IF( (!lineno), (!col)));
"then"  => (col:=yypos - (!eolpos);TokenList := "then \""^yytext^"\""::(!TokenList);BTokens.THEN( (!lineno), (!col)));
"else"  => (col:=yypos - (!eolpos);TokenList := "else \""^yytext^"\""::(!TokenList);BTokens.ELSE( (!lineno), (!col)));
"fi"  => (col:=yypos - (!eolpos);TokenList := "fi \""^yytext^"\""::(!TokenList);BTokens.FI( (!lineno), (!col)));
"("     => (col:=yypos - (!eolpos);TokenList := "LPAREN \""^yytext^"\""::(!TokenList);BTokens.LPAREN( (!lineno), (!col)));
")"     => (col:=yypos - (!eolpos);TokenList := "RPAREN \""^yytext^"\""::(!TokenList);BTokens.RPAREN( (!lineno), (!col)));
"let"   => (col:=yypos - (!eolpos);TokenList := "let \""^yytext^"\""::(!TokenList);BTokens.LET( (!lineno), (!col)));
"in"   => (col:=yypos - (!eolpos);TokenList := "in \""^yytext^"\""::(!TokenList);BTokens.IN( (!lineno), (!col)));
"end"   => (col:=yypos - (!eolpos);TokenList := "end \""^yytext^"\""::(!TokenList);BTokens.END( (!lineno), (!col)));
"PLUS"   => (col:=yypos - (!eolpos);TokenList := "PLUS \""^yytext^"\""::(!TokenList);BTokens.PLUS( (!lineno), (!col)));
"MINUS"   => (col:=yypos - (!eolpos);TokenList := "MINUS \""^yytext^"\""::(!TokenList);BTokens.MINUS( (!lineno), (!col)));
"TIMES"   => (col:=yypos - (!eolpos);TokenList := "TIMES \""^yytext^"\""::(!TokenList);BTokens.TIMES( (!lineno), (!col)));
"NEGATE"   => (col:=yypos - (!eolpos);TokenList := "NEGATE \""^yytext^"\""::(!TokenList);BTokens.NEGATE( (!lineno), (!col)));
"LESSTHAN"   => (col:=yypos - (!eolpos);TokenList := "LESSTHAN \""^yytext^"\""::(!TokenList);BTokens.LESSTHAN((!lineno), (!col)));
"GREATERTHAN"   => (col:=yypos - (!eolpos);TokenList := "GREATERTHAN \""^yytext^"\""::(!TokenList);BTokens.GREATERTHAN( (!lineno), (!col)));
"NOT"   => (col:=yypos - (!eolpos);TokenList := "NOT \""^yytext^"\""::(!TokenList);BTokens.NOT((!lineno), (!col)));
"IMPLIES"=> (col:=yypos - (!eolpos);TokenList := "IMPLIES \""^yytext^"\""::(!TokenList);BTokens.IMPLIES( (!lineno), (!col)));

{alpha} => (col:=yypos - (!eolpos);TokenList := "ID \""^yytext^"\""::(!TokenList);BTokens.ID(yytext, (!lineno), (!col)));
{num} => (col:=yypos - (!eolpos);TokenList := "NUM \""^yytext^"\""::(!TokenList);BTokens.NUM(valOf(Int.fromString(yytext)), (!lineno), (!col)));
{remaining}=> (col:=yypos - (!eolpos); error(yytext, !lineno, !col); raise LexError);



