datatype lexresult= CONST | EOF | EOS | ID | LPAREN |
                    NUM of int | NOT | PRINT | RPAREN |
                    AND | OR | XOR | EQUALS | IMPLIES | IF | ELSE | THEN |
                    TRUE | FALSE 
val eof = fn () => EOF
val args = CommandLine.arguments()
fun sum l = foldr op+ 0 (map (valOf o Int.fromString) l)
val _ = print ("size: " ^ Int.toString (length args) ^ "\n")
val _ = print ("sum: " ^ Int.toString (sum args) ^ "\n")
val _ = OS.Process.exit(OS.Process.success)
(*USER DECLARATIONS*)

%%
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t\n];
%%

{ws}+ =>(lex());
"TRUE" => (TRUE);
"FALSE" => (FALSE);
"NOT" => (NOT);
"AND" => (AND);
"OR" => (OR);
"XOR" => (XOR);
"EQUALS" => (EQUALS);
"IMPLIES" => (IMPLIES);
"IF" => (IF);
"ELSE" => (ELSE);
"(" => (LPAREN);
")" => (RPAREN);
{alpha}+=>(ID);
