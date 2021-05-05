
CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "ast.sml";
use "Bool.yacc.sig";
use "Bool.yacc.sml";
use "Bool.lex.sml";
use "load-Bool.sml";
Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;
Control.Print.printLength := 1000;
parseFile "test2.txt";
parseFile "test4.txt";

(* TokenList:=[]; *)

(* parseString "A AND B;";
val a= parseString "A AND B IMPLIES TRUE;"); *)