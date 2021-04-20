Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;
Control.Print.printLength := 1000;
CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "Bool.yacc.sig";
use "Bool.lex.sml";
use "Bool.yacc.sml";
use "load-Bool.sml";
parseFile "f";
(* parseString "A AND B;";
val a= parseString "A AND B IMPLIES TRUE;"); *)