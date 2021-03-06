(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 

%%
(* required declarations *)
%name Calc

%term
  ID of string | NUM of int
| PLUS | TIMES | SUB | DIV  | RPAREN | LPAREN | EOF

%nonterm EXP of int | START of string option 

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%left SUB PLUS
%left TIMES DIV
(* %right *)
  (* %nonassoc*)
%start START
%verbose

%%
START: EXP (SOME (Int.toString(EXP)))
      |    (NONE )

EXP: NUM (Int.toString(NUM))
  | ID (ID)
  | EXP PLUS EXP (AST.expToString(AST.BinApp(AST.Add,EXP1,EXP2)))
  | EXP SUB EXP (AST.expToString(AST.BinApp(AST.Sub,EXP1,EXP2)))
  | EXP TIMES EXP (AST.expToString(AST.BinApp(AST.Mul,EXP1,EXP2)))
  | EXP DIV EXP (AST.expToString(AST.BinApp(AST.Div,EXP1,EXP2)))
