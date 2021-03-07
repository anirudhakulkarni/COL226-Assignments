%%
(*Required declarations*)
%name Bool
%term AND of string | OR of string | XOR of string | EQUALS of string 
    | TERM of string
    | CONST of string 
    | IF of string | THEN of string | ELSE of string 
    | LPAREN of string | RPAREN of string 
    | ID of string 
    | IMPLIES of string 
    | NOT of string 
    | EOF
%nonterm program of string 
    | statement of string 
    | formula of string
    | binop of string
%pos int
%eop EOF
%noshift EOF
%right IF THEN ELSE
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT

%start program
%verbose

%%

program : statement (statement) | ( "")
statement : formula TERM statement ("FORMULA "^formula^TERM^statement) | formula TERM ("FORMULA "^formula^TERM)
formula: IF formula THEN formula ELSE formula (formula3^ELSE^formula2^THEN^formula1^IF)
    | formula IMPLIES formula ( formula1^IMPLIES^formula2)
    | formula binop formula (formula1^binop^formula2)
    | NOT formula ( NOT^formula)
    | LPAREN formula RPAREN (LPAREN^formula^RPAREN)
    | CONST (CONST) | ID (ID)
binop: AND (AND) | OR (OR) | XOR (XOR)| EQUALS (EQUALS)
