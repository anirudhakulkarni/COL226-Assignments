

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
%nonterm program of string option
    | statement of string | statements of string
    | formula of string | formula_I of string | formula_II of string | formula_III of string | formula_IV of string | formula_V of string
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

program : statements (SOME statements) | (NONE )
statements : statement statements (statement^statements)
statement: formula TERM (formula^TERM)
formula: formula_I IF formula THEN formula ELSE formula ( formula_I^IF^formula1^THEN^formula2^ELSE^formula3) | formula_I (formula_I)
formula_I : formula_II formula_I IMPLIES formula_I ( formula_II^formula_I1^IMPLIES^formula_I2) | formula_II ( formula_II)
formula_II : formula_II binop formula_II formula_III (formula_II1^binop^formula_II2^formula_III) | formula_III ( formula_III)
formula_III : formula_IV NOT formula_III ( formula_IV^NOT^formula_III) | formula_IV (formula_IV)
formula_IV : formula_V ( formula_V) | LPAREN formula RPAREN ( LPAREN^formula^RPAREN)
formula_V : CONST (CONST) | ID (ID)
binop: AND (AND) | OR (OR) | XOR (XOR)| EQUALS (EQUALS)
