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

program : statement (statement^"<statements>, <program>") | ( "<program>")
statement : formula TERM statement (formula^", <formula>, "^TERM^", <TERM>, "^statement^", <statement>, ") | formula TERM (formula^", <formula>, "^TERM^", <TERM>, ")
formula: IF formula THEN formula ELSE formula (formula3^ELSE^formula2^THEN^formula1^IF)
    | formula IMPLIES formula ( formula1^IMPLIES^formula2)
    | formula AND formula (formula1^AND^formula2)
    | formula OR formula (formula1^OR^formula2)
    | formula XOR formula (formula1^XOR^formula2)
    | formula EQUALS formula (formula1^EQUALS^formula2)
    | NOT formula ( NOT^formula)
    | LPAREN formula RPAREN (LPAREN^formula^RPAREN)
    | CONST (CONST^", <CONST>") | ID (ID^", <ID>")
