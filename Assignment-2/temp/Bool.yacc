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

program : statement (statement^"<statement>, <program>") | ( "<program>")
statement : formula TERM statement (formula^", <formula>, "^TERM^", <TERM>, "^statement^", <statement>, ") | formula TERM (formula^", <formula>, \""^TERM^"\", <TERM>, ")
formula: IF formula THEN formula ELSE formula ( "\""^IF^"\", IF, "^formula1^", <formula>, \""^THEN^"\", THEN, "^formula2^", <formula>, \""^ELSE^"\", ELSE, "^formula1^", <formula>")
    | formula IMPLIES formula ( formula1^", <formula>, "^IMPLIES^", "^formula2^", <formula>")
    | formula AND formula (formula1^", <formula>, \""^AND^"\", AND, "^formula2^", <formula>")
    | formula OR formula (formula1^", <formula>, \""^OR^"\", OR, "^formula2^", <formula>")
    | formula XOR formula (formula1^", <formula>, \""^XOR^"\", XOR, "^formula2^", <formula>")
    | formula EQUALS formula (formula1^", <formula>, \""^EQUALS^"\", EQUALS, "^formula2^", <formula>")
    | NOT formula ( "\""^NOT^"\", NOT, "^formula^", <formula>")
    | LPAREN formula RPAREN ("\""^LPAREN^"\", LPAREN, "^formula^", RPAREN, "^RPAREN^"\"")
    | CONST ("\""^CONST^"\", <CONST>") | ID ("\""^ID^"\", <ID>")
