%%
(*Required declarations*)
%name Bool
%term AND | OR | XOR | EQUALS | PLUS  | MINUS  | TIMES 
    | TERM | NUM of int | CONST of string 
    | IF | THEN | ELSE | FI
    | LPAREN | RPAREN | ID of string 
    | IMPLIES | NOT | NEGATE | LESSTHAN | GREATERTHAN | LET | IN | END | EQ
    | DIV | MUL | SUB | VAL | NEQ | ASSIGN
    | FUN | FN | ARROW | DARROW | COLON | COMMA
    | STRING of string
    | INT | BOOL

    | EOF 
%nonterm program of AST.program
    | Type of AST.typ
    | statement of AST.statement
    | EXP of AST.exp
    | DECL of AST.decl
%pos int
%eop EOF 
%noshift EOF 
%right ARROW
%nonassoc DARROW
%left ASSIGN
%left EQ NEQ
%right IF THEN ELSE FI
%right IMPLIES
%left AND OR XOR EQUALS PLUS MINUS LESSTHAN GREATERTHAN
%left TIMES DIV
%right NOT NEGATE

%start program
%verbose

%%

program : statement (AST.postorder:=(!AST.postorder)^"program => statement";AST.Statement(statement))
statement : EXP TERM statement (AST.postorder:=(!AST.postorder)^"TERM ;, statement => EXP TERM statement, ";AST.Exps(EXP,statement)) | EXP TERM (AST.postorder:=(!AST.postorder)^"TERM ;, statement => EXP TERM, ";AST.Exp(EXP))
EXP: IF EXP THEN EXP ELSE EXP FI (AST.postorder:=(!AST.postorder)^"IF IF, THEN THEN, ELSE ELSE, FI FI, EXP => if EXP then EXP else EXP fi, ";AST.IfExp(EXP1,EXP2,EXP3))
    | EXP IMPLIES EXP (AST.postorder:=(!AST.postorder)^"IMPLIES IMPLIES, EXP => EXP IMPLIES EXP, ";AST.BoolExp(AST.Implies, EXP1,  EXP2))
    | EXP AND EXP (AST.postorder:=(!AST.postorder)^"AND AND, EXP => EXP AND EXP, ";AST.BoolExp(AST.And, EXP1,  EXP2))
    | EXP OR EXP (AST.postorder:=(!AST.postorder)^"OR OR, EXP => EXP OR EXP, ";AST.BoolExp(AST.Or, EXP1,  EXP2))
    | EXP XOR EXP (AST.postorder:=(!AST.postorder)^"XOR XOR, EXP => EXP XOR EXP, ";AST.BoolExp(AST.Xor, EXP1,  EXP2))
    | EXP EQUALS EXP (AST.postorder:=(!AST.postorder)^"EQUALS EQUALS, EXP => EXP EQUALS EXP, ";AST.BoolExp(AST.Equals, EXP1,  EXP2))
    | EXP GREATERTHAN EXP (AST.postorder:=(!AST.postorder)^"GREATERTHAN GREATERTHAN, EXP => EXP GREATERTHAN EXP, ";AST.BinExp(AST.Greaterthan, EXP1,  EXP2))
    | NOT EXP (AST.postorder:=(!AST.postorder)^"NOT NOT, EXP => NOT EXP, ";AST.NegExp(AST.Not, EXP))
    | LPAREN EXP RPAREN (AST.postorder:=(!AST.postorder)^"LPAREN (, RPAREN ), EXP => LPAREN EXP RPAREN, ";AST.ParenExp(EXP))
    | CONST (AST.postorder:=(!AST.postorder)^"CONST "^CONST^", EXP => CONST, ";AST.TFExp(CONST))
    | NUM (AST.postorder:=(!AST.postorder)^"NUM "^Int.toString(NUM)^", EXP => NUM, ";AST.NumExp(NUM))
    | ID (AST.postorder:=(!AST.postorder)^"ID "^ID^", EXP => ID, ";AST.VarExp(ID))
    | EXP PLUS EXP (AST.postorder:=(!AST.postorder)^"PLUS PLUS, EXP => EXP PLUS EXP, ";AST.BinExp(AST.Add, EXP1,  EXP2))
    | EXP MINUS  EXP (AST.postorder:=(!AST.postorder)^"MINUS MINUS, EXP => EXP MINUS EXP, ";AST.BinExp(AST.Sub,  EXP1,  EXP2))
    | EXP TIMES  EXP (AST.postorder:=(!AST.postorder)^"TIMES TIMES, EXP => EXP TIMES EXP, ";AST.BinExp(AST.Mul,  EXP1, EXP2))
    | EXP DIV  EXP (AST.postorder:=(!AST.postorder)^"DIV DIV, EXP => EXP DIV EXP, ";AST.BinExp(AST.Div, EXP1, EXP2))
    | LET DECL IN EXP END (AST.postorder:=(!AST.postorder)^"LET LET, IN IN, END END, EXP => LET DECL IN EXP END, ";AST.LetExp(DECL, EXP))
    | EXP EQ EXP (AST.postorder:=(!AST.postorder)^"EQ, EQ => EXP EQ EXP, ";AST.BinExp(AST.Eq, EXP1, EXP2))
    | FN LPAREN ID COLON Type RPAREN COLON Type DARROW EXP (AST.postorder:=(!AST.postorder)^"LPAREN (, ID"^ID^", COLON :, RPAREN ), COLON :, DARROW =>,  EXP => FN LPAREN ID COLON Type RPAREN COLON Type DARROW EXP, ";AST.FnExp(ID,Type1,Type2,EXP))
    | FUN ID LPAREN ID COLON Type RPAREN COLON Type DARROW EXP (AST.postorder:=(!AST.postorder)^"ID"^ID^", LPAREN (,"^"ID "^ID^", COLON :, RPAREN ), COLON :, DARROW =>,  EXP => FUN ID LPAREN ID COLON Type RPAREN COLON Type DARROW EXP, ";AST.FunExp(ID1,ID2,Type1,Type2,EXP))
    | LPAREN EXP EXP RPAREN (AST.postorder:=(!AST.postorder)^"LPAREN (, RPAREN ), EXP => LPAREN EXP EXP RPAREN, ";AST.AppExp(EXP1,EXP2))
DECL: ID EQ EXP (AST.postorder:=(!AST.postorder)^"ID "^ID^", EQ EQ, DECL => ID EQ EXP, ";AST.ValDecl(ID, EXP))

Type:
    Type ARROW Type     (AST.FnTy(Type1,Type2))
    | Type TIMES Type   (AST.TupTyp(Type1::Type2::nil))
    | INT               (AST.IntTy)
    | BOOL               (AST.BoolTy)
    | STRING               (AST.StrTy)
    | LPAREN Type RPAREN    (Type)