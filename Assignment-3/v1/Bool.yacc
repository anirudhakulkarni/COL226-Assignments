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
%left AND OR XOR EQUALS PLUS MINUS TIMES LESSTHAN GREATERTHAN
%left TIMES DIV
%right NOT NEGATE

%start program
%verbose

%%

program : statement (AST.Statement(statement))
statement : EXP TERM statement (AST.Exps(EXP,statement)) | EXP TERM (AST.Exp(EXP))
EXP: IF EXP THEN EXP ELSE EXP FI (AST.IfExp(EXP1,EXP2,EXP3))
    | EXP IMPLIES EXP (AST.BoolExp(AST.Implies, EXP1,  EXP2))
    | EXP AND EXP (AST.BoolExp(AST.And, EXP1,  EXP2))
    | EXP OR EXP (AST.BoolExp(AST.Or, EXP1,  EXP2))
    | EXP XOR EXP (AST.BoolExp(AST.Xor, EXP1,  EXP2))
    | EXP EQUALS EXP (AST.BoolExp(AST.Equals, EXP1,  EXP2))
    | EXP GREATERTHAN EXP (AST.BinExp(AST.Greaterthan, EXP1,  EXP2))
    | NOT EXP (AST.NegExp(AST.Not, EXP))
    | LPAREN EXP RPAREN (AST.ParenExp(EXP))
    | CONST (AST.TFExp(CONST))
    | NUM (AST.NumExp(NUM))
    | ID (AST.VarExp(ID))
    | EXP PLUS EXP (AST.BinExp(AST.Add, EXP1,  EXP2))
    | EXP MINUS  EXP (AST.BinExp(AST.Sub,  EXP1,  EXP2))
    | EXP TIMES  EXP (AST.BinExp(AST.Mul,  EXP1, EXP2))
    | EXP DIV  EXP (AST.BinExp(AST.Div, EXP1, EXP2))
    | LET DECL IN EXP END (AST.LetExp(DECL, EXP))
    | EXP EQ EXP (AST.BinExp(AST.Eq, EXP1, EXP2))
    | FN LPAREN ID COLON Type RPAREN COLON Type DARROW EXP (AST.FnExp(ID,Type1,Type2,EXP))
    | FUN ID LPAREN ID COLON Type RPAREN COLON Type DARROW EXP (AST.FunExp(ID1,ID2,Type1,Type2,EXP))
    | LPAREN EXP EXP RPAREN (AST.AppExp(EXP1,EXP2))
DECL: ID EQ EXP (AST.ValDecl(ID, EXP))

Type:
    Type ARROW Type     (AST.FnTy(Type1,Type2))
    | Type TIMES Type   (AST.TupTyp(Type1::Type2::nil))
    | INT               (AST.IntTy)
    | BOOL               (AST.BoolTy)
    | STRING               (AST.StrTy)
    | LPAREN Type RPAREN    (Type)