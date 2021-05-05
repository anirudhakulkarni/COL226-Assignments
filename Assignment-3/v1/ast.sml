structure AST =
struct

type id = string
datatype typ = IntTy
        | BoolTy
        | StrTy
        | FnTy of typ * typ
        | TupTyp of typ list
datatype binop = Add | Sub | Mul | Div | Eq | Greaterthan | Lessthan
datatype bbinop = Equals | Implies | And | Or | Xor
datatype negop = Not
datatype decl = ValDecl of id * exp
and exp = NumExp of int
    	| StringExp of string
    	| VarExp of id
	    | BinExp of binop * exp * exp
	    | LetExp of decl * exp
        | BoolExp of bbinop * exp * exp
        | NegExp of negop*exp
        | TFExp of string
        | IfExp of exp*exp*exp
        | ParenExp of exp
        | FnExp of id*typ*typ*exp
        | TupExp of exp list
        | FunExp of id*id*typ*typ*exp
        | AppExp of exp*exp

datatype value = IntVal of int
                | StringVal of string
	            | BoolVal of bool
                (* | FnVal of id*exp*(environment ref) *)
                | TupVal of value list
type environment = (id * value) list				       
datatype statement = Exps of exp*statement
        | Exp of exp
datatype program = Statement of statement


fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail "Environment lookup error"							    
end


