functor BoolLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Bool_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\025\000\002\000\024\000\003\000\023\000\004\000\022\000\
\\005\000\021\000\006\000\020\000\007\000\019\000\008\000\018\000\
\\018\000\017\000\022\000\016\000\026\000\015\000\027\000\014\000\000\000\
\\001\000\001\000\025\000\002\000\024\000\003\000\023\000\004\000\022\000\
\\005\000\021\000\006\000\020\000\007\000\019\000\009\000\013\000\
\\010\000\012\000\011\000\011\000\015\000\010\000\016\000\050\000\
\\017\000\009\000\018\000\017\000\019\000\008\000\022\000\016\000\
\\023\000\007\000\026\000\015\000\027\000\014\000\033\000\006\000\
\\034\000\005\000\000\000\
\\001\000\001\000\025\000\002\000\024\000\003\000\023\000\004\000\022\000\
\\005\000\021\000\006\000\020\000\007\000\019\000\012\000\051\000\
\\018\000\017\000\022\000\016\000\026\000\015\000\027\000\014\000\000\000\
\\001\000\001\000\025\000\002\000\024\000\003\000\023\000\004\000\022\000\
\\005\000\021\000\006\000\020\000\007\000\019\000\013\000\065\000\
\\018\000\017\000\022\000\016\000\026\000\015\000\027\000\014\000\000\000\
\\001\000\001\000\025\000\002\000\024\000\003\000\023\000\004\000\022\000\
\\005\000\021\000\006\000\020\000\007\000\019\000\014\000\077\000\
\\018\000\017\000\022\000\016\000\026\000\015\000\027\000\014\000\000\000\
\\001\000\001\000\025\000\002\000\024\000\003\000\023\000\004\000\022\000\
\\005\000\021\000\006\000\020\000\007\000\019\000\016\000\056\000\
\\018\000\017\000\022\000\016\000\026\000\015\000\027\000\014\000\000\000\
\\001\000\001\000\025\000\002\000\024\000\003\000\023\000\004\000\022\000\
\\005\000\021\000\006\000\020\000\007\000\019\000\018\000\017\000\
\\022\000\016\000\025\000\064\000\026\000\015\000\027\000\014\000\000\000\
\\001\000\007\000\068\000\016\000\067\000\035\000\066\000\000\000\
\\001\000\007\000\068\000\016\000\075\000\035\000\066\000\000\000\
\\001\000\007\000\068\000\016\000\076\000\035\000\066\000\000\000\
\\001\000\007\000\068\000\035\000\066\000\036\000\080\000\000\000\
\\001\000\007\000\068\000\035\000\066\000\036\000\083\000\000\000\
\\001\000\009\000\013\000\010\000\012\000\011\000\011\000\015\000\010\000\
\\017\000\009\000\019\000\008\000\023\000\007\000\033\000\006\000\
\\034\000\005\000\000\000\
\\001\000\015\000\026\000\000\000\
\\001\000\015\000\046\000\000\000\
\\001\000\015\000\062\000\039\000\061\000\040\000\060\000\041\000\059\000\000\000\
\\001\000\017\000\027\000\000\000\
\\001\000\017\000\029\000\000\000\
\\001\000\017\000\045\000\000\000\
\\001\000\017\000\053\000\000\000\
\\001\000\024\000\047\000\000\000\
\\001\000\026\000\048\000\000\000\
\\001\000\037\000\052\000\000\000\
\\001\000\037\000\063\000\000\000\
\\001\000\037\000\073\000\000\000\
\\001\000\037\000\079\000\000\000\
\\001\000\042\000\000\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\009\000\013\000\010\000\012\000\011\000\011\000\015\000\010\000\
\\017\000\009\000\019\000\008\000\023\000\007\000\033\000\006\000\
\\034\000\005\000\000\000\
\\089\000\000\000\
\\090\000\001\000\025\000\002\000\024\000\003\000\023\000\004\000\022\000\
\\005\000\021\000\006\000\020\000\007\000\019\000\018\000\017\000\
\\022\000\016\000\027\000\014\000\000\000\
\\091\000\007\000\019\000\027\000\014\000\000\000\
\\092\000\007\000\019\000\027\000\014\000\000\000\
\\093\000\007\000\019\000\027\000\014\000\000\000\
\\094\000\007\000\019\000\027\000\014\000\000\000\
\\095\000\007\000\019\000\027\000\014\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\007\000\019\000\027\000\014\000\000\000\
\\102\000\007\000\019\000\027\000\014\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\001\000\025\000\002\000\024\000\003\000\023\000\004\000\022\000\
\\005\000\021\000\006\000\020\000\007\000\019\000\018\000\017\000\
\\022\000\016\000\027\000\014\000\000\000\
\\107\000\001\000\025\000\002\000\024\000\003\000\023\000\004\000\022\000\
\\005\000\021\000\006\000\020\000\007\000\019\000\018\000\017\000\
\\022\000\016\000\026\000\015\000\027\000\014\000\000\000\
\\108\000\001\000\025\000\002\000\024\000\003\000\023\000\004\000\022\000\
\\005\000\021\000\006\000\020\000\007\000\019\000\018\000\017\000\
\\022\000\016\000\026\000\015\000\027\000\014\000\000\000\
\\109\000\000\000\
\\110\000\001\000\025\000\002\000\024\000\003\000\023\000\004\000\022\000\
\\005\000\021\000\006\000\020\000\007\000\019\000\018\000\017\000\
\\022\000\016\000\026\000\015\000\027\000\014\000\000\000\
\\111\000\007\000\068\000\035\000\066\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\"
val actionRowNumbers =
"\012\000\000\000\027\000\013\000\
\\016\000\017\000\012\000\041\000\
\\012\000\012\000\039\000\040\000\
\\012\000\012\000\012\000\012\000\
\\029\000\012\000\012\000\012\000\
\\012\000\012\000\012\000\012\000\
\\018\000\014\000\020\000\021\000\
\\037\000\001\000\002\000\045\000\
\\047\000\036\000\031\000\028\000\
\\044\000\043\000\042\000\035\000\
\\034\000\033\000\032\000\022\000\
\\019\000\012\000\012\000\005\000\
\\038\000\012\000\015\000\023\000\
\\006\000\051\000\050\000\003\000\
\\007\000\055\000\054\000\056\000\
\\015\000\015\000\046\000\012\000\
\\015\000\024\000\015\000\008\000\
\\009\000\004\000\052\000\015\000\
\\053\000\057\000\025\000\030\000\
\\010\000\015\000\012\000\011\000\
\\048\000\012\000\049\000\026\000"
val gotoT =
"\
\\001\000\083\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\026\000\000\000\
\\004\000\028\000\000\000\
\\000\000\
\\004\000\029\000\000\000\
\\004\000\030\000\000\000\
\\000\000\
\\000\000\
\\004\000\031\000\000\000\
\\004\000\032\000\000\000\
\\004\000\033\000\000\000\
\\004\000\034\000\000\000\
\\003\000\035\000\004\000\001\000\000\000\
\\004\000\036\000\000\000\
\\004\000\037\000\000\000\
\\004\000\038\000\000\000\
\\004\000\039\000\000\000\
\\004\000\040\000\000\000\
\\004\000\041\000\000\000\
\\004\000\042\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\047\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\052\000\000\000\
\\004\000\053\000\000\000\
\\000\000\
\\000\000\
\\004\000\055\000\000\000\
\\002\000\056\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\067\000\000\000\
\\002\000\068\000\000\000\
\\000\000\
\\004\000\069\000\000\000\
\\002\000\070\000\000\000\
\\000\000\
\\002\000\072\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\076\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\079\000\000\000\
\\004\000\080\000\000\000\
\\000\000\
\\000\000\
\\004\000\082\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 84
val numrules = 31
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | ID of unit ->  (string)
 | CONST of unit ->  (string) | NUM of unit ->  (int)
 | DECL of unit ->  (AST.decl) | EXP of unit ->  (AST.exp)
 | statement of unit ->  (AST.statement) | Type of unit ->  (AST.typ)
 | program of unit ->  (AST.program)
end
type svalue = MlyValue.svalue
type result = AST.program
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 41) => true | _ => false
val showTerminal =
fn (T 0) => "AND"
  | (T 1) => "OR"
  | (T 2) => "XOR"
  | (T 3) => "EQUALS"
  | (T 4) => "PLUS"
  | (T 5) => "MINUS"
  | (T 6) => "TIMES"
  | (T 7) => "TERM"
  | (T 8) => "NUM"
  | (T 9) => "CONST"
  | (T 10) => "IF"
  | (T 11) => "THEN"
  | (T 12) => "ELSE"
  | (T 13) => "FI"
  | (T 14) => "LPAREN"
  | (T 15) => "RPAREN"
  | (T 16) => "ID"
  | (T 17) => "IMPLIES"
  | (T 18) => "NOT"
  | (T 19) => "NEGATE"
  | (T 20) => "LESSTHAN"
  | (T 21) => "GREATERTHAN"
  | (T 22) => "LET"
  | (T 23) => "IN"
  | (T 24) => "END"
  | (T 25) => "EQ"
  | (T 26) => "DIV"
  | (T 27) => "MUL"
  | (T 28) => "SUB"
  | (T 29) => "VAL"
  | (T 30) => "NEQ"
  | (T 31) => "ASSIGN"
  | (T 32) => "FUN"
  | (T 33) => "FN"
  | (T 34) => "ARROW"
  | (T 35) => "DARROW"
  | (T 36) => "COLON"
  | (T 37) => "COMMA"
  | (T 38) => "STRING"
  | (T 39) => "INT"
  | (T 40) => "BOOL"
  | (T 41) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34)
 $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27)
 $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20)
 $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ 
(T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.statement statement1, statement1left, 
statement1right)) :: rest671)) => let val  result = MlyValue.program
 (fn _ => let val  (statement as statement1) = statement1 ()
 in (
AST.postorder:=(!AST.postorder)^"program => statement";AST.Statement(statement)
)
end)
 in ( LrTable.NT 0, ( result, statement1left, statement1right), 
rest671)
end
|  ( 1, ( ( _, ( MlyValue.statement statement1, _, statement1right))
 :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let
 val  result = MlyValue.statement (fn _ => let val  (EXP as EXP1) = 
EXP1 ()
 val  (statement as statement1) = statement1 ()
 in (
AST.postorder:=(!AST.postorder)^"TERM ;, statement => EXP TERM statement, ";AST.Exps(EXP,statement)
)
end)
 in ( LrTable.NT 2, ( result, EXP1left, statement1right), rest671)
end
|  ( 2, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.EXP EXP1, 
EXP1left, _)) :: rest671)) => let val  result = MlyValue.statement (fn
 _ => let val  (EXP as EXP1) = EXP1 ()
 in (
AST.postorder:=(!AST.postorder)^"TERM ;, statement => EXP TERM, ";AST.Exp(EXP)
)
end)
 in ( LrTable.NT 2, ( result, EXP1left, TERM1right), rest671)
end
|  ( 3, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.EXP EXP3, _, _))
 :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP 
EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result
 = MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  EXP2 = EXP2 ()
 val  EXP3 = EXP3 ()
 in (
AST.postorder:=(!AST.postorder)^"IF IF, THEN THEN, ELSE ELSE, FI FI, EXP => if EXP then EXP else EXP fi, ";AST.IfExp(EXP1,EXP2,EXP3)
)
end)
 in ( LrTable.NT 3, ( result, IF1left, FI1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
AST.postorder:=(!AST.postorder)^"IMPLIES IMPLIES, EXP => EXP IMPLIES EXP, ";AST.BoolExp(AST.Implies, EXP1,  EXP2)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
AST.postorder:=(!AST.postorder)^"AND AND, EXP => EXP AND EXP, ";AST.BoolExp(AST.And, EXP1,  EXP2)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
AST.postorder:=(!AST.postorder)^"OR OR, EXP => EXP OR EXP, ";AST.BoolExp(AST.Or, EXP1,  EXP2)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
AST.postorder:=(!AST.postorder)^"XOR XOR, EXP => EXP XOR EXP, ";AST.BoolExp(AST.Xor, EXP1,  EXP2)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
AST.postorder:=(!AST.postorder)^"EQUALS EQUALS, EXP => EXP EQUALS EXP, ";AST.BoolExp(AST.Equals, EXP1,  EXP2)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
AST.postorder:=(!AST.postorder)^"GREATERTHAN GREATERTHAN, EXP => EXP GREATERTHAN EXP, ";AST.BinExp(AST.Greaterthan, EXP1,  EXP2)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ =>
 let val  (EXP as EXP1) = EXP1 ()
 in (
AST.postorder:=(!AST.postorder)^"NOT NOT, EXP => NOT EXP, ";AST.NegExp(AST.Not, EXP)
)
end)
 in ( LrTable.NT 3, ( result, NOT1left, EXP1right), rest671)
end
|  ( 11, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (
AST.postorder:=(!AST.postorder)^"LPAREN (, RPAREN ), EXP => LPAREN EXP RPAREN, ";AST.ParenExp(EXP)
)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (CONST
 as CONST1) = CONST1 ()
 in (
AST.postorder:=(!AST.postorder)^"CONST "^CONST^", EXP => CONST, ";AST.TFExp(CONST)
)
end)
 in ( LrTable.NT 3, ( result, CONST1left, CONST1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.EXP (fn _ => let val  (NUM as NUM1) = 
NUM1 ()
 in (
AST.postorder:=(!AST.postorder)^"NUM "^Int.toString(NUM)^", EXP => NUM, ";AST.NumExp(NUM)
)
end)
 in ( LrTable.NT 3, ( result, NUM1left, NUM1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 in (
AST.postorder:=(!AST.postorder)^"ID "^ID^", EXP => ID, ";AST.VarExp(ID)
)
end)
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
AST.postorder:=(!AST.postorder)^"PLUS PLUS, EXP => EXP PLUS EXP, ";AST.BinExp(AST.Add, EXP1,  EXP2)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
AST.postorder:=(!AST.postorder)^"MINUS MINUS, EXP => EXP MINUS EXP, ";AST.BinExp(AST.Sub,  EXP1,  EXP2)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
AST.postorder:=(!AST.postorder)^"TIMES TIMES, EXP => EXP TIMES EXP, ";AST.BinExp(AST.Mul,  EXP1, EXP2)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
AST.postorder:=(!AST.postorder)^"DIV DIV, EXP => EXP DIV EXP, ";AST.BinExp(AST.Div, EXP1, EXP2)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 19, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)
) :: _ :: ( _, ( MlyValue.DECL DECL1, _, _)) :: ( _, ( _, LET1left, _)
) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (
DECL as DECL1) = DECL1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (
AST.postorder:=(!AST.postorder)^"LET LET, IN IN, END END, EXP => LET DECL IN EXP END, ";AST.LetExp(DECL, EXP)
)
end)
 in ( LrTable.NT 3, ( result, LET1left, END1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
AST.postorder:=(!AST.postorder)^"EQ, EQ => EXP EQ EXP, ";AST.BinExp(AST.Eq, EXP1, EXP2)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.Type Type2, _, _)) :: _ :: _ :: ( _, ( MlyValue.Type Type1, _
, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, ( _, FN1left
, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val 
 (ID as ID1) = ID1 ()
 val  (Type as Type1) = Type1 ()
 val  Type2 = Type2 ()
 val  (EXP as EXP1) = EXP1 ()
 in (
AST.postorder:=(!AST.postorder)^"LPAREN (, ID"^ID^", COLON :, RPAREN ), COLON :, DARROW =>,  EXP => FN LPAREN ID COLON Type RPAREN COLON Type DARROW EXP, ";AST.FnExp(ID,Type1,Type2,EXP)
)
end)
 in ( LrTable.NT 3, ( result, FN1left, EXP1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.Type Type2, _, _)) :: _ :: _ :: ( _, ( MlyValue.Type Type1, _
, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let
 val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 val  ID2 = ID2 ()
 val  (Type as Type1) = Type1 ()
 val  Type2 = Type2 ()
 val  (EXP as EXP1) = EXP1 ()
 in (
AST.postorder:=(!AST.postorder)^"ID"^ID^", LPAREN (,"^"ID "^ID^", COLON :, RPAREN ), COLON :, DARROW =>,  EXP => FUN ID LPAREN ID COLON Type RPAREN COLON Type DARROW EXP, ";AST.FunExp(ID1,ID2,Type1,Type2,EXP)
)
end)
 in ( LrTable.NT 3, ( result, FUN1left, EXP1right), rest671)
end
|  ( 23, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP2, _,
 _)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (EXP
 as EXP1) = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
AST.postorder:=(!AST.postorder)^"LPAREN (, RPAREN ), EXP => LPAREN EXP EXP RPAREN, ";AST.AppExp(EXP1,EXP2)
)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.DECL (fn _ => let val  (ID as ID1) = ID1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (
AST.postorder:=(!AST.postorder)^"ID "^ID^", EQ EQ, DECL => ID EQ EXP, ";AST.ValDecl(ID, EXP)
)
end)
 in ( LrTable.NT 4, ( result, ID1left, EXP1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.Type Type2, _, Type2right)) :: _ :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.Type (fn _ => let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in (AST.FnTy(Type1,Type2))
end)
 in ( LrTable.NT 1, ( result, Type1left, Type2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.Type Type2, _, Type2right)) :: _ :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.Type (fn _ => let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in (AST.TupTyp(Type1::Type2::nil))
end)
 in ( LrTable.NT 1, ( result, Type1left, Type2right), rest671)
end
|  ( 27, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => (AST.IntTy))
 in ( LrTable.NT 1, ( result, INT1left, INT1right), rest671)
end
|  ( 28, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => (AST.BoolTy))
 in ( LrTable.NT 1, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right))
 :: rest671)) => let val  result = MlyValue.Type (fn _ => let val  
STRING1 = STRING1 ()
 in (AST.StrTy)
end)
 in ( LrTable.NT 1, ( result, STRING1left, STRING1right), rest671)
end
|  ( 30, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Type Type1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.Type (fn _ => let val  (Type as Type1) = Type1 ()
 in (Type)
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Bool_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun DARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
end
end
