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
\\001\000\001\000\020\000\002\000\019\000\003\000\018\000\004\000\017\000\
\\006\000\013\000\010\000\012\000\012\000\011\000\000\000\
\\001\000\005\000\022\000\000\000\
\\001\000\006\000\013\000\010\000\012\000\012\000\011\000\000\000\
\\001\000\008\000\032\000\000\000\
\\001\000\009\000\034\000\000\000\
\\001\000\011\000\029\000\000\000\
\\001\000\013\000\027\000\000\000\
\\001\000\015\000\000\000\000\000\
\\037\000\000\000\
\\038\000\006\000\013\000\010\000\012\000\012\000\011\000\000\000\
\\039\000\000\000\
\\040\000\000\000\
\\041\000\000\000\
\\042\000\007\000\021\000\000\000\
\\043\000\000\000\
\\044\000\001\000\020\000\002\000\019\000\003\000\018\000\004\000\017\000\
\\006\000\013\000\010\000\012\000\012\000\011\000\000\000\
\\045\000\000\000\
\\046\000\000\000\
\\047\000\000\000\
\\048\000\014\000\014\000\000\000\
\\049\000\000\000\
\\050\000\000\000\
\\051\000\000\000\
\\052\000\000\000\
\\053\000\000\000\
\\054\000\000\000\
\\055\000\000\000\
\\056\000\000\000\
\"
val actionRowNumbers =
"\009\000\020\000\019\000\017\000\
\\015\000\013\000\001\000\008\000\
\\002\000\023\000\002\000\022\000\
\\002\000\002\000\006\000\027\000\
\\026\000\025\000\024\000\002\000\
\\011\000\010\000\005\000\018\000\
\\000\000\002\000\003\000\021\000\
\\016\000\014\000\002\000\004\000\
\\002\000\012\000\007\000"
val gotoT =
"\
\\001\000\034\000\002\000\008\000\003\000\007\000\004\000\006\000\
\\005\000\005\000\006\000\004\000\007\000\003\000\008\000\002\000\
\\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\014\000\006\000\004\000\007\000\003\000\008\000\002\000\
\\009\000\001\000\010\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\008\000\003\000\021\000\004\000\006\000\005\000\005\000\
\\006\000\004\000\007\000\003\000\008\000\002\000\009\000\001\000\000\000\
\\000\000\
\\004\000\022\000\005\000\005\000\006\000\004\000\007\000\003\000\
\\008\000\002\000\009\000\001\000\000\000\
\\000\000\
\\007\000\023\000\008\000\002\000\009\000\001\000\000\000\
\\006\000\024\000\007\000\003\000\008\000\002\000\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\026\000\005\000\005\000\006\000\004\000\007\000\003\000\
\\008\000\002\000\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\028\000\008\000\002\000\009\000\001\000\010\000\013\000\000\000\
\\005\000\029\000\006\000\004\000\007\000\003\000\008\000\002\000\
\\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\031\000\005\000\005\000\006\000\004\000\007\000\003\000\
\\008\000\002\000\009\000\001\000\000\000\
\\000\000\
\\004\000\033\000\005\000\005\000\006\000\004\000\007\000\003\000\
\\008\000\002\000\009\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 35
val numrules = 20
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
 | NOT of unit ->  (string) | IMPLIES of unit ->  (string)
 | ID of unit ->  (string) | RPAREN of unit ->  (string)
 | LPAREN of unit ->  (string) | ELSE of unit ->  (string)
 | THEN of unit ->  (string) | IF of unit ->  (string)
 | CONST of unit ->  (string) | TERM of unit ->  (string)
 | EQUALS of unit ->  (string) | XOR of unit ->  (string)
 | OR of unit ->  (string) | AND of unit ->  (string)
 | binop of unit ->  (string) | formula_V of unit ->  (string)
 | formula_IV of unit ->  (string) | formula_III of unit ->  (string)
 | formula_II of unit ->  (string) | formula_I of unit ->  (string)
 | formula of unit ->  (string) | statements of unit ->  (string)
 | statement of unit ->  (string)
 | program of unit ->  (string option)
end
type svalue = MlyValue.svalue
type result = string option
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
fn (T 14) => true | _ => false
val showTerminal =
fn (T 0) => "AND"
  | (T 1) => "OR"
  | (T 2) => "XOR"
  | (T 3) => "EQUALS"
  | (T 4) => "TERM"
  | (T 5) => "CONST"
  | (T 6) => "IF"
  | (T 7) => "THEN"
  | (T 8) => "ELSE"
  | (T 9) => "LPAREN"
  | (T 10) => "RPAREN"
  | (T 11) => "ID"
  | (T 12) => "IMPLIES"
  | (T 13) => "NOT"
  | (T 14) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 14)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.statements statements1, statements1left, 
statements1right)) :: rest671)) => let val  result = MlyValue.program
 (fn _ => let val  (statements as statements1) = statements1 ()
 in (SOME statements)
end)
 in ( LrTable.NT 0, ( result, statements1left, statements1right), 
rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.program (fn _ => (
NONE ))
 in ( LrTable.NT 0, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.statements statements1, _, statements1right)
) :: ( _, ( MlyValue.statement statement1, statement1left, _)) :: 
rest671)) => let val  result = MlyValue.statements (fn _ => let val  (
statement as statement1) = statement1 ()
 val  (statements as statements1) = statements1 ()
 in (statement^statements)
end)
 in ( LrTable.NT 2, ( result, statement1left, statements1right), 
rest671)
end
|  ( 3, ( ( _, ( MlyValue.TERM TERM1, _, TERM1right)) :: ( _, ( 
MlyValue.formula formula1, formula1left, _)) :: rest671)) => let val  
result = MlyValue.statement (fn _ => let val  (formula as formula1) = 
formula1 ()
 val  (TERM as TERM1) = TERM1 ()
 in (formula^TERM)
end)
 in ( LrTable.NT 1, ( result, formula1left, TERM1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.formula formula3, _, formula3right)) :: ( _,
 ( MlyValue.ELSE ELSE1, _, _)) :: ( _, ( MlyValue.formula formula2, _,
 _)) :: ( _, ( MlyValue.THEN THEN1, _, _)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: ( _, ( MlyValue.IF IF1, _, _)) :: ( _, ( 
MlyValue.formula_I formula_I1, formula_I1left, _)) :: rest671)) => let
 val  result = MlyValue.formula (fn _ => let val  (formula_I as 
formula_I1) = formula_I1 ()
 val  (IF as IF1) = IF1 ()
 val  formula1 = formula1 ()
 val  (THEN as THEN1) = THEN1 ()
 val  formula2 = formula2 ()
 val  (ELSE as ELSE1) = ELSE1 ()
 val  formula3 = formula3 ()
 in ( formula_I^IF^formula1^THEN^formula2^ELSE^formula3)
end)
 in ( LrTable.NT 3, ( result, formula_I1left, formula3right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.formula_I formula_I1, formula_I1left, 
formula_I1right)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  (formula_I as formula_I1) = formula_I1 ()
 in (formula_I)
end)
 in ( LrTable.NT 3, ( result, formula_I1left, formula_I1right), 
rest671)
end
|  ( 6, ( ( _, ( MlyValue.formula_I formula_I2, _, formula_I2right))
 :: ( _, ( MlyValue.IMPLIES IMPLIES1, _, _)) :: ( _, ( 
MlyValue.formula_I formula_I1, _, _)) :: ( _, ( MlyValue.formula_II 
formula_II1, formula_II1left, _)) :: rest671)) => let val  result = 
MlyValue.formula_I (fn _ => let val  (formula_II as formula_II1) = 
formula_II1 ()
 val  formula_I1 = formula_I1 ()
 val  (IMPLIES as IMPLIES1) = IMPLIES1 ()
 val  formula_I2 = formula_I2 ()
 in ( formula_II^formula_I1^IMPLIES^formula_I2)
end)
 in ( LrTable.NT 4, ( result, formula_II1left, formula_I2right), 
rest671)
end
|  ( 7, ( ( _, ( MlyValue.formula_II formula_II1, formula_II1left, 
formula_II1right)) :: rest671)) => let val  result = 
MlyValue.formula_I (fn _ => let val  (formula_II as formula_II1) = 
formula_II1 ()
 in ( formula_II)
end)
 in ( LrTable.NT 4, ( result, formula_II1left, formula_II1right), 
rest671)
end
|  ( 8, ( ( _, ( MlyValue.formula_III formula_III1, _, 
formula_III1right)) :: ( _, ( MlyValue.formula_II formula_II2, _, _))
 :: ( _, ( MlyValue.binop binop1, _, _)) :: ( _, ( MlyValue.formula_II
 formula_II1, formula_II1left, _)) :: rest671)) => let val  result = 
MlyValue.formula_II (fn _ => let val  formula_II1 = formula_II1 ()
 val  (binop as binop1) = binop1 ()
 val  formula_II2 = formula_II2 ()
 val  (formula_III as formula_III1) = formula_III1 ()
 in (formula_II1^binop^formula_II2^formula_III)
end)
 in ( LrTable.NT 5, ( result, formula_II1left, formula_III1right), 
rest671)
end
|  ( 9, ( ( _, ( MlyValue.formula_III formula_III1, formula_III1left, 
formula_III1right)) :: rest671)) => let val  result = 
MlyValue.formula_II (fn _ => let val  (formula_III as formula_III1) = 
formula_III1 ()
 in ( formula_III)
end)
 in ( LrTable.NT 5, ( result, formula_III1left, formula_III1right), 
rest671)
end
|  ( 10, ( ( _, ( MlyValue.formula_III formula_III1, _, 
formula_III1right)) :: ( _, ( MlyValue.NOT NOT1, _, _)) :: ( _, ( 
MlyValue.formula_IV formula_IV1, formula_IV1left, _)) :: rest671)) =>
 let val  result = MlyValue.formula_III (fn _ => let val  (formula_IV
 as formula_IV1) = formula_IV1 ()
 val  (NOT as NOT1) = NOT1 ()
 val  (formula_III as formula_III1) = formula_III1 ()
 in ( formula_IV^NOT^formula_III)
end)
 in ( LrTable.NT 6, ( result, formula_IV1left, formula_III1right), 
rest671)
end
|  ( 11, ( ( _, ( MlyValue.formula_IV formula_IV1, formula_IV1left, 
formula_IV1right)) :: rest671)) => let val  result = 
MlyValue.formula_III (fn _ => let val  (formula_IV as formula_IV1) = 
formula_IV1 ()
 in (formula_IV)
end)
 in ( LrTable.NT 6, ( result, formula_IV1left, formula_IV1right), 
rest671)
end
|  ( 12, ( ( _, ( MlyValue.formula_V formula_V1, formula_V1left, 
formula_V1right)) :: rest671)) => let val  result = 
MlyValue.formula_IV (fn _ => let val  (formula_V as formula_V1) = 
formula_V1 ()
 in ( formula_V)
end)
 in ( LrTable.NT 7, ( result, formula_V1left, formula_V1right), 
rest671)
end
|  ( 13, ( ( _, ( MlyValue.RPAREN RPAREN1, _, RPAREN1right)) :: ( _, (
 MlyValue.formula formula1, _, _)) :: ( _, ( MlyValue.LPAREN LPAREN1, 
LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.formula_IV
 (fn _ => let val  (LPAREN as LPAREN1) = LPAREN1 ()
 val  (formula as formula1) = formula1 ()
 val  (RPAREN as RPAREN1) = RPAREN1 ()
 in ( LPAREN^formula^RPAREN)
end)
 in ( LrTable.NT 7, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.formula_V (fn _ => let val  (
CONST as CONST1) = CONST1 ()
 in (CONST)
end)
 in ( LrTable.NT 8, ( result, CONST1left, CONST1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.formula_V (fn _ => let val  (ID as ID1) = 
ID1 ()
 in (ID)
end)
 in ( LrTable.NT 8, ( result, ID1left, ID1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.AND AND1, AND1left, AND1right)) :: rest671)
) => let val  result = MlyValue.binop (fn _ => let val  (AND as AND1)
 = AND1 ()
 in (AND)
end)
 in ( LrTable.NT 9, ( result, AND1left, AND1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.OR OR1, OR1left, OR1right)) :: rest671)) =>
 let val  result = MlyValue.binop (fn _ => let val  (OR as OR1) = OR1
 ()
 in (OR)
end)
 in ( LrTable.NT 9, ( result, OR1left, OR1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.XOR XOR1, XOR1left, XOR1right)) :: rest671)
) => let val  result = MlyValue.binop (fn _ => let val  (XOR as XOR1)
 = XOR1 ()
 in (XOR)
end)
 in ( LrTable.NT 9, ( result, XOR1left, XOR1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EQUALS EQUALS1, EQUALS1left, EQUALS1right))
 :: rest671)) => let val  result = MlyValue.binop (fn _ => let val  (
EQUALS as EQUALS1) = EQUALS1 ()
 in (EQUALS)
end)
 in ( LrTable.NT 9, ( result, EQUALS1left, EQUALS1right), rest671)
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
fun AND (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.AND (fn () => i),p1,p2))
fun OR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.OR (fn () => i),p1,p2))
fun XOR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.XOR (fn () => i),p1,p2))
fun EQUALS (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.EQUALS (fn () => i),p1,p2))
fun TERM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.TERM (fn () => i),p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun IF (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.IF (fn () => i),p1,p2))
fun THEN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.THEN (fn () => i),p1,p2))
fun ELSE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.ELSE (fn () => i),p1,p2))
fun LPAREN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.LPAREN (fn () => i),p1,p2))
fun RPAREN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.RPAREN (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun IMPLIES (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.IMPLIES (fn () => i),p1,p2))
fun NOT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.NOT (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
end
end
