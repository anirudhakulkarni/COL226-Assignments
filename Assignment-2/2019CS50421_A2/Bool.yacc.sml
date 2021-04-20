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
\\001\000\001\000\015\000\002\000\014\000\003\000\013\000\004\000\012\000\
\\005\000\011\000\013\000\010\000\000\000\
\\001\000\001\000\015\000\002\000\014\000\003\000\013\000\004\000\012\000\
\\008\000\026\000\013\000\010\000\000\000\
\\001\000\001\000\015\000\002\000\014\000\003\000\013\000\004\000\012\000\
\\009\000\028\000\013\000\010\000\000\000\
\\001\000\001\000\015\000\002\000\014\000\003\000\013\000\004\000\012\000\
\\011\000\025\000\013\000\010\000\000\000\
\\001\000\006\000\009\000\007\000\008\000\010\000\007\000\012\000\006\000\
\\014\000\005\000\000\000\
\\001\000\015\000\000\000\000\000\
\\031\000\000\000\
\\032\000\006\000\009\000\007\000\008\000\010\000\007\000\012\000\006\000\
\\014\000\005\000\000\000\
\\033\000\000\000\
\\034\000\006\000\009\000\007\000\008\000\010\000\007\000\012\000\006\000\
\\014\000\005\000\000\000\
\\035\000\001\000\015\000\002\000\014\000\003\000\013\000\004\000\012\000\
\\013\000\010\000\000\000\
\\036\000\001\000\015\000\002\000\014\000\003\000\013\000\004\000\012\000\
\\013\000\010\000\000\000\
\\037\000\000\000\
\\038\000\000\000\
\\039\000\000\000\
\\040\000\000\000\
\\041\000\000\000\
\\042\000\000\000\
\\043\000\000\000\
\\044\000\000\000\
\"
val actionRowNumbers =
"\007\000\000\000\006\000\004\000\
\\019\000\004\000\004\000\018\000\
\\004\000\009\000\004\000\004\000\
\\004\000\004\000\016\000\003\000\
\\001\000\011\000\008\000\015\000\
\\014\000\013\000\012\000\017\000\
\\004\000\002\000\004\000\010\000\
\\005\000"
val gotoT =
"\
\\001\000\028\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\003\000\014\000\000\000\
\\000\000\
\\003\000\015\000\000\000\
\\003\000\016\000\000\000\
\\000\000\
\\003\000\017\000\000\000\
\\002\000\018\000\003\000\001\000\000\000\
\\003\000\019\000\000\000\
\\003\000\020\000\000\000\
\\003\000\021\000\000\000\
\\003\000\022\000\000\000\
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
\\003\000\025\000\000\000\
\\000\000\
\\003\000\027\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 29
val numrules = 14
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
 | formula of unit ->  (string) | statement of unit ->  (string)
 | program of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = string
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
of  ( 0, ( ( _, ( MlyValue.statement statement1, statement1left, 
statement1right)) :: rest671)) => let val  result = MlyValue.program
 (fn _ => let val  (statement as statement1) = statement1 ()
 in (statement^"<statement>, <program>")
end)
 in ( LrTable.NT 0, ( result, statement1left, statement1right), 
rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.program (fn _ => (
 "<program>"))
 in ( LrTable.NT 0, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.statement statement1, _, statement1right))
 :: ( _, ( MlyValue.TERM TERM1, _, _)) :: ( _, ( MlyValue.formula 
formula1, formula1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  (formula as formula1) = formula1
 ()
 val  (TERM as TERM1) = TERM1 ()
 val  (statement as statement1) = statement1 ()
 in (
formula^", <formula>, "^TERM^", <TERM>, "^statement^", <statement>, ")

end)
 in ( LrTable.NT 1, ( result, formula1left, statement1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.TERM TERM1, _, TERM1right)) :: ( _, ( 
MlyValue.formula formula1, formula1left, _)) :: rest671)) => let val  
result = MlyValue.statement (fn _ => let val  (formula as formula1) = 
formula1 ()
 val  (TERM as TERM1) = TERM1 ()
 in (formula^", <formula>, \""^TERM^"\", <TERM>, ")
end)
 in ( LrTable.NT 1, ( result, formula1left, TERM1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.formula formula3, _, formula3right)) :: ( _,
 ( MlyValue.ELSE ELSE1, _, _)) :: ( _, ( MlyValue.formula formula2, _,
 _)) :: ( _, ( MlyValue.THEN THEN1, _, _)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: ( _, ( MlyValue.IF IF1, IF1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  (IF as IF1) =
 IF1 ()
 val  (formula as formula1) = formula1 ()
 val  (THEN as THEN1) = THEN1 ()
 val  formula2 = formula2 ()
 val  (ELSE as ELSE1) = ELSE1 ()
 val  formula3 = formula3 ()
 in (
 "\""^IF^"\", IF, "^formula1^", <formula>, \""^THEN^"\", THEN, "^formula2^", <formula>, \""^ELSE^"\", ELSE, "^formula3^", <formula>"
)
end)
 in ( LrTable.NT 2, ( result, IF1left, formula3right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: ( _,
 ( MlyValue.IMPLIES IMPLIES1, _, _)) :: ( _, ( MlyValue.formula 
formula1, formula1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  (formula as formula1) = formula1 ()
 val  (IMPLIES as IMPLIES1) = IMPLIES1 ()
 val  formula2 = formula2 ()
 in ( formula1^", <formula>, "^IMPLIES^", "^formula2^", <formula>")

end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 6, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: ( _,
 ( MlyValue.AND AND1, _, _)) :: ( _, ( MlyValue.formula formula1, 
formula1left, _)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  (formula as formula1) = formula1 ()
 val  (AND as AND1) = AND1 ()
 val  formula2 = formula2 ()
 in (formula1^", <formula>, \""^AND^"\", AND, "^formula2^", <formula>"
)
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: ( _,
 ( MlyValue.OR OR1, _, _)) :: ( _, ( MlyValue.formula formula1, 
formula1left, _)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  (formula as formula1) = formula1 ()
 val  (OR as OR1) = OR1 ()
 val  formula2 = formula2 ()
 in (formula1^", <formula>, \""^OR^"\", OR, "^formula2^", <formula>")

end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 8, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: ( _,
 ( MlyValue.XOR XOR1, _, _)) :: ( _, ( MlyValue.formula formula1, 
formula1left, _)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  (formula as formula1) = formula1 ()
 val  (XOR as XOR1) = XOR1 ()
 val  formula2 = formula2 ()
 in (formula1^", <formula>, \""^XOR^"\", XOR, "^formula2^", <formula>"
)
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: ( _,
 ( MlyValue.EQUALS EQUALS1, _, _)) :: ( _, ( MlyValue.formula formula1
, formula1left, _)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  (formula as formula1) = formula1 ()
 val  (EQUALS as EQUALS1) = EQUALS1 ()
 val  formula2 = formula2 ()
 in (
formula1^", <formula>, \""^EQUALS^"\", EQUALS, "^formula2^", <formula>"
)
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: ( _
, ( MlyValue.NOT NOT1, NOT1left, _)) :: rest671)) => let val  result =
 MlyValue.formula (fn _ => let val  (NOT as NOT1) = NOT1 ()
 val  (formula as formula1) = formula1 ()
 in ( "\""^NOT^"\", NOT, "^formula^", <formula>")
end)
 in ( LrTable.NT 2, ( result, NOT1left, formula1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.RPAREN RPAREN1, _, RPAREN1right)) :: ( _, (
 MlyValue.formula formula1, _, _)) :: ( _, ( MlyValue.LPAREN LPAREN1, 
LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  (LPAREN as LPAREN1) = LPAREN1 ()
 val  (formula as formula1) = formula1 ()
 val  (RPAREN as RPAREN1) = RPAREN1 ()
 in ("\""^LPAREN^"\", LPAREN, "^formula^", RPAREN, "^RPAREN^"\"")
end)
 in ( LrTable.NT 2, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.formula (fn _ => let val  (
CONST as CONST1) = CONST1 ()
 in ("\""^CONST^"\", <CONST>")
end)
 in ( LrTable.NT 2, ( result, CONST1left, CONST1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.formula (fn _ => let val  (ID as ID1) = 
ID1 ()
 in ("\""^ID^"\", <ID>")
end)
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
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
