structure SimpLex=
   struct
    structure UserDeclarations =
      struct
datatype lexresult= DIV | EOF | EOS | ID of string | LPAREN |
                    NUM of int | PLUS | PRINT | RPAREN | SUB | TIMES | EQ
val linenum = ref 1
val error = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof = fn () => EOF
fun refinc x =  (x := !x + 1; !x)
  
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\016\018\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\016\003\003\003\003\003\003\003\015\014\013\012\003\011\003\010\
\\008\008\008\008\008\008\008\008\008\008\003\007\003\006\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003"
),
 (4, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (8, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\009\009\009\009\009\009\009\009\009\009\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (16, 
"\000\000\000\000\000\000\000\000\000\017\017\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 28)], trans = 0},
{fin = [(N 22),(N 28)], trans = 4},
{fin = [(N 22)], trans = 4},
{fin = [(N 12),(N 28)], trans = 0},
{fin = [(N 8),(N 28)], trans = 0},
{fin = [(N 15),(N 28)], trans = 8},
{fin = [(N 15)], trans = 8},
{fin = [(N 6),(N 28)], trans = 0},
{fin = [(N 24),(N 28)], trans = 0},
{fin = [(N 19),(N 28)], trans = 0},
{fin = [(N 26),(N 28)], trans = 0},
{fin = [(N 17),(N 28)], trans = 0},
{fin = [(N 10),(N 28)], trans = 0},
{fin = [(N 4),(N 28)], trans = 16},
{fin = [(N 4)], trans = 16},
{fin = [(N 1),(N 4)], trans = 16}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => (refinc linenum; lex())
| 10 => (LPAREN)
| 12 => (EQ)
| 15 => let val yytext=yymktext() in NUM  (List.foldr (fn(a,r)=>ord(a)-ord(#"0")+10*r) 0 (explode yytext)) end
| 17 => (RPAREN)
| 19 => (PLUS)
| 22 => let val yytext=yymktext() in if yytext="print" then PRINT else ID yytext end
| 24 => (SUB)
| 26 => (TIMES)
| 28 => let val yytext=yymktext() in error ("calc: ignoring bad character "^yytext); lex() end
| 4 => (lex())
| 6 => (DIV)
| 8 => (EOS)
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
