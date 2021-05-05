structure BoolLrVals = BoolLrValsFun(structure Token = LrParser.Token);
structure BoolLex = BoolLexFun(structure Tokens = BoolLrVals.Tokens);
structure BoolParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = BoolLrVals.ParserData
     	       structure Lex = BoolLex);
     
fun invoke lexstream =
    	     	let fun print_error (s,lineno,col) =
		    	TextIO.output(TextIO.stdErr, "Syntax Error:" ^ (Int.toString lineno)^":"^(Int.toString col) ^ ":" ^ s ^ "\n")
			
		in
		    BoolParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  BoolParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = BoolLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = BoolParser.Stream.get lexer
    in
        if BoolParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o stringToLexer

fun parseFile filename = 
    let val file = TextIO.openIn filename
        val textread = TextIO.inputAll file
        val _ = TextIO.closeIn file
		(* val _ = print textread *)
		(* val _ = print("[ ") *)
		val _ = print("Lexer output:\n")
		val parsedData = parseString textread
		(* val _ = print("]") *)
		(* val _ = print("\n") *)
		val _ = print("Parser output:\n")
		(* val _ = print parsedData *)
		(* val _ = print("\n") *)
    in 
		parsedData
    end
(* val args = CommandLine.arguments()
(* val name =hd args *)
val _ = parseFile "test1.txt"
val _ = OS.Process.exit(OS.Process.success) *)