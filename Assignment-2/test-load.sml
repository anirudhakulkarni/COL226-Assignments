structure BoolLrVals =
  BoolLrValsFun(structure Token = LrParser.Token)

structure BoolLex =
  BoolLexFun(structure Tokens = BoolLrVals.Tokens);

structure BoolParser =
  Join(structure LrParser = LrParser
       structure ParserData = BoolLrVals.ParserData
       structure Lex = BoolLex)
fun invoke lexstream =
    let fun print_error (s,i:int,_) =
            TextIO.output(TextIO.stdOut,
                          "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
     in BoolParser.parse(0,lexstream,print_error,())
    end
fun parse () = 
    let val lexer = BoolParser.makeLexer
                      (fn _ => TextIO.inputLine TextIO.stdIn)
        val dummyEOF = BoolLrVals.Tokens.EOF(0,0)
        val dummySEMI = BoolLrVals.Tokens.SEMI(0,0)
        fun loop lexer =
            let val (result,lexer) = invoke lexer
                val (nextToken,lexer) = BoolParser.Stream.get lexer
             in case result
                  of SOME r =>
                      TextIO.output(TextIO.stdOut,
                             "result = " ^ (Int.toString r) ^ "\n")
                   | NONE => ();
                if BoolParser.sameToken(nextToken,dummyEOF) then ()
                else loop lexer
            end
     in loop lexer
    end
