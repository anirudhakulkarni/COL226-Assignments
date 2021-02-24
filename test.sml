Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;
Control.Print.printLength := 1000;
fun write(filename:string , texttowrite:string) =
    let val file = TextIO.openOut(filename)
        val _ = TextIO.output(file , texttowrite)
    in TextIO.closeOut(file)
    end

fun read(filename) =
    let val file = TextIO.openIn filename
        val textread = TextIO.inputAll file
        val _ = TextIO.closeIn file
    (* in String.fields (fn c => c = #"\n") textread *)
    in textread
    end

fun replacedelim(toreplace , delim1 , delim2) = 
    let
        val quotes = 0
        (* fun isqote char=
            if char = #"\"" then true else false *)
        fun increment a = 
            let
              (* val fuck =print(a^"FUCK")  *)
            in
              if a = "\"" then 1 else 0
            end
        
        fun magic (toreplace , quotes) =
            case toreplace of
                "" => ""
                | _ =>
            let
                val firstchar = String.substring(toreplace,0,1)
                val secondpart = String.substring(toreplace,1,String.size toreplace-1)
                (* val f = print(firstchar^"\n") *)
                val isquote = increment firstchar
                val abab=print(Int.toString(quotes)^"\n")
                
                
            in
                if firstchar = String.str(delim1) then 
                    if quotes mod 2 = 1 then 
                        firstchar^ magic (secondpart, quotes+isquote)       
                    else (String.str(delim2)^ magic (secondpart,quotes+isquote)) else
                firstchar^ magic (secondpart,quotes+isquote)
            end
            
    in
        magic (toreplace,quotes)
    end
    
fun sanityCheck textread=
    let
      bindings
    in
      body
    end
 
fun convertDelimiters(infilename , delim1 , outfilename , delim2)= 
    let val textread = read(infilename)
        val sanity = sanityCheck textread
        val replacedtext = replacedelim(textread , delim1 , delim2)
    in 
        write(outfilename , replacedtext)
    end
fun csv2tsv(infilename , outfilename)=
    convertDelimiters(infilename , #"," , outfilename , #";")
fun tsv2csv(infilename , outfilename)=
    convertDelimiters(infilename , #"t" , outfilename , #",")
fun replaceNewlines(toreplace , newline1 , newline2)="0"


(* fun convertNewlines(infilename , newline1 , outfilename , newline2)=
    let val textread = read(infilename)
        val replacedtext = replaceNewlines(textread , newline1 , newline2)
    in 
        write(outfilename , replacedtext)
    end

fun unix2dos(infilename , outfilename)=
    convertNewlines(infilename , "\r\n" , outfilename , "\n")
fun dos2unix(infilename , outfilename)=
    convertNewlines(infilename , "\n" , outfilename , "\r\n") *)

