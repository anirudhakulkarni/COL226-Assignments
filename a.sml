Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;
Control.Print.printLength := 1000;
fun read(filename) =
    let val file = TextIO.openIn filename
        val textread = TextIO.inputAll file
        val _ = TextIO.closeIn file
    (* in String.fields (fn c => c = #"\n") textread *)
    in textread
    end
val delim = #","
val delim2= #";"
fun test a=print(a)
fun doubled(text, curr, quotes)=
    case curr > String.size (text)-1 of
    true => ""
    | false =>
    let
        (* val t=Int.toString(curr)
        val t2=Int.toString(String.size(text))
        val temp=test (t^t2^"\n")
        val temp1=test(text) *)
      
    in
      
    if String.sub(text,curr)= delim  then 
        if quotes mod 2 = 0 then
            if curr>0 then
                if String.sub(text,curr-1)= #"\"" then
                    if String.sub(text,curr+1)= #"\"" then
                        String.substring(text,0,curr)^Char.toString(delim2)^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes)
                    else
                        String.substring(text,0,curr)^Char.toString(delim2)^"\""^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes)
                else
                    if String.sub(text,curr+1)= #"\"" then
                        String.substring(text,0,curr)^"\""^Char.toString(delim2)^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes)
                    else
                        String.substring(text,0,curr)^"\""^Char.toString(delim2)^"\""^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes)
            else
                if String.sub(text,curr+1)= #"\"" then
                    String.substring(text,0,curr)^"\""^Char.toString(delim2)^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes)
                else
                    String.substring(text,0,curr)^"\""^Char.toString(delim2)^"\""^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes)
        else
            doubled(text,curr+1,quotes)
    else    
        if String.sub(text,curr)= #"\""  then
            doubled(text,curr+1,quotes+1)
        else
            if String.sub(text,curr)= #"\n" then
                if quotes mod 2 = 0 then
                    if curr<String.size(text)-1 then
                        if String.sub(text,curr-1)= #"\"" then
                            if String.sub(text,curr+1)= #"\"" then
                                String.substring(text,0,curr+2)^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes)
                            else
                                String.substring(text,0,curr+1)^"\""^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes)
                        else
                            if String.sub(text,curr+1)= #"\"" then
                                String.substring(text,0,curr)^"\"\n"^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes)
                            else
                                String.substring(text,0,curr)^"\"\n\""^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes)
                    else
                        if String.sub(text,curr-1)= #"\"" then
                            text
                        else
                            String.substring(text,0,curr)^"\"\n\""
                            (* String.substring(text,0,curr)^"\""^doubled(String.substring(text,curr+1,String.size(text) -curr-2),0,quotes) *)
                            
                else
                    doubled(text,curr+1,quotes)
            else
                doubled(text,curr+1,quotes)

                    end

fun write(filename:string , texttowrite:string) =
    let val file = TextIO.openOut(filename)
        val _ = TextIO.output(file , texttowrite)
    in TextIO.closeOut(file)
    end


fun convertDelimiters(infilename , delim1 , outfilename , delim2)= 
    let val textread = read(infilename)
        (* val sanity = sanityCheck textread *)
        val replacedText = doubled(textread , 0 , 0)
        val replacedText = "\""^replacedText
    in 
        write(outfilename , replacedText)
    end
    
fun csv2tsv(infilename , outfilename)=
    convertDelimiters(infilename , #"," , outfilename , #";")
fun tsv2csv(infilename , outfilename)=
    convertDelimiters(infilename , #"t" , outfilename , #",")
