(* Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;
Control.Print.printLength := 1000; *)
exception UnevenFields of string
exception emptyInputFile
fun write(filename:string , texttowrite:string) =
    let val file = TextIO.openOut(filename)
        val _ = TextIO.output(file , texttowrite)
    in TextIO.closeOut(file)
    end
fun read(filename) =
    let val file = TextIO.openIn filename
        val textread = TextIO.inputAll file
        val _ = TextIO.closeIn file
    in textread
    end
fun replacedelim(toreplace , delim1 , delim2) = 
    case toreplace of
       "" => raise emptyInputFile
     | _ => 
    let
        val quotes = 0
        val delim = delim1
        fun increment (a) = if a = "\"" then 1 else 0
        fun lstr(text, curr, quotes)=
            case String.size text <2 of
            true => []
            | false =>
            let
                (* val t2=String.substring(text,curr,1)
                val tt=String.size text
                val t3=Int.toString(tt)
                val t3=t3^t2
            val t = print t3 *)
            in
                if String.sub(text,curr)= #"\n"  then 
                    if quotes mod 2 = 0 then
                        [String.substring(text,0,curr)]@ lstr(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes)
                    else
                        lstr(text,curr+1,quotes)
                else    
                    if String.sub(text,curr)= #"\""  then
                        lstr(text,curr+1,quotes+1)
                    else
                        lstr(text,curr+1,quotes)
            end
        

        fun doubled(text, curr, quotes,isStart)=
            case curr > String.size (text)-1 of
            true => ""
            | false =>
            let
                val t=Int.toString(curr)
                (*val t2=Int.toString(String.size(text)) *)
                val temp2=String.sub(text,curr)
                val temp1=print(str(temp2)^t^Int.toString(String.size(text)))
            
            in
            if isStart=true then
               if String.sub(text,curr)= #"\"" then
                    doubled(text,0,quotes,false)
               else 
                    "\""^doubled(String.substring(text,curr,String.size(text) -curr),0,quotes,false)
                (* "\""^String.substring(text,curr,1)^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes,false) *)
            else
            
            if String.sub(text,curr)= delim  then 
                if quotes mod 2 = 0 then
                    if curr>0 then
                        if String.sub(text,curr-1)= #"\"" then
                            if String.sub(text,curr+1)= #"\"" then
                                String.substring(text,0,curr)^str(delim2)^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes,false)
                            else
                                if String.sub(text,curr+1)= delim orelse String.sub(text,curr+1)= #"\"" then
                                    String.substring(text,0,curr)^str(delim2)^"\"\""^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes,false)
                                else
                                    String.substring(text,0,curr)^str(delim2)^"\""^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes,false)
                        else
                            if String.sub(text,curr+1)= #"\"" then
                                String.substring(text,0,curr)^"\""^str(delim2)^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes,false)
                            else
                                if String.sub(text,curr+1)= delim then
                                    String.substring(text,0,curr)^"\""^str(delim2)^"\"\""^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes,false)
                                else
                                    String.substring(text,0,curr)^"\""^str(delim2)^"\""^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes,false)
                    else
                        if String.sub(text,curr+1)= #"\"" then
                            str(delim2)^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes,false)
                        else
               (*here*)             if String.sub(text,curr+1)= delim then
                                str(delim2)^"\"\""^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes,false)
                            else
                                str(delim2)^"\""^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes,false)
                else
                    doubled(text,curr+1,quotes,false)
            else    
                if String.sub(text,curr)= #"\""  then
                    (* if String.sub(text,curr-1)= #"\n" then *)
                    doubled(text,curr+1,quotes+1,false)
                else
                    if String.sub(text,curr)= #"\n" then
                        if quotes mod 2 = 0 then
                            if curr<String.size(text)-1 then
                                if String.sub(text,curr-1)= #"\"" then
                                    if String.sub(text,curr+1)= #"\"" then
                                        (* String.substring(text,0,curr+1)^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes) *)
                                        doubled(text,curr+1,quotes,false)
                                    else
                                        String.substring(text,0,curr+1)^"\""^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes,false)
                                else
                                    if String.sub(text,curr+1)= #"\"" then
                                        String.substring(text,0,curr)^"\"\n"^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes,false)
                                    else
                                        String.substring(text,0,curr)^"\"\n\""^doubled(String.substring(text,curr+1,String.size(text) -curr-1),0,quotes,false)
                            else
                                if curr=0 then
                                    "\"\n"
                                else
                                    if String.sub(text,curr-1)= #"\"" then
                                        text
                                    else
                                        String.substring(text,0,curr)^"\"\n"
                                    
                                    
                                    
                                    (* String.substring(text,0,curr)^"\""^doubled(String.substring(text,curr+1,String.size(text) -curr-2),0,quotes) *)
                                    
                        else
                            doubled(text,curr+1,quotes,false)
                    else
                        doubled(text,curr+1,quotes,false)

            end
        val liststring1 = lstr(toreplace,0,0)
        fun nf(text,quotes)=
            case text of
                "" => 0
                | _ =>
                let
                (* val t = test text *)
                in
                    if String.sub(text,0)=delim then
                        if quotes mod 2 = 0 then
                            1+nf(String.substring(text,1,String.size text -1),quotes)
                        else
                            nf(String.substring(text,1,String.size text -1),quotes)
                    else
                        if String.sub(text,0)= #"\"" then
                            nf(String.substring(text,1,String.size text -1),quotes+1)
                        else
                            nf(String.substring(text,1,String.size text -1),quotes)

                end
        val firstlinefields = nf(List.nth(liststring1 , 0),0)
        fun firsterror (liststring:string list)=
            let
            (* val ff=List.nth(liststring , 0)
            (* val fff=Int.toString ff *)
            val ffff=test ff *)
            in
                if List.length liststring = 0 then [1,0]
                else (*Highly inefficient think better if time permits*)
                    if nf(List.nth(liststring , 0),0) = firstlinefields
                        then [1 + List.hd(firsterror(List.drop (liststring,1))),List.nth(firsterror(List.drop (liststring,1)),1)]
                    else
                        [1,nf(List.nth(liststring,0),0)]
            end
        fun isCorrect(liststring)=
            let
                val errorline = firsterror liststring1
                (* val t2= Int.toString(errorline)
                val tq = print t2 *)
            in    
                if List.hd errorline = List.length liststring + 1 then
                    print("")
                else
                    (* raise UnevenFields(errorline) *)
                   ( print("Expected: "^ Int.toString (firstlinefields+1)^" fields, Present: "^Int.toString(List.nth(errorline,1)+1)^" fields on Line "^Int.toString(List.hd(errorline))^"\n") ;raise UnevenFields ("Expected: "^ Int.toString (firstlinefields+1)^" fields, Present: "^Int.toString(List.nth(errorline,1)+1)^" fields on Line "^Int.toString(List.hd(errorline))))
                     
                    (* "Not Correct "^Int.toString(errorline) *)
            end
        val correct = isCorrect(liststring1)
    in
        (* magic (toreplace,quotes) *)
        doubled (toreplace,0,0,true)
    end
fun convertDelimiters(infilename , delim1 , outfilename , delim2)= 
    let val textread = read(infilename)
        val replacedText = replacedelim(textread , delim1 , delim2)
        (* val wrote = write(outfilename , replacedText) *)
    in
        write(outfilename , replacedText)
    end
    (* handle
        (* emptyInputFile(a)=> print(a) |  *)
        UnevenFields(errorline)=> print(errorline) *)
    
fun csv2tsv(infilename , outfilename)=
    convertDelimiters(infilename , #"," , outfilename , #"\t")
fun tsv2csv(infilename , outfilename)=
    convertDelimiters(infilename , #"\t" , outfilename , #",")



(* fun magic (toreplace , quotes) =
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
            end *)
            