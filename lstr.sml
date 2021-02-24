Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;
Control.Print.printLength := 1000;
fun test a=print(a)
fun lstr(text, curr, quotes)=
    case String.size text <2 of
       true => []
     | false =>
     let
        (* val t2=String.substring(text,curr,1)
        val tt=String.size text
        val t3=Int.toString(tt)
        val t3=t3^t2
       val t = test t3 *)
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
fun read(filename) =
    let val file = TextIO.openIn filename
        val textread = TextIO.inputAll file
        val _ = TextIO.closeIn file
        val textread = lstr(textread,0,0)
        
    (* in String.fields (fn c => c = #"\n") textread *)
    in textread
    end
