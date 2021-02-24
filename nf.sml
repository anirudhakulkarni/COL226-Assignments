Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;
Control.Print.printLength := 1000;
val delim = #","
fun test a=print(a)
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
fun read(filename) =
    let val file = TextIO.openIn filename
        val textread = TextIO.inputAll file
        val _ = TextIO.closeIn file
        val f = nf(textread,0)
    (* in String.fields (fn c => c = #"\n") textread *)
    in f
    end
