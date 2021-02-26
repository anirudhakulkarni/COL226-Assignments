Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;
Control.Print.printLength := 1000;
fun test a=print(a)
val firstlinefields= 3
val delim = #","
exception UnevenFields of string

(* fun read(filename) =
    let val file = TextIO.openIn filename
        val textread = TextIO.inputAll file
        val _ = TextIO.closeIn file
    (* in String.fields (fn c => c = #"\n") textread *)
    in textread
    end
val textarea=read("him.csv")

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
val liststring1=lstr(textarea,0,0) *)
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
fun firsterror (liststring:string list)=
let
(* val ff=List.nth(liststring , 0)
  (* val fff=Int.toString ff *)
  val ffff=test ff *)
in
  

    if List.length liststring = 0 then 1
    else
        if nf(List.nth(liststring , 0),0) = firstlinefields
            then 1 + firsterror(List.drop (liststring,1))
        else
            1
            end
val liststring1=["Na;me,Nickname;,;Classic Dialogue,Children,Occupation",
   "Ted,Teddy Westside,\"\"\"I think I am in love with you.\"\"\",\"Penny,Luke\",\"Architect\nProfessor\"",
   "Marshall,Big Fudge,\"\"\"I can walk that far!\"\"\",\"Marvin,Daisy\",\"Lawyer\nJudge\"",
   "Barney,Swarley,\"\"\"Suit Up!\"\"\",Ellie,P.L.E.A.S.E",
   "Robin,Sparkles,\"\"\"Let's Go To The Mall\"\"\",,\"Teenage Popstar\nReporter\"",
   "Lily,Lilypad,\"\"\"Where's the poop?\"\"\",\"Marvin,Daisy\",\"Kindergarden Teacher\nArt Consultant\""]

fun isCorrect(liststring)=
    let
       val errorline = firsterror liststring1
val t2= Int.toString(errorline)
val tq = print t2
    in
      
    if errorline = List.length liststring + 1 then
        1
    else
        raise UnevenFields(errorline)
    end
    handle
        UnevenFields(errorline) => print "this shit works\n"