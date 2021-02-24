fun read(filename) =
    let val file = TextIO.openIn filename
        val textread = TextIO.inputAll file
        val _ = TextIO.closeIn file
    (* in String.fields (fn c => c = #"\n") textread *)
    in textread
    end
fun isqote char=
	if char = #"\"" then true
	else false
fun magic toreplace =
	case toreplace of
		   "" => ""
		 | _ =>
	let
		val firstchar = String.substring(toreplace,0,1)
		val secondpart = String.substring(toreplace,1,String.size toreplace-1)
		val f = print(firstchar^"\n")
	in
		if firstchar = "\"" then String.str(#"b")^ magic secondpart else firstchar^ magic secondpart
	end