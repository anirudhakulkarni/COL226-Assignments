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