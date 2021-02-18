fun write(filename:string, texttowrite:string) =
    let val file = TextIO.openOut(filename)
        val _ = TextIO.output(file, texttowrite)
    in TextIO.closeOut(file)
    end

(* Read a nice poem from a file into a list of strings *)
fun read(filename) =
    let val file = TextIO.openIn filename
        val textread = TextIO.inputAll file
        val _ = TextIO.closeIn file
    (* in String.tokens (fn c => c = #"\n") poem *)
    in textread
    end
