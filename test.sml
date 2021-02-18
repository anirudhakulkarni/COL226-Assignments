fun write(filename:string, texttowrite:string) =
    let val file = TextIO.openOut(filename)
        val _ = TextIO.output(file, texttowrite)
    in TextIO.closeOut(file)
    end

fun read(filename) =
    let val file = TextIO.openIn filename
        val textread = TextIO.inputAll file
        val _ = TextIO.closeIn file
    (* in String.tokens (fn c => c = #"\n") poem *)
    in textread
    end

fun replacedelim(toreplace, delim1,delim2) = 
    toreplace

fun convertDelimiters(infilename, delim1,outfilename,delim2)= 
    let val textread = read(infilename)
        val replacedtext = replacedelim(textread, delim1, delim2)
    in 
        write(outfilename, replacedtext)
    end
fun csv2tsv(infilename,outfilename)=
    convertDelimiters(infilename,#",",outfilename,#"t")
fun tsv2csv(infilename,outfilename)=
    convertDelimiters(infilename,#"t",outfilename,#",")
fun replaceNewlines(toreplace,newline1,newline2)=


fun convertNewlines(infilename, newline1, outfilename, newline2)=
    let val textread = read(infilename)
        val replacedtext = replaceNewlines(textread, newline1, newline2)
    in 
        write(outfilename, replacedtext)
    end

fun unix2dos(infilename,outfilename)=
    convertNewlines(infilename,"\r\n",outfilename,"\n")
fun dos2unix(infilename,outfilename)=
    convertNewlines(infilename,"\n",outfilename,"\r\n")
