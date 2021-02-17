fun read_file (infile:string) =
   let 
    	val instream = TextIO.openIn infile
	fun loop instream =
		case TextIO.inputLine instream of
	             SOME line => line :: loop instream
    	    	   | NONE      => []
    in
	 loop instream before TextIO.closeIn instream
    end

fun write_file (outfile:string, outstream:string) =
    TextIO.openOut outstream
(* fun convertDelimiters(infilename, delim1,outfilename, delim2 ) = 
fun csv2tsv(infilename, outfilename)=
fun tsv2csv(infilename,outfilename)=

fun converNewlines(infilename, newline1,outfilename,newline2)=

fun unix2dos(infilename,outfilename)=
fun dos2unix(infilename,outfilename)= *)
