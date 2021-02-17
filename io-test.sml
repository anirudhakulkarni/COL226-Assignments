fun test (filename, mySubstr) =
    let val instr = TextIO.openIn filename
        fun loop () = case TextIO.inputLine instr of
                          SOME line => if String.isSubstring mySubstr line
                                       then line :: loop () else loop ()
                        | NONE => []
        val lines = loop ()
        val _ = TextIO.closeIn instr
    in lines end