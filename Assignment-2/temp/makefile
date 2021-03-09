run:
	cmd /c @ml-yacc Bool.yacc
	cmd /c @ml-lex Bool.lex
	cmd /c @sml "loader.sml"
# run:
# 	cmd /c @ml-yacc Bool.yacc
# 	cmd /c @ml-lex Bool.lex
# 	cmd /c @mlton loader.sml
# 	./test 1 2 3	

.PHONY: run
ubu:
	ml-yacc Bool.yacc
	ml-lex Bool.lex
	sml "loader.sml"

