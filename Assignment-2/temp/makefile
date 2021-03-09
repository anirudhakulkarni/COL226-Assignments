run:
	ml-yacc Bool.yacc
	ml-lex Bool.lex
	mlton "a2.mlb"
win:
	cmd /c @ml-yacc Bool.yacc
	cmd /c @ml-lex Bool.lex
	cmd /c @sml "loader.sml"
.PHONY: run
