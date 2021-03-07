test:
	cmd /c @ml-yacc Bool.yacc
	cmd /c @ml-lex Bool.lex
	cmd /c @sml "loader.sml"
run:
	cmd /c @ml-yacc Bool.yacc
	cmd /c @ml-lex Bool.lex
	cmd /c @mlton loader.sml
	./test 1 2 3	

.PHONY: test
arg:
	ml-lex test.lex
	mv test.lex.sml test.sml
	mlton test.sml
	./test 1 2 3