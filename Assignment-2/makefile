test:
	@ml-lex test.lex
	@mv test.lex.sml test.sml
	@mlton test.sml
	@./test
.PHONY: test
arg:
	ml-lex test.lex
	mv test.lex.sml test.sml
	mlton test.sml
	./test 1 2 3