

compile_tclp_asp:
	ciaoc -x -o tclp_asp tclp_asp.pl

main:
	ciaoc -x -o parser_tasp parser_tasp.pl

clean:
	@ciao clean-tree .
	@rm *~
