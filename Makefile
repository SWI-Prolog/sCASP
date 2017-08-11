

compile_tclp_asp:
	ciaoc -x -o hanoi tclp_asp.pl


clean:
	@ciao clean-tree .
	@rm *~
