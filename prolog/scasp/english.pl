:- module(casp_english,
          []).

:- multifile
    prolog:message//1,
    prolog:error_message//1.

prolog:message(scasp(Term)) -->
    scasp_message(Term).
prolog:message(sasp(Term)) -->
    sasp_message(Term).

prolog:error_message(sasp(Term)) -->
    sasp_error(Term).
prolog:error_message(existence_error(scasp_query, scasp_main)) -->
    [ 'sCASP: the program does not contain a query'-[] ].
prolog:error_message(existence_error(scasp_query, M)) -->
    [ 'sCASP: no query in module ~p'-[M] ].


		 /*******************************
		 *            CASP		*
		 *******************************/

scasp_message(no_input_files) -->
    [ 'No input file specified!' ].
scasp_message(undefined_operator(Op)) -->
    [ 'clp operator ~p not defined'-[Op] ].


		 /*******************************
		 *            SASP		*
		 *******************************/

sasp_message(illegal_tokens) -->
    [ 'Failed to tokenize input'-[] ].
sasp_message(illegal_tokens(Errors)) -->
    [ '~D illegal tokens in input'-[Errors] ].
sasp_message(syntax_error(What)) -->
    syntax_error(What).

syntax_error(unexpected_eof) -->
    ['Unexpected end of file'-[] ].
syntax_error(lexical(Char, (File, Line, Col))) -->
    ['~w:~d:~d Illegal character: ~w'-[File, Line, Col, Char] ].

sasp_error(syntax(invalid_program)) -->
    ['One or more errors occurred during parsing'-[]].
