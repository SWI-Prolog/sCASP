:- use_package(assertions).

:- doc(filetype, documentation).

:- doc(author, "Joaquin Arias").

:- doc(title,"Usage").

:- doc(module, "

@section{Help}

@begin{verbatim}
Usage: scasp [options] InputFile(s)
@end{verbatim}

s(CASP) computes stable models of ungrounded normal logic programs.
Command-line switches are case-sensitive!

@begin{verbatim}
 General Options:

  -h, -?, --help        Print this help message and terminate.
  -i, --interactive     Run in user / interactive mode.
  -a, --auto            Run in automatic mode (no user interaction).
  -sN, -nN              Compute N answer sets, where N >= 0. 0 for all.
  -v, --verbose         Enable verbose progress messages.
  -j, --justification   Print proof tree for each solution.
  -d0                   Print the program translated (with duals and nmr_check).
@end{verbatim}


@section{Examples of use}

To obtain one model of the program (i.e. test.pl)
@begin{verbatim}
   $ ./scasp test.pl
   Answer: 1
   { q(?Var6), not p(?Var6) }
@end{verbatim}

To obtain all the models (answers) of test.pl
@begin{verbatim}
   $ ./scasp -s0 test.pl
@end{verbatim}

To obtain 5 answers of test.pl
@begin{verbatim}
   $ ./scasp -s5 test.pl
@end{verbatim}

To print the 'translation' of the code (with duals predicates and
check-rules)
@begin{verbatim}
   $ ./scasp -d0 test.pl
@end{verbatim}

To use scasp with its iterative mode:
@begin{verbatim}
   $ ./scasp -i test.pl
   ?- p(X).
   { p(?Var6), not q(?Var6) } ? ;

   false.
   ?- q(X).
   { q(?Var6), not p(?Var6) } ? ;

   false.
   ?- halt.
@end{verbatim}

The example program @file{test.pl} (include the query in order to be use without
iterative mode) is:

@begin{verbatim}
   p(X) :- not q(X).
   q(X) :- not p(X).

   ?- q(X).
@end{verbatim}

").
