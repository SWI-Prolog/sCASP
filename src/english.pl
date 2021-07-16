:- module(casp_english,
          []).

:- multifile prolog:message//1.

prolog:message(scasp(Term)) -->
    message(Term).

message(no_input_files) -->
    [ 'No input file specified!' ].
