:- module(under_test,
          [ library_under_test/1        % -Library
          ]).

/** <module> The implementation the shared suites test

The suites in `test/` itself state what s(CASP) *means*, and they are
written against `scasp/1,2` and the `scasp_*` API, which both libraries
export under the same names.  So a suite loads this module instead of
either library, and the Prolog flag `sscasp` says which one it
gets:

    swipl -Dsscasp -g test_all -t halt test/test_all.pl

The default is `scasp`, the old implementation.  `make check` runs both.

Where one library exports what the other keeps internal, this is where
the two are made to agree: `library(sscasp)` exports
scasp_query_clauses/2, and `library(scasp)` has it only in
`scasp/dyncall.pl`.

Relative paths rather than library(...), because `library(scasp)`
resolves through the installed pack, which is a different checkout.
*/

%   library(scasp) is the switch: under `-Dsscasp` it re-exports
%   library(sscasp) (see prolog/scasp.pl) -- all but
%   scasp_query_clauses/2, which the old library(scasp) does not export
%   either, so it comes from the implementation under test directly.

:- reexport('../prolog/scasp').
:- if(\+ current_prolog_flag(sscasp, true)).
:- reexport('../prolog/scasp/dyncall', [scasp_query_clauses/2]).
:- else.
:- reexport('../prolog/sscasp', [scasp_query_clauses/2]).
:- endif.

%!  library_under_test(-Library) is det.
%
%   Library is `sscasp` if the flag `sscasp` is `true`, and `scasp`
%   otherwise.

library_under_test(Library) :-
    (   current_prolog_flag(sscasp, true)
    ->  Library = sscasp
    ;   Library = scasp
    ).
