:- module(test_just,
          [ test_just/0
          ]).
:- use_module('../../prolog/scasp').
:- use_module('../../prolog/scasp/human').
:- use_module(library(plunit)).
:- use_module(library(debug)).

:- meta_predicate
    human(0, +).

test_just :-
    run_tests([ scasp_just_1,
                scasp_describe
              ]).

:- begin_tests(scasp_just_1).
:- dynamic q/0.

p.
pq :- not q.
p(X) :- X #> 3.

test(p) :-
    human(p, "p holds").
test(p1) :-
    human(p(_X), "p holds for any number greater than 3").
test(np1) :-
    human(not p(_X), "there is no evidence that p holds for \c
                      any number less than or equal to 3").
test(pq) :-
    human(pq, "pq holds, because there is no evidence that q holds").

:- end_tests(scasp_just_1).

%   `:- describe` is `:- pred`, spelled as library(sscasp) does.  A
%   placeholder names a variable, which may be underscored.

:- begin_tests(scasp_describe).

sex(male).
patient(S) :- sex(S).

:- describe sex(_S) :: "the patient is {{S}}".
:- describe patient(_S) :: "a patient who is @(S)".

test(described) :-
    human(patient(_), "a patient who is male, because the patient is male").

:- end_tests(scasp_describe).


		 /*******************************
		 *            HARNASS		*
		 *******************************/

human(G, Expected) :-
    once(scasp(G, [tree(Tree)])),
    G = M:_,
    with_output_to(string(String0),
                   human_justification_tree(M:Tree, [])),
    split_string(String0, "\n", " \n\u220e", Strings),
    atomics_to_string(Strings, " ", String),
    assertion(Expected == String).
