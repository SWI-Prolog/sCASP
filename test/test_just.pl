:- module(test_just,
          [ test_just/0
          ]).
:- use_module('../prolog/scasp').
:- use_module('../prolog/scasp/human').
:- use_module(library(plunit)).
:- use_module(library(debug)).

:- meta_predicate
    human(0, +).

test_just :-
    run_tests([ scasp_just_1
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


		 /*******************************
		 *            HARNASS		*
		 *******************************/

human(G, Expected) :-
    once(scasp(G, [tree(Tree)])),
    with_output_to(string(String0),
                   human_justification_tree(Tree, [])),
    split_string(String0, "\n", " \n\u220e", Strings),
    atomics_to_string(Strings, " ", String),
    assertion(Expected == String).
