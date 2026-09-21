:- module(test_prolog_calls,
          [ test_prolog_calls/0
          ]).
:- use_module('../prolog/scasp').
:- use_module(scasp_plunit).
:- use_module(library(plunit)).
:- use_module(library(lists), [memberchk/2, member/2]).

/** <module> Calling Prolog from s(CASP)

`:- prolog p/2.` says a predicate is to be run by Prolog rather than
compiled into the s(CASP) program.  Two modes:

  - `justified` (the default) runs it under the meta-interpreter in
    prolog.pl, which extends the justification tree and understands
    `,`, `;`, `(If->Then;Else)`, `\\+`, `!` and `=>` rules;
  - `opaque` calls it natively and shows only the call.

Negating one is the interesting part.  A ground goal uses Prolog `\\+`,
which is safe because it cannot flounder.  A non-ground goal is answered
constructively instead: collect every solution, then constrain the
variables to differ from all of them - the same answer shape as negating
a predicate defined by facts.
*/

test_prolog_calls :-
    run_tests([ prolog_basic,
                prolog_opaque,
                prolog_control,
                prolog_negation
              ]).

		 /*******************************
		 *           BASIC		*
		 *******************************/

%   double/2 is Prolog arithmetic, which s(CASP) has no way to compile.
%   twice/2 is an s(CASP) rule that calls it.

:- begin_tests(prolog_basic).

:- prolog double/2.
double(X, Y) :- Y is X*2.

twice(X, Y) :- double(X, Y).

test(the_prolog_goal_is_called) :-
    scasp(twice(2, 4)).
test(and_computes_its_answer, all(Y == [6])) :-
    scasp(twice(3, Y)).
test(a_wrong_answer_fails, fail) :-
    scasp(twice(2, 5)).

test(the_call_appears_in_the_justification) :-
    scasp(twice(2, 4), [tree(Tree)]),
    proof_literals(Tree, Literals),
    assertion(memberchk(plain-twice(2, 4), Literals)).

:- end_tests(prolog_basic).

		 /*******************************
		 *           OPAQUE		*
		 *******************************/

%   The same predicate declared opaque: called natively, and only the
%   call itself is recorded, not how it was proved.

:- begin_tests(prolog_opaque).

:- prolog triple/2 as opaque.
triple(X, Y) :- Y is X*3.

thrice(X, Y) :- triple(X, Y).

test(the_opaque_goal_is_called, all(Y == [9])) :-
    scasp(thrice(3, Y)).
test(a_wrong_answer_fails, fail) :-
    scasp(thrice(3, 10)).
test(the_justification_is_still_a_tree) :-
    scasp(thrice(3, 9), [tree(Tree)]),
    assertion(Tree = _-_).

:- end_tests(prolog_opaque).

		 /*******************************
		 *      CONTROL CONSTRUCTS	*
		 *******************************/

%   The meta-interpreter has a case for each of these, so each needs a
%   goal that reaches it.

:- begin_tests(prolog_control).

% Declared together: each `:- prolog` expands to a pr_prolog_predicate/2
% fact, and interleaving them with the clauses they describe makes those
% facts discontiguous.

:- prolog classify/2.
:- prolog either/1.
:- prolog first_only/1.
:- prolog absent/1.

classify(X, Sign) :-
    (   X > 0
    ->  Sign = positive
    ;   X < 0
    ->  Sign = negative
    ;   Sign = zero
    ).

either(X) :- ( X = a ; X = b ).

first_only(X) :- member(X, [1,2,3]), !.

absent(X) :- \+ member(X, [1,2,3]).

sign_of(X, S) :- classify(X, S).
one_of(X) :- either(X).
head(X) :- first_only(X).
missing(X) :- absent(X).

test(if_then_else_takes_the_then_branch, all(S == [positive])) :-
    scasp(sign_of(5, S)).
test(if_then_else_takes_a_later_branch, all(S == [negative])) :-
    scasp(sign_of(-5, S)).
test(if_then_else_takes_the_else_branch, all(S == [zero])) :-
    scasp(sign_of(0, S)).

test(disjunction_gives_both, all(X == [a, b])) :-
    scasp(one_of(X)).

test(cut_keeps_only_the_first, all(X == [1])) :-
    scasp(head(X)).

test(prolog_negation_inside_the_goal) :-
    scasp(missing(9)).
test(prolog_negation_inside_the_goal_fails, fail) :-
    scasp(missing(2)).

:- end_tests(prolog_control).

		 /*******************************
		 *      NEGATING THE CALL	*
		 *******************************/

%   colour/1 has three Prolog solutions.  `not likes(X)` with X bound is
%   Prolog's \+; with X unbound it must be constructive - the answer is
%   an X that differs from every solution, not a failure.

:- begin_tests(prolog_negation).

:- prolog colour/1.
colour(red).
colour(green).
colour(blue).

likes(X) :- colour(X).

test(ground_negation_holds_off_the_list) :-
    scasp(not likes(yellow)).
test(ground_negation_fails_on_the_list, fail) :-
    scasp(not likes(red)).

test(positive_query_enumerates, all(X == [red, green, blue])) :-
    scasp(likes(X)).

test(unbound_negation_is_constructive) :-
    scasp(not likes(X)),
    constrained_to(X, [yellow, purple], [red, green, blue]).

:- end_tests(prolog_negation).
