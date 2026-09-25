:- module(test_disequality,
          [ test_disequality/0
          ]).
:- use_module(under_test).
:- use_module(scasp_plunit).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(lists), [member/2]).

/** <module> Constructive disunification

Negating a predicate defined by facts cannot enumerate the complement -
it is infinite - so s(CASP) answers with the variable itself, carrying a
disequality constraint recording what it may not be.  That store is
`clp/disequality.pl`, and the user-facing form of it is the `∉`
operator: `X ∉ [a,b]` reads "X is not one of a, b".

The tests below say what each answer admits and excludes rather than
what it prints, because the constraint lives in the variable's
attributes rather than in the term.
*/

test_disequality :-
    run_tests([ diseq_over_facts,
                diseq_not_in,
                diseq_compound,
                diseq_accumulates
              ]).

		 /*******************************
		 *        NEGATING FACTS		*
		 *******************************/

%   colour(red).  colour(green).
%
%   `not colour(X)` holds of everything that is not red and not green,
%   which is one answer with X constrained, not two answers or none.

:- begin_tests(diseq_over_facts).

colour(red).
colour(green).

test(a_fact_holds) :-
    scasp(colour(red)).
test(a_non_fact_fails, fail) :-
    scasp(colour(blue)).
test(enumerates_the_facts, all(X == [red, green])) :-
    scasp(colour(X)).

test(ground_negation_holds_outside) :-
    scasp(not colour(blue)).
test(ground_negation_fails_inside, fail) :-
    scasp(not colour(red)).

test(the_answers_exclude_every_fact_and_admit_the_rest) :-
    answers_admit(scasp(not colour(X), []), X,
                  [red, green, blue, yellow, 1, f(a)], Admits),
    assertion(Admits == [blue, yellow, 1, f(a)]).

:- end_tests(diseq_over_facts).

		 /*******************************
		 *       THE NOT-IN OPERATOR	*
		 *******************************/

%   `X ∉ [a,b]` is the same constraint written directly.  Ground on the
%   left it is a membership test; unbound it constrains.
%
%   It works as a goal, but *not* inside an s(CASP) program: predicates.pl
%   does not list it in scasp_builtin/1, so dyncall.pl treats it as a user
%   predicate and tries to collect its clauses, which call ground/1 - a
%   compiled built-in it then refuses.  Both are pinned below.

:- begin_tests(diseq_not_in).

not_one_of(X) :- X ∉ [a, b].

test(a_value_outside_the_list_is_admitted) :-
    _X ∉ [a, b].
test(the_constraint_excludes_the_list) :-
    X ∉ [a, b],
    constrained_to(X, [c, d], [a, b]).
test(a_value_in_the_list_is_refused, fail) :-
    X = b,
    X ∉ [a, b].
test(an_earlier_binding_outside_the_list_is_kept, [nondet]) :-
    X = c,
    X ∉ [a, b].

% Using it in a program body is the part that does not work: predicates.pl
% does not list it in scasp_builtin/1, so it is collected as a user
% predicate and its call to ground/1 is then refused.  doc/bugs.md.

:- if(expect_semantics).

test(not_in_is_usable_in_a_program, [nondet]) :-
    scasp(not_one_of(c)).

:- else.

test(not_in_is_not_usable_in_a_program,
     [ throws(error(permission_error(scasp, procedure, system:ground/1), _)) ]) :-
    scasp(not_one_of(c)).

:- endif.

:- end_tests(diseq_not_in).

		 /*******************************
		 *        COMPOUND TERMS		*
		 *******************************/

%   Disequality over structure, not just atoms.  `not p(f(X))` must
%   exclude f(1) while leaving f(2) and any other shape available.

:- begin_tests(diseq_compound).

p(f(1)).
p(g(2)).

test(a_compound_fact_holds) :-
    scasp(p(f(1))).
test(a_different_argument_fails, fail) :-
    scasp(p(f(2))).
test(a_different_functor_fails, fail) :-
    scasp(p(h(1))).

test(ground_negation_of_a_compound) :-
    scasp(not p(f(2))).
test(ground_negation_fails_on_the_fact, fail) :-
    scasp(not p(f(1))).

test(negation_constrains_the_argument) :-
    scasp(not p(f(X))),
    constrained_to(X, [2, 3], [1]).

:- end_tests(diseq_compound).

		 /*******************************
		 *       ACCUMULATING		*
		 *******************************/

%   Two negations of the same variable must both hold of the answer, so
%   the constraints accumulate rather than replace one another.

:- begin_tests(diseq_accumulates).

a(1).
a(2).
b(2).
b(3).

neither(X) :- not a(X), not b(X).

test(a_value_in_neither_holds) :-
    scasp(neither(9)).
test(a_value_in_the_first_fails, fail) :-
    scasp(neither(1)).
test(a_value_in_the_second_fails, fail) :-
    scasp(neither(3)).
test(a_value_in_both_fails, fail) :-
    scasp(neither(2)).

test(the_answer_excludes_every_value_of_both) :-
    scasp(neither(X)),
    constrained_to(X, [9, 0, foo], [1, 2, 3]).

:- end_tests(diseq_accumulates).
