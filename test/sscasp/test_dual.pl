:- module(test_dual,
          [ test_dual/0
          ]).
:- use_module('../scasp_plunit', []).        % sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/dual)).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).

/** <module> Dualising a clause body

`sscasp/dual.pl` turns one clause body into the bodies of its dual clauses:
one per literal, each preceded by the literals before it.

The expected duals below are taken from the current implementation's `--code`
output, so a difference is a difference from a system that works rather than
from my reading of the papers.
*/

test_dual :-
    run_tests([ dual_negate,
                dual_bodies,
                dual_body_variables
              ]).

		 /*******************************
		 *        ONE LITERAL		*
		 *******************************/

:- begin_tests(dual_negate).

pair(p(x),        not(p(x))).
pair(not(p(x)),   p(x)).
pair(a = b,       a \= b).
pair(a \= b,      a = b).
pair(1 #= 2,      1 #<> 2).
pair(1 #<> 2,     1 #= 2).
pair(1 #< 2,      1 #>= 2).
pair(1 #>= 2,     1 #< 2).
pair(1 #> 2,      1 #=< 2).
pair(1 #=< 2,     1 #> 2).

test(negates, [forall(pair(Literal, Negated))]) :-
    negate_literal(Literal, N),
    assertion(N == Negated).

%   Every pair is symmetric, so negating twice is the identity.  That is
%   worth having: the negation of a dual is the original, and something
%   will want to read one back.

test(negation_is_an_involution, [forall(pair(Literal, _))]) :-
    negate_literal(Literal, Once),
    negate_literal(Once, Twice),
    assertion(Twice == Literal).

test(an_ordinary_goal_gets_not) :-
    negate_literal(flies(tweety), N),
    assertion(N == not(flies(tweety))).
test(a_zero_arity_goal) :-
    negate_literal(p, N),
    assertion(N == not(p)).

%   A two-argument term whose name is not one of the pairs is an ordinary
%   goal, not a comparison.

test(an_unrelated_binary_goal_gets_not) :-
    negate_literal(edge(a, b), N),
    assertion(N == not(edge(a, b))).

test(an_unbound_literal_is_refused,
     [ throws(error(instantiation_error, _)) ]) :-
    negate_literal(_, _).

:- end_tests(dual_negate).

		 /*******************************
		 *         A WHOLE BODY		*
		 *******************************/

:- begin_tests(dual_bodies).

test(one_literal) :-
    clause_duals([a], Duals),
    assertion(Duals == [[not(a)]]).

test(each_literal_gets_a_clause) :-
    clause_duals([a, b, c], Duals),
    assertion(Duals == [ [not(a)],
                         [a, not(b)],
                         [a, b, not(c)]
                       ]).

%   Verbatim from the current implementation for `p :- not a, b.' — the
%   prior *negative* literal is kept, which P4's wording would not have.

test(a_prior_negative_literal_is_kept) :-
    clause_duals([not(a), b], Duals),
    assertion(Duals == [ [a],
                         [not(a), not(b)]
                       ]).

test(and_again_with_three, [true(Duals == Expected)]) :-
    clause_duals([b, not(a), b], Duals),
    Expected = [ [not(b)],
                 [b, a],
                 [b, not(a), not(b)]
               ].

%   From `p(X) :- q(X), not t(X,Y).' — the shape the papers use as their
%   worked example.

test(the_worked_example) :-
    clause_duals([q(X), not(t(X, Y))], Duals),
    assertion(var(X)), assertion(var(Y)),
    assertion(Duals = [ [not(q(_))],
                        [q(_), t(_, _)]
                      ]).

%   A clause with no body is unconditionally true, so nothing makes it
%   fail and its negation has no way to succeed: no dual clauses at all.

test(an_empty_body_has_no_duals) :-
    clause_duals([], Duals),
    assertion(Duals == []).

%   The guards head abstraction produces are literals like any other, so
%   `q(1).' — abstracted to `q(A) :- A = 1.' — dualises to `A \= 1'.

test(an_abstraction_guard_dualises_to_disequality) :-
    clause_duals([A = 1], Duals),
    assertion(Duals == [[A \= 1]]).

:- end_tests(dual_bodies).

		 /*******************************
		 *       BODY VARIABLES		*
		 *******************************/

%   Negating "there is a Y such that ..." gives "for every Y, not ...", so
%   the variables of the body that are not in the head are what the
%   quantifier will range over — and what a domain has to supply a range
%   for.

:- begin_tests(dual_body_variables).

test(a_variable_only_in_the_body) :-
    Head = p(X),
    Body = [q(X), t(X, Y)],
    body_variables(Head, Body, Vars),
    assertion(Vars == [Y]).

test(no_body_only_variables) :-
    Head = p(X),
    body_variables(Head, [q(X)], Vars),
    assertion(Vars == []).

test(several_in_order_of_appearance) :-
    Head = p(X),
    Body = [q(X, A), r(B), s(A, B)],
    body_variables(Head, Body, Vars),
    assertion(Vars == [A, B]).

test(a_ground_body_has_none) :-
    body_variables(p, [q(a), r(b)], Vars),
    assertion(Vars == []).

test(a_head_with_no_arguments) :-
    body_variables(p, [q(Y)], Vars),
    assertion(Vars == [Y]).

:- end_tests(dual_body_variables).
