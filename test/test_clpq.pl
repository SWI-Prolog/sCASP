:- module(test_clpq,
          [ test_clpq/0
          ]).
:- use_module(under_test).
:- use_module(scasp_plunit).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(lists), [member/2]).

/** <module> CLP(Q) constraints and their duals

s(CASP) answers a query about an unbound variable with a *constraint* on
it rather than a set of values, and it answers a negated query by
negating that constraint.  That second half - the dual of a constraint
store - is what makes `not` constructive, and it is the part with the
least coverage.

Each unit is one small program.  The comment above it states what the
semantics say the answers are; the tests check the reasoner agrees.
*/

test_clpq :-
    run_tests([ clpq_threshold,
                clpq_equality,
                clpq_disequality,
                clpq_interval,
                clpq_rational,
                clpq_two_variables,
                clpq_guarded_fact,
                clpq_store_dual
              ]).

		 /*******************************
		 *         ONE THRESHOLD	*
		 *******************************/

%   gt(X) :- X #> 10.
%
%   gt holds of everything above 10.  Its dual is the single constraint
%   X #=< 10, so `not gt(X)` has exactly one answer.

:- begin_tests(clpq_threshold).

gt(X) :- X #> 10.

test(above_the_threshold) :-
    scasp(gt(20)).
test(on_the_threshold, fail) :-
    scasp(gt(10)).
test(below_the_threshold, fail) :-
    scasp(gt(5)).

test(unbound_query_is_answered_with_a_constraint) :-
    scasp(gt(X)),
    constrained_to(X, [11, 1000], [10, 0, -5]).

test(negation_holds_below) :-
    scasp(not gt(5)).
test(negation_holds_on_the_threshold) :-
    scasp(not gt(10)).
test(negation_fails_above, fail) :-
    scasp(not gt(20)).

test(dual_of_one_constraint_is_the_complement) :-
    answers_admit(scasp(not gt(X), []), X,
                  [-5, 0, 10, 11, 1000], Admits),
    assertion(Admits == [-5, 0, 10]).

:- end_tests(clpq_threshold).

		 /*******************************
		 *           EQUALITY		*
		 *******************************/

%   eq(X) :- X #= 5.
%
%   The complement of a point is everything either side of it.  CLP(Q)
%   has no single constraint for that, so the dual splits: clpq.pl has
%   dual_clpq_(A #= B, A #> B) and dual_clpq_(A #= B, A #< B).  Two
%   answers, therefore, not one.

:- begin_tests(clpq_equality).

eq(X) :- X #= 5.

test(the_point_holds) :-
    scasp(eq(5)).
test(anything_else_fails, fail) :-
    scasp(eq(3)).
test(unbound_query_binds_the_point) :-
    scasp(eq(X)),
    constrained_to(X, [5], [4, 6]).

test(negation_holds_below_the_point, [nondet]) :-
    scasp(not eq(3)).
test(negation_holds_above_the_point) :-
    scasp(not eq(7)).
test(negation_fails_at_the_point, fail) :-
    scasp(not eq(5)).

% Two answers today, one either side of the point.  What the test states
% is the set they cover between them: everything but 5.

test(dual_of_equality_covers_everything_but_the_point) :-
    answers_admit(scasp(not eq(X), []), X,
                  [0, 4, 5, 6, 100], Admits),
    assertion(Admits == [0, 4, 6, 100]).

:- end_tests(clpq_equality).

		 /*******************************
		 *         DISEQUALITY		*
		 *******************************/

%   neq(X) :- X #<> 5.
%
%   The mirror image: neq holds everywhere but 5, and its dual is the
%   single constraint X #= 5 - one answer, and a ground one.

:- begin_tests(clpq_disequality).

neq(X) :- X #<> 5.

test(holds_away_from_the_point, [nondet]) :-
    scasp(neq(3)).
test(fails_at_the_point, fail) :-
    scasp(neq(5)).
% CLP(Q) has no disequality: disequality_clpq/2 implements X #<> 5 as
% `X #> 5 ; X #< 5`, so an unbound query splits into two answers, one
% either side of the point.  Neither admits the other side.

test(the_answers_cover_everything_but_the_point) :-
    answers_admit(scasp(neq(X), []), X,
                  [0, 4, 5, 6, 100], Admits),
    assertion(Admits == [0, 4, 6, 100]).

test(negation_holds_at_the_point) :-
    scasp(not neq(5)).
test(negation_fails_away_from_it, fail) :-
    scasp(not neq(3)).
test(dual_of_disequality_is_the_point) :-
    scasp(not neq(X)),
    constrained_to(X, [5], [4, 6]).

:- end_tests(clpq_disequality).

		 /*******************************
		 *          AN INTERVAL		*
		 *******************************/

%   between_(X) :- X #> 1, X #< 5.
%
%   A store of two constraints.  dual_clpq/2 negates such a store one
%   constraint at a time, keeping the earlier ones:
%
%       X #=< 1                 (negate the first)
%       X #> 1, X #>= 5         (keep the first, negate the second)
%
%   so `not between_(X)` has two answers, one for each side of the
%   interval, and together they cover everything outside it.

:- begin_tests(clpq_interval).

between_(X) :- X #> 1, X #< 5.

test(inside_holds) :-
    scasp(between_(3)).
test(on_the_lower_bound, fail) :-
    scasp(between_(1)).
test(on_the_upper_bound, fail) :-
    scasp(between_(5)).
test(outside_fails, fail) :-
    scasp(between_(9)).

test(unbound_query_is_the_interval) :-
    scasp(between_(X)),
    constrained_to(X, [2, 3, 4], [1, 5, 0, 9]).

test(negation_holds_below, [nondet]) :-
    scasp(not between_(0)).
test(negation_holds_above) :-
    scasp(not between_(9)).
test(negation_holds_on_the_lower_bound, [nondet]) :-
    scasp(not between_(1)).
test(negation_holds_on_the_upper_bound) :-
    scasp(not between_(5)).
test(negation_fails_inside, fail) :-
    scasp(not between_(3)).

% Two answers today, one for each side of the interval.  Between them
% they must cover exactly the complement of 1 < X < 5.

test(dual_of_a_two_constraint_store_covers_the_complement) :-
    answers_admit(scasp(not between_(X), []), X,
                  [-1, 0, 1, 2, 3, 4, 5, 9], Admits),
    assertion(Admits == [-1, 0, 1, 5, 9]).

:- end_tests(clpq_interval).

		 /*******************************
		 *          RATIONALS		*
		 *******************************/

%   r(X) :- X #> 1/3 + 1/4.
%
%   CLP(Q) is exact: the bound is 7/12, not a float near it.  0.583 is
%   below 7/12 (0.58333...), so it must be excluded while 7/12 itself is
%   excluded too - the constraint is strict.

:- begin_tests(clpq_rational).

r(X) :- X #> 1/3 + 1/4.

test(above_the_rational_bound) :-
    scasp(r(1)).
test(on_the_bound_is_excluded, fail) :-
    scasp(r(7r12)).
test(just_below_the_bound_is_excluded, fail) :-
    scasp(r(583r1000)).
test(just_above_the_bound_is_included) :-
    scasp(r(584r1000)).

test(the_answer_keeps_the_exact_bound) :-
    scasp(r(X)),
    constrained_to(X, [584r1000, 1], [7r12, 583r1000, 0]).

test(negation_is_the_closed_complement) :-
    scasp(not r(X)),
    constrained_to(X, [7r12, 583r1000, 0], [584r1000, 1]).

:- end_tests(clpq_rational).

		 /*******************************
		 *        TWO VARIABLES		*
		 *******************************/

%   lt(X, Y) :- X #< Y.
%
%   A constraint relating two variables rather than a variable and a
%   constant.  The dual is X #>= Y, which still relates them.

:- begin_tests(clpq_two_variables).

lt(X, Y) :- X #< Y.

test(ordered_pair_holds) :-
    scasp(lt(1, 2)).
test(reversed_pair_fails, fail) :-
    scasp(lt(2, 1)).
test(equal_pair_fails, fail) :-
    scasp(lt(2, 2)).

test(one_bound_constrains_the_other) :-
    scasp(lt(3, Y)),
    constrained_to(Y, [4, 100], [3, 2]).

test(negation_admits_equal) :-
    scasp(not lt(2, 2)).
test(negation_admits_reversed) :-
    scasp(not lt(2, 1)).
test(negation_rejects_ordered, fail) :-
    scasp(not lt(1, 2)).

test(negation_relates_the_variables) :-
    scasp(not lt(3, Y)),
    constrained_to(Y, [3, 2, 0], [4, 100]).

:- end_tests(clpq_two_variables).

		 /*******************************
		 *   CONSTRAINT MEETS A FACT	*
		 *******************************/

%   big_enough(X) :- size(X, S), S #> 10.
%   size(small, 5).  size(large, 20).
%
%   The constraint is on a variable the rule introduces, not on the
%   argument, so the dual has to quantify over it: `not big_enough(X)`
%   holds when *every* size of X fails the constraint.

:- begin_tests(clpq_guarded_fact).

size(small, 5).
size(large, 20).
big_enough(X) :- size(X, S), S #> 10.

test(large_is_big_enough) :-
    scasp(big_enough(large)).
test(small_is_not, fail) :-
    scasp(big_enough(small)).
test(enumerates_the_big_ones, all(X == [large])) :-
    scasp(big_enough(X)).

test(negation_holds_of_the_small_one, [nondet]) :-
    scasp(not big_enough(small)).
test(negation_fails_of_the_large_one, fail) :-
    scasp(not big_enough(large)).

:- end_tests(clpq_guarded_fact).

		 /*******************************
		 *           HELPERS		*
		 *******************************/




		 /*******************************
		 *     NEGATING A WHOLE STORE	*
		 *******************************/

%   band :- level(Y), Y #> 3.
%   level(Y) :- Y #> 0, Y #< 10.
%
%   The dual of `band` quantifies over Y, and by the time the forall is
%   checked Y carries a *store* rather than a single constraint.
%   Negating it is dual_clpq/2's job: it walks the store, negating one
%   constraint at a time and keeping the earlier ones.
%
%   Y = 4 satisfies every constraint, so band holds and `not band` must
%   have no answer - under each of the four algorithms, since two of
%   them reach this code and two do not.

:- begin_tests(clpq_store_dual).

level(Y) :- Y #> 0, Y #< 10.
band :- level(Y), Y #> 3.

% out has no satisfying Y: Y #> 0, Y #< 10 and Y #> 20 cannot all hold.

out_level(Y) :- Y #> 0, Y #< 10.
out :- out_level(Y), Y #> 20.

count(Alg, Goal, N) :-
    with_forall(Alg, ( findall(x, scasp(Goal, []), L), length(L, N) )).

test(the_band_is_satisfiable) :-
    scasp(band).
test(so_its_negation_fails, fail) :-
    scasp(not band).

test(the_empty_band_is_unsatisfiable, fail) :-
    scasp(out).
test(so_its_negation_holds, [nondet]) :-
    scasp(not out).

test(every_algorithm_agrees_on_the_band,
     [ forall(member(Alg, [all, all_c, prev, sasp])) ]) :-
    count(Alg, band, P),
    count(Alg, not band, N),
    assertion(P >= 1),
    assertion(N == 0).

test(every_algorithm_agrees_on_the_empty_band,
     [ forall(member(Alg, [all, all_c, prev, sasp])) ]) :-
    count(Alg, out, P),
    count(Alg, not out, N),
    assertion(P == 0),
    assertion(N >= 1).

:- end_tests(clpq_store_dual).
