:- module(test_builtins,
          [ test_builtins/0
          ]).
:- use_module(under_test).
:- use_module(scasp_plunit).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(lists), [memberchk/2]).

/** <module> Built-ins, findall/3, abducibles and classical negation

The goals s(CASP) does not compile into rules but executes directly, and
the two directives that add rules of their own.

`predicates.pl` decides what counts: `prolog_builtin/1` (unification,
the arithmetic comparisons, `true`/`fail`), `clp_builtin/1` (the `#`
constraints), `clp_interval/1` (`inf/2`, `sup/2`), plus `is/2`,
`findall/3` and `prolog/2`.  `comp_duals.pl` has to know the dual of
each, since negating a rule means negating the built-ins in its body.
*/

test_builtins :-
    run_tests([ builtin_comparison,
                builtin_arithmetic_comparison,
                builtin_standard_order,
                builtin_unification,
                builtin_constants,
                builtin_arithmetic,
                builtin_interval,
                builtin_findall,
                builtin_abducible,
                builtin_classical_negation
              ]).

		 /*******************************
		 *        COMPARISONS		*
		 *******************************/

%   The duals are the obvious ones: not (X =< Y) is X > Y, and so on.
%   comp_duals.pl needs them because they appear in rule bodies.

:- begin_tests(builtin_comparison).

le(X, Y) :- X =< Y.
lt(X, Y) :- X < Y.
ge(X, Y) :- X >= Y.
gt(X, Y) :- X > Y.

test(less_or_equal_holds) :-
    scasp(le(1, 2)).
test(less_or_equal_holds_on_equal) :-
    scasp(le(2, 2)).
test(less_or_equal_fails_above, fail) :-
    scasp(le(3, 2)).
test(its_dual_is_greater_than) :-
    scasp(not le(3, 2)).
test(its_dual_fails_where_it_holds, fail) :-
    scasp(not le(1, 2)).

test(strictly_less_excludes_equal, fail) :-
    scasp(lt(2, 2)).
test(greater_or_equal_admits_equal) :-
    scasp(ge(2, 2)).
test(strictly_greater_excludes_equal, fail) :-
    scasp(gt(2, 2)).
test(dual_of_strictly_greater_admits_equal) :-
    scasp(not gt(2, 2)).

:- end_tests(builtin_comparison).

		 /*******************************
		 *   ARITHMETIC COMPARISON	*
		 *******************************/

%   =:=/2 and =\=/2 compare the *values* of two expressions, so both
%   sides are evaluated first and both must be bound enough to evaluate.
%   They are each other's dual.

:- begin_tests(builtin_arithmetic_comparison).

aeq(X, Y) :- X =:= Y.
ane(X, Y) :- X =\= Y.

test(equal_values_are_equal) :-
    scasp(aeq(2, 2)).
test(the_arguments_are_evaluated) :-
    scasp(aeq(1+1, 2)).
test(different_values_are_not, fail) :-
    scasp(aeq(2, 3)).
test(the_dual_of_equality_is_inequality) :-
    scasp(not aeq(2, 3)).
test(and_fails_where_the_values_are_equal, fail) :-
    scasp(not aeq(2, 2)).

test(different_values_are_unequal) :-
    scasp(ane(2, 3)).
test(equal_values_are_not_unequal, fail) :-
    scasp(ane(2, 2)).
test(the_dual_of_inequality_is_equality) :-
    scasp(not ane(2, 2)).

% An unbound argument cannot be evaluated, so the comparison raises.  It
% used to fail instead - exec_goal/1 swallowed every error - which made the
% goal *and* its negation fail, something that cannot hold of a
% ground-decidable predicate.  That was doc/bugs.md #17, now fixed; raising
% is the honest intermediate state.
%
% Decided: the constructive reading.  `X =:= 2` constrains X exactly as
% `X #= 2` does, and its negation constrains the other way, so an unbound
% query stays answerable - which is the point of s(CASP).  `#=` remains
% the way to say it explicitly; this makes =:= agree with it rather than
% introduce a second, weaker meaning.

:- if(expect_semantics).

test(an_unbound_argument_is_constrained, all(X == [2])) :-
    scasp(aeq(X, 2)).
test(and_its_negation_constrains_the_other_way) :-
    answers_admit(scasp(not aeq(X, 2), []), X, [0, 1, 2, 3], Admits),
    assertion(Admits == [0, 1, 3]).

:- else.

% The negation raises from the dual, which is =\=/2 rather than =:=/2.

test(an_unbound_argument_raises,
     [ throws(error(instantiation_error, _)) ]) :-
    scasp(aeq(_, 2)).
test(and_so_does_its_negation,
     [ throws(error(instantiation_error, _)) ]) :-
    scasp(not aeq(_, 2)).

:- endif.

:- end_tests(builtin_arithmetic_comparison).

		 /*******************************
		 *       STANDARD ORDER		*
		 *******************************/

%   @</2 and friends compare *terms* in the standard order of terms,
%   not values: they never evaluate, and they apply to any two terms.
%   @< and @>= are duals, as are @> and @=<.

:- begin_tests(builtin_standard_order).

tlt(X, Y) :- X @< Y.
tgt(X, Y) :- X @> Y.
tle(X, Y) :- X @=< Y.
tge(X, Y) :- X @>= Y.

test(atoms_are_ordered_alphabetically) :-
    scasp(tlt(a, b)).
test(and_not_the_other_way, fail) :-
    scasp(tlt(b, a)).
test(the_dual_of_less_is_greater_or_equal) :-
    scasp(not tlt(b, a)).
test(the_dual_fails_where_less_holds, fail) :-
    scasp(not tlt(a, b)).

test(greater_than_is_the_mirror) :-
    scasp(tgt(b, a)).
test(less_or_equal_admits_equal) :-
    scasp(tle(a, a)).
test(greater_or_equal_admits_equal) :-
    scasp(tge(a, a)).
test(strict_order_excludes_equal, fail) :-
    scasp(tlt(a, a)).

% It is an order on terms, not on numbers: compound terms are ordered by
% arity, then name, then arguments.

test(compound_terms_are_ordered_by_argument) :-
    scasp(tlt(f(1), f(2))).
test(numbers_precede_atoms) :-
    scasp(tlt(1, a)).

% The caveat.  Standard order is defined on every term, including an
% unbound variable, and a variable precedes everything else - so
% tlt(X, b) *succeeds* with X still unbound.  That is not a constraint:
% nothing stops X being bound to `z` afterwards, which is not @< b.
%
% This is the one place where an s(CASP) answer does not constrain the
% variable it answered about: tlt(X, b) succeeds with X unbound, and
% nothing then stops X being bound to `z`, which is not @< b.
% doc/bugs.md #16.
%
% Decided: raise, but only where the outcome actually depends on a
% variable.  Standard order settles many comparisons before reaching one -
% f(X) @< g(a) holds whatever X is, because the names decide it - and
% those stay usable.  Requiring ground arguments would have been simpler
% to specify but would reject them.

test(a_comparison_the_functors_settle_holds) :-
    scasp(tlt(f(_), g(a))).

:- if(expect_semantics).

test(an_unbound_variable_is_not_comparable,
     [ throws(error(instantiation_error, _)) ]) :-
    scasp(tlt(_, b)).
test(nor_is_one_on_the_right,
     [ throws(error(instantiation_error, _)) ]) :-
    scasp(tlt(a, _)).
test(nor_is_a_deciding_argument_that_is_unbound,
     [ throws(error(instantiation_error, _)) ]) :-
    scasp(tlt(f(_), f(a))).

% The case that rules out the tempting shortcut "decidable iff the terms
% do not unify".  f(X,a) and f(b,c) do *not* unify - a and c clash - yet
% standard order decides at argument 1, where X is, and the answer flips
% once X is bound:
%
%     X unbound:  f(X,a) @<  f(b,c)
%     X = z:      f(z,a) @>= f(b,c)
%
% So unifiability is not the test; where the comparison is *decided* is.

test(nor_when_a_later_clash_hides_the_deciding_argument,
     [ throws(error(instantiation_error, _)) ]) :-
    scasp(tlt(f(_, a), f(b, c))).

:- else.

test(an_unbound_variable_precedes_an_atom) :-
    scasp(tlt(_, b)).
test(so_the_negation_fails, fail) :-
    scasp(not tlt(_, b)).
test(and_nothing_precedes_an_unbound_variable, fail) :-
    scasp(tlt(a, _)).
test(and_an_unbound_deciding_argument_goes_undetected) :-
    scasp(tlt(f(_), f(a))).
test(nor_is_one_hidden_behind_a_later_clash) :-
    scasp(tlt(f(_, a), f(b, c))).

:- endif.

:- end_tests(builtin_standard_order).

		 /*******************************
		 *        UNIFICATION		*
		 *******************************/

%   =/2 and \=/2.  Negating =/2 is where constructive disunification
%   comes from: `not eq(X, a)` constrains X rather than failing.

:- begin_tests(builtin_unification).

eq(X, Y) :- X = Y.
ne(X, Y) :- X \= Y.

test(unifiable_terms_are_equal) :-
    scasp(eq(a, a)).
test(different_atoms_are_not, fail) :-
    scasp(eq(a, b)).
test(different_atoms_are_unequal) :-
    scasp(ne(a, b)).
test(equal_atoms_are_not_unequal, fail) :-
    scasp(ne(a, a)).

test(negating_equality_constrains) :-
    scasp(not eq(X, a)),
    constrained_to(X, [b, c], [a]).

:- end_tests(builtin_unification).

		 /*******************************
		 *         TRUE AND FAIL		*
		 *******************************/

:- begin_tests(builtin_constants).

always :- true.
never :- fail.

test(true_succeeds) :-
    scasp(always).
test(its_negation_fails, fail) :-
    scasp(not always).
test(fail_fails, fail) :-
    scasp(never).
test(its_negation_holds) :-
    scasp(not never).

:- end_tests(builtin_constants).

		 /*******************************
		 *         ARITHMETIC		*
		 *******************************/

%   is/2 evaluates; it does not constrain.  So it needs its arguments
%   bound enough to evaluate, and negating it compares the value.

:- begin_tests(builtin_arithmetic).

plus3(X, Y) :- Y is X+3.

test(evaluates_the_expression, all(Y == [7])) :-
    scasp(plus3(4, Y)).
test(a_wrong_value_fails, fail) :-
    scasp(plus3(4, 8)).
test(negation_of_a_wrong_value_holds) :-
    scasp(not plus3(4, 8)).
test(negation_of_the_right_value_fails, fail) :-
    scasp(not plus3(4, 7)).

:- end_tests(builtin_arithmetic).

		 /*******************************
		 *      INTERVAL LIMITS		*
		 *******************************/

%   inf/2 and sup/2 read the bounds of a CLP(Q) store.  For 3 < X < 9
%   the infimum is 3 and the supremum 9, neither of them attained.

:- begin_tests(builtin_interval).

lowest(X, L)  :- X #> 3, X #< 9, inf(X, L).
highest(X, H) :- X #> 3, X #< 9, sup(X, H).

test(infimum_of_an_open_interval, all(L == [3])) :-
    scasp(lowest(_, L)).
test(supremum_of_an_open_interval, all(H == [9])) :-
    scasp(highest(_, H)).
test(the_constraint_survives_alongside_the_bound) :-
    scasp(lowest(X, 3)),
    constrained_to(X, [4, 8], [3, 9, 0]).

:- end_tests(builtin_interval).

		 /*******************************
		 *          FINDALL		*
		 *******************************/

%   findall/3 collects the answers of its goal.  Those answers are part
%   of the proof, so they appear in the model alongside the collecting
%   goal.

:- begin_tests(builtin_findall).

item(a).
item(b).
all_items(L) :- findall(X, item(X), L).

test(collects_every_answer, all(L == [[a,b]])) :-
    scasp(all_items(L)).
test(the_collected_answers_are_in_the_model) :-
    scasp(all_items(_), [model(Model)]),
    assertion(memberchk(item(a), Model)),
    assertion(memberchk(item(b), Model)),
    assertion(memberchk(all_items([a,b]), Model)).
test(a_wrong_list_fails, fail) :-
    scasp(all_items([a])).

% Negating findall/3 is not implemented: exec_neg_findall/5 in solve.pl
% prints `PENDING:` and fails.  doc/bugs.md.
%
% What it should do: findall collects [a,b], which does not unify with
% [X], so the findall goal fails and its negation therefore holds.

no_items :- not findall(X, item(X), [X]).

:- if(expect_semantics).

test(negated_findall_holds_when_the_findall_fails, [nondet]) :-
    scasp(no_items).

:- else.

test(negated_findall_always_fails, fail) :-
    scasp(no_items).

:- endif.

:- end_tests(builtin_findall).

		 /*******************************
		 *         ABDUCIBLES		*
		 *******************************/

%   An abducible atom may be assumed either way, so a goal resting on
%   one has a model where it holds and a model where it does not - both
%   `holds` and `not holds` succeed, each in its own model.

:- begin_tests(builtin_abducible).

%   Not `maybe`: that is a predicate of library(random), and where the
%   declaration does not load, `holds` would reach it and toss a coin.

:- abducible abd_maybe.
holds :- abd_maybe.

:- abducible pick(1).
chosen(X) :- pick(X).

test(the_goal_holds_by_assuming_it, [nondet]) :-
    scasp(holds).
test(and_fails_by_assuming_the_other_way) :-
    scasp(not holds).
test(the_assumption_is_recorded_in_the_model, [nondet]) :-
    scasp(holds, [model(Model)]),
    assertion(memberchk(abd_maybe, Model)),
    assertion(memberchk(holds, Model)).
test(an_abducible_with_arguments, [nondet]) :-
    scasp(chosen(1)).
test(only_the_declared_instance, fail) :-
    scasp(chosen(2)).

:- end_tests(builtin_abducible).

		 /*******************************
		 *     CLASSICAL NEGATION	*
		 *******************************/

%   -p is a separate atom from p, not the absence of it.  modules.pl
%   interns the sign into the functor name, so `-cold` and `cold` are
%   two predicates: what is asserted of one says nothing about the
%   other, and a predicate with no clauses at all does not exist rather
%   than failing.

:- begin_tests(builtin_classical_negation).

-cold(water).
cold(ice).

uses_negative :- -cold(water).

test(the_negative_atom_holds, [nondet]) :-
    scasp(-cold(water)).
test(the_positive_atom_holds_of_something_else, [nondet]) :-
    scasp(cold(ice)).
test(the_positive_atom_does_not_follow, fail) :-
    scasp(cold(water)).
test(nor_does_the_negative_one, fail) :-
    scasp(-cold(ice)).
test(a_body_may_call_a_negative_atom, [nondet]) :-
    scasp(uses_negative).

% Default negation is a third thing again: `not cold(water)` says only
% that cold(water) cannot be derived, which is so.

test(naf_holds_where_the_positive_atom_does_not, [nondet]) :-
    scasp(not cold(water)).
test(naf_fails_where_it_does, fail) :-
    scasp(not cold(ice)).

:- end_tests(builtin_classical_negation).
