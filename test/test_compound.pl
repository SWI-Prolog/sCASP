:- module(test_compound,
          [ test_compound/0
          ]).
:- use_module(under_test).
:- use_module(scasp_plunit).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).

/** <module> Compound terms in heads, bodies and negation

A clause head whose argument is a compound term is compiled specially.
`comp_duals.pl:abstract_structures/4` replaces every such argument with a
fresh variable and moves the structure into the body as a unification, so

    wrap(f(X)) :- item(X).

becomes a dual of this shape:

    not o_wrap_1_vh2(A, B) :- A \= f(B).
    not o_wrap_1_vh2(A, B) :- A = f(B), not item(B).

These tests fix what a program *observes* of that: which calls succeed,
what an unbound query is bound to, and what a negated query answers.  They
are deliberately about behaviour rather than about the compiled form, so
that the compilation can be changed and these still say whether it still
works.  `compound_head_rewriting_is_neutral` below is the unit that made
that pay off: it compares the head rewriting against writing the
unification out by hand.
*/

test_compound :-
    run_tests([ compound_head,
                compound_nested,
                compound_constant,
                compound_several_args,
                compound_repeated_variable,
                compound_shared_variable,
                compound_lists,
                compound_facts,
                compound_negation,
                compound_head_rewriting_is_neutral
              ]).

		 /*******************************
		 *      A COMPOUND IN THE HEAD	*
		 *******************************/

%   wrap(f(X)) :- item(X).   item(a).
%
%   wrap holds of f(a) and of nothing else: not of f(b), because item(b)
%   does not hold, and not of g(a), because the functor differs.

:- begin_tests(compound_head).

item(a).
wrap(f(X)) :- item(X).

test(the_matching_structure_holds) :-
    scasp(wrap(f(a))).
test(a_different_argument_fails, fail) :-
    scasp(wrap(f(b))).
test(a_different_functor_fails, fail) :-
    scasp(wrap(g(a))).
test(an_atom_fails, fail) :-
    scasp(wrap(a)).

test(an_unbound_query_builds_the_structure, all(X == [f(a)])) :-
    scasp(wrap(X)).

test(negation_holds_of_a_different_argument, [nondet]) :-
    scasp(not wrap(f(b))).
test(negation_holds_of_a_different_functor, [nondet]) :-
    scasp(not wrap(g(a))).
test(negation_holds_of_an_atom, [nondet]) :-
    scasp(not wrap(b)).
test(negation_fails_of_the_matching_structure, fail) :-
    scasp(not wrap(f(a))).

:- end_tests(compound_head).

		 /*******************************
		 *           NESTED		*
		 *******************************/

%   Nesting is not special: the structure is rebuilt to whatever depth
%   the head has.

:- begin_tests(compound_nested).

item(a).
deep(g(h(X))) :- item(X).

test(the_nested_structure_holds) :-
    scasp(deep(g(h(a)))).
test(an_unbound_query_builds_both_levels, all(X == [g(h(a))])) :-
    scasp(deep(X)).
test(the_wrong_inner_functor_fails, fail) :-
    scasp(deep(g(k(a)))).
test(the_wrong_outer_functor_fails, fail) :-
    scasp(deep(k(h(a)))).
test(one_level_short_fails, fail) :-
    scasp(deep(h(a))).
test(negation_of_a_wrong_nesting_holds, [nondet]) :-
    scasp(not deep(g(k(a)))).

:- end_tests(compound_nested).

		 /*******************************
		 *    A CONSTANT INSIDE IT	*
		 *******************************/

%   The structure in the head need not contain a variable at all.

:- begin_tests(compound_constant).

item(a).
fixed(f(a)) :- item(a).

test(the_constant_structure_holds) :-
    scasp(fixed(f(a))).
test(any_other_argument_fails, fail) :-
    scasp(fixed(f(b))).
test(an_unbound_query_gives_the_constant, all(X == [f(a)])) :-
    scasp(fixed(X)).
test(negation_holds_elsewhere, [nondet]) :-
    scasp(not fixed(f(b))).

:- end_tests(compound_constant).

		 /*******************************
		 *      SEVERAL ARGUMENTS	*
		 *******************************/

:- begin_tests(compound_several_args).

item(a).
two(f(X, Y)) :- item(X), item(Y).

test(both_arguments_are_filled, all(T == [f(a,a)])) :-
    scasp(two(T)).
test(the_matching_pair_holds) :-
    scasp(two(f(a, a))).
test(a_wrong_second_argument_fails, fail) :-
    scasp(two(f(a, b))).

% A compound of a different arity is a different term, even with the
% same functor name.

ar(f(1)).
ar(f(1, 2)).

test(one_argument_holds) :-
    scasp(ar(f(1))).
test(two_arguments_hold_as_well) :-
    scasp(ar(f(1, 2))).
test(a_different_argument_fails, fail) :-
    scasp(ar(f(2))).
test(negation_holds_of_the_absent_one) :-
    scasp(not ar(f(2))).

:- end_tests(compound_several_args).

		 /*******************************
		 *      A REPEATED VARIABLE	*
		 *******************************/

%   f(X,X) matches only a structure whose two arguments are equal, so
%   the head itself carries the equality.

:- begin_tests(compound_repeated_variable).

item(a).
item(b).
same(f(X, X)) :- item(X).

test(equal_arguments_hold) :-
    scasp(same(f(a, a))).
test(the_other_equal_pair_holds) :-
    scasp(same(f(b, b))).
test(unequal_arguments_fail, fail) :-
    scasp(same(f(a, b))).
test(both_equal_pairs_are_enumerated, set(T == [f(a,a), f(b,b)])) :-
    scasp(same(T)).
test(negation_holds_of_an_unequal_pair, [nondet]) :-
    scasp(not same(f(a, b))).

:- end_tests(compound_repeated_variable).

		 /*******************************
		 *   A VARIABLE SHARED OUTSIDE	*
		 *******************************/

%   A variable inside the structure also appearing as another head
%   argument ties the two together.

:- begin_tests(compound_shared_variable).

item(a).
both(f(X), X) :- item(X).

test(the_tied_pair_holds) :-
    scasp(both(f(a), a)).
test(an_untied_pair_fails, fail) :-
    scasp(both(f(a), b)).
test(an_unbound_query_ties_them, all(X-Y == [f(a)-a])) :-
    scasp(both(X, Y)).
test(negation_holds_of_an_untied_pair, [nondet]) :-
    scasp(not both(f(a), b)).

:- end_tests(compound_shared_variable).

		 /*******************************
		 *           LISTS		*
		 *******************************/

%   A list is a compound term like any other, so the same machinery has
%   to handle '[|]'/2.

:- begin_tests(compound_lists).

item(a).
lst([H|_]) :- item(H).

test(a_one_element_list_holds) :-
    scasp(lst([a])).
test(a_longer_list_holds_on_its_head) :-
    scasp(lst([a, b])).
test(a_wrong_head_fails, fail) :-
    scasp(lst([b])).
test(the_empty_list_fails, fail) :-
    scasp(lst([])).
test(negation_holds_of_a_wrong_head, [nondet]) :-
    scasp(not lst([b])).
test(negation_holds_of_the_empty_list, [nondet]) :-
    scasp(not lst([])).

:- end_tests(compound_lists).

		 /*******************************
		 *      COMPOUNDS AS FACTS	*
		 *******************************/

%   Facts with compound arguments, where the structure is all there is.

:- begin_tests(compound_facts).

fact(f(1)).
fact(g(2)).

test(each_fact_holds) :-
    scasp(fact(f(1))).
test(the_other_fact_holds) :-
    scasp(fact(g(2))).
test(both_are_enumerated, all(T == [f(1), g(2)])) :-
    scasp(fact(T)).
test(a_crossed_pair_fails, fail) :-
    scasp(fact(f(2))).
test(negation_holds_of_a_crossed_pair) :-
    scasp(not fact(f(2))).
test(negation_holds_of_a_new_functor) :-
    scasp(not fact(h(0))).
test(negation_fails_of_a_fact, fail) :-
    scasp(not fact(f(1))).

:- end_tests(compound_facts).

		 /*******************************
		 *  WHERE THE NEGATION STOPS	*
		 *******************************/

%   Negating a rule whose head holds a structure is *incomplete* for an
%   unbound query.
%
%   wrap(f(X)) :- item(X).  item(a).
%
%   `not wrap(T)` holds of every T except f(a) - including g(a) and the
%   atom b, both of which succeed when asked directly.  But the unbound
%   query has a single answer, `f(Y)` with `Y ∉ [a]`: the shape that is
%   *not* f(_) is never offered.
%
%   The dual has a clause for it - `not o_wrap_1_vh2(A, B) :- A \= f(B)`
%   - but under `forall(B, ...)` an unbound A cannot satisfy it, so only
%   the `A = f(B)` branch survives.
%
%   Pinned as it behaves.  These fail the day the general answer appears,
%   which is the point: a reimplementation should decide deliberately
%   whether to reproduce this.

:- begin_tests(compound_negation).

item(a).
wrap(f(X)) :- item(X).

% Asked about a ground term, negation is right in every case: wrap holds
% of f(a) and of nothing else, so not wrap holds of everything else.

test(a_different_functor_succeeds, [nondet]) :-
    scasp(not wrap(g(a))).
test(an_atom_succeeds, [nondet]) :-
    scasp(not wrap(b)).
test(the_one_shape_that_holds_is_excluded, fail) :-
    scasp(not wrap(f(a))).

% Asked about an unbound term it is incomplete: only the f/1 shape is
% offered, so the general answer is strictly weaker than the set of
% ground instances above.  doc/bugs.md #15 - and not a porting error but
% a limitation of the published method, see doc/literature.md.
%
% The two branches are the two expectations.  What is compared is the set
% of shapes the answers admit, not how many answers they arrive in: the
% solution set is semantics, the number of answers it is split into is
% an implementation choice.

:- if(expect_semantics).

test(the_unbound_answer_admits_every_shape_that_holds,
     [ set(Shape == [atom, other_functor, same_functor]) ]) :-
    scasp(not wrap(T)),
    admits(T, Shape).

:- else.

test(the_unbound_answer_admits_only_the_f_shape,
     [ set(Shape == [same_functor]) ]) :-
    scasp(not wrap(T)),
    admits(T, Shape).

:- endif.

% True either way, and the half that matters most: the one term of which
% wrap *does* hold must never be offered.

test(the_unbound_answer_never_admits_the_one_that_holds, fail) :-
    scasp(not wrap(T)),
    \+ T \= f(a).

%!  admits(+Term, -Shape) is nondet.
%
%   Shape is each of the shapes Term can still be bound to.  Called
%   inside the solution so the constraints on Term are still live.

admits(T, same_functor)  :- \+ T \= f(b).
admits(T, other_functor) :- \+ T \= g(a).
admits(T, atom)          :- \+ T \= b.

:- end_tests(compound_negation).

		 /*******************************
		 *  HEAD REWRITING IS NEUTRAL	*
		 *******************************/

%   Three ways of writing one rule:
%
%       w1(f(X)) :- item(X).                  % compound in the head
%       w2(Y)    :- Y = f(X), item(X).        % the unification by hand
%       w3(Y)    :- Y = f(Z), Z = X, item(X). % one indirection further
%
%   abstract_structures/4 rewrites w1 into w2, so those two compile to
%   byte-identical code.  w3 is the shape the compiler's other, now
%   removed path would have built - rebuilding the structure from fresh
%   variables and unifying each argument separately - and it does compile
%   to a longer dual, with a branch of its own for the inner `B \= C`.
%
%   All three answer alike, the incomplete negation above included.  That
%   is the point of the unit: the missing answer is not something moving
%   the structure out of the head can recover, by hand or otherwise, so a
%   reimplementation is free to pick whichever rewriting it likes.

:- begin_tests(compound_head_rewriting_is_neutral).

item(a).
w1(f(X)) :- item(X).
w2(Y) :- Y = f(X), item(X).
w3(Y) :- Y = f(Z), Z = X, item(X).

test(all_three_prove_the_same_thing,
     [ forall(member(W, [w1, w2, w3])) ]) :-
    G =.. [W, T],
    findall(T, scasp(G, []), L),
    assertion(L == [f(a)]).

test(all_three_admit_another_functor_asked_directly,
     [ forall(member(W, [w1, w2, w3])) ]) :-
    G =.. [W, g(a)],
    assertion(\+ \+ scasp(not G, [])).

test(all_three_admit_an_atom_asked_directly,
     [ forall(member(W, [w1, w2, w3])) ]) :-
    G =.. [W, b],
    assertion(\+ \+ scasp(not G, [])).

%   And all three are incomplete in the same way: one answer, the f/1
%   shape, never the g/1 shape or the atom they each accept above.

test(all_three_give_the_same_single_negative_answer,
     [ forall(member(W, [w1, w2, w3])) ]) :-
    G =.. [W, T],
    findall(Shape, ( scasp(not G, []), shape(T, Shape) ), L),
    assertion(L == [only_f]).

shape(T, Shape) :-
    (   \+ T \= f(b),
        T \= f(a),
        T \= g(a),
        T \= b
    ->  Shape = only_f
    ;   Shape = other
    ).

:- end_tests(compound_head_rewriting_is_neutral).
