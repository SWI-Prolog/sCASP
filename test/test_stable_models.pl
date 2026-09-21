:- module(test_stable_models,
          [ test_stable_models/0
          ]).
:- use_module(stable_models).
:- use_module(library(lists), [memberchk/2]).
:- use_module(library(plunit)).

/** <module> The oracle, checked against worked examples

`stable_models.pl` is only useful if it is right, and it is the one thing in
the suite that cannot be checked against s(CASP) - that would defeat its
purpose.  So it is checked against textbook programs whose answer sets are
known independently, and against the algebraic properties of the definition.

Every program here is small enough to verify by hand, and the comment above
each says why the answer is what it is.
*/

test_stable_models :-
    run_tests([ sm_definite,
                sm_negation,
                sm_odd_loop,
                sm_positive_loop,
                sm_constraints,
                sm_entailment,
                sm_reduct,
                sm_syntax,
                sm_classical_negation,
                sm_grounding,
                sm_clp
              ]).

		 /*******************************
		 *      DEFINITE PROGRAMS	*
		 *******************************/

%   With no negation the reduct is the program itself, so there is exactly
%   one answer set: the least model.

:- begin_tests(sm_definite).

test(a_fact_is_its_own_model) :-
    stable_models([p], M),
    assertion(M == [[p]]).
test(the_empty_program_has_the_empty_model) :-
    stable_models([], M),
    assertion(M == [[]]).
test(a_chain_closes_under_consequence) :-
    stable_models([p, (q :- p), (r :- q)], M),
    assertion(M == [[p,q,r]]).
test(an_unsupported_atom_is_absent) :-
    stable_models([(q :- p)], M),
    assertion(M == [[]]).
test(a_disjunctive_definition_needs_only_one_body) :-
    stable_models([p, (r :- p), (r :- q)], M),
    assertion(M == [[p,r]]).

:- end_tests(sm_definite).

		 /*******************************
		 *          NEGATION		*
		 *******************************/

:- begin_tests(sm_negation).

%   p :- not q.   q :- not p.
%
%   The even loop.  Two answer sets: assuming q absent supports p, and the
%   other way round.  Neither {} nor {p,q} is stable - {} is not closed, and
%   the reduct of {p,q} is empty so its least model is {}.

test(an_even_loop_has_two_answer_sets) :-
    stable_models([(p :- not q), (q :- not p)], M),
    assertion(M == [[p],[q]]).

%   Negation of an underivable atom is free.

test(negation_of_an_unsupported_atom_holds) :-
    stable_models([(p :- not q)], M),
    assertion(M == [[p]]).

%   A fact defeats the negation that depends on it.

test(a_fact_defeats_the_negation) :-
    stable_models([q, (p :- not q)], M),
    assertion(M == [[q]]).

%   The three-cycle over negation: no answer set.  Any interpretation makes
%   exactly one of the three unsupported, which then supports the next.

test(an_odd_cycle_of_three_has_none) :-
    stable_models([(p :- not q), (q :- not r), (r :- not p)], M),
    assertion(M == []).

%   Stratified negation: exactly one answer set, the one the strata build.

test(stratified_negation_is_deterministic) :-
    stable_models([ a,
                    (b :- a),
                    (c :- not b),
                    (d :- not c)
                  ], M),
    assertion(M == [[a,b,d]]).

:- end_tests(sm_negation).

		 /*******************************
		 *         ODD LOOPS		*
		 *******************************/

%   p :- not p.  has no answer set.  Without p, the reduct keeps the rule
%   and derives p; with p, the reduct drops the rule and derives nothing.

:- begin_tests(sm_odd_loop).

test(the_smallest_odd_loop_has_no_answer_set) :-
    stable_models([(p :- not p)], M),
    assertion(M == []).

%   But it is harmless when the atom is derivable another way: {p} is then
%   stable, because the reduct still contains `p.' and derives p.

test(an_odd_loop_is_defused_by_another_rule) :-
    stable_models([p, (p :- not p)], M),
    assertion(M == [[p]]).

%   The classic guarded form: the loop rules out the models in which q
%   holds, leaving only those in which it does not.

%   q :- not r.  r :- not q.  p :- q, not p.
%
%   The odd loop forces q out: if q held, the third rule would need p, but
%   with p present that rule is deleted from the reduct and nothing else
%   supports p.  So {r} is the only answer set - the loop acts as `:- q'.
%   That equivalence is P2/P4's justification for compiling odd loops into
%   the NMR check (doc/literature.md), so it is worth pinning both ways.

test(an_odd_loop_acts_as_a_constraint) :-
    stable_models([ (q :- not r),
                    (r :- not q),
                    (p :- q, not p)
                  ], M),
    assertion(M == [[r]]).
test(and_agrees_with_writing_that_constraint_out) :-
    stable_models([ (q :- not r),
                    (r :- not q),
                    (:- q)
                  ], M),
    assertion(M == [[r]]).

:- end_tests(sm_odd_loop).

		 /*******************************
		 *       POSITIVE LOOPS		*
		 *******************************/

%   Positive loops are unfounded: nothing supports them from outside, so
%   the least model of the reduct never contains them.  This is exactly
%   where naive coinduction computes the greatest fixpoint instead and gets
%   {p,q} - the restriction in doc/literature.md (P2 3.3.3) exists to stop
%   that.

:- begin_tests(sm_positive_loop).

test(self_support_is_not_support) :-
    stable_models([(p :- p)], M),
    assertion(M == [[]]).
test(a_two_cycle_is_unfounded_too) :-
    stable_models([(p :- q), (q :- p)], M),
    assertion(M == [[]]).
test(but_an_external_fact_founds_it) :-
    stable_models([(p :- q), (q :- p), p], M),
    assertion(M == [[p,q]]).

:- end_tests(sm_positive_loop).

		 /*******************************
		 *        CONSTRAINTS		*
		 *******************************/

:- begin_tests(sm_constraints).

test(a_constraint_removes_the_offending_model) :-
    stable_models([(p :- not q), (q :- not p), (:- p)], M),
    assertion(M == [[q]]).
test(a_constraint_can_remove_every_model) :-
    stable_models([p, (:- p)], M),
    assertion(M == []).
test(a_constraint_that_bites_nothing_changes_nothing) :-
    stable_models([p, (:- q)], M),
    assertion(M == [[p]]).
test(a_negative_constraint_body) :-
    stable_models([(p :- not q), (q :- not p), (:- not p)], M),
    assertion(M == [[p]]).
test(a_conjunctive_constraint_needs_every_literal) :-
    stable_models([(p :- not q), (q :- not p), (:- p, q)], M),
    assertion(M == [[p],[q]]).

:- end_tests(sm_constraints).

		 /*******************************
		 *        ENTAILMENT		*
		 *******************************/

:- begin_tests(sm_entailment).

even_loop([(p :- not q), (q :- not p)]).

test(brave_entailment_needs_only_one_model) :-
    even_loop(P),
    assertion(brave_entails(P, p)),
    assertion(brave_entails(P, q)).
test(cautious_entailment_needs_all_of_them) :-
    even_loop(P),
    assertion(\+ cautious_entails(P, p)),
    assertion(\+ cautious_entails(P, q)).
test(a_fact_is_entailed_both_ways) :-
    assertion(brave_entails([p], p)),
    assertion(cautious_entails([p], p)).
test(negation_is_entailed_bravely_in_the_other_model) :-
    even_loop(P),
    assertion(brave_entails(P, not p)),
    assertion(brave_entails(P, not q)).
test(nothing_is_bravely_entailed_without_a_model) :-
    assertion(\+ brave_entails([(p :- not p)], p)),
    assertion(\+ brave_entails([(p :- not p)], not p)).

:- end_tests(sm_entailment).

		 /*******************************
		 *      THE REDUCT ITSELF	*
		 *******************************/

%   Checked directly, because every answer above rests on it.

:- begin_tests(sm_reduct).

test(a_rule_whose_negative_body_meets_the_model_is_deleted) :-
    reduct([rule(p, [], [q])], [q], R),
    assertion(R == []).
test(a_rule_whose_negative_body_misses_it_is_kept_without_negation) :-
    reduct([rule(p, [r], [q])], [], R),
    assertion(R == [definite(p, [r])]).
test(a_definite_rule_is_untouched) :-
    reduct([rule(p, [q], [])], [q], R),
    assertion(R == [definite(p, [q])]).

test(the_least_model_of_a_chain) :-
    least_model([definite(p, []), definite(q, [p]), definite(r, [q])], M),
    assertion(M == [p,q,r]).
test(the_least_model_ignores_unsupported_heads) :-
    least_model([definite(q, [p])], M),
    assertion(M == []).
test(the_least_model_of_a_positive_loop_is_empty) :-
    least_model([definite(p, [q]), definite(q, [p])], M),
    assertion(M == []).

:- end_tests(sm_reduct).

		 /*******************************
		 *          SYNTAX		*
		 *******************************/

:- begin_tests(sm_syntax).

test(the_herbrand_base_collects_every_atom) :-
    program_literals([(p :- q, not r), (:- s)], Atoms),
    assertion(Atoms == [p,q,r,s]).
test(a_number_is_not_a_literal,
     [ throws(error(type_error(literal, _), _)) ]) :-
    stable_models([1], _).
test(a_variable_is_rejected,
     [ throws(error(instantiation_error, _)) ]) :-
    stable_models([_], _).

:- end_tests(sm_syntax).

		 /*******************************
		 *    CLASSICAL NEGATION	*
		 *******************************/

%   `-p` is a literal in its own right: distinct from `p`, and distinct
%   again from `not p`.  What it adds to the definition is a consistency
%   requirement - no answer set contains both `p` and `-p` - so a program
%   deriving both has no answer set at all.

:- begin_tests(sm_classical_negation).

test(a_negative_literal_is_its_own_atom) :-
    stable_models([-p], M),
    assertion(M == [[-p]]).
test(it_does_not_make_the_positive_one_false) :-
    program_literals([-p, p], L),
    assertion(L == [p, -p]).
test(a_contradiction_has_no_answer_set) :-
    stable_models([p, -p], M),
    assertion(M == []).
test(and_takes_the_unrelated_atoms_with_it) :-
    stable_models([p, -p, q], M),
    assertion(M == []).
test(the_two_are_independent_when_both_are_derivable_apart) :-
    stable_models([(p :- not q), (-p :- not r)], M),
    assertion(M == []).            % both fire, so p and -p collide
test(but_not_when_only_one_fires) :-
    stable_models([r, (p :- not q), (-p :- not r)], M),
    assertion(M == [[p,r]]).

%   Default negation of a classical literal is a third thing again:
%   `not -p` asks whether -p is underivable, which it is here.

test(default_negation_of_a_classical_literal) :-
    stable_models([(q :- not -p)], M),
    assertion(M == [[q]]).
test(and_is_defeated_when_the_classical_literal_holds) :-
    stable_models([-p, (q :- not -p)], M),
    assertion(M == [[-p]]).

:- end_tests(sm_classical_negation).

		 /*******************************
		 *         GROUNDING		*
		 *******************************/

%   Rules with variables are instantiated over the constants in the
%   program.  There are no function symbols, so the Herbrand universe is
%   finite and the propositional machinery takes over from there.

:- begin_tests(sm_grounding).

test(a_ground_program_is_unchanged) :-
    ground_program([p, (q :- p)], G),
    assertion(G == [p, (q :- p)]).

test(a_rule_is_instantiated_over_the_constants) :-
    ground_program([q(a), q(b), (p(X) :- q(X))], G),
    assertion(memberchk((p(a) :- q(a)), G)),
    assertion(memberchk((p(b) :- q(b)), G)),
    assertion(\+ memberchk((p(a) :- q(b)), G)).

test(the_herbrand_base_is_over_ground_literals) :-
    program_literals([q(a), q(b), (p(X) :- q(X))], L),
    assertion(L == [p(a), p(b), q(a), q(b)]).

test(a_first_order_program_has_the_expected_model) :-
    stable_models([q(a), q(b), (p(X) :- q(X))], M),
    assertion(M == [[p(a), p(b), q(a), q(b)]]).

test(negation_works_per_instance) :-
    stable_models([ q(a),
                    r(b),
                    (p(X) :- q(X), not r(X))
                  ], M),
    assertion(M == [[p(a), q(a), r(b)]]).

test(two_variables_ground_independently) :-
    ground_program([e(a,b), (p(X,Y) :- e(X,Y))], G),
    assertion(memberchk((p(a,a) :- e(a,a)), G)),
    assertion(memberchk((p(a,b) :- e(a,b)), G)),
    assertion(memberchk((p(b,a) :- e(b,a)), G)),
    assertion(memberchk((p(b,b) :- e(b,b)), G)).

test(an_even_loop_per_constant) :-
    stable_models([ d(1), d(2),
                    (p(X) :- d(X), not q(X)),
                    (q(X) :- d(X), not p(X))
                  ], M),
    length(M, N),
    assertion(N == 4).                  % two independent choices

%   Unsafe rules are refused, because s(CASP) reads them over an extended
%   Herbrand universe and a finite grounding would disagree by design
%   rather than by defect.  See the module comment.

test(an_unsafe_rule_is_refused,
     [ throws(error(domain_error(safe_rule, _), _)) ]) :-
    stable_models([married(john), (:- not married(_))], _).
test(a_variable_only_in_a_negative_body_is_unsafe,
     [ throws(error(domain_error(safe_rule, _), _)) ]) :-
    stable_models([q(a), (p(X) :- not q(X))], _).
test(a_variable_in_the_head_alone_is_unsafe,
     [ throws(error(domain_error(safe_rule, _), _)) ]) :-
    stable_models([q(a), (p(_) :- q(a))], _).
test(variables_with_no_constants_are_refused,
     [ throws(error(domain_error(program_with_constants, _), _)) ]) :-
    stable_models([(p(X) :- q(X))], _).

:- end_tests(sm_grounding).

		 /*******************************
		 *    CONSTRAINTS IN BODIES	*
		 *******************************/

%   A CLP(Q) comparison over the finite domain the constants provide is
%   evaluated while grounding: an instance whose comparison is false is
%   dropped, a true one has the comparison removed.  That is what a
%   grounder does, and it leaves the propositional machinery untouched.

:- begin_tests(sm_clp).

domain([dom(1), dom(2), dom(3), dom(4)]).

with_rule(Rule, Program) :-
    domain(Domain),
    append(Domain, [Rule], Program).

test(a_threshold_selects_the_values_above_it) :-
    with_rule((p(X) :- dom(X), X #> 2), P),
    stable_models(P, [M]),
    assertion(memberchk(p(3), M)),
    assertion(memberchk(p(4), M)),
    assertion(\+ memberchk(p(2), M)).
test(an_equality_selects_one) :-
    with_rule((p(X) :- dom(X), X #= 2), P),
    stable_models(P, [M]),
    assertion(memberchk(p(2), M)),
    assertion(\+ memberchk(p(1), M)),
    assertion(\+ memberchk(p(3), M)).
test(a_disequality_selects_the_rest) :-
    with_rule((p(X) :- dom(X), X #<> 2), P),
    stable_models(P, [M]),
    assertion(\+ memberchk(p(2), M)),
    assertion(memberchk(p(1), M)),
    assertion(memberchk(p(4), M)).
test(two_comparisons_intersect) :-
    with_rule((p(X) :- dom(X), X #> 1, X #< 4), P),
    stable_models(P, [M]),
    assertion(memberchk(p(2), M)),
    assertion(memberchk(p(3), M)),
    assertion(\+ memberchk(p(1), M)),
    assertion(\+ memberchk(p(4), M)).
test(an_unsatisfiable_comparison_leaves_no_instance) :-
    with_rule((p(X) :- dom(X), X #> 9), P),
    stable_models(P, [M]),
    assertion(\+ memberchk(p(_), M)).

test(the_comparison_is_stripped_from_the_grounding) :-
    ground_program([dom(1), dom(2), (p(X) :- dom(X), X #> 1)], G),
    assertion(G == [dom(1), dom(2), (p(2) :- dom(2))]).

%   A comparison binds nothing, so it cannot make a variable safe.

test(a_comparison_does_not_make_a_variable_safe,
     [ throws(error(domain_error(safe_rule, _), _)) ]) :-
    stable_models([dom(1), (p(X) :- X #> 0)], _).

:- end_tests(sm_clp).
