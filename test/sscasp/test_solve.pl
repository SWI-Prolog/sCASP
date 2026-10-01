:- module(test_solve,
          [ test_solve/0
          ]).
:- use_module('../scasp_plunit', [with_scasp_flag/3]).
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/run)).
:- use_module(library(sscasp/solve), [check/7, done/4, proved/4,
                                      model/2, empty_proof/1]).
:- use_module('../stable_models', [stable_models/2, models_entail/2,
                              program_literals/2]).
:- use_module('../differential', [random_program/4]).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(lists), [member/2, memberchk/2, append/3]).

/** <module> The solver

`sscasp/run.pl` compiles a program, makes it runnable and answers a query
against it.  What the tests below are mostly about is the one thing Prolog
does not do by itself: deciding what to make of a literal that is already
assumed.

The last unit is the one that matters.  Everything else in this project
descends from the implementation it judges; `stable_models.pl` does not, being
written from the Gelfond-Lifschitz definition, so pointing the two at the same
program is the only check here that could catch a defect the design has always
had.  It found one — see `solve_loops` below.
*/

test_solve :-
    run_tests([ solve_worked,
                solve_justification,
                solve_loops,
                solve_constraints,
                solve_constructive,
                solve_clpq,
                solve_domains,
                solve_quantifiers,
                solve_collecting,
                solve_abducibles,
                solve_counting,
                solve_differential,
                solve_without_nmr,
                solve_tabling
              ]).

%!  models(+Program, ?Query, -Models) is det.

models(Program, Query, Models) :-
    findall(Query-Model, scasp(Program, Query, Model), Pairs),
    pairs_models(Pairs, Models).

pairs_models([], []).
pairs_models([_-Model|T], [Model|Rest]) :- pairs_models(T, Rest).

%!  answers(+Program, ?Query) is semidet.

answers(Program, Query) :-
    \+ \+ scasp(Program, Query, _).

		 /*******************************
		 *      WORKED PROGRAMS		*
		 *******************************/

:- begin_tests(solve_worked).

birds([ (flies(X) :- bird(X), not penguin(X)),
        (penguin(X) :- bird(X), abnormal(X)),
        bird(tweety),
        bird(polly),
        abnormal(polly)
      ]).

test(the_default_conclusion) :-
    birds(P),
    models(P, flies(tweety), Models),
    assertion(Models == [[ bird(tweety), flies(tweety),
                           not abnormal(tweety), not penguin(tweety) ]]).

%   The model holds what the proof needed and nothing else — `bird(polly)`
%   is a fact of the program but no part of this answer.

test(the_model_is_the_proof_not_the_program) :-
    birds(P),
    models(P, flies(tweety), [Model]),
    assertion(\+ memberchk(bird(polly), Model)).

test(the_exception) :-
    birds(P),
    assertion(\+ answers(P, flies(polly))).

%   Not through models/3: findall/3 copies, so the query's own bindings
%   have to be read from the call itself.

test(an_unbound_query_finds_the_one_answer) :-
    birds(P),
    findall(X, scasp(P, flies(X), _), Xs),
    assertion(Xs == [tweety]).

%   `p` and `-p` are different atoms, and the consistency constraint the
%   compiler generates is what stops a program deriving both.

test(classical_negation) :-
    models([(-(p) :- q), q], -(p), Models),
    assertion(Models == [[q, -p]]).

test(deriving_both_polarities_has_no_model) :-
    assertion(\+ answers([(-(p) :- q), p, q], p)).

:- end_tests(solve_worked).

		 /*******************************
		 *      JUSTIFICATION		*
		 *******************************/

%   The proof is what the solver threads, so asking why costs nothing
%   that answering did not already cost.  A node is `Literal-Children`,
%   with `chs/1` and `proved/1` around a literal established without
%   expanding it.

:- begin_tests(solve_justification).

%!  why(+Program, ?Query, -Forest) is semidet.
%
%   The first answer's justification, without the NMR node — which the
%   tests that care about it ask for separately.

why(Program, Query, Forest) :-
    scasp(Program, Query, _, Justification),
    !,
    once(append(Forest, ['$nmr'-_], Justification)).

test(a_fact_is_a_leaf) :-
    why([q(1)], q(1), Forest),
    assertion(Forest == [q(1)-[]]).

test(a_rule_has_its_body_as_children) :-
    why([(p :- q, r), q, r], p, Forest),
    assertion(Forest == [p-[q-[], r-[]]]).

test(and_the_children_are_in_call_order) :-
    why([(p :- r, q), q, r], p, Forest),
    assertion(Forest == [p-[r-[], q-[]]]).

test(the_tree_nests) :-
    why([(p :- q), (q :- r), r], p, Forest),
    assertion(Forest == [p-[q-[r-[]]]]).

%   A negated literal is a node like any other, and what proved it are
%   its children.

test(a_negation_is_a_node) :-
    why([(p :- not q)], p, Forest),
    assertion(Forest == [p-[(not q)-[]]]).

%   The two statuses.  Showing them is the point: a justification that
%   dropped them would claim a literal was proved from nothing.

test(a_coinductive_step_says_so) :-
    why([(p :- not q), (q :- not p)], p, Forest),
    assertion(Forest == [p-[(not q)-[chs(p)-[]]]]).

test(a_literal_an_earlier_branch_proved_says_so) :-
    Program = [ (flies(X) :- bird(X), not penguin(X)),
                (penguin(X) :- bird(X), abnormal(X)),
                bird(tweety),
                abnormal(polly)
              ],
    why(Program, flies(tweety), Forest),
    assertion(Forest ==
              [ flies(tweety)-[ bird(tweety)-[],
                                (not penguin(tweety))-
                                  [ proved(bird(tweety))-[],
                                    (not abnormal(tweety))-[]
                                  ]
                              ]
              ]).

%   A built-in is not a step.  `dif(X, 1)` is a constraint on the answer,
%   and it is already said where it belongs — on the variable.

test(a_builtin_is_not_a_step) :-
    why([(p(X) :- not q(X)), q(1)], p(2), Forest),
    assertion(Forest == [p(2)-[(not q(2))-[]]]).

%   The query and the check are the same proof but not the same
%   question, so the justification says which is which.

test(the_check_has_a_node_of_its_own) :-
    scasp([p, (:- q)], p, _, Justification),
    !,
    assertion(Justification == [p-[], '$nmr'-[(not q)-[]]]).

test(and_is_empty_when_there_is_nothing_to_check) :-
    scasp([p], p, _, Justification),
    !,
    assertion(Justification == [p-[], '$nmr'-[]]).

%   The model is the literals of the forest and nothing else, so it is
%   not carried separately — which is `doc/design-next.md` open question
%   5's property, asserted rather than assumed.

answerable([(p :- q, r), q, r],                            p).
answerable([(p :- not q), (q :- not p)],                   p).
answerable([(flies(X) :- bird(X), not penguin(X)),
            (penguin(X) :- bird(X), abnormal(X)),
            bird(tweety)],                                 flies(tweety)).
answerable([p, (:- q)],                                    p).
answerable([(p(X) :- not q(X)), q(1)],                     p(2)).
answerable([(-(p) :- q), q],                               -(p)).
answerable([(c :- c)],                                     not c).

test(the_model_is_the_literals_of_the_proof,
     [forall(answerable(Program, Query))]) :-
    scasp(Program, Query, Model, Justification),
    !,
    literals_of(Justification, Literals),
    sort(Literals, Expected),
    assertion(Model == Expected).

%   `'$nmr'` is the one node that is not a literal: it groups what the
%   check needed rather than naming something proved.

literals_of([], []).
literals_of(['$nmr'-Children|T], Literals) :-
    !,
    literals_of(Children, Sub),
    literals_of(T, Rest),
    append(Sub, Rest, Literals).
literals_of([Node-Children|T], Literals) :-
    unwrap(Node, Literal),
    literals_of(Children, Sub),
    literals_of(T, Rest),
    append([Literal|Sub], Rest, Literals).

unwrap(chs(L), L) :- !.
unwrap(proved(L), L) :- !.
unwrap(L, L).

:- end_tests(solve_justification).

		 /*******************************
		 *          LOOPS		*
		 *******************************/

%   The cases the coinductive check exists for.  `check/8` sees a goal,
%   its negation, its polarity, the ancestors and what is proved, and
%   decides between failing, succeeding and expanding.

:- begin_tests(solve_loops).

test(an_even_loop_over_negation_has_two_models) :-
    Program = [(p :- not q), (q :- not p)],
    models(Program, p, [Mp]),
    models(Program, q, [Mq]),
    assertion(Mp == [p, not q]),
    assertion(Mq == [q, not p]).

test(an_odd_loop_over_negation_has_none) :-
    assertion(\+ answers([(p :- not p)], p)).

%   A literal supported only by itself is unfounded, so it fails: a
%   stable model is a least fixpoint, not a greatest one.

test(a_positive_loop_supports_nothing) :-
    assertion(\+ answers([(p :- q), (q :- p)], p)).

%   The same loop, negated, says the opposite — and has to.  Dualising
%   `c :- c` gives `not c :- not c`, and `c` being unfounded is exactly
%   why `not c` holds.  Failing here would make `not c` unprovable for
%   every unfounded `c`.
%
%   This is the defect the oracle found: with a zero-transition loop
%   failing on both polarities, `[a, b, (c :- c), (:- not c, not b)]` has
%   the answer set {a, b} and nothing could reach it.

test(and_the_same_loop_negated_succeeds) :-
    models([(c :- c)], not c, Models),
    assertion(Models == [[not c]]).

test(the_program_that_found_it) :-
    Program = [a, b, (c :- c), (:- not c, not b)],
    assertion(answers(Program, a)),
    assertion(answers(Program, b)),
    assertion(answers(Program, not c)),
    assertion(\+ answers(Program, c)).

%   A longer even loop, to check that the count is of *negations* and not
%   of frames: the path p -> not q -> r -> not s -> p crosses four
%   polarity changes but only two `dual` predicates.

test(a_four_cycle_is_even) :-
    Program = [(p :- not q), (q :- not r), (r :- not s), (s :- not p)],
    assertion(answers(Program, p)),
    assertion(answers(Program, q)).

test(a_three_cycle_is_odd) :-
    Program = [(p :- not q), (q :- not r), (r :- not p)],
    assertion(\+ answers(Program, p)).

%   The same loop through a quantifier the program writes: `p` holds iff
%   no `r(X)` does, and `r(X)` holds iff `p` does.  No answer set, and
%   the check that says so is only generated if the call graph looks
%   inside the `forall/2`.

test(an_odd_loop_through_a_written_forall) :-
    Program = [q, (p :- forall(X, not r(X))), (r(_) :- p)],
    assertion(\+ answers(Program, q)).

test(and_through_a_negated_one) :-
    Program = [q, (p :- not forall(X, r(X))), (r(_) :- p)],
    assertion(\+ answers(Program, q)).

%   A positive loop hidden behind a negation.  `p` needs `r` and `r`
%   needs `p`, which is a loop over no negation and so unfounded; the
%   only answer set is {q}.  The solver used to answer `p` anyway, with
%   the model `[p, r, not q]` — and then fail `r`, which that very model
%   contains.
%
%   The route was `p` -> `not q` -> (`q`'s dual needs `r`) -> `r` ->
%   `r :- p` -> **chs(p)**, two negations, so the coinductive hypothesis
%   let it stand.  `r` was then reused for `p`'s own body as *proved*,
%   which closed the positive loop without anything noticing: `r`'s proof
%   rested on assuming `p`.
%
%   The old implementation answers `p` here too, so this was inherited
%   rather than introduced.  What fixed it is `check/7` reusing only a
%   proof that rests on no assumption — see `solve_counting` below.

unfounded([ (p :- not q, r),
            (q :- not r),
            (r :- p)
          ]).

test(a_positive_loop_behind_a_negation_is_still_unfounded) :-
    unfounded(Program),
    assertion(\+ answers(Program, p)),
    assertion(\+ answers(Program, r)),
    assertion(answers(Program, q)).

%   The same shape *without* the positive loop does have the answer set
%   {p, r}, and the two differ only in `r` being in `p`'s body.  So no
%   rule about the loop alone tells them apart, which is why the fix is
%   about what a proof may be reused for.

test(but_the_same_shape_without_it_is_founded) :-
    Program = [(p :- not q), (q :- not r), (r :- p)],
    assertion(answers(Program, p)),
    assertion(answers(Program, r)),
    assertion(answers(Program, q)).

%   The program the oracle found it on, which is the same defect with an
%   odd loop and a denial on top.

test(the_program_the_oracle_found_it_on) :-
    Program = [ (a :- not b, c),
                (a :- not c, b),
                (b :- a),
                (b :- b, not b),
                (c :- not b, not c),
                (false :- b, not b)
              ],
    assertion(\+ answers(Program, a)),
    assertion(\+ answers(Program, b)),
    assertion(\+ answers(Program, c)).

%   A goal that is *less constrained* than an ancestor is a more general
%   question, not a loop.  Below `p(A)` with `A #> 3`, `q(A)` asks
%   `p(_)` with nothing on it, and `p(1)` answers that; so every A > 3 is
%   an answer.  Read as an instance of the ancestor, `p(_)` was narrowed
%   to `A > 3` and failed as unfounded, and the second answer was lost.
%   The old implementation gives it.

constrained_answer(Program, Query, Var, Constraints) :-
    scasp(Program, Query, _),
    copy_term(Var, Copy, Constraints0),
    Constraints0 \== [],
    Var = Copy,
    Constraints = Constraints0.

test(a_less_constrained_goal_is_not_a_loop_dense) :-
    Program = [p(1), (p(X) :- X #> 3, q(X)), (q(_) :- p(_))],
    assertion(constrained_answer(Program, p(A), A, [{A > 3}])).

test(a_less_constrained_goal_is_not_a_loop_herbrand) :-
    Program = [p(a), (p(X) :- X \= b, q(X)), (q(_) :- p(_))],
    assertion(constrained_answer(Program, p(A), A, [A ∉ [b]])).

:- end_tests(solve_loops).

		 /*******************************
		 *   COUNTING THE NEGATIONS	*
		 *******************************/

%   The number of negations between a goal and an ancestor is the number
%   of polarity transitions on the path.  Counting `dual` frames instead
%   would undercount: dualising `q :- not p` turns `not q`'s body into a
%   positive call to `p`, so one of the two negations in
%   `p :- not q. q :- not p.` is consumed by the compilation.
%
%   These call `check/8` on hand-built chains, which is the one place the
%   counting can be looked at directly.

:- begin_tests(solve_counting).

frame(Goal, Neg, Pol, a(Goal, Neg, Pol)).

%!  decision(+Goal, +Pol, +Chain, -Action) is semidet.
%
%   Chain is a list of Goal-Polarity pairs, nearest ancestor first.

decision(Goal, Pol, Chain, Action) :-
    ancestors(Chain, Anc),
    negation(Goal, Pol, Neg),
    empty_proof(Proof),
    check(Goal, Neg, Pol, Anc, Proof, _, Action).

ancestors([], []).
ancestors([Goal-Pol|T], [Frame|Rest]) :-
    negation(Goal, Pol, Neg),
    frame(Goal, Neg, Pol, Frame),
    ancestors(T, Rest).

%   The compiler emits a goal's negation as a constant; here it is a
%   naming convention, so that a chain can be written down.

negation(Goal, +, not(Goal)) :- !.
negation(not(Goal), -, Goal) :- !.
negation(Goal, -, Goal).

test(no_ancestors_means_expand) :-
    decision(p, +, [], Action),
    assertion(Action == expand).

test(an_unrelated_ancestor_means_expand) :-
    decision(p, +, [q-(+)], Action),
    assertion(Action == expand).

%   Zero transitions: a positive loop.

test(a_positive_self_loop_fails) :-
    assertion(\+ decision(p, +, [p-(+)], _)).

test(a_negative_self_loop_succeeds) :-
    decision(not(p), -, [not(p)-(-)], Action),
    assertion(Action == chs).

%   Two transitions: coinductive success, on either side.

test(two_transitions_succeed) :-
    decision(p, +, [not(q)-(-), p-(+)], Action),
    assertion(Action == chs).

test(and_four) :-
    decision(p, +, [not(s)-(-), r-(+), not(q)-(-), p-(+)], Action),
    assertion(Action == chs).

%   An ancestor of the *opposite* polarity is a contradiction, whatever
%   the path length — and this is where an odd loop lands, which is why
%   there is no odd case in the count.

test(the_negation_as_an_ancestor_fails) :-
    assertion(\+ decision(p, +, [not(p)-(-)], _)).

test(and_further_up) :-
    assertion(\+ decision(p, +, [r-(+), q-(+), not(p)-(-)], _)).

%   A proved literal contradicts just as an open one does: it makes no
%   difference which branch of the proof put it there.

test(a_proved_negation_fails) :-
    negation(p, +, Neg),
    empty_proof(Empty),
    done(not(p), proof, Empty, Proof),
    assertion(\+ check(p, Neg, +, [], Proof, _, _)).

test(a_ground_proved_goal_needs_no_expansion) :-
    negation(p, +, Neg),
    empty_proof(Empty),
    done(p, proof, Empty, Proof),
    check(p, Neg, +, [], Proof, _, Action),
    assertion(Action == proved).

%   And one proved inside a sub-proof counts too: it makes no difference
%   to a model which branch of the proof established it.  The index holds
%   every node for that reason, not only the ones still at the top level.

%   But a proof that **rests on an assumption** is not reused.  It was
%   proved, and the model is right to hold it, yet leaning on it again is
%   how a positive loop closes unnoticed: the literal is only there
%   because the goal now asking for it was assumed.  P4 §3.2 offers the
%   reuse as an optimisation — "it avoids recomputation and keeps the
%   justification tree smaller" — and the old implementation takes it
%   unconditionally, which is why it answers `p` for the program in
%   `solve_loops` above.
%
%   Refusing it where the proof leaned on an assumption costs `make
%   check` 4.7 s against 6.8 s, the differential sweep 4%, and the golden
%   corpus nothing at all.  Refusing it *outright* costs the sweep 19.5 s
%   against 217 s, which is why the rule is about the proof rather than
%   about the reuse.

test(a_proof_resting_on_an_assumption_is_not_reused) :-
    negation(p, +, Neg),
    empty_proof(Empty),
    done(q, chs, Empty, Assumed),
    frame(p, not(p), +, Frame),
    proved([Frame], Empty, Assumed, Proof),
    check(p, Neg, +, [], Proof, _, Action),
    assertion(Action == expand).

%   Where nothing was assumed, it is reused as before.

test(and_one_that_rests_on_nothing_still_is) :-
    negation(p, +, Neg),
    empty_proof(Empty),
    done(q, proof, Empty, Body),
    frame(p, not(p), +, Frame),
    proved([Frame], Empty, Body, Proof),
    check(p, Neg, +, [], Proof, _, Action),
    assertion(Action == proved).

test(a_negation_proved_deeper_down_also_fails) :-
    negation(p, +, Neg),
    empty_proof(Empty),
    done(not(p), proof, Empty, Inner),
    frame(q, not(q), +, Frame),
    proved([Frame], Empty, Inner, Proof),
    assertion(\+ check(p, Neg, +, [], Proof, _, _)).

:- end_tests(solve_counting).

		 /*******************************
		 *   CONSTRAINTS AND CHECKS	*
		 *******************************/

:- begin_tests(solve_constraints).

test(a_global_constraint_rules_a_model_out) :-
    assertion(\+ answers([(:- p, q), p, q], p)).

test(and_leaves_the_others) :-
    models([(:- p, q), p], p, Models),
    assertion(Models == [[p, not q]]).

%   The check runs against what the query proved, so a constraint the
%   query's own answer violates is caught rather than checked in a
%   vacuum.

test(the_check_sees_the_query_s_own_proof) :-
    assertion(\+ answers([(:- p), p], p)).

test(a_constraint_on_nothing_the_query_reaches_still_applies) :-
    assertion(\+ answers([(:- q), q, p], p)).

%   `true` and `fail` negate to each other.  `not true` reads oddly and
%   is what a generated program writes when a body literal turns out to
%   be unconditionally true.

test(the_negation_of_true_is_fail) :-
    assertion(\+ answers([(p :- not true)], p)),
    assertion(answers([(p :- not fail)], p)).

%   A built-in with a partner, written negated, is its partner:
%   `not X = a` is `X \= a`, `not X #< 3` is `X #>= 3`.  It used to be
%   passed through as `not(X = a)`, which nothing could run.

test(a_negated_builtin_is_its_partner) :-
    Program = [r(a), r(b), (p(X) :- r(X), not X = a)],
    models(Program, p(_), Models),
    assertion(Models == [[p(b), r(b)]]),
    assertion(answers([(q(X) :- not X #< 3)], q(5))),
    assertion(\+ answers([(q(X) :- not X #< 3)], q(1))).

%   A **query** is a clause body and may hold a constraint like one.
%   `?- T #< 40, holds(T, ...)` is an ordinary s(CASP) query; calling
%   each query literal as a predicate looks for a `#</5`.

test(a_query_may_hold_a_constraint) :-
    Program = [ (p(X) :- dom(X)), dom(1), dom(7) ],
    findall(X, scasp(Program, (X #< 5, p(X)), _), Xs),
    assertion(Xs == [1]).

test(and_a_constraint_alone) :-
    findall(X-Store,
            ( scasp([p(1)], (X #> 3, X #< 5), _),
              copy_term(X, _, Store) ),
            Answers),
    assertion(Answers = [_]).

:- end_tests(solve_constraints).

		 /*******************************
		 *   CONSTRUCTIVE NEGATION	*
		 *******************************/

%   `not q(X)` for an unbound X must say *which* X, not merely fail.
%   `dif/2` is what says it — decision 4 — and the answer carries the
%   constraint rather than a copy of it.

:- begin_tests(solve_constructive).

test(a_ground_negation) :-
    models([(p(X) :- not q(X)), q(1)], p(2), Models),
    assertion(Models == [[not q(2), p(2)]]).

test(an_unbound_negation_constrains_the_query) :-
    scasp([(p(X) :- not q(X)), q(1)], p(V), _),
    !,
    assertion(var(V)),
    copy_term(V, _, Attributes),
    assertion(Attributes =@= ['∉'(_, [1])]).

%   And the model shares with the query.  It is collected by walking
%   rather than with findall/3, which copies: a model collected that way
%   carries none of the constraints that say what the answer *is*.

test(the_model_shares_with_the_query) :-
    scasp([(p(X) :- not q(X)), q(1)], p(V), Model),
    !,
    assertion(Model = [not q(W), p(W)]),
    Model = [_, p(W2)],
    assertion(V == W2).

test(a_value_the_constraint_excludes_has_no_answer) :-
    assertion(\+ answers([(p(X) :- not q(X)), q(1)], p(1))).

:- end_tests(solve_constructive).

		 /*******************************
		 *          CLP(Q)		*
		 *******************************/

%   Decision 4 keeps two stores because two stores is what s(CASP) is: a
%   dense order for arithmetic and Herbrand disunification for the rest.
%   A comparison is a constraint, not a literal, so it is not in the
%   model and not a step in the justification — it is on the variable.

:- begin_tests(solve_clpq).

test(a_constraint_that_holds) :-
    models([(p(X) :- X #> 3), (q :- p(5))], q, Models),
    assertion(Models == [[q, p(5)]]).

test(a_constraint_that_does_not) :-
    assertion(\+ answers([(p(X) :- X #> 3), (q :- p(2))], q)).

test(an_unbound_argument_carries_the_region) :-
    scasp([(p(X) :- X #> 3)], p(V), _),
    !,
    copy_term(V, _, Attributes),
    assertion(Attributes =@= [{_ > 3}]).

%   The dual of a comparison is a comparison — `dual.pl` pairs them — so
%   negation over the dense domain needs nothing the clause duals do not
%   already do.

test(the_negation_of_a_region_is_its_complement) :-
    scasp([(p(X) :- X #> 3)], not p(V), _),
    !,
    copy_term(V, _, Attributes),
    assertion(Attributes =@= [{_ =< 3}]).

%   The complement of an interval is two regions, and they arrive as two
%   answers because that is what they are.  How many answers a solution
%   set arrives in is an implementation choice; that between them they
%   are the complement is not.

test(the_complement_of_an_interval_is_two_answers) :-
    findall(A,
            ( scasp([(p(X) :- X #> 3, X #< 10)], not p(V), _),
              copy_term(V, _, A)
            ),
            Attributes),
    assertion(Attributes =@= [[{_ =< 3}], [{_ >= 10}]]).

test(a_value_in_the_hole_is_excluded) :-
    assertion(\+ answers([(p(X) :- X #> 3, X #< 10)], not p(5))).

test(and_one_outside_it_is_not) :-
    assertion(answers([(p(X) :- X #> 3, X #< 10)], not p(20))).

%   A comparison is not a step in the proof: it says what the answer *is*
%   rather than being part of establishing it.

test(a_comparison_is_not_in_the_justification) :-
    scasp([(p(X) :- X #> 3), (q :- p(5))], q, _, Justification),
    !,
    assertion(Justification == [q-[p(5)-[]], '$nmr'-[]]).

:- end_tests(solve_clpq).

		 /*******************************
		 *         DOMAINS		*
		 *******************************/

%   A declared domain turns a quantifier into an enumeration, which is
%   decision 5's case 1 and the whole argument for annotations.

:- begin_tests(solve_domains).

%   A signature `:- pred q(d)` is shorthand for guards (`doc/design-next.md`
%   open question 3): `d(X)` in every clause of `q`, and in front of every
%   `not q(T)` a body or a query writes.  So `q` is false outside the
%   domain, and a negation written in the program asks only about the
%   domain: `not q(_)` is witnessed by the values of `d` that `q` is false
%   of and by nothing else.

%   The program that showed the signature guarding only the dual: `q(3)`
%   held and `not q(3)` failed, and `outside` and `not outside` were both
%   proved.

signature_program([ (:- domain d = [1, 2]),
                    (:- pred q(d)),
                    r(1), r(2), r(3),
                    (q(X) :- r(X)),
                    (outside :- q(W), not d(W))
                  ]).

test(a_value_outside_the_domain_is_neither) :-
    signature_program(P),
    assertion(\+ answers(P, q(3))),
    assertion(\+ answers(P, not q(3))).

test(so_a_literal_and_its_negation_never_both_hold) :-
    signature_program(P),
    assertion(\+ answers(P, outside)),
    assertion(answers(P, not outside)).

test(a_domain_enumerates_the_witnesses) :-
    Program = [ (:- domain d = [a, b, c]),
                (:- pred q(d)),
                (p :- not q(_)),
                q(a)
              ],
    models(Program, p, Models),
    assertion(( member(M, Models), memberchk(not(q(b)), M) )),
    assertion(( member(M, Models), memberchk(not(q(c)), M) )),
    assertion(\+ ( member(M, Models), memberchk(not(q(a)), M) )),
    assertion(\+ ( member(M, Models), memberchk(not(d(_)), M) )).

%   The case that decided it: with `q` true of the whole domain there is
%   nothing for `not q(_)` to be true of.

test(and_says_no_when_the_domain_is_exhausted) :-
    Program = [ (:- domain d = [a, b]),
                (:- pred q(d)),
                (p :- not q(_)),
                q(a), q(b)
              ],
    assertion(\+ answers(Program, p)),
    assertion(answers(Program, not p)).

%   Under `scasp_domain_error` a ground value outside the domain is an
%   error, asked either way; an unbound one is still only asked about
%   the domain.

test(a_value_outside_the_domain_raises,
     [ forall(member(Query, [q(3), not q(3)])),
       error(domain_error(d, 3), context(q/1, _))
     ]) :-
    signature_program(P),
    with_scasp_flag(scasp_domain_error, true, answers(P, Query)).

test(an_unbound_one_does_not) :-
    signature_program(P),
    with_scasp_flag(scasp_domain_error, true,
                    ( assertion(answers(P, not outside)),
                      assertion(\+ answers(P, not q(_)))
                    )).

%   Without a domain the same program still answers, because the
%   quantifier is not reached: `not q(X)` in a positive body is
%   existential, and only a *dual* quantifies.

test(an_undomained_body_variable_is_existential) :-
    assertion(answers([(p :- not q(_)), q(1)], p)).

%   A range is a domain too, and the one the enumeration would be wrong
%   for: `0..23` is written that way precisely to avoid listing it.  It
%   compiles to a CLP(Q) region, and the quantifier covers the region.

hours([ (:- domain hour = 0..23),
        (:- pred q(hour)),
        q(9)
      ]).

covered([ (:- domain hour = 0..23),
          (:- pred q(hour)),
          (q(H) :- H #>= 0, H #=< 23)
        ]).

test(a_gap_in_the_range) :-
    hours(P),
    models(P, not q(3), Models),
    assertion(Models == [[hour(3), not q(3)]]).

%   `not q(V)` for an unbound V holds of every hour but 9, and of
%   nothing that is not an hour.

test(an_unbound_negation_over_a_range, [forall(member(V-Holds, [3-true, 9-false, 99-false, -1-false]))]) :-
    hours(P),
    (   answers(P, not q(V))
    ->  assertion(Holds == true)
    ;   assertion(Holds == false)
    ).

test(and_its_answer_is_constrained) :-
    hours(P),
    once(scasp(P, not q(V), _)),
    copy_term(V, _, Attributes),
    assertion(Attributes =@= [{H >= 0, H =< 23}, '∉'(H, [9])]).

%   The quantifier really does cover the region: with `q` true of every
%   hour there is no hour it is false of.

test(a_range_with_no_gap_has_no_negation) :-
    covered(P),
    assertion(\+ answers(P, not q(3))),
    assertion(\+ answers([(p :- not q(_))|P], p)).

%   The values of a domain are a conjunction, proved in **one** proof.
%   Each value used to be proved from the caller's proof and none was
%   kept, so `ok(1)` could rest on `a` and `ok(2)` on `b` with nothing
%   noticing that the two cannot both hold, and `q` was answered for a
%   program with no answer set.  Fact unfolding hides it by compiling
%   the quantifier away, so it is asked twice: two values with
%   unfolding off, and nine, more than `sscasp_unfold_limit`, with the
%   defaults.

%!  choice_program(+N, +Last, -Program) is det.
%
%   `ok(1..N-1)` need `a`, `ok(N)` needs Last, and the constraint needs
%   every `ok`.  With Last = `b` there is no answer set.

choice_program(N, Last, Program) :-
    findall(d(I), between(1, N, I), Ds),
    N1 is N-1,
    findall((ok(I) :- a), between(1, N1, I), Oks),
    append([ Ds, Oks,
             [ (ok(N) :- Last),
               (a :- not b), (b :- not a),
               (:- d(X), not ok(X)),
               q
             ]
           ], Program).

domain_case(2, false).
domain_case(9, true).

test(the_values_of_a_domain_hold_together,
     [forall(domain_case(N, Unfold))]) :-
    choice_program(N, b, P),
    (   N =< 2                          % the oracle is exponential
    ->  assertion(stable_models(P, []))
    ;   true
    ),
    with_scasp_flag(sscasp_unfold_facts, Unfold,
                    assertion(\+ answers(P, q))).

%   And what they needed is in the model.

test(and_what_they_needed_is_in_the_model,
     [forall(domain_case(N, Unfold))]) :-
    choice_program(N, a, P),
    with_scasp_flag(sscasp_unfold_facts, Unfold,
                    models(P, q, Models)),
    assertion(Models \== []),
    forall(member(M, Models),
           ( assertion(memberchk(a, M)),
             assertion(memberchk(ok(N), M))
           )).

:- end_tests(solve_domains).

		 /*******************************
		 *       QUANTIFIERS		*
		 *******************************/

%   `forall/2` is one recursion for all of decision 5's decidable cases:
%   solve once, see what part of the domain the answer covered, cover the
%   rest the same way.  They differ only in how a complement is taken.

:- begin_tests(solve_quantifiers).

%   Case 2: the answer left the variable alone, so it holds everywhere.

%   `q(_)` is true of everything, so the inner dual of `p :- not q(X)`
%   succeeds without looking at X at all.

test(an_answer_that_constrains_nothing_covers_everything) :-
    models([(p :- not q(_)), q(_)], not p, Models),
    assertion(Models = [_]).

%   And where it does not hold for every value, the quantifier says so:
%   `q(X) :- r(X)` with no `r` is false of everything.

test(and_fails_where_the_goal_does_not_hold_everywhere) :-
    assertion(\+ answers([(p :- not q(_)), (q(X) :- r(X))], not p)).

%   Case 3: the answer left disequalities, so the prohibited values are
%   what is left to check — a finite set.

test(a_finite_prohibited_set_is_checked) :-
    Program = [ (p(X) :- dom(X), not r(X)),
                dom(1), dom(2),
                r(1), r(2)
              ],
    assertion(\+ answers(Program, p(_))).

test(and_a_gap_in_it_is_found) :-
    Program = [ (p(X) :- dom(X), not r(X)),
                dom(1), dom(2),
                r(1)
              ],
    findall(X, scasp(Program, p(X), _), Xs),
    assertion(Xs == [2]).

%   ## One quantifier for all of a clause's body-only variables
%
%   `dualise.pl` puts them in one `forall/2` and the solver covers the
%   tuple, rather than nesting one quantifier each.  The two say the same
%   thing; nesting costs the *product* of the coverings, because the
%   inner one is re-covered once per region of the outer.  None of the
%   five event calculus programs finished while it nested.
%
%   What the tuple changes here is what an answer that **binds** may do.
%   Per variable it could be rejected outright and the goal asked for
%   another — a point covers nothing of an infinite domain — but a tuple
%   answer with one component bound is the ordinary case, and refusing it
%   costs `queens.pl` and `family.pl` every answer they have.  So a
%   ground binding is accepted and its complement is the disequality.

test(a_binding_answer_is_covered_by_its_complement) :-
    Program = [ (p(X) :- dom(X), not r(X)),
                dom(1), dom(2), dom(3),
                (r(X) :- dom(X), X #> 1)
              ],
    findall(X, scasp(Program, p(X), _), Xs),
    assertion(Xs == [1]).

%   Case 4 again, with the region stated in terms of a variable the
%   quantifier is *not* covering.  `v(B) :- X #>= B` has X body-only, so
%   `not v(B)` is `forall(X, X #< B)` — true for no B at all.  Projecting
%   the answer onto X alone eliminates B and reports that the answer
%   covered everything, and then `not v(B)` succeeds for a free B while
%   correctly failing for a ground one.  The goal's own variables stay in
%   the projection for that reason.

relative([(v(B) :- _X #>= B)]).

test(a_region_may_be_relative_to_the_goals_other_variables) :-
    relative(Program),
    assertion(answers(Program, v(3))),
    assertion(\+ answers(Program, not v(3))),
    assertion(\+ answers(Program, not v(_))).

%   ## One covering, and then somewhere else
%
%   A covering is committed to: the goal holding everywhere is one fact,
%   however many ways there are to show it, so a second covering of the
%   same ground is a second copy of the same answer.  What is a second
%   answer is the goal holding everywhere while the *other* variables
%   are somewhere else, and those come from partitioning them against
%   what the covering fixed them to.
%
%   `s(B) :- u(X), X #> B, X #< B + 3` over `u(5)` is the plainest case:
%   `not s(B)` holds where 5 is outside the window, which is B >= 5 or
%   B =< 2 — two disjoint regions, and the second is reachable only
%   through the partitioning.

windowed([ (s(B) :- u(X), X #> B, X #< B + 3), u(5) ]).

%   It is also the test that fails if `covered/7` discards the
%   complement's store outright instead of carrying its narrowing out:
%   the branch covering X = 5 is what says anything about B at all, and
%   without it `not s(B)` succeeds for every B, `not s(4)` included.

test(a_committed_covering_still_answers_for_the_other_regions) :-
    windowed(Program),
    findall(Store,
            ( scasp(Program, not s(B), _),
              copy_term(B, _, Store) ),
            Stores),
    assertion(Stores =@= [[{_ >= 5}], [{_ =< 2}]]).

test(and_for_no_region_between_them) :-
    windowed(Program),
    assertion(answers(Program, not s(6))),
    assertion(answers(Program, not s(0))),
    assertion(\+ answers(Program, not s(4))).

%   The commitment is to the covering of the whole *nest*, not to one
%   per quantifier in it — `dualise.pl` nests one per body-only variable,
%   and the outermost's search runs through them all, so committing to
%   its first success commits to the first covering of the nest.  That is
%   what `solve_c_forall/8` commits to as well, having collected the nest
%   into one `c_forall(Vars, Goal)` first.
%
%   Committing at each level separately is a much stronger commitment.
%   It was measured rather than argued: with it, `make check` takes 9.2 s
%   against 4.8 s and `bec_light` 0.43 s against 0.25 s, for no answer
%   either way.
%
%   Below is `family.pl`'s NMR check, which is what makes a nest of
%   quantifiers the ordinary case rather than a corner: three predicates
%   on odd loops through each other, whose sub-checks quantify over two
%   variables each.  It had no answers at all while the covering was
%   committed to but a region was still projected onto the quantified
%   variable alone.

nested([ (test1(X1) :- not test2(X1, _Y)),
         (test1(_X2)),
         (test2(X3, Y3) :- Y3 \= 1, not test3(X3)),
         (test2(X4, 1) :- not test3(X4)),
         (test3(X5) :- not test1(X5)),
         ok
       ]).

test(the_commitment_is_to_the_whole_nest) :-
    nested(Program),
    assertion(answers(Program, ok)),
    assertion(answers(Program, test1(a))).

%   ## A disequality between compound terms
%
%   `f(X, Y) \= f(1, 2)` says the *pair* is not (1, 2), and that is not a
%   prohibited value for either variable: X may be 1 as long as Y is not
%   2.  `dif/2` carries the disjunction; decomposing it to the first
%   position that could differ — which is what
%   `clp/disequality.pl:loop_term/2` does upstream and what this once
%   did for `consistent/3` — commits to `X \= 1` and loses the answer
%   X = 1, Y = 1.
%
%   The old implementation keeps the disjunction as a **branch** and
%   gets the three answers as four, `X = 2, Y = 1` twice.  So this is
%   what `dif/2` is here for, and it earns its keep once rather than per
%   answer.

test(a_disequality_between_compound_terms_is_a_disjunction) :-
    Program = [ d(1), d(2),
                (p(X, Y) :- f(X, Y) \= f(1, 2), d(X), d(Y))
              ],
    findall(X-Y, scasp(Program, p(X, Y), _), Pairs),
    assertion(Pairs == [1-1, 2-1, 2-2]).

%   `consistent/3` posts one between **whole literals**, so it carries
%   the same disjunction.  `p(X, Y)` is asked while `not p(1, 2)` is
%   proved, so the pair is kept away from (1, 2) and from nothing else;
%   decomposing that to `X \= 1` loses X = 1, Y = 1.

test(and_so_is_the_one_the_coinductive_check_posts) :-
    Program = [ d(1), d(2), s(1, 2),
                (q(X, Y) :- not p(1, 2), r(X, Y)),
                (r(X, Y) :- p(X, Y), d(X), d(Y)),
                (p(X, Y) :- d(X), d(Y), not s(X, Y))
              ],
    findall(X-Y, scasp(Program, q(X, Y), _), Pairs),
    assertion(Pairs == [1-1, 2-1, 2-2]).

%   ## A disequality the quantifier must not lose
%
%   `lion.pl`, and the reason it used to overflow the stack: reading a
%   variable's `dif/2` constraints with `dif:attribute_goals/3`
%   **deletes** them, so the narrowing a branch had applied was gone by
%   the time the branch solved, and the same two complement pieces came
%   back for ever — a narrowing 964 deep.
%
%   It is a soundness bug before it is a performance one.  `rat` lives in
%   an apartment, so `-live(rat, Z)` — which needs some place `rat` lives
%   that is not Z — holds for every Z but `apartment`.  Without the fix
%   the disequality keeping Z away from `apartment` was read and thereby
%   deleted, and `-live(rat, apartment)` came out true as well.

lion([ animal(lion), animal(snake), animal(monkey), animal(rat),
       live(monkey, forest), live(snake, zoo), live(rat, apartment),
       (live(X, forest) :- animal(X), not ab(live_animal(X)),
                           not -live(X, forest)),
       (-(live(X, Z)) :- live(X, Y), Y \= Z)
     ]).

test(a_disequality_the_quantifier_reads_is_still_there) :-
    lion(Program),
    assertion(answers(Program, -live(rat, forest))),
    assertion(\+ answers(Program, -live(rat, apartment))),
    assertion(answers(Program, not -live(rat, apartment))),
    assertion(answers(Program, live(lion, _))).

%   ## The complement of a disequality the quantifier is covering
%
%   Dualising `attack(_, Y, [q(_, Y)|_])` gives a quantifier over the
%   body-only variables of the head: for all D and R,
%   `Board \= [q(D, Y)|R]`.  Covering that means covering the complement
%   of what the first answer left, and what it leaves is one `dif/2`
%   between the board and the pattern.
%
%   Read **per variable** that is `D \= 2` and `R \= []` — two pieces,
%   each an over-approximation of the region the answer covered.  Read as
%   the pair it is, it is a single excluded point, `(D, R) \= (2, [])`,
%   and its complement is that point alone: exactly the instance that
%   makes the disequality fail.  Both are correct; the joint one is the
%   answer's actual region, so it neither re-covers ground nor splits.
%
%   The pair has to be the very variables `snapshot/4` handed out, not a
%   copy of them — a `findall/3` here quietly makes the constraint
%   nobody's, `residue/3` drops it as not the quantifier's own, and the
%   covering then claims the whole domain.  That is how `not attack/3`
%   came to hold of every board, and `queens.pl` to answer 256 times.

attack([ (attack(X, _, [q(X, _)|_])),
         (attack(_, Y, [q(_, Y)|_])),
         (attack(X, Y, [_|T]) :- attack(X, Y, T))
       ]).

test(the_complement_of_a_compound_disequality_is_the_point_it_excludes) :-
    attack(Program),
    assertion(answers(Program, attack(1, 1, [q(2, 1)]))),
    assertion(\+ answers(Program, not attack(1, 1, [q(2, 1)]))),
    assertion(answers(Program, not attack(1, 1, [q(2, 2)]))),
    assertion(\+ answers(Program, not attack(1, 1, [q(2,1), q(3,1), q(4,1)]))).

%   ## A `forall/2` the author wrote
%
%   Not every quantifier comes from dualising.  `forall/2` is part of the
%   language, and a clause may use one — which means it needs a dual, and
%   the dual of "for every X" is "for **some** X, not": the quantifier
%   drops and the variable is left free, which in a dual clause body
%   already means some.

subset([ (subset(S1, S2) :- forall(X, contained(X, S1, S2))),
         (contained(X, S1, S2) :- member(X, S1), member(X, S2)),
         (contained(X, S1, S2) :- member(X, S2), not member(X, S1)),
         (member(X, [Y|_]) :- X #= Y),
         (member(X, [_|R]) :- member(X, R))
       ]).

%   It holds for no list pair here, since `contained` says nothing about
%   an element of neither list — which is what the old implementation
%   answers too.

test(an_author_written_forall_is_solved) :-
    subset(Program),
    assertion(\+ answers(Program, subset([1], [1, 3]))).

%   What it needed is that the goal inside gets translated at all: left
%   alone it is a call to whatever the user's predicate is named, in a
%   module that has no such predicate.

test(and_its_goal_is_translated) :-
    subset(Program),
    assertion(answers(Program, contained(1, [1], [1, 3]))),
    assertion(\+ answers(Program, contained(5, [1], [1, 3]))).

%   An answer that *binds* the variable is rejected and the goal asked
%   for another.  It covers one point of an infinite domain, so no finite
%   number of them could cover it: any covering contains an answer that
%   leaves the variable free or in a region, and the points then fall out
%   of that answer's complement.
%
%   Rejecting them is not an optimisation at the margin.  Enumerating
%   instead took 37 seconds on the five-clause program below, which the
%   old implementation answers in nine milliseconds.

point_answers([ dom(1), dom(2), dom(3),
                (a(X) :- dom(X), a(X), X #> 3),
                (a(X) :- dom(X), b(X), b(X), X #> 3),
                (b(X) :- dom(X), not b(X), a(X)),
                (b(X) :- dom(X), a(X), not a(X)),
                (:- not a(3))
              ]).

test(point_answers_do_not_make_the_quantifier_enumerate,
     [ forall(member(Query, [not a(1), not a(2), a(1), b(1)])) ]) :-
    point_answers(Program),
    \+ \+ ( answers(Program, Query) -> true ; true ).

:- end_tests(solve_quantifiers).

		 /*******************************
		 *        COLLECTING		*
		 *******************************/

%   `findall/3` is the one built-in that takes a *goal*, so it is the one
%   that has to be solved rather than called.  Each solution proves
%   something, and what it proved is part of the answer: the golden data
%   for `test/all_programs/solve_findall.pl` lists every `member/2` the
%   collection walked through, not only the three it collected.

:- begin_tests(solve_collecting).

collecting([ (validate(L) :- findall(X, member(X, L), L)),
             (member(X, [X|_])),
             (member(X, [_|R]) :- member(X, R))
           ]).

test(it_collects_the_solutions) :-
    findall(L, scasp([(p(L) :- findall(X, q(X), L)), q(1), q(2)], p(L), _), Ls),
    assertion(Ls == [[1, 2]]).

test(an_empty_collection_still_succeeds) :-
    findall(L, scasp([(p(L) :- findall(X, q(X), L))], p(L), _), Ls),
    assertion(Ls == [[]]).

%   Every literal the collection proved is in the model, which is the
%   whole difference between solving the goal and calling it.

test(what_the_collection_proved_is_in_the_model) :-
    collecting(Program),
    models(Program, validate([1, 2, 3]), [Model]),
    assertion(memberchk(member(1, [1, 2, 3]), Model)),
    assertion(memberchk(member(3, [3]), Model)).

%   And in the justification, under the literal whose body collected
%   them.  There is no `findall/3` node: it is bookkeeping rather than a
%   step, which is what `proof_skeleton.pl`'s `literals` level deletes.

test(and_in_the_justification) :-
    collecting(Program),
    once(scasp(Program, validate([1, 2, 3]), _, Justification)),
    assertion(Justification = [validate([1, 2, 3])-_, '$nmr'-_]),
    Justification = [_-Children|_],
    assertion(memberchk(member(1, [1, 2, 3])-_, Children)).

%   Its negation holds where the list is apart from the solutions.  Not
%   where p holds: failing the negation outright, as the old
%   implementation does, makes `not p` succeed for a p that holds.

test(the_negation_fails_where_the_collection_holds, fail) :-
    answers([(p :- findall(X, q(X), [X])), q(1)], not p).

test(the_negation_holds_where_the_list_differs, nondet) :-
    answers([(p :- findall(X, q(X), [X])), q(1), q(2)], not p).

%   The list is usually a body variable, and so universally quantified in
%   the dual: `not first_q(F)` needs every list to either not be the
%   solutions or to start with something else.

test(the_negation_of_a_function_of_the_list, nondet) :-
    answers([ (first_q(F) :- findall(X, q(X), L), L = [F|_]),
              q(3), q(1)
            ], not first_q(1)).
test(is_false_for_its_value, fail) :-
    answers([ (first_q(F) :- findall(X, q(X), L), L = [F|_]),
              q(3), q(1)
            ], not first_q(3)).

:- end_tests(solve_collecting).

		 /*******************************
		 *        ABDUCIBLES		*
		 *******************************/

%   `#abducible p` says p may be assumed, either way, with nothing to
%   prove.  Written out it is four rules, two of them an even loop over
%   negation, and an even loop *is* a free choice: the call meets itself
%   across two negations and the coinductive hypothesis lets it stand.
%   There is no special case in the solver.

:- begin_tests(solve_abducibles).

abducible([ (:- abducible(maybe)),
            (rain :- maybe)
          ]).

test(an_abducible_can_be_assumed) :-
    abducible(Program),
    models(Program, rain, Models),
    assertion(Models == [[maybe, rain]]).

test(and_can_be_assumed_not_to_hold) :-
    abducible(Program),
    models(Program, not rain, Models),
    assertion(Models == [[not maybe, not rain]]).

%   Both, which is what makes it abducible rather than either a fact or
%   an absence.

test(so_both_are_answers) :-
    abducible(Program),
    assertion(answers(Program, maybe)),
    assertion(answers(Program, not maybe)).

%   The negative half is classical negation: assuming the abducible does
%   not hold derives `-p`, which is stronger than `not p` and is what an
%   abducible is for.

test(the_other_half_is_classical_negation) :-
    abducible(Program),
    models(Program, -maybe, Models),
    assertion(Models == [[-maybe]]).

%   Not both at once, and this is the one thing that has to be checked
%   rather than assumed: the two halves are different predicates, so it
%   is the even loop that keeps them apart.  Assuming both means holding
%   a literal and its negation in one proof, which `consistent/3` rejects
%   where the literal is.

test(but_not_both_at_once) :-
    abducible(Program),
    assertion(\+ answers(Program, (maybe, -maybe))).

%   So an abducible needs no `:- p, -p` sub-check and must not have one —
%   proving that means proving `not -p(X)` for a quantified X, and the
%   only way to do *that* is to assume p(X), deciding the abducible for
%   every individual at once.  A predicate with rules of its own for `-p`
%   still gets the check, and then the two really do interact.

test(a_rule_for_the_negative_half_still_constrains_it) :-
    Program = [ (:- abducible(maybe)),
                (rain :- maybe),
                (-(maybe) :- storm),
                storm
              ],
    assertion(\+ answers(Program, rain)),
    assertion(answers(Program, not rain)).

%   An abducible with arguments is a predicate's worth of choices rather
%   than one, and they are independent: assuming p(a) says nothing about
%   p(b).

test(an_abducible_with_arguments) :-
    Program = [ (:- abducible(happens(off, _))),
                happens(on, 5)
              ],
    assertion(answers(Program, happens(off, 3))),
    assertion(answers(Program, not happens(off, 3))),
    assertion(answers(Program, happens(on, 5))).

%   And it may be assumed under a quantifier, which is what the event
%   calculus programs need: nothing happened anywhere in an interval.

test(an_abducible_under_a_quantifier) :-
    Program = [ (:- abducible(happens(bang, _))),
                (broken :- happens(bang, T), T #> 0, T #< 10),
                (quiet :- not broken)
              ],
    assertion(answers(Program, quiet)),
    assertion(answers(Program, broken)).

%   The worked example: three ways for a bird to be abnormal, three
%   answers, which is what the old implementation gives.  It was 48
%   before the consistency sub-checks came out.

test(abduced_birds) :-
    Program = [ (:- abducible(wounded_bird(_))),
                (:- abducible(penguin(_))),
                (:- abducible(ab(_))),
                bird(tweety),
                (ab(X) :- penguin(X)),
                (ab(X) :- wounded_bird(X)),
                (flies(X) :- bird(X), not ab(X))
              ],
    models(Program, not flies(tweety), Models),
    assertion(length(Models, 3)),
    assertion(memberchk([ab(tweety), bird(tweety), not -ab(_), not flies(tweety)],
                        Models)).

%   `ab/1` has rules of its own as well as being abducible, so it gets a
%   consistency check and `not -ab(X)` is in every model: the check
%   discharges by there being nothing known *not* to be abnormal.
%   `penguin/1` and `wounded_bird/1` are abducible and nothing else, so
%   they get none — see `test_compile.pl:compile_nmr`.

:- end_tests(solve_abducibles).

		 /*******************************
		 *     AGAINST THE ORACLE	*
		 *******************************/

%   The comparison is **brave entailment**, which is what a goal-directed
%   solver computes: a query succeeds when some answer set supports it.
%   So for every atom `a` of the program, both must hold:
%
%       sscasp answers a        <=>  a is in some answer set
%       sscasp answers not a    <=>  a is absent from some answer set
%
%   Sharp in both directions.  It catches an answer no answer set
%   supports, and an answer set the solver cannot reach — which counting
%   models would not, since a solver finding the right number of wrong
%   models would pass a count.
%
%   Unlike `test_differential.pl` there is no scoping to arrange:
%   `scasp/3` is given the whole program, so both sides are asked the
%   same question.
%
%   The first-order and CLP shapes are what bring the *quantifiers* into
%   range: the NMR sub-check of a rule with a variable is a `forall` over
%   it, so those seeds exercise `cover/5` on every program rather than
%   only where a test thought to put one.
%
%   Seed counts are small enough to keep `make check` quick; the oracle
%   is exponential in the Herbrand base while the solver is not, so
%   widening a sweep is a one-line change to `shape/5`.

:- begin_tests(solve_differential).

shape(plain,       3, 1, [],                                120).
shape(wider,       4, 2, [],                                 80).
shape(constrained, 3, 3, [],                                 60).
shape(classical,   3, 1, [classical(true)],                  80).
shape(first_order, 2, 1, [first_order([1, 2])],              40).
shape(clp,         2, 1, [first_order([1, 2, 3]),
                          clp(true)],                        40).

seed(Shape, Seed) :-
    shape(Shape, NAtoms, NConstraints, _, Count),
    between(1, Count, N),
    Seed is N*104729 + NAtoms*31 + NConstraints.

%!  agrees(+Program) is semidet.

agrees(Program) :-
    stable_models(Program, Models),
    program_literals(Program, Atoms),
    forall(( member(Atom, Atoms),
             ( Literal = Atom ; Literal = not(Atom) )
           ),
           agrees_on(Program, Models, Literal)).

agrees_on(Program, Models, Literal) :-
    (   answers(Program, Literal)
    ->  Ours = yes
    ;   Ours = no
    ),
    (   models_entail(Models, Literal)
    ->  Theirs = yes
    ;   Theirs = no
    ),
    (   Ours == Theirs
    ->  true
    ;   print_message(error,
                      format("~p: ~p  sscasp=~w  oracle=~w",
                             [Program, Literal, Ours, Theirs])),
        fail
    ).

test(random_programs, [forall(( shape(Shape, NAtoms, NCons, Options, _),
                                seed(Shape, Seed) ))]) :-
    set_random(seed(Seed)),
    random_program(NAtoms, NCons, Options, Program),
    agrees(Program).

%   The worked programs too, so that a shape the generator does not reach
%   is still compared.

worked([(p :- not q), (q :- not p)]).
worked([(p :- not p)]).
worked([(p :- q), (q :- p)]).
worked([(c :- c), (:- not c)]).
worked([a, b, (c :- c), (:- not c, not b)]).
worked([(p :- not q), (q :- not r), (r :- not p)]).
worked([(p :- not q), (q :- not r), (r :- not s), (s :- not p)]).
worked([(:- p, q), p, q]).
worked([(-(p) :- q), q]).
worked([(-(p) :- q), p, q]).

test(worked_programs, [forall(worked(Program))]) :-
    agrees(Program).

:- end_tests(solve_differential).

		 /*******************************
		 *      WITHOUT THE CHECK	*
		 *******************************/

/** Turning the NMR check off

`scasp_compile_nmr` and `scasp_compile_olon` are upstream's flags,
`test_nmr.pl:nmr_flags` covers what they do to the compiler, and these
three cover that the solver sees the difference — a check that is not
compiled is a check that does not reject an answer.

Not a mode to run a program in: an answer that violates a constraint is
wrong, and with the flag off nothing says so.  It is for asking what a
query costs on its own, which is how the four event-calculus gaps in
`test_corpus.pl` were traced to the one check they all wait on.
*/

:- begin_tests(solve_without_nmr).

%   `q` is rejected by a global constraint and by nothing else.

constrained([ q,
              (:- q) ]).

test(a_global_constraint_rejects_the_answer) :-
    constrained(Program),
    assertion(\+ answers(Program, q)).

test(and_does_not_when_it_is_not_compiled) :-
    constrained(Program),
    with_scasp_flag(scasp_compile_nmr, false,
                    assertion(answers(Program, q))).

%   `scasp_compile_olon` leaves a global constraint alone: upstream drops
%   only the rules on an odd loop, and a denial with a head is what that
%   means.

test(dropping_the_loops_leaves_a_constraint_checked) :-
    constrained(Program),
    with_scasp_flag(scasp_compile_olon, false,
                    assertion(\+ answers(Program, q))).

:- end_tests(solve_without_nmr).

/** `sscasp_table` is an experiment (`doc/design-next.md` §7a) and off by
    default.  It answers a repeated goal from a table instead of proving
    it again, so it may change what a model holds but never which
    answers there are.  This keeps it honest while it shares the loop
    check's walks.
*/

:- begin_tests(solve_tabling).

tabled([(p :- not q), (q :- not p)], p).
tabled([(p :- not q, r), (q :- not r), (r :- p)], q).
tabled([p(1), (p(X) :- X #> 3, q(X)), (q(_) :- p(_))], p(_)).
tabled([q(1), q(2), (r(X) :- not q(X))], r(_)).
tabled([edge(a, b), edge(b, c), (path(X, Y) :- edge(X, Y)),
        (path(X, Y) :- edge(X, Z), path(Z, Y))], path(a, _)).

test(tabling_gives_the_same_answers, [forall(tabled(Program, Query))]) :-
    findall(Query, scasp(Program, Query, _), Off),
    with_scasp_flag(sscasp_table, true,
                    findall(Query, scasp(Program, Query, _), On)),
    assertion(On =@= Off).

:- end_tests(solve_tabling).

/** `--dcc` checks a global constraint as soon as a literal completes it,
    rather than only at the end.  It is meant to prune and never to
    change an answer: the NMR check still runs, and a branch that
    completes a constraint either fails anyway or ends in a model the
    check rejects.  The whole corpus answers the same with it on, and so
    do the oracle sweeps; this is the case in miniature.
*/

:- begin_tests(solve_dcc).

%   Choose exactly one item.  Each clause names its own variables: a
%   variable shared between two terms of the list would be one variable.

choose_one([ item(1), item(2), item(3),
             (choose(A) :- item(A), not skip(A)),
             (skip(B) :- item(B), not choose(B)),
             (:- choose(C), choose(D), C \= D) ]).

test(the_answers_do_not_change) :-
    choose_one(Program),
    with_scasp_flag(sscasp_dcc, false,
                    answer_set(Program, choose(_), Off)),
    with_scasp_flag(sscasp_dcc, true,
                    answer_set(Program, choose(_), On)),
    assertion(Off == On),
    assertion(Off \== []).

answer_set(Program, Query, Set) :-
    findall(Key,
            ( scasp(Program, Query, Model),
              copy_term_nat(Query-Model, Key),
              numbervars(Key, 0, _)
            ),
            Keys),
    sort(Keys, Set).

:- end_tests(solve_dcc).

		 /*******************************
		 *        A KEPT MODULE		*
		 *******************************/

%   `module(?Module)` leaves the program loaded, which is what the
%   profiler needs: it will not keep track of a temporary module.

:- begin_tests(solve_module).

test(an_unbound_module_is_made_and_kept) :-
    scasp_run([p], p, [module(M), model(Model)]),
    assertion(atom(M)),
    assertion(Model == [p]),
    assertion(current_module(M)),
    assertion(( current_predicate(M:Name/_), sub_atom(Name, _, _, _, '$p$') )).

%   A module given again holds the new program and nothing of the old:
%   `q` held in the first and has no clause in the second, where it is
%   closed to false.

test(a_given_module_is_emptied_first) :-
    M = test_solve_kept,
    assertion(scasp_run([q, (p :- q)], p, [module(M)])),
    assertion(\+ scasp_run([p :- q], p, [module(M)])).

%   Only what a compiled program defines is removed.

test(and_nothing_else_in_it_is_touched,
     [ setup(assertz(test_solve_kept_too:mine(1))),
       cleanup(retractall(test_solve_kept_too:mine(_)))
     ]) :-
    assertion(scasp_run([p], p, [module(test_solve_kept_too)])),
    assertion(test_solve_kept_too:mine(1)).

:- end_tests(solve_module).
