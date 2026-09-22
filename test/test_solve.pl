:- module(test_solve,
          [ test_solve/0
          ]).
:- use_module(scasp_plunit, []).        % sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/run)).
:- use_module(library(sscasp/solve), [check/8, model/2]).
:- use_module(stable_models, [stable_models/2, models_entail/2,
                              program_literals/2]).
:- use_module(differential, [random_program/4]).
:- use_module(library(plunit)).
:- use_module(library(lists), [member/2, memberchk/2]).

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
                solve_loops,
                solve_constraints,
                solve_constructive,
                solve_domains,
                solve_counting,
                solve_differential
              ]).

%!  models(+Program, ?Query, -Models) is det.

models(Program, Query, Models) :-
    findall(Query-Model, sscasp(Program, Query, Model), Pairs),
    pairs_models(Pairs, Models).

pairs_models([], []).
pairs_models([_-Model|T], [Model|Rest]) :- pairs_models(T, Rest).

%!  answers(+Program, ?Query) is semidet.

answers(Program, Query) :-
    \+ \+ sscasp(Program, Query, _).

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
    findall(X, sscasp(P, flies(X), _), Xs),
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
    check(Goal, Neg, Pol, Anc, [], _, _, Action).

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
    assertion(Action == done).

%   Two transitions: coinductive success, on either side.

test(two_transitions_succeed) :-
    decision(p, +, [not(q)-(-), p-(+)], Action),
    assertion(Action == done).

test(and_four) :-
    decision(p, +, [not(s)-(-), r-(+), not(q)-(-), p-(+)], Action),
    assertion(Action == done).

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
    frame(not(p), p, -, Proved),
    assertion(\+ check(p, Neg, +, [], [Proved], _, _, _)).

test(a_ground_proved_goal_needs_no_expansion) :-
    negation(p, +, Neg),
    frame(p, not(p), +, Proved),
    check(p, Neg, +, [], [Proved], _, _, Action),
    assertion(Action == done).

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
    sscasp([(p(X) :- not q(X)), q(1)], p(V), _),
    !,
    assertion(var(V)),
    copy_term(V, _, Attributes),
    assertion(Attributes =@= [dif(_, 1)]).

%   And the model shares with the query.  It is collected by walking
%   rather than with findall/3, which copies: a model collected that way
%   carries none of the constraints that say what the answer *is*.

test(the_model_shares_with_the_query) :-
    sscasp([(p(X) :- not q(X)), q(1)], p(V), Model),
    !,
    assertion(Model = [not q(W), p(W)]),
    Model = [_, p(W2)],
    assertion(V == W2).

test(a_value_the_constraint_excludes_has_no_answer) :-
    assertion(\+ answers([(p(X) :- not q(X)), q(1)], p(1))).

:- end_tests(solve_constructive).

		 /*******************************
		 *         DOMAINS		*
		 *******************************/

%   A declared domain turns a quantifier into an enumeration, which is
%   decision 5's case 1 and the whole argument for annotations.

:- begin_tests(solve_domains).

test(a_domain_enumerates_the_witnesses) :-
    Program = [ (:- domain d = [a, b, c]),
                (:- pred q(d)),
                (p :- not q(_)),
                q(a)
              ],
    models(Program, p, Models),
    assertion(Models = [_, _]),
    assertion(forall(member(M, Models), memberchk(p, M))).

test(and_says_no_when_the_domain_is_exhausted) :-
    Program = [ (:- domain d = [a, b]),
                (:- pred q(d)),
                (p :- not q(_)),
                q(a), q(b)
              ],
    assertion(\+ answers(Program, p)).

%   Without a domain the same program still answers, because the
%   quantifier is not reached: `not q(X)` in a positive body is
%   existential, and only a *dual* quantifies.

test(an_undomained_body_variable_is_existential) :-
    assertion(answers([(p :- not q(_)), q(1)], p)).

:- end_tests(solve_domains).

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
%   `sscasp/3` is given the whole program, so both sides are asked the
%   same question.
%
%   Seed counts are small enough to keep `make check` quick; the oracle
%   is exponential in the Herbrand base while the solver is not, so
%   widening a sweep is a one-line change to `shape/5`.

:- begin_tests(solve_differential).

shape(plain,       3, 1, [],                120).
shape(wider,       4, 2, [],                 80).
shape(constrained, 3, 3, [],                 60).
shape(classical,   3, 1, [classical(true)],  80).

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
