:- module(test_differential,
          [ test_differential/0
          ]).
:- use_module(differential).
:- use_module(stable_models, [stable_models/2, op(900, fy, not),
                              op(700, xfx, [#=, #<>, #<, #>, #=<, #>=])]).
:- use_module(scasp_plunit, [expect_semantics/0]).
:- use_module(library(plunit)).
:- use_module(library(lists), [member/2]).

/** <module> s(CASP) against the stable-model definition

Every other oracle in this suite descends from the implementation it judges:
the `.pass` data records what this solver produces, and the sibling checkout
at ../sCASP is the same codebase.  None of them can catch a defect the
implementation has always had.

`stable_models.pl` is written from the Gelfond-Lifschitz definition and
shares nothing with the solver, so it can.  This suite points the two at the
same programs and compares them on **brave entailment**, which is what a
goal-directed solver computes:

    scasp answers `a'       <=>  a is in some answer set
    scasp answers `not a'   <=>  a is absent from some answer set

Sharp in both directions.  It catches unsoundness - an answer no answer set
supports - and incompleteness - an answer set the solver cannot reach.
Counting models would catch neither: a solver that found the right number of
wrong models would pass a count.

Everything lives in one plunit unit on purpose: `scasp_load/2` compiles into
its caller's module, which for every caller is `differential.pl`, so two units
running under `jobs(4)` would destroy each other's program.

The programs are propositional.  That is the fragment where the compiler's
constructs interfere with one another (duals, odd loops, the NMR check) and
where the semantics is unambiguous enough for a definitional oracle to be
authoritative.  Constraints over dense domains and ungrounded answers are out
of its reach; `test_clpq.pl` and the complement-by-sampling tests cover those.
*/

test_differential :-
    run_tests([ diff_worked_examples,
                diff_known_divergence,
                diff_random
              ]).

		 /*******************************
		 *      WORKED EXAMPLES		*
		 *******************************/

%   Programs whose answer sets are stated in doc/literature.md or are small
%   enough to check by hand.  `test_stable_models.pl` pins what the oracle
%   says about each; here the question is only whether s(CASP) agrees.

:- begin_tests(diff_worked_examples).

example(even_loop,        [(p :- not q), (q :- not p)]).
example(odd_loop,         [(p :- not p)]).
example(odd_loop_defused, [p, (p :- not p)]).
example(odd_loop_guards,  [(q :- not r), (r :- not q), (p :- q, not p)]).
example(three_cycle,      [(p :- not q), (q :- not r), (r :- not p)]).
example(positive_loop,    [(p :- p)]).
example(positive_2cycle,  [(p :- q), (q :- p)]).
example(positive_founded, [(p :- q), (q :- p), p]).
example(chain,            [p, (q :- p), (r :- q)]).
example(stratified,       [a, (b :- a), (c :- not b), (d :- not c)]).
example(constraint,       [(p :- not q), (q :- not p), (:- p)]).
example(constraint_neg,   [(p :- not q), (q :- not p), (:- not p)]).
example(constraint_conj,  [(p :- not q), (q :- not p), (:- p, q)]).
example(constraint_all,   [p, (:- p)]).
example(two_odd_loops,    [ (a :- not b), (b :- not a),
                            (c :- a, not c), (d :- b, not d) ]).

%   Classical negation.  `-p` is a literal of its own, and no answer set
%   may hold it alongside `p` - so a program deriving both has none at
%   all, and the atoms with nothing to do with the contradiction go with
%   it.  Default negation of a classical literal is a third thing again.

example(classical_fact,   [-p]).
example(contradiction,    [p, -p]).
example(contradiction_plus, [p, -p, q]).
example(classical_naf,    [-p, (q :- not -p)]).
example(classical_guarded, [r, (p :- not q), (-p :- not r), q]).

%   First-order.  The oracle grounds over the program's constants and
%   runs the same machinery; s(CASP) never grounds at all, which is the
%   whole point of it, so agreement here is worth more than in the
%   propositional cases.

example(fo_simple,        [q(a), q(b), (p(X) :- q(X))]).
example(fo_negation,      [q(a), q(b), r(b), (p(X) :- q(X), not r(X))]).
example(fo_even_loop,     [ dom(1), dom(2),
                            (p(X) :- dom(X), not q(X)),
                            (q(X) :- dom(X), not p(X)) ]).
example(fo_odd_loop,      [dom(a), (p(X) :- dom(X), not p(X))]).
example(fo_constraint,    [ dom(a), dom(b),
                            (p(X) :- dom(X), not q(X)),
                            (q(X) :- dom(X), not p(X)),
                            (:- p(a)) ]).
example(fo_classical,     [dom(a), (p(X) :- dom(X)), (-p(X) :- dom(X))]).

%   Constraints over the finite domain the constants provide.  The oracle
%   evaluates them while grounding; s(CASP) carries them as a constraint
%   store and has to dualise that store to answer `not p(X)'.  That last
%   is the part of solve.pl with the least independent checking, which is
%   what `clp_negated' reaches.

example(clp_threshold,    [ dom(1), dom(2), dom(3), dom(4),
                            (p(X) :- dom(X), X #> 2) ]).
example(clp_equality,     [dom(1), dom(2), dom(3), (p(X) :- dom(X), X #= 2)]).
example(clp_disequality,  [dom(1), dom(2), dom(3), (p(X) :- dom(X), X #<> 2)]).
example(clp_interval,     [ dom(1), dom(2), dom(3), dom(4),
                            (p(X) :- dom(X), X #> 1, X #< 4) ]).
example(clp_guarded,      [ dom(1), dom(2), dom(3),
                            (p(X) :- dom(X), X #> 1, not q(X)),
                            (q(X) :- dom(X), X #= 3) ]).
example(clp_negated,      [ dom(1), dom(2), dom(3),
                            (p(X) :- dom(X), X #> 1),
                            (r(X) :- dom(X), not p(X)) ]).

test(agrees_with_the_definition, [forall(example(Name, Program))]) :-
    assertion(ground(Name)),
    agree(Program).

%   The two that drive the point home, spelled out.  An odd loop is a
%   constraint (P2/P4, doc/literature.md) and a positive loop is unfounded -
%   the greatest-fixpoint answer {p,q} is exactly what the coinduction
%   restriction exists to prevent.

test(an_odd_loop_leaves_no_answer_set) :-
    stable_models([(p :- not p)], Models),
    assertion(Models == []),
    assertion(\+ scasp_brave([(p :- not p)], p)),
    assertion(\+ scasp_brave([(p :- not p)], not p)).

test(a_positive_loop_is_unfounded) :-
    Program = [(p :- q), (q :- p)],
    stable_models(Program, Models),
    assertion(Models == [[]]),
    assertion(\+ scasp_brave(Program, p)),
    assertion(scasp_brave(Program, not p)).


		 /*******************************
		 *      KNOWN DIVERGENCE	*
		 *******************************/

:- end_tests(diff_worked_examples).

		 /*******************************
		 *      KNOWN DIVERGENCE	*
		 *******************************/

%   The one place the hand-written battery found the two disagreeing.
%   doc/bugs.md #17: an atom with no rules is underivable, so `not p' holds
%   in the unique answer set - but the query is rejected outright, while the
%   same literal inside a body behaves correctly.


:- begin_tests(diff_known_divergence).

undefined_atom([(q :- p)]).

test(the_definition_says_the_negation_holds) :-
    undefined_atom(Program),
    stable_models(Program, Models),
    assertion(Models == [[]]).

:- if(expect_semantics).

test(undefined_atom_in_a_query_is_answered) :-
    undefined_atom(Program),
    agree(Program).

:- else.

%   The predicate indicator in the error carries the interned module name -
%   `'tmp-1-4183594005075597591:p'/0' rather than `p/0' - because
%   modules.pl interns the module into the functor and scasp/2 compiles
%   into a temporary one.  Matched loosely for that reason; the name a
%   user sees is doc/bugs.md #17's second half.

test(undefined_atom_in_a_query_raises_rather_than_answering,
     [ throws(error(existence_error(scasp_predicate, _), _)) ]) :-
    undefined_atom(Program),
    scasp_brave(Program, not p).

% ... while the same atom reached through a rule body simply fails, which
% is the correct behaviour and the reason the two are inconsistent.

test(but_the_same_atom_in_a_body_fails_correctly) :-
    undefined_atom(Program),
    assertion(\+ scasp_brave(Program, q)),
    assertion(scasp_brave(Program, not q)).

:- endif.


		 /*******************************
		 *      RANDOM PROGRAMS		*
		 *******************************/

:- end_tests(diff_known_divergence).

		 /*******************************
		 *      RANDOM PROGRAMS		*
		 *******************************/

:- begin_tests(diff_random).

%   Generated programs, over three to five atoms, with and without
%   constraints.  Each test seeds the generator from its own `forall'
%   variable, so a failure names the seed that reproduces it and nothing
%   depends on test order.
%
%   Roughly seven in ten of the plain shapes have at least one answer set,
%   and four in ten of the classical and first-order ones - classical
%   negation makes a contradiction likely, and a first-order program has
%   more rules and so more ways to be inconsistent.  A program with no
%   answer set is still a real check, since s(CASP) has to find nothing
%   too, but it is a weaker one.
%
%   The seed counts differ per shape because the *oracle* is exponential
%   in the size of the Herbrand base, and grounding drives that up fast:
%   three predicates over three constants is 2^12 interpretations, about
%   twenty times the cost of two constants.  So the three-constant shapes
%   get a handful of seeds and the two-constant ones carry the volume.
%   s(CASP) itself is unaffected - it never grounds - so the whole cost
%   here is the price of having an independent answer to check against.
%   A wider sweep is a one-line change to the counts below, worth doing by
%   hand when the solver changes.  The
%   generator gives every atom a rule, which keeps the divergence above from
%   being the only thing these ever find; see random_program/3.


shape(3, 0, [],                   30).
shape(3, 1, [],                   30).
shape(4, 0, [],                   30).
shape(4, 1, [],                   30).
shape(3, 0, [classical(true)],    30).
shape(3, 1, [classical(true)],    30).
shape(4, 1, [classical(true)],    30).
shape(3, 1, [first_order([a,b])], 20).
shape(3, 1, [first_order([a,b]), classical(true)], 20).
shape(3, 1, [first_order([a,b,c])], 3).
shape(3, 1, [first_order([1,2]), clp(true)], 20).
shape(3, 1, [first_order([1,2]), clp(true), classical(true)], 15).
shape(3, 1, [first_order([1,2,3]), clp(true)], 3).

test(random_programs_agree_with_the_definition,
     [ forall(( shape(Preds, Constraints, Options, Seeds),
                between(1, Seeds, Seed) )) ]) :-
    set_random(seed(Seed)),
    random_program(Preds, Constraints, Options, Program),
    agree(Program).

:- end_tests(diff_random).
