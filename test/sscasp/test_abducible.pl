:- module(test_abducible,
          [ test_abducible/0
          ]).
:- use_module('../scasp_plunit', []).        % sets the library search path
:- use_module(oracle_agreement, [agrees_with_oracle/1]).
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/run), [scasp/3]).
:- use_module(library(sscasp/compile), [compile_program/3]).
:- use_module(library(plunit)).
:- use_module(library(lists), [member/2, memberchk/2]).
:- use_module(library(debug), [assertion/1]).

/** <module> `#abducible` and the consistency check

`#abducible p` compiles to four rules:

    p :- abducible(p).            -p :- abducible(-p).
    abducible(p)  :- not abducible(-p).
    abducible(-p) :- not abducible(p).

The last two are an even loop, which is a free choice, and that is the
whole mechanism — there is no special case in the solver.

The question this file exists for is whether `:- p, -p` is generated for
such a predicate.  For a predicate that is **only** abducible it must not
be: the two answer sets of those four rules are `{p}` and `{-p}`, never
both, so the check is entailed, and generating it anyway is a free choice
the solver must make once per query that drags the declaration in — the
answers double.

But `p` may have rules of its own, and then assuming `-p` gives a model
holding both.  Testing only the *negative* half for that, which is what
the compiler used to do, left `?- -p.` answering where no answer set has
it.  `stable_models.pl` is the referee: it computes answer sets straight
from the Gelfond-Lifschitz definition and enforces consistency, so a
program deriving `p` and `-p` has no answer set at all.
*/

test_abducible :-
    run_tests([ abducible_choice,
                abducible_consistency,
                abducible_against_the_oracle
              ]).

%!  answers(+Program, +Query, -Models) is det.

answers(Program, Query, Models) :-
    findall(Model, scasp(Program, Query, Model), Models).

%!  checks(+Program, -Checks) is det.

checks(Program, Checks) :-
    compile_program(m, Program, Compiled),
    Checks = Compiled.nmr.checks.

		 /*******************************
		 *         THE CHOICE		*
		 *******************************/

:- begin_tests(abducible_choice).

test(an_abducible_can_be_assumed_either_way,
     [ forall(member(Q, [maybe, -maybe])) ]) :-
    answers([(:- abducible(maybe))], Q, Models),
    assertion(Models = [_|_]).

test(and_a_pure_one_gets_no_check) :-
    checks([(:- abducible(maybe)), (rain :- maybe)], Checks),
    assertion(Checks == []).

%   Which matters because a declaration travels with every query that can
%   see it: a check here would be a free choice made again for every
%   query in the program, and the answers would double.

test(so_an_unrelated_query_is_not_doubled) :-
    answers([(:- abducible(maybe)), p(1), p(2)], p(X), Models),
    assertion(length(Models, 2)),
    assertion(X == X).

:- end_tests(abducible_choice).

		 /*******************************
		 *       THE CONSISTENCY		*
		 *******************************/

:- begin_tests(abducible_consistency).

%   `p` is derivable, so assuming `-p` gives a model holding both, which
%   is not a model.  The only answer set is `{p, q}`.

derivable([(:- abducible(p)), q, (p :- q)]).

test(the_positive_half_having_rules_makes_a_check) :-
    derivable(P),
    checks(P, Checks),
    assertion(Checks =@= [check(1, consistency(p/0), (-p, p))]).

test(and_the_negation_is_not_entailed) :-
    derivable(P),
    answers(P, -p, Models),
    assertion(Models == []).

%   Two routes to `p` — the rule and the assumption — so two answers.
%   Both are partial models of the one answer set.

test(while_the_positive_still_is) :-
    derivable(P),
    answers(P, p, Models),
    assertion(Models = [_|_]),
    assertion(forall(member(M, Models), memberchk(p, M))).

%   The same the other way round, which is the half that was always
%   tested.

test(the_negative_half_having_rules_makes_one_too) :-
    checks([(:- abducible(p)), q, (-(p) :- q)], Checks),
    assertion(Checks =@= [check(1, consistency(p/0), (-p, p))]).

test(and_then_the_positive_is_not_entailed) :-
    answers([(:- abducible(p)), q, (-(p) :- q)], p, Models),
    assertion(Models == []).

%   First order, where the check is a quantifier over the argument.

test(a_first_order_abducible_with_rules) :-
    P = [(:- abducible(p(_))), q(1), (p(X) :- q(X))],
    answers(P, -p(1), Models),
    assertion(Models == []).

test(and_it_leaves_the_other_individuals_alone) :-
    P = [(:- abducible(p(_))), q(1), (p(X) :- q(X))],
    answers(P, -p(2), Models),
    assertion(Models = [_|_]).

:- end_tests(abducible_consistency).

		 /*******************************
		 *         THE ORACLE		*
		 *******************************/

/** `stable_models.pl` shares no design decision with either
    implementation, so it can say who is right where the corpus can only
    say whether the two agree.  Abducibles are given to it as the even
    loop they compile to, since `#abducible` is not something it reads.

    The rules have to be safe — the oracle refuses an unsafe one, because
    s(CASP) reads it over an extended Herbrand universe and a finite
    grounding would disagree by design — so the choice is made over a
    domain.
*/

:- begin_tests(abducible_against_the_oracle).

%!  as_even_loop(+Abducible, -Clauses) is det.
%
%   `#abducible p` as the four rules it compiles to, written for the
%   oracle: `ap` and `anp` stand for the two assumptions.

test(a_derivable_abducible_has_one_answer_set) :-
    agrees_with_oracle([ (p :- ap), (-(p) :- anp),
                         (ap :- not anp), (anp :- not ap),
                         q, (p :- q)
                       ]).

test(a_free_abducible_has_two) :-
    agrees_with_oracle([ (p :- ap), (-(p) :- anp),
                         (ap :- not anp), (anp :- not ap)
                       ]).

test(and_one_the_program_contradicts_has_none) :-
    agrees_with_oracle([ (p :- ap), (-(p) :- anp),
                         (ap :- not anp), (anp :- not ap),
                         q, (p :- q), (-(p) :- q)
                       ]).

:- end_tests(abducible_against_the_oracle).
