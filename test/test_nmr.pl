:- module(test_nmr,
          [ test_nmr/0
          ]).
:- use_module(scasp_plunit, []).        % sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/nmr)).
:- use_module(library(sscasp/annotations), [program_annotations/2]).
:- use_module(library(plunit)).
:- use_module(library(lists), [member/2, memberchk/2, nth1/3, append/3]).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(yall), [(>>)/3]).

/** <module> The NMR check

`sscasp/nmr.pl` turns the global constraints and the rules on an odd loop
into one goal the solver calls per answer.  Both kinds say that something
must not hold, so both become a **denial**, and the check is the conjunction
of their negations.

The first unit tests the denial on its own, because that is the part carrying
the semantics: a constraint is a denial already, and a rule is violated when
its body holds and its head does not.  The rest test the predicates built
around it.
*/

test_nmr :-
    run_tests([ nmr_denials,
                nmr_structure,
                nmr_quantification,
                nmr_checks
              ]).

%!  check(+Olon, +Constraints, -Nmr) is det.
%
%   The two kinds the tests below mostly use, in the order nmr.pl is
%   given them.  The sources are what it actually takes, so a test about
%   a third kind builds the list itself.

check(Olon, Constraints, Nmr) :-
    program_annotations([], Annotations),
    maplist([C, olon(C)]>>true, Olon, OlonSources),
    maplist([B, constraint(B)]>>true, Constraints, ConstraintSources),
    append(OlonSources, ConstraintSources, Sources),
    nmr_check(m, Annotations, Sources, Nmr).

head_of((Head :- _), Head) :- !.
head_of(Fact, Fact).

clause_for(Nmr, Name, Clause) :-
    member(Clause, Nmr.clauses),
    head_of(Clause, Head),
    functor(Head, Name, _).

		 /*******************************
		 *         DENIALS		*
		 *******************************/

:- begin_tests(nmr_denials).

%   A global constraint is a denial already.

test(a_constraint_is_its_own_denial) :-
    denial(constraint((q(X), r(X))), D),
    assertion(D == (q(X), r(X))).

%   A rule is violated when its body holds and its head does not.

test(a_rule_denies_its_body_with_its_head_negated) :-
    denial(olon((p(X) :- q(X))), D),
    assertion(D == (q(X), not p(X))).

%   `p :- not p' already denies its own head, so the denial is the body
%   unchanged.  Adding `not p' twice would only make the dual generator
%   produce a clause that cannot succeed.

test(a_head_already_denied_is_not_denied_twice) :-
    denial(olon((p :- not p)), D),
    assertion(D == (not p)).

test(and_the_same_in_a_longer_body) :-
    denial(olon((p :- q, not p, r)), D),
    assertion(D == (q, not p, r)).

%   A different literal negated in the body is not the head.

test(another_negated_literal_is_not_the_head) :-
    denial(olon((p :- not q)), D),
    assertion(D == (not q, not p)).

test(a_classically_negated_head) :-
    denial(olon((-(p) :- q)), D),
    assertion(D == (q, not(-(p)))).

:- end_tests(nmr_denials).

		 /*******************************
		 *        STRUCTURE		*
		 *******************************/

:- begin_tests(nmr_structure).

test(the_check_calls_one_sub_check_per_denial) :-
    check([], [a, b], Nmr),
    assertion(Nmr.goal == 'm$nmr$$0'),
    once(clause_for(Nmr, 'm$nmr$$0', Clause)),
    Clause = ('m$nmr$$0' :- Body),
    assertion(Body == ('m$dualchk.1$$0', 'm$dualchk.2$$0')).

%   A program with nothing to check still gets a check: one goal for the
%   solver to call is simpler than a case for it to test.

test(a_program_with_nothing_to_check_gets_a_fact) :-
    check([], [], Nmr),
    assertion(Nmr.clauses == ['m$nmr$$0']),
    assertion(Nmr.checks == []).

test(one_denial_needs_no_conjunction) :-
    check([], [a], Nmr),
    once(clause_for(Nmr, 'm$nmr$$0', Clause)),
    Clause = ('m$nmr$$0' :- Body),
    assertion(Body == 'm$dualchk.1$$0').

%   The sub-check the solver calls is the *dual*: it must succeed when
%   nothing violates the rule.

test(the_sub_check_is_the_dual_of_the_denial) :-
    check([], [a], Nmr),
    once(clause_for(Nmr, 'm$dualchk.1$$0', Clause)),
    Clause = ('m$dualchk.1$$0' :- Body),
    assertion(Body == 'm$dualchk.1.1$$0'),
    once(clause_for(Nmr, 'm$dualchk.1.1$$0', Inner)),
    assertion(Inner == ('m$dualchk.1.1$$0' :- 'm$dual$a$0')).

%   The positive predicate is never called — the duals are generated from
%   the clause, not through it — so it is not in the program.  What it
%   would have said is in `checks' instead.

test(the_positive_sub_check_is_not_emitted) :-
    check([], [a], Nmr),
    assertion(\+ clause_for(Nmr, 'm$chk.1$$0', _)).

%   `p :- not p' denies `not p', so the check requires `p' — and `p' can
%   only be proved from `not p', which is why such a program has no
%   answer.

test(the_smallest_odd_loop) :-
    check([(p :- not p)], [], Nmr),
    once(clause_for(Nmr, 'm$dualchk.1.1$$0', Clause)),
    assertion(Clause == ('m$dualchk.1.1$$0' :- 'm$plain$p$0')).

%   The loops come before the constraints, and the numbering follows the
%   order the sub-checks are called in.

test(loops_are_numbered_before_constraints) :-
    check([(p :- not p)], [a], Nmr),
    assertion(Nmr.checks = [ check(1, p/0, _),
                             check(2, constraint, _)
                           ]).

:- end_tests(nmr_structure).

		 /*******************************
		 *      QUANTIFICATION		*
		 *******************************/

%   A denial's variables are existentially quantified, so its negation
%   quantifies them universally.  Nothing in nmr.pl does that: writing the
%   sub-check as a predicate of *no arguments* makes them body-only
%   variables of a zero-arity head, which the dual generator already
%   quantifies — bounding each by its domain where one is declared.

:- begin_tests(nmr_quantification).

test(a_variable_in_a_constraint_is_quantified) :-
    check([], [(q(X), r(X))], Nmr),
    once(clause_for(Nmr, 'm$dualchk.1.1$$0', Clause)),
    Clause = ('m$dualchk.1.1$$0' :- forall(V, Inner)),
    assertion(Inner == 'm$dualchk.1.1.inner$$0'(V)).

test(the_inner_dual_has_one_clause_per_literal) :-
    check([], [(q(X), r(X))], Nmr),
    findall(C, clause_for(Nmr, 'm$dualchk.1.1.inner$$0', C), Cs),
    assertion(Cs = [ ('m$dualchk.1.1.inner$$0'(A) :- 'm$dual$q$1'(A)),
                     ('m$dualchk.1.1.inner$$0'(B) :- 'm$plain$q$1'(B),
                                                     'm$dual$r$1'(B))
                   ]).

%   And a declared domain bounds it, which is the point of the zero-arity
%   formulation: the NMR check gets domains without asking for them.

test(a_declared_domain_bounds_it) :-
    Program = [ (:- domain thing = [a, b]),
                (:- pred r(thing)),
                r(a)
              ],
    program_annotations(Program, Annotations),
    nmr_check(m, Annotations, [constraint((q(X), r(X)))], Nmr),
    once(clause_for(Nmr, 'm$dualchk.1.1$$0', Clause)),
    Clause = ('m$dualchk.1.1$$0' :- for_domain(V, Generator, _)),
    assertion(Generator == 'm$plain$thing$1'(V)).

%   A ground denial needs no quantifier at all.

test(a_ground_denial_has_no_quantifier) :-
    check([], [(q(1), r(1))], Nmr),
    once(clause_for(Nmr, 'm$dualchk.1.1$$0', Clause)),
    assertion(Clause == ('m$dualchk.1.1$$0' :- 'm$dual$q$1'(1))).

:- end_tests(nmr_quantification).

		 /*******************************
		 *        DESCRIPTIONS		*
		 *******************************/

%   `checks' says what each sub-check is for.  It is how a message names
%   one: a sub-check derives from no user predicate, so it has no name of
%   its own to give.

:- begin_tests(nmr_checks).

test(a_constraint_is_described_by_its_body) :-
    check([], [(q(X), r(X))], Nmr),
    Nmr.checks = [Check],
    Check = check(Index, What, Denial),
    assertion(Index == 1),
    assertion(What == constraint),
    assertion(Denial == (q(X), r(X))).

test(a_loop_is_described_by_its_predicate) :-
    check([(p(_) :- not p(_))], [], Nmr),
    assertion(Nmr.checks = [check(1, p/1, _)]).

test(a_classically_negated_head_names_the_predicate) :-
    check([(-(p) :- q, not(-(p)))], [], Nmr),
    assertion(Nmr.checks = [check(1, p/0, _)]).

test(the_description_carries_the_denial) :-
    check([(p :- q)], [], Nmr),
    assertion(Nmr.checks == [check(1, p/0, (q, not p))]).

%   The consistency of a classically negated predicate is a constraint
%   the language imposes rather than the program, so it says so.

test(a_consistency_constraint_names_its_predicate) :-
    program_annotations([], A),
    nmr_check(m, A, [consistency(p/1, (p(X), -(p(X))))], Nmr),
    assertion(Nmr.checks == [check(1, consistency(p/1), (p(X), -(p(X))))]).

test(and_is_denied_like_any_other_constraint) :-
    denial(consistency(p/1, (p(X), -(p(X)))), D),
    assertion(D == (p(X), -(p(X)))).

test(the_indices_run_from_one) :-
    check([], [a, b, c], Nmr),
    findall(I, member(check(I, _, _), Nmr.checks), Is),
    assertion(Is == [1, 2, 3]).

:- end_tests(nmr_checks).
