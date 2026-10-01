:- module(test_unfold,
          [ test_unfold/0
          ]).
:- use_module('../scasp_plunit', [with_scasp_flag/3]). % and the search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/unfold)).
:- use_module(library(sscasp/compile), [compile_program/3]).
:- use_module(library(sscasp/run), [scasp/3, scasp/4]).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(lists), [member/2]).
:- use_module(library(apply), [include/3, exclude/3]).
:- use_module(library(rbtrees), [rb_lookup/3]).

/** <module> Unfolding literals of fact predicates

`sscasp/unfold.pl` binds a positive literal of a predicate defined by ground
facts to the facts it unifies with, before the program is dualised, so that
the dual needs no quantifier over the variables it binds.  The first unit
says which literals are unfolded and how; the second that the answers do not
change, by solving the same programs with the flag on and off.
*/

test_unfold :-
    run_tests([ unfold_clauses,
                unfold_answers
              ]).

%!  unfolded(+Program, -Clauses) is det.
%
%   Program's clauses after unfolding, the facts left out.

unfolded(Program, Clauses) :-
    index(Program, Index),
    unfold_facts(Index, Program, All),
    include(is_rule, All, Clauses).

index(Program, Index) :-
    sscasp_compile:fact_predicates(Program, Facts),
    fact_index(Facts, Program, [], Index).

is_rule((_ :- _)).

:- begin_tests(unfold_clauses).

test(one_fact_binds_the_literal) :-
    unfolded([age(30), (old :- age(A), A > 65)], Clauses),
    assertion(Clauses == [(old :- age(30), 30 > 65)]).

%   The literal with the unique fact first: it narrows `obs(b, V)` so
%   that the clause does not split.

test(a_binding_narrows_a_later_literal) :-
    unfolded([ key(b), obs(a, 1), obs(b, 2),
               (p :- obs(K, V), key(K), V > 1)
             ], Clauses),
    assertion(Clauses == [(p :- obs(b, 2), key(b), 2 > 1)]).

test(several_facts_split_the_clause) :-
    unfolded([ obs(b, 2), obs(b, 3), (q :- obs(b, V), V > 2) ], Clauses),
    assertion(Clauses == [ (q :- obs(b, 2), 2 > 2),
                           (q :- obs(b, 3), 3 > 2) ]).

test(no_fact_leaves_the_clause) :-
    unfolded([ obs(b, 2), (r :- obs(c, V), V > 1) ], Clauses),
    assertion(Clauses = [(r :- obs(c, V1), V1 > 1)]),
    assertion(var(V1)).

%   A head variable is bound when the dual is called, so there is no
%   quantifier to remove, and binding it would put the fact into the head.

test(a_literal_with_a_head_variable_is_left) :-
    unfolded([ obs(a, 1), (s(K) :- obs(K, V), V > 0) ], Clauses),
    assertion(Clauses = [(s(K1) :- obs(K1, V1), V1 > 0)]),
    assertion(var(K1)),
    assertion(var(V1)).

test(a_negated_literal_is_left) :-
    unfolded([ obs(a, 1), (t :- not obs(a, _)) ], Clauses),
    assertion(Clauses = [(t :- not obs(a, V))]),
    assertion(var(V)).

test(a_predicate_with_a_rule_is_left) :-
    unfolded([ obs(a, 1), (obs(b, 2) :- true), (u :- obs(a, V), V > 0) ],
             Clauses),
    assertion(member((u :- obs(a, V1), V1 > 0), Clauses)),
    assertion(var(V1)).

test(more_facts_than_the_limit_are_left) :-
    with_scasp_flag(
        sscasp_unfold_limit, 1,
        unfolded([ obs(b, 2), obs(b, 3), (q :- obs(b, V), V > 2) ],
                 Clauses)),
    assertion(Clauses = [(q :- obs(b, V1), V1 > 2)]),
    assertion(var(V1)).

test(the_flag_turns_it_off) :-
    with_scasp_flag(
        sscasp_unfold_facts, false,
        unfolded([age(30), (old :- age(A), A > 65)], Clauses)),
    assertion(Clauses = [(old :- age(A1), A1 > 65)]),
    assertion(var(A1)).

%   A denial has no head, so every variable is body-only.

test(a_denial_is_split) :-
    index([obs(b, 2), obs(b, 3)], Index),
    unfold_denials(Index, [(obs(b, V), V > 2)], Bodies),
    assertion(Bodies == [(obs(b, 2), 2 > 2), (obs(b, 3), 3 > 2)]).

%   A derived predicate with a finite set of ground answers is unfolded
%   like a fact predicate: dose/2 has one answer, as pause/0 has none.

test(a_derived_answer_binds_the_literal) :-
    unfolded([ medication(cloz, 250, mg),
               (pause :- state(pause, D), D > 2),
               (dose(D, U) :- not(pause), medication(cloz, D, U)),
               (dose(0, mg) :- pause),
               (high :- dose(A, mg), A >= 450)
             ], Clauses),
    assertion(memberchk((high :- dose(250, mg), 250 >= 450), Clauses)).

test(the_answers_are_a_fixpoint) :-
    answers([ edge(a, b), edge(b, c),
              (path(X, Y) :- edge(X, Y)),
              (path(X, Z) :- edge(X, Y), path(Y, Z))
            ], path/2, Answers),
    assertion(Answers == [path(a, b), path(a, c), path(b, c)]).

test(a_ground_builtin_is_evaluated) :-
    answers([ m(a, 100), m(b, 250),
              (twice(Y) :- m(_, X), Y is X*2),
              (big(V) :- twice(V), V > 300)
            ], big/1, Answers),
    assertion(Answers == [big(500)]).

%   A head a clause may prove with a variable in it is kept as a
%   pattern: a literal that unifies with it is not unfolded, one that
%   does not can use the ground answers of the other clauses.

test(an_unground_head_is_a_pattern) :-
    all_answers([ m(a, 1),
                  (r(free, X) :- not(m(X, _))),
                  (r(bound, X) :- m(X, _))
                ], r/2, Ground, Open),
    assertion(Ground == [r(bound, a)]),
    assertion(Open = [r(free, _)]),
    unfolded([ m(a, 1),
               (r(free, X) :- not(m(X, _))),
               (r(bound, X) :- m(X, _)),
               (s :- r(bound, Y), Y \== b),
               (t :- r(free, Y), Y \== b)
             ], Clauses),
    assertion(memberchk((s :- r(bound, a), a \== b), Clauses)),
    assertion(( member((t :- r(free, V), _), Clauses), var(V) )).

%   A `:- sld` predicate's answers are not in the program: taken as
%   true, it bounds nothing.

test(an_sld_literal_bounds_nothing) :-
    Program = [ (p(X) :- prolog_p(X), m(X)), m(a) ],
    sscasp_unfold:answer_sets(Program, [prolog_p/1], Tree),
    assertion(rb_lookup(p/1, answers([p(a)], []), Tree)),
    sscasp_unfold:answer_sets([(q(X) :- prolog_p(X))], [prolog_p/1], Tree2),
    assertion(rb_lookup(q/1, answers([], [q(_)]), Tree2)).

answers(Program, PI, Ground) :-
    all_answers(Program, PI, Ground, []).

all_answers(Program, PI, Ground, Open) :-
    sscasp_unfold:answer_sets(Program, [], Tree),
    rb_lookup(PI, answers(Ground, Open), Tree).

:- end_tests(unfold_clauses).

		 /*******************************
		 *           ANSWERS		*
		 *******************************/

:- begin_tests(unfold_answers).

%!  same_answers(+Program, +Query) is semidet.
%
%   Query has the same answers, and the models the same positive
%   literals, with and without unfolding.  The negative ones may differ:
%   they are the literals the proof of a negation visited, and a clause
%   no fact can make true is proved false by the literal that has none
%   (`dualise.pl:clause_dual/7`) rather than by a quantifier.

same_answers(Program, Query) :-
    answers(true, Program, Query, With),
    answers(false, Program, Query, Without),
    assertion(With =@= Without).

answers(Unfold, Program, Query, Answers) :-
    with_scasp_flag(
        sscasp_unfold_facts, Unfold,
        findall(Query-Positive,
                ( scasp(Program, Query, Model),
                  exclude(negative, Model, Positive0),
                  msort(Positive0, Positive)
                ),
                Answers0)),
    msort(Answers0, Answers).

negative(not(_)).

program(clinic,
        [ obs(age, 70), obs(age_unit, years), obs(hb, 7.5), obs(hb, 8.5),
          (old :- obs(age, A), A > 65),
          (anaemic :- obs(hb, V), V < 8),
          (fit :- not old, not anaemic),
          (checked :- obs(bp, _))
        ]).
program(constraint,
        [ obs(hb, 7.5), obs(hb, 8.5),
          (p :- not q), (q :- not p),
          (:- p, obs(hb, V), V > 8)
        ]).

test(the_answers_do_not_change,
     forall(member(Q, [old, anaemic, fit, checked,
                       not old, not anaemic, not fit, not checked]))) :-
    program(clinic, Program),
    same_answers(Program, Q).

%   The literal no fact proves is the reason, and the model says so.

test(the_missing_fact_is_the_reason) :-
    program(clinic, Program),
    once(scasp(Program, not checked, Model)),
    assertion(memberchk(not obs(bp, _), Model)).

%   It is recorded rather than proved: a leaf, and no constraint on the
%   value it leaves free, although `obs(bp, 1, past)` agrees with it in
%   the first argument.

test(the_missing_fact_is_a_leaf) :-
    Program = [ obs(bp, 1, past),
                (checked :- obs(bp, _, current))
              ],
    once(scasp(Program, not checked, _Model, Tree)),
    assertion(Tree = [(not checked)-[(not obs(bp, V, current))-[]], '$nmr'-[]]),
    assertion(var(V)),
    assertion(\+ attvar(V)).

%   A clause whose literal has no clause to prove it has a dual that
%   records that literal, head variables or not.  It keeps the head's
%   unifications with a ground value, which dismiss a call for another
%   head cheaply, but not one with a variable of its own:
%   `A \= f(X)' would be about some X rather than every X, a constraint
%   that a quantifier over A then has to cover piece by piece.

test(a_literal_with_a_head_variable_is_recorded) :-
    compile_program(m, [(p(X) :- q(X)), (r :- q(_))], Program),
    get_dict(clauses, Program, Clauses),
    assertion(( member(('m$dual.1$p$1'(_) :- Body), Clauses),
                has_absent(Body) )),
    assertion(( member(('m$dual.1$r$0' :- Body2), Clauses),
                has_absent(Body2) )).

test(only_closed_head_unifications_are_kept) :-
    compile_program(m, [(s(a, f(X)) :- q(X))], Program),
    get_dict(clauses, Program, Clauses),
    findall(B, member(('m$dual.1$s$2'(_, _) :- B), Clauses), Bodies),
    assertion(member((_ \= a), Bodies)),
    assertion(\+ ( member(B, Bodies), sub_term(T, B), nonvar(T),
                   T = (_ \= F), nonvar(F), F = f(_) )).

has_absent(Body) :-
    sub_term(Sub, Body),
    nonvar(Sub),
    Sub = absent(_),
    !.

test(a_denial_still_denies,
     forall(member(Q, [p, q, not p, not q]))) :-
    program(constraint, Program),
    same_answers(Program, Q).

:- end_tests(unfold_answers).
