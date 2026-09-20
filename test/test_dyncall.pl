:- module(test_dyncall,
          [ test_dyncall/0
          ]).
:- use_module('../prolog/scasp').
:- use_module('../prolog/scasp/dyncall', [scasp_query_clauses/2]).
:- use_module(scasp_plunit).
:- use_module(library(plunit)).

/** <module> Tests for scasp/1,2 over ordinary Prolog clauses

scasp/2 is a different front end from the one test_scasp.pl drives: it
collects the clauses reachable from the query out of the calling module,
pulls in any global constraint whose call tree overlaps, and compiles the
result into a temporary module.  None of that is exercised by the
golden-output suite, which calls scasp_load/2 on a file instead.
*/

test_dyncall :-
    run_tests([ dyncall_basic,
                dyncall_negation,
                dyncall_constraints,
                dyncall_collection,
                dyncall_constraints_global,
                dyncall_directives,
                dyncall_dynamic,
                dyncall_errors
              ]).

		 /*******************************
		 *            BASIC		*
		 *******************************/

:- begin_tests(dyncall_basic).

:- discontiguous bird/1.

bird(tweety).
penguin(sam).
bird(X) :- penguin(X).
ab(X) :- penguin(X).
flies(X) :- bird(X), not ab(X).

test(scasp_1_succeeds, [nondet]) :-
    scasp(flies(tweety)).
test(scasp_1_fails, fail) :-
    scasp(flies(sam)).
test(binds_the_query, all(X == [tweety])) :-
    scasp(flies(X)).
test(model_of_an_answer) :-
    scasp_models(flies(tweety), Models),
    assertion(models_equal(Models,
                           [[ not ab(tweety), bird(tweety), flies(tweety),
                              not penguin(tweety)
                           ]])).
test(no_answer_gives_no_models, all(X == [])) :-
    X = flies(sam),
    scasp(X).
test(all_answers_enumerated, set(X == [sam, tweety])) :-
    scasp(bird(X)).
test(tree_is_returned) :-
    scasp_proofs(flies(tweety), support, Skeleton),
    assertion(ground(Skeleton)).

:- end_tests(dyncall_basic).

		 /*******************************
		 *          NEGATION		*
		 *******************************/

:- begin_tests(dyncall_negation).

p(X) :- not q(X).
q(X) :- not p(X).

r(1).
-r(2).

test(even_loop_succeeds) :-
    scasp(p(1)).
test(naf_is_in_the_model) :-
    scasp_models(p(1), [Model|_]),
    assertion(memberchk(not q(1), Model)).
test(classical_negation_holds, [nondet]) :-
    scasp(-r(2)).
test(classical_negation_is_not_naf, fail) :-
    scasp(-r(1)).
test(naf_positive_holds, [nondet]) :-
    scasp(r(1)).
test(naf_does_not_follow_from_classical_negation, fail) :-
    scasp(r(2)).

:- end_tests(dyncall_negation).

		 /*******************************
		 *         CONSTRAINTS		*
		 *******************************/

:- begin_tests(dyncall_constraints).

big(X) :- X #> 10.
between_(X) :- X #> 1, X #< 5.
distinct(X) :- X #<> 7.

% The model of an unbound constrained query carries the constraint on the
% variable's attributes.  scasp/2 does not inline it into the term - that
% is inline_constraints/2, which the file front end applies - so the way
% to check it survived is that the variable still rejects a value outside
% the constraint and accepts one inside.

test(clpq_constraint_survives_to_the_model) :-
    scasp_models(big(_), [[big(X)]|_]),
    assertion(get_attr(X, clpqr_itf, _)),
    assertion(\+ X = 5),
    assertion(X = 20).
test(constrained_query_succeeds) :-
    scasp(big(20)).
test(constraint_excludes, fail) :-
    scasp(big(5)).
test(two_sided_constraint_admits_inside) :-
    scasp(between_(3)).
test(two_sided_constraint_excludes_outside, fail) :-
    scasp(between_(9)).
test(disequality_admits_other, fail) :-
    scasp(distinct(7)).
test(disequality_excludes_the_value, [nondet]) :-
    scasp(distinct(6)).

:- end_tests(dyncall_constraints).

		 /*******************************
		 *      CLAUSE COLLECTION	*
		 *******************************/

:- begin_tests(dyncall_collection).

% Only the predicates reachable from the query are compiled.  unrelated/1
% shares no call path with reached/1 and must not be pulled in.

reached(1).
reached(X) :- deeper(X).
deeper(2).
unrelated(3).

test(reachable_clauses_are_collected, set(X == [1,2])) :-
    scasp(reached(X)).
test(unrelated_predicate_is_not_collected) :-
    collected_heads(reached(_), Heads),
    assertion(\+ memberchk(unrelated/1, Heads)).
test(transitively_reachable_is_collected) :-
    collected_heads(reached(_), Heads),
    assertion(memberchk(deeper/1, Heads)),
    assertion(memberchk(reached/1, Heads)).

%!  collected_heads(:Query, -PIs) is det.
%
%   The predicates scasp_query_clauses/2 pulled in for Query.  Clauses
%   come back as source(ClauseRef, Module:(Head:-Body)).

collected_heads(Query, PIs) :-
    scasp_query_clauses(Query, Clauses),
    findall(Name/Arity,
            ( member(source(_Ref, Qualified), Clauses),
              strip_module(Qualified, _, (Head:-_Body)),
              functor(Head, Name, Arity)
            ),
            PIs0),
    sort(PIs0, PIs).

:- end_tests(dyncall_collection).

		 /*******************************
		 *      GLOBAL CONSTRAINTS	*
		 *******************************/

:- begin_tests(dyncall_constraints_global).

% A global constraint whose call tree overlaps the query must be applied,
% one that does not overlap must be ignored.  This is the part of
% scasp_query_clauses/2 that include_global_constraint/3 implements.
%
% In a Prolog module a global constraint is written `false :- Body.`;
% `:- Body.` is an ordinary Prolog directive here and would simply run at
% load time.  Only inside begin_scasp/end_scasp does `:- Body.` mean a
% constraint - see test_embed.pl.

:- discontiguous (-)/0.             % global constraints below

colour(red).
colour(blue).
chosen(X) :- colour(X), not rejected(X).
rejected(X) :- colour(X), not chosen(X).
false :- chosen(red).

other(1).
false :- other(2).

test(constraint_leaves_only_blue, set(X == [blue])) :-
    scasp(chosen(X)).
test(non_overlapping_constraint_is_ignored) :-
    scasp(other(1)).

:- end_tests(dyncall_constraints_global).

		 /*******************************
		 *          DIRECTIVES		*
		 *******************************/

:- begin_tests(dyncall_directives).

:- abducible maybe_it(1).
holds :- maybe_it(1).

% The template variable is reported singleton because the expansion of
% :- pred keeps it only in the head.  Real programs carry the same
% directive; every file in test/le_programs/ does.
:- style_check(-singleton).
:- pred shown(Thing) :: '@(Thing) is shown'.
:- style_check(+singleton).
shown(1).

test(abducible_can_be_in_the_model) :-
    scasp_models(holds, Models),
    assertion(model_has(Models, maybe_it(1))).
test(abducible_query_succeeds, [nondet]) :-
    scasp(holds).
test(pred_directive_does_not_break_solving) :-
    scasp(shown(1)).

:- end_tests(dyncall_directives).

		 /*******************************
		 *           DYNAMIC		*
		 *******************************/

:- begin_tests(dyncall_dynamic,
               [ cleanup(scasp_retractall(likes(_,_))) ]).

:- scasp_dynamic likes/2.

test(asserted_clauses_are_seen, [ setup(scasp_retractall(likes(_,_))) ]) :-
    scasp_assert(likes(jan, prolog)),
    scasp(likes(jan, prolog)).
test(retracted_clauses_are_gone, [ setup(scasp_retractall(likes(_,_))) ]) :-
    scasp_assert(likes(jan, prolog)),
    scasp_assert(likes(jan, lisp)),
    scasp_retract(likes(jan, prolog)),
    assertion(\+ scasp(likes(jan, prolog))),
    scasp(likes(jan, lisp)).
test(the_cache_is_invalidated,
     [ setup(( scasp_retractall(likes(_,_)),
               scasp_assert(likes(jan, prolog))
             )),
       set(X == [lisp, prolog])
     ]) :-
    scasp_models(likes(jan, _), _),      % compile and cache likes/2 ...
    scasp_assert(likes(jan, lisp)),      % ... then change it
    scasp(likes(jan, X)).

:- end_tests(dyncall_dynamic).

		 /*******************************
		 *           ERRORS		*
		 *******************************/

:- begin_tests(dyncall_errors).

% Two s(CASP) calls in one conjunction do not work.  embed.pl documents
% the opposite - "successive sCASP calls in one conjunction accumulate one
% joint model" - and scasp_call/1 implements it by feeding the previous
% call's stack to the next solve/4.  But save_stack/1 records that stack
% qualified with the module it belongs to, and the qualified term is
% passed back unchanged, so stack_parents/3 gets a term that is not a list
% and fails its det contract.  The line carries a `% TBD: Properly handle
% the module` comment.
%
% Passing a plain list gets one layer further and then fails in
% justification_tree/3 instead: two concatenated proofs have two roots and
% it builds a single tree, so the feature needs a decision about what the
% justification of a conjunction should be, not a one-line fix.
%
% Pinned here so it is visible and so this test fails, rather than the
% behaviour changing unnoticed, once that decision is made.

test(two_calls_in_one_conjunction_fail,
     [ throws(error(determinism_error(scasp_solve:stack_parents/3,
                                      det, fail, property), _)) ]) :-
    scasp(conj_a(_)),
    scasp(conj_b(_)).

conj_a(1).
conj_b(2).

uses_undefined :- no_such_predicate_anywhere.
uses_compiled  :- atom_length(abc, _).

% Passed per call rather than by setting the scasp_unknown flag: the flag
% is process-global, so changing it would race with any unit running
% concurrently under run_tests/2's jobs(N).

test(undefined_predicate_is_reported, [ throws(error(scasp_undefined(_), _)) ]) :-
    scasp(uses_undefined, [unknown(error)]).
test(compiled_predicate_is_refused,
     [ throws(error(permission_error(scasp, procedure, _), _)) ]) :-
    scasp(uses_compiled).

:- end_tests(dyncall_errors).
