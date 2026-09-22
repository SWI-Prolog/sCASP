:- module(test_proof_model,
          [ test_proof_model/0
          ]).
:- use_module(scasp_plunit).
:- use_module('../prolog/scasp').
:- use_module(proof_skeleton, [proof_skeleton/3, answers_model_key/2]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(pairs)).

/** <module> The model is a function of the proof

An answer carries a *partial* stable model - the subset of an answer set
supporting the query - and which subset that is depends on which proof was
found.  A more roundabout proof carries a larger model.

That raises the question of whether the model is an independent choice, which
would matter a great deal: `CLAUDE.md` compares models *exactly* in the golden
tests, so if two correct implementations could return different models for the
same proof, that comparison would be pinning an accident.

They cannot.  This suite checks the property:

> two answers whose proofs are equal as *sets of literal parent-child edges*
> always carry the same model.

`proof_skeleton/3` at `support` is exactly that equivalence - it deletes
bookkeeping nodes, reparents their children, and compares the edges as a set,
so two derivations differing only in the *order* of their steps map to the same
skeleton.  The property therefore says: **reordering the steps of a proof does
not change the model.**

What follows if it holds: the model is determined by the proof, so "which
partial model should a query return?" is not a separate question - it reduces
to "which proofs should the reasoner enumerate?".  And comparing the proof at
`support` subsumes comparing the model, rather than being independent of it.

Note the converse does *not* hold, and is not claimed: the reasoner may return
several answers for one proof class.  `two_clauses_same_literals` below is the
case - `s :- a, b.` and `s :- b, a.` are two clauses, two answers, one proof
and one model.  That redundancy is an implementation choice
(`doc/design-next.md`), not a semantic one.

These use `scasp/2` rather than `scasp_load/2` throughout.  The file front end
compiles into its caller's module and wipes what was there (`doc/bugs.md` #5),
which makes comparing two runs in one process unreliable; `scasp/2` collects
the clauses reachable from the query and has no such problem.
*/

test_proof_model :-
    run_tests([proof_determines_model]).

:- begin_tests(proof_determines_model).

		 /*******************************
		 *         THE PROGRAMS		*
		 *******************************/

% two derivations of p from *different* literals
q_. r_.
p_ :- q_.
p_ :- r_.

% two clauses with the same literals in a different order
a_. b_.
s_ :- a_, b_.
s_ :- b_, a_.

% one derivation properly contained in the other
lemma_ :- q_.
t_ :- lemma_.
t_ :- lemma_, a_.

% a body-only variable, so the dual introduces a forall
edge_(1,2). edge_(2,3).
reach_(X,Y) :- edge_(X,Y).
isolated_(X) :- not reach_(X,_).

% constraints, so the dual has to negate a store
num_(1). num_(2). num_(3). num_(4).
big_(X) :- num_(X), X #> 2.
small_(X) :- num_(X), not big_(X).

% an even loop: the atom holds in one answer set of two
pa_ :- not qa_.
qa_ :- not pa_.

% classical negation alongside default negation
-cold_(ice).
warm_(X) :- not -cold_(X).

program(different_literals,   p_).
program(same_literals,        s_).
program(nested_derivations,   t_).
program(forall_from_body_var, isolated_(4)).
program(negated_forall,       not isolated_(1)).
program(clp_positive,         small_(_)).
program(clp_negated,          not big_(_)).
program(even_loop,            pa_).
program(even_loop_negated,    not pa_).
program(classical_negation,   warm_(sun)).

		 /*******************************
		 *        THE PROPERTY		*
		 *******************************/

test(equal_proofs_carry_equal_models, [forall(program(Name, Goal))]) :-
    assertion(ground(Name)),
    answers(Goal, Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, ByProof),
    forall(member(Proof-Models, ByProof),
           ( sort(Models, Distinct),
             assertion(one_model(Distinct, Proof)) )).

one_model([_], _) :- !.
one_model(Models, Proof) :-
    length(Models, N),
    print_message(error,
                  format("one proof carries ~d distinct models", [N])),
    print_message(error, format("  proof: ~q", [Proof])),
    fail.

%   And, on the programs above, the correspondence is exact in the other
%   direction too: as many distinct models as distinct proofs.  That is a
%   consequence of the property rather than an independent claim, but it
%   fails loudly if the skeleton ever abstracts away something the model
%   depends on.

test(as_many_models_as_proofs, [forall(program(Name, Goal))]) :-
    assertion(ground(Name)),
    answers(Goal, Pairs),
    pairs_keys_values(Pairs, Proofs, Models),
    sort(Proofs, DistinctProofs), length(DistinctProofs, NP),
    sort(Models, DistinctModels), length(DistinctModels, NM),
    assertion(NP == NM).

%   The converse is not claimed: two clauses whose literals differ only in
%   order give two answers, one proof and one model.

test(two_clauses_same_literals_give_one_proof) :-
    answers(s_, Pairs),
    length(Pairs, Answers),
    pairs_keys(Pairs, Proofs), sort(Proofs, DistinctProofs),
    length(DistinctProofs, NP),
    assertion(Answers == 2),
    assertion(NP == 1).

%!  answers(:Goal, -Pairs) is det.
%
%   Pairs is Proof-Model for each answer to Goal, the proof taken at
%   `support` so that two derivations differing only in the order of
%   their steps compare equal.

:- meta_predicate answers(0, -).

answers(Goal, Pairs) :-
    findall(Proof-Model,
            ( scasp(Goal, [model(M), tree(T)]),
              answers_model_key([M], Model),
              proof_skeleton(T, support, Proof)
            ),
            Pairs).

:- end_tests(proof_determines_model).
