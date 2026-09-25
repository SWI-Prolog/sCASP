:- module(test_olon,
          [ test_olon/0
          ]).
:- use_module(under_test).
:- use_module(scasp_plunit).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(lists), [memberchk/2]).
:- use_module(library(apply), [maplist/3]).

/** <module> Loops over negation, coinduction and the NMR check

A loop through `not` is what separates s(CASP) from Prolog, and which
kind of loop it is decides the answer:

  - an **even** number of negations round the cycle may succeed
    *coinductively* - the goal is assumed because it is its own
    ancestor an even number of negations away, and the justification
    records that with a `chs/1` node;
  - an **odd** number cannot: it is an OLON, and `nmr_check.pl` turns it
    into a global constraint, so a program containing one has a stable
    model only if something outside the loop satisfies that constraint;
  - a **positive** loop is not supported at all, so the goal fails,
    exactly as the well-founded reading says.

Every expected answer below is the stable-model semantics worked out by
hand for that program, not what the implementation happened to print.
*/

test_olon :-
    run_tests([ even_loop,
                odd_loop,
                positive_loop,
                longer_cycles,
                olon_as_constraint,
                nmr_query_scope,
                justification_nodes
              ]).

		 /*******************************
		 *          EVEN LOOP		*
		 *******************************/

%   p :- not q.
%   q :- not p.
%
%   Two stable models, {p} and {q}: each atom is supported by the other
%   being absent.  So both p and q hold, each in its own model, and
%   neither `not p` nor `not q` is refuted.

:- begin_tests(even_loop).

p :- not q.
q :- not p.

test(one_side_holds) :-
    scasp(p).
test(the_other_side_holds) :-
    scasp(q).
% One supporting model, {p, not q} - stated as the set of distinct models
% rather than as an answer count, which is an implementation choice.

test(p_has_exactly_one_supporting_model) :-
    scasp_models(p, Models),
    maplist(msort, Models, Sorted),          % a model is a set
    sort(Sorted, Distinct),
    assertion(Distinct == [[p, not q]]).

test(the_model_records_the_absent_atom) :-
    scasp(p, [model(Model)]),
    assertion(memberchk(p, Model)),
    assertion(memberchk(not q, Model)).

test(negation_holds_too) :-
    scasp(not p).

:- end_tests(even_loop).

		 /*******************************
		 *           ODD LOOP		*
		 *******************************/

%   sp :- not sp.
%
%   The classic inconsistent program.  sp cannot be false, because then
%   its rule would derive it; and it cannot be true, because its only
%   support requires it to be false.  No stable model, so *nothing*
%   holds - not sp, and not `not sp` either.

:- begin_tests(odd_loop).

sp :- not sp.

test(the_atom_does_not_hold, fail) :-
    scasp(sp).
test(nor_does_its_negation, fail) :-
    scasp(not sp).

:- end_tests(odd_loop).

		 /*******************************
		 *        POSITIVE LOOP		*
		 *******************************/

%   pp :- pp.
%
%   Nothing supports pp, so the one stable model is empty: pp fails and
%   `not pp` holds.  solve.pl detects this as a positive loop rather
%   than looping forever.

:- begin_tests(positive_loop).

pp :- pp.

test(an_unsupported_atom_fails, fail) :-
    scasp(pp).
test(so_its_negation_holds) :-
    scasp(not pp).

% Left recursion through a conjunction is the same thing.

lr :- lr, base.
base.

test(left_recursion_fails, fail) :-
    scasp(lr).
test(its_negation_holds, [nondet]) :-
    scasp(not lr).

:- end_tests(positive_loop).

		 /*******************************
		 *       LONGER CYCLES		*
		 *******************************/

%   Parity is what counts, not length.
%
%   a3 :- not b3.  b3 :- not c3.  c3 :- not a3.        three negations
%   a4 :- not b4.  b4 :- not c4.  c4 :- not d4.  d4 :- not a4.   four
%
%   The three-cycle is odd, so it is an OLON and has no stable model.
%   The four-cycle is even and has two, {a4,c4} and {b4,d4}.

:- begin_tests(longer_cycles).

a3 :- not b3.
b3 :- not c3.
c3 :- not a3.

a4 :- not b4.
b4 :- not c4.
c4 :- not d4.
d4 :- not a4.

test(odd_cycle_has_no_model, fail) :-
    scasp(a3).
test(odd_cycle_negation_has_none_either, fail) :-
    scasp(not a3).

test(even_cycle_holds) :-
    scasp(a4).
test(even_cycle_negation_holds_too) :-
    scasp(not a4).
test(the_even_cycle_model_alternates) :-
    scasp(a4, [model(Model)]),
    assertion(memberchk(a4, Model)),
    assertion(memberchk(c4, Model)).

:- end_tests(longer_cycles).

		 /*******************************
		 *      OLON AS A CONSTRAINT	*
		 *******************************/

%   An OLON with an extra condition is a constraint rather than an
%   outright contradiction: `p :- not p, q.` says "if q then p must
%   hold", because any model with q and without p would derive p.
%   nmr_check.pl compiles exactly that into the NMR check.

:- begin_tests(olon_as_constraint).

% The condition holds and nothing else can supply p, so there is no
% model at all.

p1 :- not p1, q1.
q1.

test(the_constraint_cannot_be_met, fail) :-
    scasp(p1).
test(and_its_negation_fails_too, fail) :-
    scasp(not p1).

% The condition fails, so the constraint is vacuous and the empty model
% stands.

p2 :- not p2, q2.
q2 :- fails_always.

test(a_vacuous_constraint_leaves_the_atom_false, fail) :-
    scasp(p2).
test(so_its_negation_holds) :-
    scasp(not p2).

% The condition holds and another rule supplies p, which satisfies the
% constraint: one model, {p3, r3}.

p3 :- not p3, q3.
p3 :- r3.
r3.
q3.

test(a_satisfiable_constraint_forces_the_atom, [nondet]) :-
    scasp(p3).
test(so_its_negation_fails, fail) :-
    scasp(not p3).
test(the_model_shows_what_supplied_it, [nondet]) :-
    scasp(p3, [model(Model)]),
    assertion(memberchk(p3, Model)),
    assertion(memberchk(r3, Model)).

:- end_tests(olon_as_constraint).

		 /*******************************
		 *      THE SCOPE OF THE CHECK	*
		 *******************************/

%   The two front ends disagree about an OLON the query cannot reach,
%   and a reimplementation needs to know which it is copying.
%
%   scasp/2 collects only the clauses reachable from the query, so an
%   OLON elsewhere in the module is not part of the program it compiles
%   and cannot constrain the answer.  The file front end compiles the
%   whole file, so the same query yields no model at all.
%
%   Here q4 is reachable on its own; the OLON on p4 is not reachable
%   from it.  Under whole-program semantics this program is
%   inconsistent, so `q4` answering at all is a consequence of the
%   query-relative scoping, not of the stable-model semantics.
%   test/all_programs/olon_unreachable.pl pins the other side.

:- begin_tests(nmr_query_scope).

p4 :- not p4, q4.
q4.

test(an_unreachable_olon_does_not_constrain_the_query) :-
    scasp(q4).
test(but_reaching_it_does, fail) :-
    scasp(p4).

:- end_tests(nmr_query_scope).

		 /*******************************
		 *     JUSTIFICATION NODES	*
		 *******************************/

%   The tree records *why* each step held.  A coinductive assumption is
%   wrapped in chs/1, and a goal already proved in a completed branch in
%   proved/1, rather than being proved again.

:- begin_tests(justification_nodes).

cp :- not cq.
cq :- not cp.

dup :- s, t.
s :- u.
t :- u.
u.

test(coinductive_success_is_marked) :-
    scasp(cp, [tree(Tree)]),
    proof_literals(Tree, Literals),
    assertion(memberchk(chs-cp, Literals)).

test(a_repeated_subgoal_is_marked_proved) :-
    scasp(dup, [tree(Tree)]),
    proof_literals(Tree, Literals),
    assertion(memberchk(plain-u, Literals)),
    assertion(memberchk(proved-u, Literals)).

test(both_branches_are_in_the_justification) :-
    scasp(dup, [tree(Tree)]),
    proof_literals(Tree, Literals),
    assertion(memberchk(plain-s, Literals)),
    assertion(memberchk(plain-t, Literals)).

:- end_tests(justification_nodes).
