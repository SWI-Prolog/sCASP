/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(sscasp_solve,
          [ check/7,                    % +Goal, +Neg, +Pol, +Anc, +S0,
                                        %   -Anc1, -Action
            proved/4,                   % +Anc1, +S0, +S2, -S
            done/4,                     % +Goal, +Action, +S0, -S
            dcc/4,                      % +Goal, +Anc, +S, +Rules
            for_domain/6,               % +Var, :Gen, :Goal, +Anc, +S0, -S
            forall/5,                   % +Vars, :Goal, +Anc, +S0, -S
            call_goal/4,                % :Goal, +Anc, +S0, -S
            findall_/6,                 % +Template, :Goal, -List, +Anc,
                                        %   +S0, -S
            sld_goal/5,                 % +Goal, +Type, +Anc, +S0, -S
            model/2,                    % +Proof, -Model
            justification/2,            % +Proof, -Forest
            children/3,                 % +S2, +S0, -Children
            empty_proof/1               % -Proof
          ]).
:- use_module(ops).
:- use_module(mangle, [demangle/2]).
:- use_module(sld, [sld_proof/3]).
:- use_module(library(lists), [member/2, append/3, same_length/2, nth1/3]).
:- use_module(library(pairs), [pairs_keys_values/3]).
:- use_module(library(clpq), [dump/3, entailed/1, inf/2, sup/2, {}/1]).

%   CLP(R) for nothing it exports.  `library(clpqr)` compiles one source
%   into a `_q` and an `_r` instance and its shared modules name both, so
%   loading only CLP(Q) leaves the `_r` halves referenced and undefined.
%   Nothing calls them, but `swipl -c --undefined=error` cannot know that
%   and the saved state will not build.  The old implementation does the
%   same, in `scasp/clp/clpq.pl`.

:- use_module(library(clpr), []).
                                        % also: the emitted program calls {}/1,
                                        % via run.pl importing clpq into its module
%   The Herbrand store is `herbrand.pl`'s: `dif/2` and `∉` behind one
%   interface, so that nothing here has to know which of the two holds a
%   given disequality.  `apart/2` is both the constraint's name in a
%   description and the way to post it again.

:- use_module(herbrand, [ apart/2, constrained/1, node_count/2,
                          read_store/2, projection/4
                        ]).
:- use_module(library(apply), [maplist/2, maplist/3, include/3, foldl/4]).
:- use_module(library(assoc), [empty_assoc/1, get_assoc/3, put_assoc/4]).

%   With the imports and before the first clause, because a
%   meta-predicate mode is part of the module's **signature**: a reader
%   taking the interface from the head of the file stops at the first
%   clause.
%
%   `call_goal/4` is the one goal here that is called in the *caller's*
%   module.  Everything a quantifier holds as a term is qualified by
%   `emit.pl` when it writes the call, because a declaration cannot do
%   it: it applies when a clause is compiled and those are `assertz/1`-ed.

:- meta_predicate
    call_goal(0, +, +, -).


/** <module> The solver's runtime

The compiled program *is* Prolog, so most of solving is Prolog calling it.
What Prolog does not do by itself is the part that makes goal-directed
evaluation agree with Gelfond–Lifschitz: before expanding a literal, look at
what is already assumed.

Three extra arguments carry what that needs.  `Anc` is the ancestor chain and
goes **down**; `S0`/`S` carry the proof and come **up**.  They are different
things with different modes, so they are different arguments; nothing is
reconstructed from anything else.

An ancestor frame is `a(Goal, Negation, Polarity)`: the goal, the goal it
contradicts, and `+` for a literal or `-` for a negated one.  All three are
*static* — the compiler knows a predicate's polarity from its `Kind` field and
emits the negated functor as a constant — so nothing is demangled while
solving.

## The proof is a forest, and an index of it

A proof node is `n(Goal, Status, Children)`, and `Children` is a list of the
same, so what comes up the third argument is the **justification tree** —
there is no reconstruction pass.

Beside it travels an index from `Name/Arity` to the goals proved under it.
That is derived state, which decision 3 was right to be wary of, but the
objection it makes is to *rebuilding* one structure from another on every
entry — `stack_parents/2` and `stack_proved/2` — and this is not that.  It is
maintained as the proof grows, shares the goal terms rather than copying
them, and exists because the alternative was measured: walking the whole
forest to answer "is this literal's negation already assumed?" made the
solver quadratic in the size of the proof, and `queens.pl` never finished.
The old implementation indexes the same lookup the same way.

It costs nothing to build.  A literal's children are exactly what its body
added: the body is called with the literal's own `S0` and returns `S2`, and
since a proof is only ever *prepended* to, `S0` is a suffix of `S2` and the
difference is the children.  `children/3` splits it by walking to the shared
tail.

The model falls out of the same structure — the literals of the forest,
sorted — so it is not carried separately either.  That is the property
`doc/design-next.md` open question 5 settles: the model is a function of the
proof, so representing the proof represents the model.

`Status` says how the literal was established: `proof` from its clauses, `chs`
by the coinductive hypothesis, `proved` by an earlier branch.  The last two
have no children, and showing them is the point — a justification that dropped
them would claim a literal was proved from nothing.

## Counting negations

The number of negations between a goal and an ancestor is the number of
**polarity transitions** on the path, which is the number of negative edges on
the corresponding dependency path.  Counting `dual` frames instead would
undercount: dualising `q :- not p` turns `not q`'s body into a positive call to
`p`, so one of the two negations in `p :- not q. q :- not p.` is consumed by
the compilation and never appears as a frame.

## Three cases, not six

| | |
|---|---|
| the goal's negation unifies with an ancestor or a proved literal | fail |
| an ancestor unifies with the goal across **0** transitions | by polarity — below |
| an ancestor unifies with the goal across an **even, non-zero** number | succeed coinductively |
| the goal is ground and already proved | succeed |

**There is no odd case.**  Two frames of the same polarity have an even number
of transitions between them by construction, so an ancestor that unifies with
the goal is never reached across an odd number of negations — an odd loop
reaches the goal's *negation* instead, and that is the first row.

## A loop with no negation in it depends on which side you are on

For a **literal**, a loop with no negation is a literal supported only by
itself.  `c :- c` gives `c` no support, so it fails; succeeding would compute
the greatest fixpoint, which is not what a stable model is.

For a **negated literal** the same loop says the opposite, and it has to.
Dualising `c :- c` gives `not c :- not c`, so asking `not c` reaches itself
across no negation.  But `c` being unfounded is exactly why `not c` *holds* —
the loop is the proof that the clause cannot support `c` without `c`, so the
clause does not support it.  Failing here would make `not c` unprovable for
every unfounded `c`, which the oracle catches at once: `[a, b, (c :- c),
(:- not c, not b)]` has the answer set `{a, b}` and nothing could reach it.

So the rule is by polarity: a zero-transition loop fails on `+` and succeeds
on `-`.  The two readings are the same reading — a clause whose body needs
what it is trying to prove supports nothing — seen from either side.
*/

		 /*******************************
		 *         THE CHECK		*
		 *******************************/

%!  check(+Goal, +Neg, +Pol, +Anc, +S0, -Anc1, -Action) is semidet.
%
%   Decide what to do with Goal before expanding it.  Action is `expand`
%   when its clauses should be tried, in which case Anc1 is the chain its
%   body runs under; otherwise it says how the literal is already
%   established — `chs` by the coinductive hypothesis, `proved` by an
%   earlier branch — and becomes the node's status.
%
%   Fails when assuming Goal would contradict something assumed, which is
%   what makes the failure of an odd loop a failure of the query.
%
%   ## Only a proof that rests on nothing may be reused
%
%   `proved_goal/2` asks the index for a literal already established, and
%   answers only for one proved **from no assumption**.  Reusing one that
%   leaned on the coinductive hypothesis is how a positive loop closes
%   without anything noticing:
%
%       p :- not q, r.        % the only answer set is {q}
%       q :- not r.
%       r :- p.
%
%   `p` -> `not q` -> (q's dual needs `r`) -> `r` -> `r :- p` -> chs(p):
%   two negations, so the even-loop rule lets it stand.  Then `r` is
%   reused for `p`'s own body, and p's support is r, whose support is the
%   assumption of p.  The old implementation answers `p` here, and then
%   fails `r` — a literal in the model it just reported.
%
%   P4 §3.2 offers the reuse as an optimisation, "it avoids
%   recomputation", and it is worth keeping where it is safe: qualifying
%   it costs 4% of the differential sweep, where dropping it altogether
%   costs 19.5 s against 217 s.  So the rule is about the proof rather
%   than about the reuse, and `proved/4` carries the answer.

check(Goal, Neg, Pol, Anc, S0, Anc1, Action) :-
    consistent(Neg, Anc, S0),
    (   ancestor(Anc, Goal, Pol, Negations)
    ->  loop_succeeds(Negations, Pol),
        Action = chs,
        Anc1 = Anc
    ;   ground(Goal),
        proved_goal(S0, Goal)
    ->  Action = proved,
        Anc1 = Anc
    ;   Action = expand,
        Anc1 = [a(Goal, Neg, Pol)|Anc]
    ).

%!  consistent(+Neg, +Anc, +S0) is semidet.
%
%   Keep the goal from contradicting something already assumed, whether
%   an open ancestor or something proved.  Both count: a model holding
%   `p` and `not p` is not a model, and it makes no difference which
%   branch of the proof put them there.
%
%   It **constrains** rather than tests, and that is not a refinement.
%   Asking `not dom(X)` for an unbound X where `dom(1)` is proved is not
%   a contradiction — it is a contradiction only at X = 1 — so refusing
%   it loses every answer that needed the other values.  `dif/2` says
%   exactly that, and says it on the variable: two literals that could be
%   made to contradict are constrained so that they cannot.
%
%   This is `check_CHS_`'s last case, "constrain the goal against the
%   negations already visited", and it is the same decision-4 move as
%   using `dif/2` for `\=` — the relation stated so that it narrows an
%   answer instead of rejecting it.
%
%   Only literals that could unify are worth constraining, so the functor
%   is compared first.  That keeps the common case a scan and posts no
%   constraint at all.

consistent(Neg, Anc, proof(_, Index)) :-
    functor(Neg, Name, Arity),
    consistent_ancestors(Anc, Name, Arity, Neg),
    relatives(Name/Arity, Index, Relatives),
    consistent_relatives(Relatives, Neg).

%   The ancestors are a list because they are one: the chain is as deep
%   as the proof and indexing it would cost more than walking it.

consistent_ancestors([], _, _, _).
consistent_ancestors([a(Goal, _, _)|T], Name, Arity, Neg) :-
    (   functor(Goal, Name, Arity)
    ->  apart(Goal, Neg)
    ;   true
    ),
    consistent_ancestors(T, Name, Arity, Neg).

consistent_relatives([], _).
consistent_relatives([_-Goal|T], Neg) :-
    apart(Goal, Neg),
    consistent_relatives(T, Neg).

%!  relatives(+PI, +Index, -Goals) is det.
%
%   The goals proved under this functor.  Nothing else can contradict a
%   literal, so nothing else is looked at.

relatives(PI, Index, Goals) :-
    (   get_assoc(PI, Index, Goals)
    ->  true
    ;   Goals = []
    ).

%!  proved_goal(+Proof, +Goal) is semidet.
%
%   Goal is already proved, anywhere in the forest.  A literal proved
%   inside a sub-proof is proved: it makes no difference to a model which
%   branch established it, which is why the index holds every node and
%   not only the ones still at the top level.

proved_goal(proof(_, Index), Goal) :-
    functor(Goal, Name, Arity),
    relatives(Name/Arity, Index, Relatives),
    established(Goal, Relatives).

established(Goal, [Status-G|T]) :-
    (   Status == proof,
        G == Goal
    ->  true
    ;   established(Goal, T)
    ).

memberchk_eq(Goal, [G|T]) :-
    (   G == Goal
    ->  true
    ;   memberchk_eq(Goal, T)
    ).

%!  ancestor(+Anc, +Goal, +Pol, -Negations) is semidet.
%
%   The nearest ancestor the goal makes a **loop** with, and the number
%   of polarity transitions on the path from it down to the goal.
%
%   Merely *unifying* with an ancestor is not a loop, and reading it as
%   one is wrong in both directions.  `initiates(E, F, T)` unifies with
%   an ancestor `initiates(overflow, spilling, 15)` and is not the same
%   question — it is a more general one, and the ancestor says nothing
%   about the other instances.  Taking it for a loop binds E, F and T to
%   that one instance and then fails it as a positive loop, which is how
%   `holdsAt(spilling, T)` came to have no proof in any of the event
%   calculus programs.
%
%   So a loop is one of two things, which is what
%   `solve.pl:check_parents/4` tests as well:
%
%     - the goal is a **variant** of the ancestor — the same question
%       with the variables renamed, which is a loop whichever polarity
%       it is on and whatever the negation count;
%     - the ancestor **subsumes** the goal, and the path crosses no
%       negation, in which case the goal is an instance of something
%       already being proved from itself and so unfounded.  Upstream
%       reads it the same way, in `type_loop_fail_pos/2`.
%
%   Anything else is an ordinary call and the walk goes on past it.

ancestor(Anc, Goal, Pol, Negations) :-
    ancestor_(Anc, Goal, Pol, 0, Negations).

ancestor_([a(A, _, AP)|T], Goal, Pol, N0, N) :-
    transition(AP, Pol, N0, N1),
    (   loop(A, Goal, N1, Pol)
    ->  A = Goal,
        N = N1
    ;   ancestor_(T, Goal, AP, N1, N)
    ).

loop(A, Goal, _, _) :-
    same_question(A, Goal),
    !.
loop(A, Goal, 0, +) :-
    subsumes_term(A, Goal).

%!  same_question(+A, +Goal) is semidet.
%
%   True when Goal is the ancestor's question again: the same literal
%   with its variables renamed, asking no more of the dense store than
%   the ancestor already did.
%
%   Both halves are needed and `=@=` alone is neither.  **It sees
%   attributes**: `f(X) =@= f(_)` is false once X carries a constraint,
%   and two variables carrying *different* ones are never variants — so
%   a recursion whose steps constrain their own variable never looks like
%   a loop, which is every recursion over time in an event-calculus
%   program.  `bec_loan` descends
%   `not holdsAt(requested_on(the_borrower, 1000, 2), T)` ->
%   `not initiates(ends(1), is_liable_to_litigation(the_lender), T)` ->
%   the same pair one step earlier in time, for ever: the ancestor chain
%   reaches **6,677** frames of it.
%
%   Ignoring the store instead is wrong the other way, and `ancestor/4`
%   shows where: it unifies the goal with the ancestor it matched, and a
%   goal that is *less* constrained would be narrowed by one that is
%   more.  That unification then simply fails, so the loop is not
%   reported either.
%
%   So the ancestor's store must be **entailed** by the goal's, which is
%   exactly what makes that unification add nothing.  It is upstream's
%   rule (`prolog/scasp/clp/clpq.pl:entail_terms/2`), and the order
%   matters for cost: the term comparison is what produces a candidate at
%   all, and only a candidate is worth a projection.  The comparison is
%   itself asked once per *ancestor* per goal, so the functors are
%   compared first and an ordinary `=@=` is tried before the one that has
%   to take the store apart — the chain is long and almost none of it
%   looks like the goal.

same_question(A, Goal) :-
    functor(A, Name, Arity),
    functor(Goal, Name, Arity),
    (   A =@= Goal
    ->  true                    % identical stores: nothing left to ask
    ;   \+ \+ ( term_attvars(A-Goal, Vars),
                 maplist(del_attrs, Vars),
                 A =@= Goal ),
        herbrand_instance(A, Goal),
        dense_instance(A, Goal)
    ).

%!  herbrand_instance(+A, +Goal) is semidet.
%
%   True when unifying the ancestor into the goal adds no `dif/2`: every
%   or-node A carries, Goal carries already.
%
%   This is the Herbrand half of the same rule `dense_instance/2` states
%   for the dense store, and on `bec_loan` it is the half that matters —
%   of the 1,378 ancestors that are the goal's own question with the
%   stores set aside, only **2** are still variants once the dense store
%   is put back, so what makes them differ is the disequalities
%   `consistent/3` posts, not the arithmetic.
%
%   Counted rather than compared, because `dif/2` keeps its or-nodes in
%   a `vardif/2` attribute shared between the variables they relate:
%   unifying cannot lose one, so the count is unchanged exactly when
%   nothing was added.  Reading it this way also avoids
%   `dif:attribute_goals//1`, which deletes what it reports.
%
%   It is the half that decides whether a loop is seen at all, and it is
%   strict: it holds when the ancestor's disequalities are among the
%   goal's, which is what happens when they accumulate down one branch,
%   and not when the two goals were each constrained against literals the
%   other never met.  A recursion of the second kind is still not
%   detected, and that is the safe direction — the loop's verdict on a
%   positive, zero-transition cycle is to **fail** the goal, so accepting
%   one that does not hold would lose answers.

herbrand_instance(A, Goal) :-
    node_count(Goal, Before),
    (   Before == 0
    ->  \+ \+ A = Goal          % nothing to lose, but they must agree
    ;   \+ \+ ( A = Goal,
                 node_count(Goal, After),
                 After == Before )
    ).

%!  dense_instance(+A, +Goal) is semidet.
%
%   True when everything A's dense store says is already said by Goal's.
%
%   The two are projected onto **one** set of fresh names, which is what
%   pairs their variables up: the terms are variants by the time this is
%   asked, so `term_variables/2` walks them in the same order.  Goal's
%   constraints are then posted and A's asked of them — one A carries
%   that Goal does not is one the goal would be narrowed by, and there is
%   no loop.
%
%   `within/2` first, because a projection is the expensive thing here
%   and most candidates fail on their bounds alone.

dense_instance(A, Goal) :-
    term_variables(Goal, GoalVars),
    (   include(dense, GoalVars, [_|_])
    ->  term_variables(A, AncestorVars),
        within(GoalVars, AncestorVars),
        \+ \+ ( dump(GoalVars, Names, GoalStore0),
                 dump(AncestorVars, Names, AncestorStore0),
                 include(about(Names), GoalStore0, GoalStore),
                 include(about(Names), AncestorStore0, AncestorStore),
                 maplist(posted, GoalStore),
                 maplist(entailed, AncestorStore) )
    ;   true                    % nothing dense to say anything about
    ).

posted(Constraint) :-
    {Constraint}.

%   A constraint `dump/3` could not eliminate a variable from — the slack
%   of an `=\=`, or anything relating these variables to one the proof
%   invented — says nothing about the variables asked for: its projection
%   onto them is `true`.  Both stores drop those, or each dump reports
%   its own fresh variable for the same thing and nothing is ever
%   entailed.  `residue/3` follows the same rule for the same reason.

about(Names, Constraint) :-
    term_variables(Constraint, Variables),
    forall(member(Variable, Variables), memberchk_eq(Variable, Names)).

%!  within(+GoalVars, +AncestorVars) is semidet.
%
%   A cheap **necessary** condition for the projection test: each of the
%   goal's variables lies inside the bounds of the ancestor's
%   corresponding one.  `inf/2` and `sup/2` are simplex over the store
%   the two already share, where `dump/3` is Fourier–Motzkin over its
%   whole class.
%
%   Necessary, not sufficient, so it may only **reject**: it says nothing
%   about how the variables are related to each other.  Rejecting a loop
%   that does hold is the safe direction — a loop not reported is a
%   branch explored.  So is reading the bounds from the store the two are
%   *both* in, where each looks at least as tight as it is alone, and so
%   is walking the two lists in step: they pair up because the terms are
%   variants by the time this is asked, and where they do not the walk
%   runs out and fails.

within([], []).
within([Goal|Goals], [Ancestor|Ancestors]) :-
    bounds(Ancestor, Low, High),
    (   Low == none,
        High == none
    ->  true                        % the ancestor says nothing about it
    ;   bounds(Goal, GoalLow, GoalHigh),
        at_least(GoalLow, Low),
        at_most(GoalHigh, High)
    ),
    within(Goals, Ancestors).

bounds(Var, Low, High) :-
    (   dense(Var)
    ->  (   inf(Var, Low) -> true ; Low = none ),
        (   sup(Var, High) -> true ; High = none )
    ;   number(Var)
    ->  Low = Var,
        High = Var
    ;   Low = none,
        High = none
    ).

at_least(_, none) :- !.
at_least(none, _) :- !, fail.
at_least(Goal, Low) :- Goal >= Low.

at_most(_, none) :- !.
at_most(none, _) :- !, fail.
at_most(Goal, High) :- Goal =< High.

transition(P, P, N, N) :- !.
transition(_, _, N0, N) :- N is N0+1.

%!  loop_succeeds(+Negations, +Polarity) is semidet.
%
%   A loop across at least one negation succeeds coinductively whichever
%   side it is on.  A loop across none is a clause whose body needs what
%   it is proving: no support for a literal, and for a negated one, the
%   proof that there is none.

loop_succeeds(Negations, _) :-
    Negations > 0,
    !.
loop_succeeds(0, -).

%!  proved(+Anc1, +S0, +S2, -S) is det.
%
%   Record the goal just proved from its clauses.  Anc1 is the chain
%   `check/7` built, so its head names the goal; S0 is what the proof
%   held before the body ran and S2 what it holds after, so the
%   difference is the children.

proved([a(Goal, _, _)|_], proof(F0, _), proof(F2, Index2), proof(Forest, Index)) :-
    forest_children(F2, F0, Children),
    (   assumed(Children)
    ->  Status = assumed
    ;   Status = proof
    ),
    Forest = [n(Goal, Status, Children)|F0],
    index(Goal, Status, Index2, Index).

%!  assumed(+Children) is semidet.
%
%   True when the body leaned on the coinductive hypothesis anywhere
%   beneath it.  Only the direct children are looked at: each of them
%   already carries the answer for its own subtree.

assumed([n(_, Status, _)|T]) :-
    (   resting(Status)
    ->  true
    ;   assumed(T)
    ).

resting(chs).
resting(assumed).

%   Index2, not the one this literal started with: the body's proofs
%   become this node's children rather than being discarded, so what
%   they added to the index stays there too.

index(Goal, Status, Index0, Index) :-
    functor(Goal, Name, Arity),
    relatives(Name/Arity, Index0, Goals),
    put_assoc(Name/Arity, Index0, [Status-Goal|Goals], Index).

%!  done(+Goal, +Action, +S0, -S) is det.
%
%   Record a literal established without expanding it.  It is a node with
%   no children, which is what it is: nothing was proved *from* it.

done(Goal, Action, proof(Forest, Index0), proof([n(Goal, Action, [])|Forest], Index)) :-
    (   Action == chs
    ->  Status = chs
    ;   Status = proof
    ),
    index(Goal, Status, Index0, Index).

%!  dcc(+Goal, +Anc, +S, +Rules) is semidet.
%
%   The dynamic consistency check (`--dcc`, `compile.pl:dcc_rules/3`):
%   fail when Goal, just established and ground, completes a global
%   constraint with literals the proof already holds or is proving.
%   Rules are the `dcc(Trigger, Others)` whose trigger is Goal's
%   predicate; an other is `lit(Literal)`, found in the proof or among
%   the ancestors, or `run(Goal)`, a built-in.
%
%   **Only prunes.**  Everything the proof holds and every open ancestor
%   ends up in the model if this branch succeeds, so a constraint whose
%   literals are all there would fail the NMR check anyway.  That is not
%   true inside a quantifier's complement, whose proof is thrown away,
%   so the check is off there: `sscasp_no_dcc`, which covered/7 and
%   elsewhere/7 set for the length of the run.
%
%   Tried inside `\+ \+`: finding the others binds and constrains, and
%   none of it is the answer's.

dcc(Goal, Anc, S, Rules) :-
    (   ground(Goal),
        \+ nb_current(sscasp_no_dcc, true),
        member(Rule, Rules),
        copy_term(Rule, dcc(Goal, Others)),
        \+ \+ dcc_holds(Others, Anc, S)
    ->  fail
    ;   true
    ).

dcc_holds([], _, _).
dcc_holds([Other|Others], Anc, S) :-
    dcc_other(Other, Anc, S),
    dcc_holds(Others, Anc, S).

dcc_other(lit(Literal), Anc, S) :-
    (   S = proof(_, Index),
        functor(Literal, Name, Arity),
        relatives(Name/Arity, Index, Relatives),
        member(_-Literal, Relatives)
    ;   member(a(Literal, _, _), Anc)
    ).
dcc_other(run(Goal), _, _) :-
    call(Goal).

%!  empty_proof(-Proof) is det.

empty_proof(proof([], Index)) :-
    empty_assoc(Index).

%!  children(+S2, +S0, -Children) is det.
%
%   The nodes S2 holds and S0 does not.  A proof is only ever prepended
%   to, so S0 is a suffix of S2 and this is a walk to the shared tail —
%   which is why building the tree costs nothing beyond the nodes.

children(proof(F2, Index), proof(F0, _), proof(Children, Index)) :-
    forest_children(F2, F0, Children).

forest_children(F2, F0, Children) :-
    (   F2 == F0
    ->  Children = []
    ;   F2 = [Node|T],
        Children = [Node|Rest],
        forest_children(T, F0, Rest)
    ).

		 /*******************************
		 *        QUANTIFIERS		*
		 *******************************/

%!  for_domain(+Var, :Generator, :Goal, +Anc, +S0, -S) is nondet.
%
%   Goal holds for every value Generator gives Var.  This is decision 5's
%   case 1 and the reason a domain is worth declaring: the quantifier has
%   something to range over.
%
%   Each of the generator's answers is a *restriction* on Var rather than
%   necessarily a value — an enumerated domain gives values, a range
%   gives one CLP(Q) region — so each is handed to `cover/6` in turn,
%   which is the same machinery `forall/5` uses.  A value is the case
%   where it has nothing left to do.
%
%   The proofs accumulate, so every literal the quantifier needed is in
%   the model: the quantifier is part of the answer, not a side condition
%   checked and forgotten.

for_domain(Var, Generator, Goal, Anc, S0, S) :-
    findall([Copy]-Attributes,
            ( call_goal(Generator, Anc, S0, _),
              copy_term(Var, Copy, Attributes)
            ),
            Restrictions),
    cover_each(Restrictions, [Var], Goal, [], Anc, S0, S).

%!  forall(+Var, :Goal, +Anc, +S0, -S) is nondet.
%
%   Goal holds for *every* value of Var, with no domain to enumerate.
%   The domain is whatever Var is already constrained to, which for a
%   quantifier the compiler introduced is everything.
%
%   ## One covering, and then somewhere else
%
%   There are two reasons a quantifier could be asked for another answer,
%   and they are not the same.  A covering is a *proof* that the goal
%   holds everywhere, and the goal holding everywhere is one fact,
%   however many ways there are to show it — so a second covering of the
%   same ground is a second copy of the same answer.  What is a second
%   answer is the goal holding everywhere *while the rest of the program
%   says something else*: the covering constrains the variables the goal
%   shares with its caller, and where those can be constrained
%   differently there is more to find.
%
%   So the covering is committed to, and the alternatives come from
%   partitioning the other variables against what it fixed them to.
%   This is `solve.pl:solve_other_forall/11`'s shape and it is decision
%   5c reversed — the decision was right that cutting *alone* loses
%   answers, and wrong that the cut was therefore not transferable.
%   `bec_light`, enumerating its answers:
%
%       nothing cut              500 answers, **1** distinct, no end
%       cut, no partitioning       2 answers in 0.12 s, but `family`'s
%                                  NMR check and `bec_tap_01`'s query
%                                  lose their only answer
%       cut and partitioned        2 answers in 0.23 s, which is the
%                                  count the old implementation gives
%
%   500 answers carrying one model is what a quantifier looks like when
%   its coverings are alternatives: a branch's proof is discarded, so a
%   different way of discharging one cannot change the answer it is part
%   of.  The whole corpus is faster for it — `queens.pl` 0.113 s to
%   0.063 s — and the other four event-calculus programs still do not
%   finish, so the search they are lost in is something else again.

forall(Vars, Goal, Anc, S0, S) :-
    outer_variables(Goal, Vars, Others),
    forall_(Vars, Goal, Others, Anc, S0, S).

forall_(Vars, Goal, Others, Anc, S0, S) :-
    (   once(cover(Vars, Goal, [], Anc, S0, S))
    ;   elsewhere(Vars, Goal, Others, Anc, S0, Names, Pieces),
        Names = Others,
        member(Piece, Pieces),
        maplist(call, Piece),
        forall_(Vars, Goal, Others, Anc, S0, S)
    ).

%!  elsewhere(+Vars, :Goal, +Others, +Anc, +S0, -Names, -Pieces) is semidet.
%
%   Where else to look, once the first covering has been committed to.
%
%   The covering is run again and thrown away — `fail` undoes it, and
%   `nb_setarg/3` is how the one thing worth keeping gets out: a
%   *description* of what it fixed the goal's other variables to, as
%   plain terms over fresh Names that positional unification ties back.
%   Nothing attributed crosses, so nothing copies a constraint store.
%
%   Running it twice is the price of not copying the solver's state the
%   way `solve_other_forall/11` does, and it is only paid when a second
%   answer is asked for: the first costs one covering, which is what the
%   commitment was for.

elsewhere(_Vars, _Goal, Others, _Anc, _S0, _Names, _Pieces) :-
    ground(Others),
    !,
    fail.
elsewhere(Vars, Goal, Others, Anc, S0, Names, Pieces) :-
    State = state(-),
    (   b_setval(sscasp_no_dcc, true),
        once(cover(Vars, Goal, [], Anc, S0, _)),
        snapshot(Others, Others, Names0, Constraints),
        alternatives(Constraints, [], Pieces0),
        Pieces0 \== [],
        nb_setarg(1, State, Names0-Pieces0),
        fail
    ;   arg(1, State, Snapshot),
        Snapshot \== (-),
        Snapshot = Names-Pieces
    ).

%!  snapshot(+Kept, +Wanted, -Names, -Constraints) is det.
%
%   Wanted is the part of Kept the caller will keep constraints *about*;
%   the rest is there so that a constraint relating the two survives the
%   projection.  When nothing in Wanted is in the CLP(Q) store there is
%   no region to compute — every constraint `dump/3` returned would be
%   discarded — and skipping it is 71% of the projections on
%   `bec_tap_01`, which is where a third of that program's time goes.
%
%   What the covering left on those variables, said over
%   fresh plain Names that `forall_/6` ties back by unifying them: a
%   binding is an equation, a `dif/2` is what it excludes, and the dense
%   part is the projection of the store onto them.  A `{}/1` holding a
%   conjunction is one goal but several constraints, so it is taken
%   apart — the complement is taken per constraint.

snapshot(Kept, Wanted, Names, Constraints) :-
    same_length(Kept, Names),
    herbrand(Kept, Wanted, Names, Equations, Difs),
    (   include(dense, Wanted, [_|_])
    ->  dense_pairs(Kept, Names, Dense, DenseNames),
        region_constraints(Dense, DenseNames, Region)
    ;   Region = []
    ),
    append([Equations, Difs, Region], Constraints).

herbrand(Kept, Wanted, Names, Equations, Difs) :-
    bound(Kept, Names, Equations),
    disequalities(Kept, Wanted, Names, Difs).

bound([], [], []).
bound([Kept|Rest], [Name|Names], Equations) :-
    (   ground(Kept)
    ->  Equations = [(Name = Kept)|More]
    ;   Equations = More
    ),
    bound(Rest, Names, More).

%!  disequalities(+Kept, +Wanted, +Names, -Difs) is det.
%
%   What `dif/2` keeps these variables away from, **jointly**.
%
%   `dif(q(X, Y), q(2, 3))` says the *pair* is not (2, 3); it does not
%   say X is not 2.  Read one variable at a time it becomes
%   `X \= 2, Y \= 3`, which is a stronger statement — so the region
%   comes out smaller than it is, the complement larger, and the
%   quantifier covers ground it has already covered.  Worse, it costs a
%   piece per variable where the pair costs one: the complement of
%   "not (2, 3)" is the single point (2, 3).
%
%   Where the two terms differ only at positions holding one of these
%   variables against a ground value, that is exactly what comes out.
%   Anything else — a difference at a variable the covering invented, or
%   against a term that is not ground — has no exact reading, and there
%   the per-variable over-approximation is kept, because it errs towards
%   covering more than is left rather than less.
%
%   Gathered inside one `\+ \+`: `dif:attribute_goals//1` deletes each
%   or-node as it reports it, so asking every variable in turn reports a
%   shared one once, which is the deduplication wanted.  What crosses out
%   is **positions and ground values**, never a term holding one of
%   these variables, so `nb_setarg/3` has nothing to copy that matters.

disequalities(Kept, Wanted, Names, Difs) :-
    (   include(constrained, Kept, [_|_])
    ->  State = state([]),
        (   \+ \+ ( read_store(Kept, Goals),
                     descriptions(Goals, Kept, Wanted, Sets),
                     nb_setarg(1, State, Sets) )
        ->  arg(1, State, Sets0),
            maplist(joint(Names), Sets0, Difs0),
            sort(Difs0, Difs)
        ;   Difs = []
        )
    ;   Difs = []
    ).


descriptions([], _, _, []).
descriptions([Goal|Goals], Kept, Wanted, Sets) :-
    (   Goal = apart(A, B),
        escapable(A, B, Kept)
    ->  Sets = Rest
    ;   Goal = apart(A, B),
        differ(A, B, Kept, Wanted, Pairs, []),
        Pairs \== []
    ->  Sets = [Pairs|Rest]
    ;   Goal = apart(A, B),
        between_kept(A, B, Kept, Wanted, I, J)
    ->  Sets = [kept(I, J)|Rest]
    ;   Goal = apart(A, B)
    ->  approximate(Kept, 1, A, B, Singles),
        append(Singles, Rest, Sets)
    ;   Sets = Rest
    ),
    descriptions(Goals, Kept, Wanted, Rest).

%!  between_kept(+A, +B, +Kept, +Wanted, -I, -J) is semidet.
%
%   The disequality keeps one quantified variable away from **another
%   variable the snapshot has a name for**, and from nothing else.  That
%   is describable after all: both ends get a name, the complement is the
%   equation between them, and `restrict/2` posts it.
%
%   `apart/6` will not have it because it wants a ground value, and there
%   is none — but a ground value is what it needs to build a *tuple*, and
%   this is one pair.  Dualising `p(f(X)) :- X #> 3, X #< 10` gives
%   `not p(A) :- forall(X, A \= f(X))`, and asked for `not p(f(V))` that
%   quantifier's answer leaves exactly `dif(V, X)`: its region is
%   everything but the one point `X = V`, and covering that point is what
%   turns up the two answers, `V =< 3` and `V >= 10`.
%
%   Reading it as "no constraint left" instead claimed the whole domain,
%   so `not p(f(V))` came back as one answer saying nothing while
%   `not p(f(5))` correctly failed — the general answer contradicting the
%   specific one.  That is `doc/bugs.md` §15 arriving as **unsoundness**
%   rather than as the incompleteness the old implementation has, and it
%   is the reason `test_crossings.pl` exists.

between_kept(A, B, Kept, Wanted, I, J) :-
    apart_variables(A, B, Kept, Pairs, []),
    Pairs = [I-J],
    (   wanted_at(I, Kept, Wanted)
    ;   wanted_at(J, Kept, Wanted)
    ),
    !.

apart_variables(A, B, Kept, Pairs, Tail) :-
    (   A == B
    ->  Pairs = Tail
    ;   var(A),
        var(B),
        position(A, Kept, I),
        position(B, Kept, J)
    ->  Pairs = [I-J|Tail]
    ;   compound(A),
        compound(B),
        functor(A, Name, Arity),
        functor(B, Name, Arity)
    ->  apart_variable_arguments(1, Arity, A, B, Kept, Pairs, Tail)
    ;   fail
    ).

apart_variable_arguments(I, Arity, A, B, Kept, Pairs, Tail) :-
    (   I > Arity
    ->  Pairs = Tail
    ;   arg(I, A, Ai),
        arg(I, B, Bi),
        apart_variables(Ai, Bi, Kept, Pairs, Middle),
        J is I+1,
        apart_variable_arguments(J, Arity, A, B, Kept, Middle, Tail)
    ).

wanted_at(I, Kept, Wanted) :-
    nth1(I, Kept, Variable),
    memberchk_eq(Variable, Wanted).

%!  escapable(+A, +B, +Kept) is semidet.
%
%   True when the disequality can be satisfied without saying anything
%   about Kept: it differs at a position holding a variable the answer
%   itself invented, and that variable is free to be something else.
%
%   This is the Herbrand half of the rule the dense store already
%   follows.  `dif(f(A, C), f(B, tapOff))` with C a variable of the
%   proved literal `consistent/3` compared against says the *pair* is
%   not that pair, and C may be chosen to make it so whatever A and B
%   are — so its projection onto the quantified variables is `true` and
%   there is nothing to complement.  Over-approximating it to
%   `B \= tapOff` instead is safe but not free: it hands the quantifier
%   a piece of domain that the answer already covered, and on
%   `bec_tap_01` that is 27,767 of 49,818 disequalities read.
%
%   Free is meant strictly.  A variable in the dense store may be
%   pinned to a point, so being a variable is not enough to be able to
%   move it; those keep the over-approximation.

escapable(A, B, Kept) :-
    A \== B,
    (   loose(A, Kept)
    ->  true
    ;   loose(B, Kept)
    ->  true
    ;   compound(A),
        compound(B),
        functor(A, Name, Arity),
        functor(B, Name, Arity),
        between(1, Arity, I),
        arg(I, A, Ai),
        arg(I, B, Bi),
        escapable(Ai, Bi, Kept),
        !
    ).

loose(Var, Kept) :-
    var(Var),
    \+ dense(Var),
    \+ memberchk_eq(Var, Kept).

%!  differ(+A, +B, +Kept, +Wanted, -Pairs, ?Tail) is semidet.
%
%   Pairs are the positions where A and B differ, as `Index-Value` over
%   Kept.  Fails where a difference has no such reading.

differ(A, B, Kept, Wanted, Pairs, Tail) :-
    (   A == B
    ->  Pairs = Tail
    ;   var(A),
        ground(B),
        wanted(A, Kept, Wanted, I)
    ->  Pairs = [I-B|Tail]
    ;   var(B),
        ground(A),
        wanted(B, Kept, Wanted, I)
    ->  Pairs = [I-A|Tail]
    ;   compound(A),
        compound(B),
        functor(A, Name, Arity),
        functor(B, Name, Arity)
    ->  apart_arguments(1, Arity, A, B, Kept, Wanted, Pairs, Tail)
    ;   fail
    ).

apart_arguments(I, Arity, A, B, Kept, Wanted, Pairs, Tail) :-
    (   I > Arity
    ->  Pairs = Tail
    ;   arg(I, A, Ai),
        arg(I, B, Bi),
        differ(Ai, Bi, Kept, Wanted, Pairs, Middle),
        J is I+1,
        apart_arguments(J, Arity, A, B, Kept, Wanted, Middle, Tail)
    ).

%   The variable has to be one the caller is asking *about*.  A joint
%   disequality that also mentions one of the goal's other variables has
%   no exact reading over the quantified ones alone — the other variable
%   may take a different value and the pair still differ — and reading it
%   as one produces a piece that **binds** that other variable.
%   `queens.pl` answers 256 times instead of twice when it does.

wanted(Var, Kept, Wanted, I) :-
    memberchk_eq(Var, Wanted),
    position(Var, Kept, I).

%   The reading that has no exact form: one prohibited value per kept
%   variable the disequality mentions, which is what `projection/4`
%   takes out of it.

approximate([], _, _, _, []).
approximate([Kept|Rest], I, A, B, Singles) :-
    J is I+1,
    (   var(Kept),
        projection(A, B, Kept, Value),
        ground(Value)
    ->  Singles = [[I-Value]|More]
    ;   Singles = More
    ),
    approximate(Rest, J, A, B, More).

position(Var, Kept, I) :-
    position_(Kept, 1, Var, I).

position_([K|Ks], I, Var, Index) :-
    (   K == Var
    ->  Index = I
    ;   J is I+1,
        position_(Ks, J, Var, Index)
    ).

%!  joint(+Names, +Pairs, -Dif) is det.
%
%   One disequality over as many of the names as the set mentions.  A
%   single position keeps the plain shape, so that its complement is the
%   equation `Name = Value` rather than a tuple unification.

joint(Names, kept(I, J), apart(Left, Right)) :-
    !,
    nth1(I, Names, Left),
    nth1(J, Names, Right).
joint(Names, [I-Value], apart(Name, Value)) :-
    !,
    nth1(I, Names, Name).
joint(Names, Pairs, apart(Tuple, Values)) :-
    named(Pairs, Names, Vars, Vs),
    Tuple =.. [t|Vars],
    Values =.. [t|Vs].

%!  named(+Pairs, +Names, -Vars, -Values) is det.
%
%   Resolve Index-Value pairs against Names.  Not findall/3: the names
%   must be the very variables snapshot/4 handed out, or residue/3 no
%   longer recognises the constraint as one of the quantifier's own.

named([], _, [], []).
named([I-V|Pairs], Names, [Name|Vars], [V|Vs]) :-
    nth1(I, Names, Name),
    named(Pairs, Names, Vars, Vs).

%   A variable CLP(Q) has something to say about.  `dump/3` is
%   Fourier-Motzkin elimination over the store, so it is not something to
%   run on a variable that is not in one.

dense(Var) :-
    var(Var),
    get_attr(Var, clpqr_itf, _).

dense_pairs([], [], [], []).
dense_pairs([Other|Others], [Name|Names], Dense, DenseNames) :-
    (   dense(Other)
    ->  Dense = [Other|Rest], DenseNames = [Name|More]
    ;   Dense = Rest, DenseNames = More
    ),
    dense_pairs(Others, Names, Rest, More).

%   `dump/3` and not `copy_term/3`, for the reason `region/3` gives: the
%   projection is what makes a constraint relating one of these variables
%   to something the covering invented come back as what it says about
%   them alone, which is usually nothing.  Asking `copy_term/3` instead
%   reports the store as it stands, and a complement taken over an
%   invented variable narrows nothing.

region_constraints([], _, []) :- !.
region_constraints(Dense, DenseNames, Region) :-
    dump(Dense, Fresh, Region0),
    Fresh = DenseNames,
    foldl(conjuncts, Region0, Region1, []),
    include(outward(DenseNames), Region1, Region).

%   What `dump/3` could not eliminate goes here rather than into a piece.
%   A disequality it cannot project — `A - B =\= 0` for a B the covering
%   invented — says nothing about A, so neither its negation nor the
%   constraint itself belongs in a partition of A's domain: posting it
%   would invent the variable again on every round.

conjuncts(Constraint, Constraints, Tail) :-
    (   Constraint = (A, B)
    ->  conjuncts(A, Constraints, Middle),
        conjuncts(B, Middle, Tail)
    ;   Constraints = [{Constraint}|Tail]
    ).

%!  alternatives(+Constraints, +Prefix, -Pieces) is det.
%
%   The complement of what the covering fixed, decomposed the same
%   disjoint way `region/3` decomposes a region — each piece is a slice
%   the answer has already ruled out, so the pieces partition what is
%   left rather than re-covering it.
%
%   Everything here is already about the goal's own variables:
%   `snapshot/3` drops what the covering invented, and drops it from the
%   prefix too.  A piece carrying such a constraint would not narrow —
%   it would invent the variable again.

alternatives([], _, []).
alternatives([Constraint|Constraints], Prefix, Pieces) :-
    (   flipped(Constraint, Negated)
    ->  append(Prefix, [Negated], Piece),
        Pieces = [Piece|Rest]
    ;   Pieces = Rest
    ),
    append(Prefix, [Constraint], Prefix1),
    alternatives(Constraints, Prefix1, Rest).

outward(Others, Constraint) :-
    term_variables(Constraint, Variables),
    known(Variables, Others).

known([], _).
known([V|Vs], Others) :-
    memberchk_eq(V, Others),
    known(Vs, Others).

flipped((Name = Value), apart(Name, Value)).
flipped(apart(A, B), (A = B)).
flipped({Comparison}, {Flipped}) :-
    negated(Comparison, Flipped).

%!  cover(+Vars, :Goal, +Narrowing, +Anc, +S0, -S) is nondet.
%
%   Goal holds everywhere in the domain of the **tuple** Vars.
%
%   Solve it once.  The answer tells us which part of the domain it
%   covered — by what the tuple is left constrained to — and the rest is
%   then covered the same way.  When nothing is left, the quantifier
%   holds.  This is P4's Algorithm 2 rather than §2.3's
%   substitute-each-value, and it makes decision 5's cases one piece of
%   code: they differ only in how a complement is taken, which is
%   `residue/3`.
%
%   ## All of the variables, not one at a time
%
%   `dualise.pl` puts every body-only variable of a clause in *one*
%   quantifier, and this covers them together.  Nesting one per variable
%   says the same thing and costs the **product** of the coverings, since
%   the inner quantifier is re-covered once per region of the outer;
%   covering the tuple reads one answer's constraints on all of them at
%   once and the pieces add up instead.  `bec_tap_02` made 83,593
%   coverings of a three-variable nest, and none of the five event
%   calculus programs finished; with the tuple they answer in
%   milliseconds, faster than the old implementation.
%   `solve.pl:solve_c_forall/8` collects its nest into one
%   `c_forall(Vars, Goal)` for the same reason.
%
%   The tuple also decides what an answer that **binds** may do.  One
%   quantifier per variable could reject a binding answer outright — a
%   point covers nothing of an infinite domain, so no finite number of
%   them closes — and ask the goal for another; the nesting made that
%   affordable because only the one variable had to come back free.  A
%   tuple answer with one component bound is the ordinary case, and
%   refusing it costs `queens.pl` and `family.pl` every answer they have.
%   So a ground binding is accepted and its complement is the
%   disequality, which is what `find_duals/4` does upstream.  It is not
%   free: `queens.pl` goes from 0.063 s to 0.42 s, still 2.3 times faster
%   than the old implementation.
%
%   A branch **checks**; it does not contribute to the proof.  Threading
%   each branch's proof into the next made the proof double per level of
%   the recursion — 131,135 nodes on `queens.pl`, where the old
%   implementation answers with 53 literals — because every branch
%   re-solves the same goal and most of what it proves repeats.  So the
%   quantifier's proof is the one its first answer gave, which is what
%   the old implementation keeps too (`solve_other_forall/11` copies the
%   stack per branch and returns one).  The branches still run, still
%   constrain the variables they share, and still have to succeed.
%
%   Where a variable's domain is finite it has to be declared, and then
%   `for_domain/6` enumerates it — outside this, one quantifier of its
%   own, because each value is a search rather than a region to
%   complement.  That is decision 5 case 6 against case 1, and the
%   difference is one annotation.

cover(Vars, Goal, _, Anc, S0, S) :-
    ground(Vars),
    !,
    call_goal(Goal, Anc, S0, S).
cover(Vars, Goal, Narrowing, Anc, S0, S) :-
    same_length(Vars, Instances),
    (   restrict_all(Narrowing, Instances)
    ->  renamed(Vars, Goal, Instances, Attempt),
        (   ground(Instances)
        ->  call_goal(Attempt, Anc, S0, S)   % narrowed to a point: just prove it
        ;   others(Attempt, Instances, Others),
            call_goal(Attempt, Anc, S0, S1),
            coverable(Instances),
            residue(Instances, Others, Rest),
            covered(Rest, Vars, Goal, Narrowing, Anc, S1, S)
        )
    ;   S = S0                      % the region is empty: nothing to cover
    ).

%   The narrowing is applied to bare variables and the **goal is copied
%   only if that succeeds**.  Most pieces of a complement are empty — on
%   `bec_tap_01`, 46,118 of 55,823 coverings — because the piece
%   contradicts what an outer level already narrowed the tuple to, and
%   there is no reason to copy a goal in order to find that out.

%!  coverable(+Instances) is semidet.
%
%   True when what the answer did to the tuple is something `snapshot/3`
%   can say: each variable is still free, or bound to a ground value
%   whose complement is a disequality.  A variable bound to a term that
%   is *not* ground has no complement worth taking — `dif(X, f(Y))` for a
%   free Y excludes one unknown point — so the answer is rejected and
%   the goal asked for another.

coverable([]).
coverable([Instance|Instances]) :-
    (   var(Instance)
    ->  true
    ;   ground(Instance)
    ),
    coverable(Instances).

%!  covered(+Rest, +Vars, +Goal, +Narrowing, +Anc, +S0, -S) is semidet.
%
%   Cover the whole complement, and keep of it **only what it says about
%   the variables the goal shares with its caller**.
%
%   A branch already contributes no proof, which is why a model holds
%   only what the first answer established.  What it must also not
%   contribute is a *constraint store*.  A branch invents variables of
%   its own, and CLP(Q) keeps every variable a constraint ever reached in
%   one class, with nothing to collect the ones that go out of scope —
%   so the class grows by everything every branch ever touched, and
%   `dump/3` is Fourier–Motzkin over that class, asked once per covering.
%   The quantifier gets quadratically slower at the thing it does most:
%   on `not holdsAt(level(10), T)` for `T > 5` the class reached 199
%   variables, a projection cost 67 ms, and the goal did not finish in 20
%   seconds.
%
%   What a branch *does* have to leave behind is its narrowing of the
%   caller's variables: `not s(B)` for `s(B) :- u(X), X #> B, X #< B+3`
%   over `u(5)` holds where 5 is outside the window, and that is said by
%   the branch covering X = 5, not by the first answer.  Dropping it
%   makes `not s(4)` succeed.
%
%   So the complement is covered inside a run that `fail` undoes, and one
%   snapshot of the shared variables comes back out through
%   `nb_setarg/3` over fresh plain names — the device `elsewhere/7`
%   already uses to carry a description out of a run it is discarding.
%   **One** snapshot for the whole complement, not one per branch: the
%   branches are a conjunction, so what survives them is the projection
%   of the store they leave, and taking it per branch costs a projection
%   per branch for the same answer — which on `bec_tap_01` was 12.4 of
%   the 20 seconds, and bought nothing the one snapshot does not.
%
%   The complement is covered **once**.  Without that the run is not a
%   run but an enumeration of every way to cover it, and even
%   `not holdsAt(level(10), T)`, which answers in 4 ms, does not finish.
%   `forall_/6` already commits to the covering it found, for the reason
%   its own comment gives; this is the same commitment one level in.

covered([], _, _, _, _, S, S) :- !.
covered(Rest, Vars, Goal, Narrowing, Anc, S0, S0) :-
    outer_variables(Goal, Vars, Shared),
    State = state(-),
    (   b_setval(sscasp_no_dcc, true),
        once(cover_each(Rest, Vars, Goal, Narrowing, Anc, S0, _)),
        snapshot(Shared, Shared, Names, Constraints),
        nb_setarg(1, State, Names-Constraints),
        fail
    ;   arg(1, State, Kept),
        Kept \== (-),
        restrict(Kept, Shared)
    ).

cover_each([], _, _, _, _, S, S).
cover_each([Restriction|Rest], Vars, Goal, Narrowing, Anc, S0, S) :-
    cover(Vars, Goal, [Restriction|Narrowing], Anc, S0, _),
    cover_each(Rest, Vars, Goal, Narrowing, Anc, S0, S).

%!  outer_variables(+Goal, +Vars, -Others) is det.
%
%   The variables the commitment is *about*: the ones the goal shares
%   with its caller.  Not the quantified variable, and not the ones a
%   quantifier nested inside the goal covers — `dualise.pl` nests one per
%   body-only variable, and those are covered under the same commitment
%   rather than being something to partition.

outer_variables(Goal, Vars, Others) :-
    term_variables(Goal, Variables),
    nested(Goal, Nested, Vars),
    exclude_vars(Variables, Nested, Others).

nested(Term, Vars, Tail) :-
    (   var(Term)
    ->  Vars = Tail
    ;   Term = forall(Quantified, Goal)
    ->  append(Quantified, More, Vars),
        nested(Goal, More, Tail)
    ;   Term = for_domain(Var, Generator, Goal)
    ->  Vars = [Var|More],
        nested(Generator, More, Middle),
        nested(Goal, Middle, Tail)
    ;   compound(Term)
    ->  Term =.. [_|Args],
        foldl(nested, Args, Vars, Tail)
    ;   Vars = Tail
    ).

exclude_vars([], _, []).
exclude_vars([V|Vs], Excluded, Others) :-
    (   memberchk_eq(V, Excluded)
    ->  Others = Rest
    ;   Others = [V|Rest]
    ),
    exclude_vars(Vs, Excluded, Rest).

%!  others(+Attempt, +Instances, -Others) is det.
%
%   The goal's *other* variables — everything this quantifier is not
%   covering.  Taken before solving, because that is when the goal still
%   has them: afterwards some are bound and the ones an answer invented
%   would be mixed in with them, and those are exactly the ones a
%   projection has to eliminate.

others(Attempt, Instances, Others) :-
    term_variables(Attempt, Variables),
    exclude_vars(Variables, Instances, Others).

restrict_all([], _).
restrict_all([Restriction|Rest], Vars) :-
    restrict(Restriction, Vars),
    restrict_all(Rest, Vars).

%!  renamed(+Vars, +Goal, -Fresh, -Renamed) is det.
%
%   Renamed is Goal with Var replaced by a fresh Fresh and **everything
%   else shared**.
%
%   Copying the whole goal instead is wrong, and quietly so.  A
%   quantifier's goal mentions two other kinds of variable: the ones its
%   clause's head carries, and the ones an *enclosing* quantifier is
%   covering.  Constraints the solving puts on either have to be seen
%   outside — the head's because `not p(A)` answers with what A may be,
%   and the enclosing quantifier's because that is how it learns which
%   part of its own domain this answer covered.
%
%   `dualise.pl` nests one quantifier per body-only variable, so the
%   second kind is the ordinary case rather than a corner of it.  With a
%   copy, `not attack(3, 4, [q(4,4)])` **succeeded**: the inner
%   quantifier constrained a copy of the outer variable, the outer saw an
%   unconstrained variable, concluded there was nothing left to cover and
%   said the disequality held for every value — which put four queens in
%   one column.
%
%   `copy_term/4` is exactly this operation, and shares whole subterms
%   that hold no renamed variable rather than walking them.
%
%   `Var` is **always plain** here, which is what makes it safe and
%   cheap.  A branch does not narrow the variable and hand the narrowed one
%   down; it carries the narrowing as a list of restrictions and re-applies
%   them to a fresh variable each time.  Two things follow.
%
%   Nothing ever copies a constraint *store*.  `copy_term/2` of a CLP(Q)
%   variable duplicates its whole connected component, so copying the
%   narrowed variable per branch grew the store until posting a single
%   constraint cost thousands of substitutions — the profiler put 95% of
%   `bec_light` inside `bv_q:solve/5` for 4,863 constraints.
%
%   And the copy is never attributed, which matters because a
%   `copy_term/4` copy of a CLP(Q) variable is inert: it is the same
%   variable the goal holds, and it reports the store it was copied with,
%   but a constraint posted on it afterwards is silently dropped.
%
%       ?- {A > 2},
%          copy_term([A], f(A,B), [A1], f(A2,_)),   % A1 == A2
%          {A2 >= 5},                               % succeeds
%          copy_term(A1, _, Store).
%       Store = [{A1 > 2}].                         % the >= 5 is gone
%
%   `copy_term/2` keeps it.  That matters here because a branch narrows
%   the quantified variable and then solves: with an inert copy the
%   solving constrains nothing, the complement comes out empty, and the
%   quantifier reports that it holds everywhere.  It is how
%   `not stoppedIn(2, light_on, 5)` came to succeed alongside
%   `stoppedIn(2, light_on, 5)`.
%
%   Both were real: the inert copy made `not stoppedIn(2, light_on, 5)`
%   succeed alongside `stoppedIn(2, light_on, 5)`, and the store growth
%   made every event-calculus program in the corpus unanswerable.

renamed(Vars, Goal, Fresh, Renamed) :-
    copy_term(Vars, Goal, Fresh, Renamed).

%!  restrict(+Restriction, ?Var) is semidet.
%
%   Narrow Var by Restriction, failing when the two cannot hold
%   together — which means that part of the domain is empty, and an
%   empty part needs no covering.
%
%   Copied on application: a restriction is a template over a variable of
%   its own, and several branches apply the same one to different copies
%   of Var.  `copy_term/4` rather than `copy_term/2`, because a
%   restriction may *mention* the goal's other variables — `Slot >= T` is
%   a restriction on Slot for whichever T the goal has — and copying
%   those would both break the sharing that makes it mean anything and
%   duplicate their constraint stores.

restrict(Terms-Attributes, Vars) :-
    copy_term(Terms, Attributes, Vars, Goals),
    maplist(call, Goals).

		 /*******************************
		 *        THE COMPLEMENT		*
		 *******************************/

%!  residue(+Instances, +Others, -Rest) is det.
%
%   Rest is the part of the domain the answer did *not* cover, as a list
%   of restrictions to try in turn.  They need not be disjoint; they need
%   to cover.
%
%   The tuple is described exactly as `forall_/6` describes the goal's
%   other variables: `snapshot/3` says what the answer fixed it to and
%   `alternatives/3` takes the complement, one slice at a time so that
%   the pieces partition what is left instead of re-covering it.  One
%   piece of machinery for both, because it is the same question asked of
%   different variables — what did this answer decide, and where else is
%   there to look.
%
%   Decision 5's cases fall out of `snapshot/3` rather than being clauses
%   here: a `dif/2` gives the prohibited values (case 3), a CLP(Q) store
%   gives a region (case 4), and an answer that left the tuple alone
%   gives nothing at all, which is case 2.
%
%   A constraint may mention the goal's **other** variables, and must:
%   `Slot >= T` is a restriction on Slot for whichever T the goal has.
%   So the two are dumped together and the others' names tied back to the
%   live variables, leaving the tuple's names fresh for `restrict/2` to
%   copy.  A constraint mentioning *neither* is about the others alone
%   and says nothing about where this quantifier has still to look.

residue(Instances, Others, Rest) :-
    append(Instances, Others, Kept),
    snapshot(Kept, Instances, Names, Constraints),
    same_length(Instances, Fresh),
    append(Fresh, OtherNames, Names),
    OtherNames = Others,
    include(quantified(Fresh), Constraints, Ours),
    alternatives(Ours, [], Pieces),
    maplist(restriction(Fresh), Pieces, Rest).

quantified(Fresh, Constraint) :-
    term_variables(Constraint, Variables),
    member(Variable, Variables),
    memberchk_eq(Variable, Fresh),
    !.

restriction(Fresh, Piece, Fresh-Piece).

		 /*******************************
		 *       THE HERBRAND HALF	*
		 *******************************/

		 /*******************************
		 *       THE DENSE HALF	*
		 *******************************/

%!  negated(+Constraint, -Flipped) is det.
%
%   The complement of one CLP(Q) comparison.

negated(Constraint, Flipped) :-
    Constraint =.. [Operator, A, B],
    (   opposite_comparison(Operator, Opposite)
    ->  true
    ;   throw(error(scasp_error(unrepresentable_complement(Constraint)), _))
    ),
    Flipped =.. [Opposite, A, B].

opposite_comparison(>,   =<).
opposite_comparison(=<,  >).
opposite_comparison(<,   >=).
opposite_comparison(>=,  <).
opposite_comparison(=:=, =\=).
opposite_comparison(=\=, =:=).
opposite_comparison(=,   =\=).

		 /*******************************
		 *       CALLING A GOAL	*
		 *******************************/

%!  call_goal(:Goal, +Anc, +S0, -S) is nondet.
%
%   Call a goal held as a term, threading the solver's state through it.
%   Used by the quantifiers, which hold their goal that way so that they
%   can re-run it; an ordinary call is threaded by the compiler and goes
%   straight through.
%
%   A quantifier is a goal too, and one quantifier's goal is often
%   another: `dualise.pl` nests one per body-only variable, so
%   `forall(A, forall(B, Inner))` is the ordinary shape for a clause with
%   two of them.  Recognising them here is what makes that work — adding
%   three arguments to a `forall/2` term would look for a `forall/5` in
%   the program.
%
%   A nested one goes straight to `cover/6` and **does not commit**.  The
%   outermost commits to the first covering it finds, and that search
%   runs through the nested ones, so the thing committed to is the first
%   covering of the whole nest — which is what
%   `solve.pl:solve_c_forall/8` commits to as well, having collected the
%   nest into one `c_forall(Vars, Goal)` beforehand.  Committing at each
%   level separately is a great deal stronger, and loses answers:
%   `family.pl`'s NMR check has none under it.

call_goal(Goal0, Anc, S0, S) :-
    strip_module(Goal0, Module, Goal),
    call_in(Goal, Module, Anc, S0, S).

call_in(forall(Vars, Inner), Module, Anc, S0, S) :-
    !,
    cover(Vars, Module:Inner, [], Anc, S0, S).
call_in(for_domain(Var, Generator, Inner), Module, Anc, S0, S) :-
    !,
    for_domain(Var, Module:Generator, Module:Inner, Anc, S0, S).
call_in(Goal, Module, Anc, S0, S) :-
    call(Module:Goal, Anc, S0, S).


%!  sld_goal(+Goal, +Type, +Anc, +S0, -S) is nondet.
%
%   Prove a goal by SLD resolution.  `compile.pl:sld_bridges/3` puts this
%   under the two predicates a `:- sld p/2.` declaration stands for, so
%   by the time it is reached the goal is either `p(...)` or
%   `not p(...)` and nothing else has to know.
%
%   The negation is the interesting half, and it is the same move as
%   everywhere else here: **constrain rather than test**.  A ground goal
%   can simply be run and refused, but `not p(X)` for an unbound X is not
%   a question Prolog can answer — `\+ p(X)` asks whether *some* X works
%   and says nothing about the others.  So all the solutions are
%   collected and the variable is kept away from each of them, which is
%   what the answer actually says.  The old implementation does the same
%   (`solve.pl`, `bindings_inequality_constraints/2`).
%
%   No node for the goal itself: the predicate that got here is one, and
%   `literal/2` demangles it into exactly the literal wanted, so it is
%   already in the model and already in the justification.  What goes in
%   is what the interpreter found *under* it, which for `opaque` is
%   nothing.

sld_goal(Module:not(Goal), Type, _Anc, S0, S) :-
    !,
    term_variables(Goal, Variables),
    (   Variables == []
    ->  \+ sld_proof(Module:Goal, Type, _)
    ;   Witness =.. [v|Variables],
        findall(Witness, Module:Goal, Solutions),
        maplist(apart(Witness), Solutions)
    ),
    S = S0.
sld_goal(Module:Goal, Type, _Anc, S0, S) :-
    sld_proof(Module:Goal, Type, Children),
    maplist(sld_node, Children, Nodes),
    splice(Nodes, S0, S).

%   The interpreter reports `Literal-Children`, which is the shape a
%   justification comes *out* in rather than the one the proof is kept
%   in, so it is turned round here.

sld_node(Literal-Children, n(sld(Literal), proof, Nodes)) :-
    maplist(sld_node, Children, Nodes).

%!  findall_(+Template, :Goal, -List, +Anc, +S0, -S) is det.
%
%   `findall/3` over a solver goal.  The list is the ordinary one; what
%   is not ordinary is that each solution *proved* something, and the
%   literals it proved belong in the model — `.pass` data for
%   `solve_findall.pl` lists every `member/2` the collection walked
%   through, not just the answers.  So each solution's proof delta is
%   collected alongside its template and the whole lot is spliced into
%   the caller's proof.
%
%   findall/3 copies, so a collected node shares no variable with the
%   goal that produced it and carries none of its attributes.  That is
%   the price of collecting at all: the solutions are alternatives to
%   each other, and a proof is a conjunction.  It costs nothing for a
%   collection over ground literals, which is what findall/3 is for here.

findall_(Template, Goal, List, Anc, S0, S) :-
    findall(Template-Nodes,
            (   call_goal(Goal, Anc, S0, S1),
                children(S1, S0, proof(Nodes, _))
            ),
            Pairs),
    collected(Pairs, List, Forest),
    splice(Forest, S0, S).

collected([], [], []).
collected([Template-Nodes|T], [Template|Templates], Forest) :-
    append(Nodes, Rest, Forest),
    collected(T, Templates, Rest).

%   A spliced node's descendants were never in this proof's index, so
%   unlike proved/4 — whose children indexed themselves on the way up —
%   the whole subtree is indexed here.

splice([], S, S).
splice([Node|Nodes], proof(Forest, Index0), S) :-
    index_node(Node, Index0, Index),
    splice(Nodes, proof([Node|Forest], Index), S).

index_node(n(Goal, Status, Children), Index0, Index) :-
    (   Status == proved
    ->  Kind = proof
    ;   Kind = Status
    ),
    index(Goal, Kind, Index0, Index1),
    index_nodes(Children, Index1, Index).

index_nodes([], Index, Index).
index_nodes([Node|Nodes], Index0, Index) :-
    index_node(Node, Index0, Index1),
    index_nodes(Nodes, Index1, Index).

		 /*******************************
		 *         THE MODEL		*
		 *******************************/

%!  model(+S, -Model) is det.
%
%   The literals proved, in the language the program was written in.
%   This is where the mangling comes off — nothing demangles while
%   solving.
%
%   Built by walking rather than with findall/3, which copies: a model
%   collected that way shares no variable with the query it answers and
%   carries none of the constraints that say what the answer *is*.  For
%   `p(X) :- not q(X)` over `q(1)`, the answer is X with `dif(X, 1)` on
%   it, and a copy is just an unbound variable.

model(proof(Forest, _), Model) :-
    literals(Forest, Literals, []),
    sort(Literals, Model).

literals([], Tail, Tail).
literals([n(Goal, _, Children)|T], Literals, Tail) :-
    (   literal(Goal, Literal)
    ->  Literals = [Literal|Rest]
    ;   Literals = Rest
    ),
    literals(Children, Rest, Rest1),
    literals(T, Rest1, Tail).

%!  justification(+Proof, -Forest) is det.
%
%   The proof in the language the program was written in, in the order
%   the literals were called: `Literal-Children`, with `chs/1` and
%   `proved/1` around a literal established without expanding it.
%
%   A built-in is not here, and should not be.  `dif(X, 1)` is a
%   constraint on the answer, not a step in proving it, and it is already
%   said where it belongs — on the variable.
%
%   Neither is a goal with no literal to its name — the two halves of an
%   abducible's even loop.  Its children take its place, which is where
%   they belong: nothing was proved *about* `abducible$p`, it was assumed,
%   and the assumption is already said by `p` being in the model.

justification(proof(Nodes, _), Forest) :-
    forest(Nodes, [], Forest).

forest([], Forest, Forest).
forest([n(Goal, Status, Children)|T], Forest0, Forest) :-
    forest(Children, [], Sub),
    (   literal(Goal, Literal)
    ->  status(Status, Literal, Node),
        Forest1 = [Node-Sub|Forest0]
    ;   append(Sub, Forest0, Forest1)
    ),
    forest(T, Forest1, Forest).

status(proof,   Literal, Literal).
status(assumed, Literal, Literal).
status(chs,    Literal, chs(Literal)).
status(proved, Literal, proved(Literal)).

%!  literal(+Goal, -Literal) is semidet.

literal(sld(Goal), Goal) :-
    !.
literal(Goal, Literal) :-
    Goal =.. [Mangled|Args],
    demangle(Mangled, pred(_, Kind, Name, _)),
    Plain =.. [Name|Args],
    wrap(Kind, Plain, Literal).

wrap(plain,    Goal, Goal).
wrap(neg,      Goal, -(Goal)).
wrap(dual,     Goal, not(Goal)).
wrap(dualneg,  Goal, not(-(Goal))).


		 /*******************************
		 *          MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(error(scasp_error(unrepresentable_complement(C)), _)) -->
    [ 's(CASP): no constraint store can say the complement of ~p'-[C] ].
