:- module(sscasp_solve,
          [ check/8,                    % +Goal, +Neg, +Pol, +Anc, +S0,
                                        %   -Anc1, -S1, -Action
            proved/3,                   % +Anc1, +S0, -S
            for_domain/6,               % +Var, :Gen, :Goal, +Anc, +S0, -S
            forall/5,                   % +Var, :Goal, +Anc, +S0, -S
            call_goal/4,                % :Goal, +Anc, +S0, -S
            model/2                     % +S, -Model
          ]).
:- use_module(ops).
:- use_module(mangle, [demangle/2]).
:- use_module(library(lists), [member/2, append/3]).

/** <module> The solver's runtime

The compiled program *is* Prolog, so most of solving is Prolog calling it.
What Prolog does not do by itself is the part that makes goal-directed
evaluation agree with Gelfond–Lifschitz: before expanding a literal, look at
what is already assumed.

Three extra arguments carry what that needs.  `Anc` is the ancestor chain and
goes **down**; `S0`/`S` carry the literals proved so far and come **up**.  They
are different things with different modes, so they are different arguments;
nothing is reconstructed from anything else.

Each frame is `a(Goal, Negation, Polarity)`: the goal, the goal it contradicts,
and `+` for a literal or `-` for a negated one.  All three are *static* — the
compiler knows a predicate's polarity from its `Kind` field and emits the
negated functor as a constant — so nothing is demangled while solving.

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

%!  check(+Goal, +Neg, +Pol, +Anc, +S0, -Anc1, -S1, -Action) is semidet.
%
%   Decide what to do with Goal before expanding it.  Action is `done`
%   when it is already established — by the coinductive hypothesis or by
%   an earlier proof — and `expand` when its clauses should be tried,
%   in which case Anc1 is the chain its body runs under.
%
%   Fails when assuming Goal would contradict something assumed, which is
%   what makes the failure of an odd loop a failure of the query.

check(Goal, Neg, Pol, Anc, S0, Anc1, S1, Action) :-
    \+ contradicted(Neg, Anc, S0),
    (   ancestor(Anc, Goal, Pol, Negations)
    ->  loop_succeeds(Negations, Pol),
        Action = done,
        Anc1 = Anc,
        S1 = S0
    ;   ground(Goal),
        memberchk_goal(Goal, S0)
    ->  Action = done,
        Anc1 = Anc,
        S1 = S0
    ;   Action = expand,
        Anc1 = [a(Goal, Neg, Pol)|Anc],
        S1 = S0
    ).

%!  contradicted(+Neg, +Anc, +S0) is semidet.
%
%   True when the goal's negation is already assumed, whether as an open
%   ancestor or as something proved.  Both count: a model holding `p` and
%   `not p` is not a model, and it makes no difference which branch of
%   the proof put them there.

contradicted(Neg, Anc, S) :-
    (   member(a(Goal, _, _), Anc)
    ;   member(a(Goal, _, _), S)
    ),
    \+ \+ Goal = Neg,
    !.

%!  ancestor(+Anc, +Goal, +Pol, -Negations) is semidet.
%
%   The nearest ancestor unifying with Goal, and the number of polarity
%   transitions on the path from it down to Goal.  Unifies: a coinductive
%   hypothesis binds, which is how `p(X) :- p(X)` answers at all.

ancestor(Anc, Goal, Pol, Negations) :-
    ancestor_(Anc, Goal, Pol, 0, Negations).

ancestor_([a(A, _, AP)|T], Goal, Pol, N0, N) :-
    transition(AP, Pol, N0, N1),
    (   \+ \+ A = Goal
    ->  A = Goal,
        N = N1
    ;   ancestor_(T, Goal, AP, N1, N)
    ).

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

%!  proved(+Anc1, +S0, -S) is det.
%
%   Record the goal just proved.  Anc1 is the chain `check/8` built, so
%   its head is the frame — a proved literal is looked at exactly as an
%   ancestor is, and nothing has to be built again.

proved([Frame|_], S, [Frame|S]).

memberchk_goal(Goal, [a(G, _, _)|T]) :-
    (   G == Goal
    ->  true
    ;   memberchk_goal(Goal, T)
    ).

		 /*******************************
		 *        QUANTIFIERS		*
		 *******************************/

%   The compiled program lives in a module of its own, so a goal a
%   quantifier holds as a term has to be called there rather than here.
%   Declaring the arguments as goals is what carries the module across.

:- meta_predicate
    for_domain(?, 0, 0, +, +, -),
    forall(?, 0, +, +, -),
    call_goal(0, +, +, -).

%!  for_domain(+Var, :Generator, :Goal, +Anc, +S0, -S) is nondet.
%
%   Goal holds for every value Generator gives Var.  This is decision 5's
%   case 1 and the reason a domain is worth declaring: the quantifier is
%   an enumeration, and enumerating is complete.
%
%   The proofs accumulate, so every literal the quantifier needed is in
%   the model — the quantifier is part of the answer, not a side
%   condition checked and forgotten.

for_domain(Var, Generator, Goal, Anc, S0, S) :-
    findall(Var, call_goal(Generator, Anc, S0, _), Values),
    for_each(Values, Var, Goal, Anc, S0, S).

for_each([], _, _, _, S, S).
for_each([Value|Values], Var, Goal, Anc, S0, S) :-
    copy_term(Var-Goal, Value-Instance),
    call_goal(Instance, Anc, S0, S1),
    for_each(Values, Var, Goal, Anc, S1, S).

%!  forall(+Var, :Goal, +Anc, +S0, -S) is nondet.
%
%   Goal holds for *every* value of Var, with no domain to enumerate.
%
%   Only decision 5's case 2 is implemented: the goal succeeds without
%   constraining Var at all, so it holds for every value trivially.  The
%   other three decidable cases need the constraint stores to report what
%   region of Var's domain an answer covers, and that is not here yet —
%   so rather than answer as if they were, this raises.
%
%   Declaring a domain turns any of them into `for_domain/6`, which is
%   decision 6a's argument for annotations in one predicate.

forall(Var, Goal, Anc, S0, S) :-
    copy_term(Var-Goal, Fresh-Instance),
    call_goal(Instance, Anc, S0, S1),
    (   var(Fresh),
        \+ attvar(Fresh)
    ->  S = S1
    ;   unbounded_quantifier(Var, Goal)
    ).

unbounded_quantifier(Var, Goal) :-
    throw(error(sscasp_error(unbounded_quantifier(Var, Goal)), _)).

%!  call_goal(:Goal, +Anc, +S0, -S) is nondet.
%
%   Call a compiled predicate, which takes three arguments more than the
%   literal names.  Used by the quantifiers, which hold their goal as a
%   term so that they can re-run it; ordinary calls are threaded by the
%   compiler and go straight through.

call_goal(Goal0, Anc, S0, S) :-
    strip_module(Goal0, Module, Goal),
    Goal =.. List,
    append(List, [Anc, S0, S], List1),
    Goal1 =.. List1,
    call(Module:Goal1).

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

model(S, Model) :-
    literals(S, Literals),
    sort(Literals, Model).

literals([], []).
literals([a(Goal, _, _)|T], Literals) :-
    (   literal(Goal, Literal)
    ->  Literals = [Literal|Rest]
    ;   Literals = Rest
    ),
    literals(T, Rest).

%!  literal(+Goal, -Literal) is semidet.

literal(Goal, Literal) :-
    Goal =.. [Mangled|Args],
    demangle(Mangled, pred(_, Kind, Name, _)),
    Plain =.. [Name|Args],
    wrap(Kind, Plain, Literal).

wrap(plain,    Goal, Goal).
wrap(neg,      Goal, -(Goal)).
wrap(dual,     Goal, not(Goal)).
wrap(dualneg,  Goal, not(-(Goal))).
