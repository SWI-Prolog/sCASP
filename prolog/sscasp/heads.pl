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

:- module(sscasp_heads,
          [ head_index/2,               % @Clauses, -Index
            head_guarded/4,             % +Index, @Clause0, -Positive, -ForDual
            head_guard/3                % ?Args, +Patterns, +Mode
          ]).
:- use_module(ops).
:- use_module(clauses, [clause_parts/3, body_list/2, list_body/2]).
:- use_module(dualise, [builtin/1]).
:- use_module(library(lists), [member/2, append/2, append/3, reverse/2]).
:- use_module(library(apply), [foldl/4, include/3, maplist/3]).
:- use_module(library(rbtrees), [rb_new/1, rb_lookup/3, rb_update/5,
                                 rb_insert_new/4, rb_map/3]).

/** <module> Head guards: failing before the body is proved

A literal can only succeed if some clause head of its predicate unifies
with it, and that can be known from the bindings already made, before a
body is proved.  The event calculus is where it matters:

```prolog
holdsAt(F2, T2) :- happens(E, T1), initiates(E, F1, T1),
                   trajectory(F1, T1, F2, T2), not stoppedIn(T1, F1, T2).
```

With `happens(overflow, T)` true at every time, the query
`holdsAt(level(H), 8)` proves `initiates(overflow, spilling, T1)` — that
is, `holdsAt(level(10), T1)`, in every cycle and each time through the
cycles before it — and only then finds `trajectory(spilling, T1,
level(H), 8)` impossible: spilling has a trajectory for `leak/1` only.
The head of `initiates(overflow, spilling, _)` already says F1 is
`spilling`, and no head of `trajectory/4` unifies with
`trajectory(spilling, _, level(_), _)`.

So in front of such a literal the compiler puts its **head projection**,
which binds the literal's arguments to one head's pattern at a time, and
a unification test for each later literal it shares a variable with:

    '$heads'(initiates/3, [E, F1, T1],
             [[tapOn, filling, _], [overflow, spilling, _]], bind),
    '$heads'(trajectory/4, [F1, T1, F2, T2],
             [[filling, _, level(_), _], [spilling, _, leak(_), _]], test),
    initiates(E, F1, T1), trajectory(F1, T1, F2, T2), ...

The patterns are stored ground, their variables numbered, and made fresh
at each call: a variable of a pattern is not a variable of the clause,
and the dualiser must not quantify over it.

The tap stays at a few milliseconds where it grew tenfold per cycle
(`doc/design-next.md` §5e).

## Why it is sound

A literal implies the disjunction of its predicate's clause heads
unified with it — under the completion that is what defining a predicate
by those clauses means — so neither the binding nor the test removes a
solution.  The binding is only made where the heads are pairwise
**non-unifiable**: then each solution of the literal is an instance of
exactly one pattern, and is found once.  Where heads overlap the
literal gets no guard.

## And the dual

Guarded, the body says what it said, so its dual is the dual of the same
thing, and the dualiser makes it as for any body: for `g1, g2, L, ...`
the branches `not g1`, `g1, not g2`, `g1, g2, not L`, ...  A guard's
negation is that its arguments unify with **no** pattern now, which is
decided at the call: `\+ head_guard(Args, Patterns, test)`.  That is
what the consistency check gains from: under `not holdsAt(level(H), 8)`
the branch with E = `overflow` and F1 = `spilling` is closed by `not`
of the `trajectory` test, without proving `not holdsAt(level(10), T1)`.

A *test* is decided at the call either way, so its two branches are
exhaustive.  A *binding* guard is not: with its arguments free it binds
them to the patterns, and `not` of it fails, so the values that match no
pattern are covered by neither.  In a dual that is complete only where
the quantifier the dual puts on the clause's body-only variables covers
the rest: its complement calls again with those variables apart from
the patterns, and then `not` of the guard holds.  So a guard group goes
into the clause the duals are made from only when the binding guard's
arguments are all **body-only**; otherwise it stays in the positive
clause alone, where it only prunes.  Either way it is a native goal, not
a literal: it is in neither the model nor the justification.

The flag `sscasp_head_guards` (default `true`) turns it off.  Read at
compile time.
*/

:- create_prolog_flag(sscasp_head_guards, true, [keep(true)]).

%   More clauses than this and the projection is a search of its own.

max_heads(16).

%!  head_index(@Clauses, -Index) is det.
%
%   The head patterns of every predicate with at most max_heads/1 clauses
%   whose heads are pairwise non-unifiable, keyed by `Name/Arity`, as
%   `heads(Patterns, Rules)`: a pattern is the list of a head's
%   arguments, and Rules is `true` if a clause has a body.  `none` when
%   the flag is off.
%
%   @arg Clauses are the program's clauses as unfolding left them, in
%       source syntax; a `-p` head is not a clause of `p`.

:- det(head_index/2).

head_index(Clauses, Index) :-
    current_prolog_flag(sscasp_head_guards, true),
    !,
    rb_new(Empty),
    foldl(add_head, Clauses, Empty, All),
    rb_map(All, reverse_list, Ordered),
    max_heads(Max),
    rb_map(Ordered, usable(Max), Index0),
    Index = heads(Index0).
head_index(_, none).

add_head(Clause, Index0, Index) :-
    clause_parts(Clause, Head, Body),
    callable(Head),
    Head \= -(_),
    !,
    Head =.. [Name|Args],
    length(Args, Arity),
    copy_term(Args, Pattern),
    (   Body == true
    ->  Entry = fact(Pattern)
    ;   Entry = rule(Pattern)
    ),
    (   rb_update(Index0, Name/Arity, Entries, [Entry|Entries], Index)
    ->  true
    ;   rb_insert_new(Index0, Name/Arity, [Entry], Index)
    ).
add_head(_, Index, Index).

reverse_list(List, Reversed) :-
    reverse(List, Reversed).

%   An unusable predicate maps to `none`, so the lookup says why.

usable(Max, Entries, Usable) :-
    length(Entries, N),
    maplist(arg(1), Entries, Patterns0),
    most_general(Patterns0, Patterns),
    (   N =< Max,
        disjoint(Patterns)
    ->  (   memberchk(rule(_), Entries)
        ->  Rules = true
        ;   Rules = false
        ),
        copy_term(Patterns, Ground),
        numbervars(Ground, 0, _),
        Usable = heads(Ground, Rules)
    ;   Usable = none
    ).

%   A pattern that is an instance of another adds nothing: binding to the
%   general one leaves the literal free to use the clause of the instance
%   too.  Unfolding makes such clauses — `initiates(overflow, spilling, T)
%   :- max_level(Max), ...` split over `max_level`'s two answers is two
%   clauses with one head — and without this the predicate would count
%   as overlapping and get no guard.

most_general(Patterns, General) :-
    most_general(Patterns, [], General).

most_general([], Kept, General) :-
    reverse(Kept, General).
most_general([P|Ps], Kept, General) :-
    (   (   member(Q, Kept)
        ;   member(Q, Ps)
        ),
        subsumes_term(Q, P),
        \+ ( subsumes_term(P, Q),          % a variant: keep the first
              \+ member_eq(Q, Kept) )
    ->  most_general(Ps, Kept, General)
    ;   most_general(Ps, [P|Kept], General)
    ).

member_eq(X, [Y|Ys]) :-
    (   X == Y
    ->  true
    ;   member_eq(X, Ys)
    ).

disjoint([]).
disjoint([P|Ps]) :-
    \+ ( member(Q, Ps),
         \+ P \= Q
       ),
    disjoint(Ps).

%!  head_guarded(+Index, @Clause0, -Positive, -ForDual) is det.
%
%   Positive is Clause0 with a guard group in front of each positive
%   literal whose predicate is in Index and which shares a variable with
%   a later such literal.  ForDual has only the groups whose binding
%   guard has body-only arguments (see "And the dual").  Both are
%   Clause0 where there is nothing to guard.

:- det(head_guarded/4).

head_guarded(none, Clause0, Positive, ForDual) =>
    Positive = Clause0,
    ForDual = Clause0.
head_guarded(heads(Index), Clause0, Positive, ForDual) =>
    (   clause_parts(Clause0, Head, Body0),
        Body0 \== true,
        body_list(Body0, Goals0),
        guard_goals(Goals0, Head, Index, Goals, DualGoals, Guarded),
        Guarded == true
    ->  list_body(Goals, Body),
        Positive = (Head :- Body),
        list_body(DualGoals, DualBody),
        ForDual = (Head :- DualBody)
    ;   Positive = Clause0,
        ForDual = Clause0
    ).

%   Only a literal of a predicate with rules gets a binding guard: for one
%   defined by facts alone the projection *is* the lookup, done twice.
%   Facts may still be tested.

guard_goals([], _, _, [], [], _).
guard_goals([Goal|Later], Head, Index, Goals, DualGoals, Guarded) :-
    (   guardable(Goal, Index, PI, Patterns, true),
        include(shares_with(Goal, Index), Later, Tested),
        Tested \== []
    ->  Goal =.. [_|Args],
        foldl(test_guard(Index), Tested, Tests, []),
        Group = ['$heads'(PI, Args, Patterns, bind)|Tests],
        append(Group, [Goal|Rest], Goals),
        (   body_only(Args, Head)
        ->  append(Group, [Goal|DualRest], DualGoals)
        ;   DualGoals = [Goal|DualRest]
        ),
        Guarded = true
    ;   Goals = [Goal|Rest],
        DualGoals = [Goal|DualRest]
    ),
    guard_goals(Later, Head, Index, Rest, DualRest, Guarded).

body_only(Args, Head) :-
    term_variables(Args, ArgVars),
    term_variables(Head, HeadVars),
    \+ ( member(V, ArgVars),
          member(W, HeadVars),
          V == W
        ).

guardable(Goal, Index, Name/Arity, Patterns, Rules) :-
    user_literal(Goal),
    functor(Goal, Name, Arity),
    rb_lookup(Name/Arity, heads(Patterns, Rules), Index).

%   A later literal is worth testing when it is guardable itself and
%   shares a variable the projection may bind.

shares_with(Goal, Index, Later) :-
    guardable(Later, Index, _, _, _),
    term_variables(Goal, GoalVars),
    term_variables(Later, LaterVars),
    member(V, LaterVars),
    member(W, GoalVars),
    V == W,
    !.

test_guard(Index, Later, [Test|Tail], Tail) :-
    guardable(Later, Index, PI, Patterns, _),
    Later =.. [_|Args],
    Test = '$heads'(PI, Args, Patterns, test).

%   A call to a predicate of the program: not a built-in, not a negation
%   and not one of the constructs a body may hold.

user_literal(Goal) :-
    callable(Goal),
    \+ builtin(Goal),
    functor(Goal, Name, Arity),
    \+ construct(Name, Arity).

construct(not, 1).
construct(-, 1).
construct(\+, 1).
construct(',', 2).
construct(;, 2).
construct(->, 2).
construct(*->, 2).
construct(forall, 2).
construct(for_domain, 3).
construct(findall, 3).
construct(abducible, 1).
construct(sld, 2).
construct('$domain_check', 3).
construct('$heads', 4).

%!  head_guard(?Args, +Patterns, +Mode) is nondet.
%
%   Run a guard.  `bind` unifies Args with each pattern in turn — the
%   patterns are pairwise non-unifiable, so at most one solution of the
%   literal belongs to each; `test` succeeds once if any pattern
%   unifies, binding nothing.  The patterns are ground while the
%   compiler works on the clause; `emit.pl` makes their variables
%   variables of the asserted clause again, so each call has its own.

head_guard(Args, Patterns, bind) =>
    member(Args, Patterns).
head_guard(Args, Patterns, test) =>
    \+ \+ member(Args, Patterns).
