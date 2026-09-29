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

:- module(sscasp_herbrand,
          [ apart/2,                    % ?A, ?B
            not_in/2,                   % ?X, ?Value
            not_in_list/2,              % ?X, ?Values
            ('∉')/2,                    % ?X, +GroundValues
            ('∈')/2,                    % ?X, +Values
            constrained/1,              % @Var
            node_count/2,               % @Term, -Count
            excluded_values/2,          % @Var, -GroundValues
            projection/4,               % @A, @B, @Var, -Excluded
            read_store/2,               % @Vars, -Goals
            normalise/1,                % ?Term
            op(700, xfx, '∉'),
            op(700, xfx, '∈')
          ]).

%   `not_in_list/2` and `∈/2` are called by the compiled program only,
%   through `runtime.pl`.  `∉/2` is the residual goal attribute_goals//1
%   reports, which no source file calls either.

:- use_module(ops).
:- use_module(library(dif), [dif/2]).   % and its attribute_goals//1
:- use_module(library(lists), [append/3, member/2]).
:- use_module(library(apply), [foldl/4]).
:- use_module(library(error), [type_error/2]).
:- use_module(library(ordsets), [ord_union/3, ord_add_element/3,
                                 ord_memberchk/2]).

/** <module> The Herbrand store

Decision 4 keeps two stores because two stores is what s(CASP) is: a dense
order for arithmetic, and Herbrand disunification for everything else.  This
is the second one, and it has **two backings** behind one interface.

| | holds | |
|---|---|---|
| `dif/2` | a disequality between whole terms | `dif(f(A,B), f(a,b))` is `A \= a` *or* `B \= b`, and commits to neither |
| `∉` | the ground values one variable may not take | `X ∉ [a, b]` |

They are not interchangeable, and which way round matters: `X ∉ [a,b]` is
always `dif(X,a), dif(X,b)`, but a `dif/2` becomes a prohibited value only
when the two terms disagree in **one** place, that place being a variable
against a ground term.  Anything else is a disjunction, and a set of values
per variable cannot hold one.

So `apart/2` posts into whichever can say it, and everything that reads the
store reads it through here.  `solve.pl` names neither.

## Why bother

Three things, and the first is what prompted it.

  - **A set is idempotent.**  `consistent/3` posts one disequality per
    proved literal a goal might contradict, and several collapse to the
    same thing once the answer has bound what they differ in.  With
    `dif/2` a variable ended up carrying `dif(X, john)` twice and the
    toplevel said so twice.
  - **Reading a `dif/2` destroys it.**  `dif:attribute_goals//1` calls
    `del_attr/2` on each or-node as it reports it, so that a node shared
    by several variables is not reported twice; `copy_term/3` and the
    toplevel undo that and a direct call does not.  A `∉` set is one
    variable's, so there is nothing to share and nothing to delete.
  - **`X ∉ [a, b]` is how an answer reads.**  It is the old
    implementation's spelling, the one its `.pass` data is written in, and
    it is shorter than the conjunction it stands for.

## The fallback

The excluded values are **ground**.  A variable excluded from a term that is
not — `X ∉ [f(Y)]` — is `dif(X, f(Y))`, because what it excludes depends on
what Y becomes, and that is a disjunction again.  The same happens the other
way round: a variable carrying a set that is later bound to a non-ground
term hands its values to `dif/2` and steps aside.  So this half is never
wrong about what it holds; it holds less than `dif/2` and hands the rest
over.

The old implementation restricts it the same way — `clp/disequality.pl`'s
`not_unify2/2` fails on a non-ground right-hand side, citing §3.1.5 — but
resolves the disjunction by *backtracking* where we leave it to `dif/2`.
*/

		 /*******************************
		 *           POSTING		*
		 *******************************/

%!  apart(?A, ?B) is semidet.
%
%   A and B must not become the same.  Into the backing that can say it:
%   one variable against a ground term is a prohibited value, anything
%   else is `dif/2`'s.
%
%   `unifiable/3` is what decides, and it is the right question — "in how
%   many places, and which, do these disagree?" is exactly what a
%   disequality is about.  It ignores attributes, which is what is wanted
%   here: whether the two *could* become the same is a question about the
%   terms, and what the store already says about them is this predicate's
%   business rather than its premise.

apart(A, B) :-
    (   unifiable(A, B, Bindings)
    ->  exclusion(Bindings, A, B)
    ;   true                            % they cannot become the same
    ).

exclusion([], _, _) =>
    fail.                               % they already are the same
exclusion([X=V], A, B) =>
    (   var(X),
        ground(V)
    ->  exclude_value(X, V)
    ;   var(V),
        ground(X)
    ->  exclude_value(V, X)
    ;   dif(A, B)
    ).
exclusion(_, A, B) =>
    dif(A, B).

%!  not_in(?X, ?Value) is semidet.
%!  not_in_list(?X, ?Values) is semidet.
%
%   `X ∉ [a, b]`, the form a program writes.  A non-ground value is
%   `dif/2`'s, as above.

not_in(X, Value) :-
    (   var(X),
        ground(Value)
    ->  exclude_value(X, Value)
    ;   ground(X),
        ground(Value)
    ->  X \== Value
    ;   dif(X, Value)
    ).

not_in_list(X, Values) :-
    (   is_list(Values)
    ->  exclude_each(Values, X)
    ;   type_error(list, Values)
    ).

'∉'(X, Values) :-
    not_in_list(X, Values).

%!  '∈'(?X, +Values) is nondet.
%
%   The negation of `∉`, and the reason it exists: `dual.pl` pairs an
%   operator with another *operator*, so `not (X ∉ [a, b])` has to be
%   something to run rather than something to fail.  It is a generator
%   where its partner is a constraint, which is what the negation of "not
%   one of these" is.

'∈'(X, Values) :-
    member(X, Values).

exclude_each([], _).
exclude_each([Value|Values], X) :-
    not_in(X, Value),
    exclude_each(Values, X).

exclude_value(X, Value) :-
    (   get_attr(X, sscasp_herbrand, Values0)
    ->  ord_add_element(Values0, Value, Values),
        (   Values == Values0
        ->  true                        % said before, and said once
        ;   put_attr(X, sscasp_herbrand, Values)
        )
    ;   put_attr(X, sscasp_herbrand, [Value])
    ).

		 /*******************************
		 *           READING		*
		 *******************************/

%!  constrained(@Var) is semidet.
%
%   True when the store says anything about Var.

constrained(Var) :-
    (   get_attr(Var, sscasp_herbrand, _)
    ->  true
    ;   get_attr(Var, dif, _)
    ).

%!  node_count(@Term, -Count) is det.
%
%   A size for what the store says about Term's variables — used to ask
%   whether unifying one term into another *adds* anything, which is
%   `solve.pl:herbrand_instance/2`.
%
%   Counted rather than compared because neither backing loses anything
%   on unification: `dif/2` keeps its or-nodes in a `vardif/2` attribute
%   shared between the variables they relate, and a `∉` set is merged by
%   union.  So the count is unchanged exactly when nothing was added.

:- det(node_count/2).

node_count(Term, Count) :-
    term_attvars(Term, Vars),
    foldl(nodes_of, Vars, 0, Count).

nodes_of(Var, Count0, Count) :-
    (   get_attr(Var, sscasp_herbrand, Values)
    ->  length(Values, Excluded)
    ;   Excluded = 0
    ),
    (   get_attr(Var, dif, vardif(Left, Right))
    ->  length(Left, Lefts),
        length(Right, Rights),
        Nodes is Lefts + Rights
    ;   Nodes = 0
    ),
    Count is Count0 + Excluded + Nodes.

%!  excluded_values(@Var, -Values) is det.
%
%   The **ground** values Var may not take.  Those are the ones worth
%   having: an excluded value that is not ground excludes one unknown
%   point and so says nothing about Var on its own, which is the Herbrand
%   twin of what `region_constraints/3` says about projection.
%
%   Safe to call anywhere.  The `dif/2` half is read inside a double
%   negation and what is kept is carried out by `nb_setarg/3`, which is
%   sound here because a list of ground values has nothing to copy that
%   matters.

:- det(excluded_values/2).

excluded_values(Var, Values) :-
    (   get_attr(Var, sscasp_herbrand, Excluded)
    ->  true
    ;   Excluded = []
    ),
    (   get_attr(Var, dif, _)
    ->  State = state([]),
        (   \+ \+ ( dif:attribute_goals(Var, Goals, []),
                    values_of(Goals, Var, Values0),
                    nb_setarg(1, State, Values0) )
        ->  arg(1, State, FromDifs)
        ;   FromDifs = []
        )
    ;   FromDifs = []
    ),
    append(Excluded, FromDifs, Values).

values_of([], _, []).
values_of([Goal|Goals], Var, Values) :-
    (   Goal = dif(A, B),
        projection(A, B, Var, Value),
        ground(Value)
    ->  Values = [Value|Rest]
    ;   Values = Rest
    ),
    values_of(Goals, Var, Rest).

%!  projection(@A, @B, @Plain, -Excluded) is semidet.
%
%   Excluded is what B has where A has Plain, descending through matching
%   functors.  The first such position is enough: a variable occurring
%   twice makes the disequality weaker, not stronger, so any one
%   projection still covers what is left.
%
%   A disequality between **compound** terms is a disjunction, not a
%   restriction on any one variable: `dif(f(A,B), f(1,2))` says the pair
%   is not (1,2), which excludes nothing from A on its own.  Projecting
%   it — taking `A = 1` as what is left to check — covers *more* of the
%   domain than is left, and that is the safe direction: the quantifier
%   then proves the goal somewhere it was already proved, where missing a
%   part would claim a `forall` that does not hold.
%
%   @arg Plain is the variable asked about, found in A or B by `==`.

projection(A, B, Plain, Excluded), A == Plain =>
    Excluded = B.
projection(A, B, Plain, Excluded), B == Plain =>
    Excluded = A.
projection(A, B, Plain, Excluded),
    compound(A),
    compound(B),
    functor(A, Name, Arity),
    functor(B, Name, Arity) =>
    between(1, Arity, I),
    arg(I, A, Ai),
    arg(I, B, Bi),
    projection(Ai, Bi, Plain, Excluded),
    !.
projection(_, _, _, _) =>
    fail.

%!  read_store(@Vars, -Goals) is det.
%
%   Everything the store says about Vars, in **one** vocabulary:
%   `apart(A, B)` terms, with a prohibited value reported as the
%   disequality it stands for.  One vocabulary because a reader is asking
%   what the answer *says*, and which backing holds it is this module's
%   business — and `apart/2` rather than `dif/2` because the same term is
%   what a caller **posts** to say it again, which is how the quantifier
%   builds the complement of a region it has covered.
%
%   **This is destructive and must be called inside `\+ \+`.**  That is
%   `dif:attribute_goals//1`'s doing, not ours: it deletes each or-node
%   as it reports it so that a node shared by several variables is
%   reported once.  Which is also why Vars is a list — asking one
%   variable at a time, each in its own double negation, would report a
%   shared node once per variable.

:- det(read_store/2).

read_store(Vars, Goals) :-
    phrase(store_goals(Vars), Goals).

store_goals([]) ==>
    [].
store_goals([Var|Vars]) ==>
    excluded(Var),
    reported_difs(Var),
    store_goals(Vars).

excluded(Var) -->
    (   { get_attr(Var, sscasp_herbrand, Values) }
    ->  excluded_goals(Values, Var)
    ;   []
    ).

excluded_goals([], _) ==>
    [].
excluded_goals([Value|Values], Var) ==>
    [apart(Var, Value)],
    excluded_goals(Values, Var).

reported_difs(Var) -->
    (   { get_attr(Var, dif, _) }
    ->  { dif:attribute_goals(Var, Reported, []) },
        aparts(Reported)
    ;   []
    ).

aparts([]) ==>
    [].
aparts([dif(A, B)|Difs]) ==>
    [apart(A, B)],
    aparts(Difs).

%!  normalise(?Term) is det.
%
%   Move into `∉` whatever the `dif/2` half has come to be able to say.
%
%   A disequality between two **variables** is `dif/2`'s — neither side
%   is ground, so there is no value to prohibit.  Once one of them is
%   bound there is: `dif(X, Y)` with `Y = tweety` is `X ∉ [tweety]`, and
%   if the set already holds tweety the two stores are saying the same
%   thing twice.  `library(dif)` simplifies its or-nodes internally and
%   never tells us, so nothing in `attr_unify_hook/2` can see it happen.
%
%   Called once an answer is complete, over the variables the answer is
%   about.  It is a *rebuild*: `dif:attribute_goals//1` deletes each
%   or-node as it reports it — which is the one time that is useful —
%   and what comes back is posted again through `apart/2`, so a
%   disequality that has become a prohibited value lands in the set and
%   the rest goes back where it was.  A node shared by two variables is
%   reported once and posted once, which is the deduplication wanted.

:- det(normalise/1).

normalise(Term) :-
    term_attvars(Term, Vars),
    collect_difs(Vars, Goals),
    repost(Goals).

collect_difs([], []).
collect_difs([Var|Vars], Goals) :-
    (   get_attr(Var, dif, _)
    ->  dif:attribute_goals(Var, Goals, Rest)
    ;   Goals = Rest
    ),
    collect_difs(Vars, Rest).

repost([]).
repost([dif(A, B)|Goals]) :-
    apart(A, B),
    repost(Goals).

		 /*******************************
		 *         THE ATTRIBUTE		*
		 *******************************/

%!  attr_unify_hook(+Values, ?Other) is semidet.
%
%   Three cases, and the third is the one worth knowing about: a variable
%   bound to a term that is **not ground** cannot keep a set of
%   prohibited values, because whether it takes one of them depends on
%   what the term's own variables become.  So the set becomes that many
%   `dif/2`s and this backing steps aside.
%
%   @arg Values is the variable's attribute: the ordered set of ground
%       values it may not take.

attr_unify_hook(Values, Other) :-
    (   var(Other)
    ->  (   get_attr(Other, sscasp_herbrand, Theirs)
        ->  ord_union(Values, Theirs, Union),
            put_attr(Other, sscasp_herbrand, Union)
        ;   put_attr(Other, sscasp_herbrand, Values)
        )
    ;   ground(Other)
    ->  \+ ord_memberchk(Other, Values)
    ;   hand_over(Values, Other)
    ).

hand_over([], _).
hand_over([Value|Values], Other) :-
    dif(Other, Value),
    hand_over(Values, Other).

%!  attribute_goals(@X)// is det.
%
%   `X ∉ [a, b]`, and **without deleting it** — see the module comment.

attribute_goals(X), get_attr(X, sscasp_herbrand, Values) ==>
    [ '∉'(X, Values) ].
attribute_goals(_) ==>
    [].

attr_portray_hook(Values, X) :-
    format('~p ∉ ~p', [X, Values]).
