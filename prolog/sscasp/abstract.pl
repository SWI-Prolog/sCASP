:- module(sscasp_abstract,
          [ abstract_head/2,            % +Clause, -Abstracted
            abstract_head/3             % +Clause, -Head, -Guards
          ]).
:- use_module(ops).
:- use_module(library(lists), [append/3]).

/** <module> Head abstraction

A dual must succeed wherever the original fails, **including where head
unification fails**.  So before a clause is dualised the unification implicit
in its head is made explicit, leaving a head whose arguments are all distinct
variables and a body that starts with the unifications that were there before.

Three cases, from Marple, Salazar & Gupta (`doc/literature.md` P1):

| in the head | becomes |
|---|---|
| a non-variable argument | a fresh variable, and `Fresh = Value` |
| a variable seen earlier in the head | a fresh variable, and `Seen = Fresh` |
| a variable seen for the first time | itself, unchanged |

```prolog
t(A, A).                  ==>  t(A, B) :- A = B.
p(0).                     ==>  p(A) :- A = 0.
wrap(f(X)) :- item(X).    ==>  wrap(A) :- A = f(X), item(X).
```

The first occurrence of a variable keeps it, so `t(A,A)` abstracts to
`t(A,B) :- A = B` rather than renaming both.  That matters only for
readability, but the dual is read by people.

## One level, not recursive

P1 specifies this "recursively on the arguments of the terms", which would
give `p(f(g(X)))` the body `A = f(B), B = g(X)` rather than `A = f(g(X))`.
This does one level.

The two were measured against each other in the current implementation, which
has both — `abstract_structures/4` does one level and the unused
`prep_args2/7` did the recursion — and they produce the same answers.  The
recursive form generates a finer decomposition of the *dual*, one clause per
level, and every extra clause is a disequality that fails for the same reason
the first one does when the argument is unbound.  So the recursion buys
nothing and costs clauses.

Neither form fixes `doc/bugs.md` #15: negating a structured head is incomplete
for an unbound argument, because covering the rest of the domain would need a
universally quantified disequality that no store can express.  That is what
domain annotations are for — given `:- pred wrap(shape)`, the dual enumerates
`shape` instead of trying to represent the complement.
*/

%!  abstract_head(+Clause, -Abstracted) is det.
%!  abstract_head(+Clause, -Head, -Guards) is det.
%
%   Abstracted is Clause with its head unification made explicit.  The
%   three-argument form gives the new head and the guards as a list,
%   which is what a dual generator wants — it has to put the guards
%   somewhere other than the front of a body.
%
%   A head whose arguments are already distinct variables comes back
%   unchanged, with no guards.

abstract_head(Clause, Abstracted) :-
    abstract_head(Clause, Head, Guards),
    clause_body(Clause, Body),
    append(Guards, Body, Goals),
    (   Goals == []
    ->  Abstracted = Head
    ;   body_term(Goals, NewBody),
        Abstracted = (Head :- NewBody)
    ).

abstract_head(Clause, Head, Guards) :-
    clause_head(Clause, Head0),
    (   compound(Head0)
    ->  Head0 =.. [Name|Args],
        abstract_args(Args, [], NewArgs, Guards),
        Head =.. [Name|NewArgs]
    ;   Head = Head0,
        Guards = []
    ).

%!  abstract_args(+Args, +Seen, -NewArgs, -Guards) is det.
%
%   Seen carries the variables already used in the head, so that a
%   repeated one is recognised.  Compared with ==/2 rather than unified
%   against: two distinct variables are distinct arguments, however alike
%   they look.

abstract_args([], _, [], []).
abstract_args([Arg|Args], Seen, [Arg|NewArgs], Guards) :-
    var(Arg),
    \+ seen(Arg, Seen),
    !,
    abstract_args(Args, [Arg|Seen], NewArgs, Guards).
abstract_args([Arg|Args], Seen, [Fresh|NewArgs], [Guard|Guards]) :-
    var(Arg),
    !,
    Guard = (Arg = Fresh),
    abstract_args(Args, [Fresh|Seen], NewArgs, Guards).
abstract_args([Arg|Args], Seen, [Fresh|NewArgs], [Guard|Guards]) :-
    Guard = (Fresh = Arg),
    abstract_args(Args, [Fresh|Seen], NewArgs, Guards).

seen(Var, Seen) :-
    member_eq(Var, Seen).

member_eq(X, [Y|T]) :-
    (   X == Y
    ->  true
    ;   member_eq(X, T)
    ).

		 /*******************************
		 *           CLAUSES		*
		 *******************************/

clause_head((Head :- _), Head) :- !.
clause_head(Fact, Fact).

clause_body((_ :- Body), Goals) :-
    !,
    body_list(Body, Goals).
clause_body(_, []).

body_list((A, B), Goals) :-
    !,
    body_list(A, GA),
    body_list(B, GB),
    append(GA, GB, Goals).
body_list(true, []) :- !.
body_list(Goal, [Goal]).

body_term([Goal], Goal) :- !.
body_term([Goal|Goals], (Goal, Body)) :-
    body_term(Goals, Body).
