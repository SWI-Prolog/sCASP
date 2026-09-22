:- module(sscasp_dual,
          [ negate_literal/2,           % ?Literal, ?Negated
            clause_duals/2,             % +Body, -Duals
            body_variables/3            % +Head, +Body, -BodyOnly
          ]).
:- use_module(ops).
:- use_module(library(lists), [append/3]).
:- use_module(library(error), [instantiation_error/1]).

/** <module> Dualising a clause body

`not p(X)` must *return the X for which `p(X)` fails*, not merely fail, so the
compiler synthesises a predicate proving the negation.  Clark's completion and
then De Morgan: the dual of a predicate is the conjunction of the duals of its
clauses, and the dual of one clause body is the *disjunction* of its negated
literals — so each negated literal becomes a clause of its own.

    p :- a, b, c.

        not-p :- not a.
        not-p :- a, not b.
        not-p :- a, b, not c.

## Why the earlier literals are kept

The clause negating the *i*-th literal first calls the literals before it.
Two reasons, and the second is easy to overlook: a goal can bind what a later
goal sees, so `not b` must be asked in the state `a` leaves behind; and
without the prefix the clauses produce **redundant answers**, each
re-exploring what an earlier one already gave.

**Every** prior literal is kept, negative ones included.  P4 §2.1 says "any
positive literal l_j with j < i", but a negated literal constrains
constructively too — `not a` may leave a disequality that `not b` has to see —
and the current implementation keeps them all:

    q :- b, not a, b.

        not-q :- not b.
        not-q :- b, a.
        not-q :- b, not a, not b.

## Negating one literal

Default negation flips.  At the end of the chain unification itself is
negated, which is where constructive disequality comes from, and the CLP
comparisons pair off:

| | |
|---|---|
| `not G` | `G` |
| `A = B` | `A \= B` |
| `A #= B` | `A #<> B` |
| `A #< B` | `A #>= B` |
| `A #> B` | `A #=< B` |

`#=` negates to a *single* literal rather than splitting into `#<` and `#>`.
The complement of a point is two regions, but that is the solver's business:
it expands `#<>` when it applies the constraint.  Keeping the dual to one
literal per literal is what makes the clause count predictable.
*/

%!  negate_literal(?Literal, ?Negated) is det.
%
%   Works in both directions, since every pair is symmetric — which is
%   worth having, because the negation of a dual is the original and
%   something will want to read one back.

negate_literal(Var, _) :-
    var(Var),
    !,
    instantiation_error(Var).
negate_literal(not(Goal), Goal) :- !.
negate_literal(Goal, Negated) :-
    dual_operator(Goal, Negated),
    !.
negate_literal(Goal, not(Goal)).

%!  dual_operator(?Goal, ?Dual) is semidet.
%
%   The built-ins whose negation is another built-in rather than `not`.
%   Each pair appears once and is used both ways.

dual_operator(Goal, Dual) :-
    nonvar(Goal),
    functor(Goal, Name, 2),
    dual_pair(Name, DualName),
    !,
    Goal =.. [_, A, B],
    Dual =.. [DualName, A, B].

dual_pair(=,   \=).
dual_pair(\=,  =).
dual_pair(#=,  #<>).
dual_pair(#<>, #=).
dual_pair(#<,  #>=).
dual_pair(#>=, #<).
dual_pair(#>,  #=<).
dual_pair(#=<, #>).

		 /*******************************
		 *          A CLAUSE		*
		 *******************************/

%!  clause_duals(+Body, -Duals) is det.
%
%   Duals is one body per literal of Body, each a list of goals: the
%   literals before it, then its negation.  Body is a list of goals, as
%   `abstract.pl` leaves it.
%
%   An **empty** body yields no duals at all, which is right and worth
%   stating: a clause with no body is unconditionally true, so nothing
%   can make it fail and its negation has no way to succeed.  `edge(X,Y).`
%   makes `not edge(X,Y)` fail for every X and Y.

clause_duals(Body, Duals) :-
    clause_duals(Body, [], Duals).

clause_duals([], _, []).
clause_duals([Literal|Literals], Prefix, [Dual|Duals]) :-
    negate_literal(Literal, Negated),
    append(Prefix, [Negated], Dual),
    append(Prefix, [Literal], NextPrefix),
    clause_duals(Literals, NextPrefix, Duals).

		 /*******************************
		 *         BODY VARIABLES	*
		 *******************************/

%!  body_variables(+Head, +Body, -BodyOnly) is det.
%
%   BodyOnly is the variables of Body that do not occur in Head, in the
%   order they first appear.  Negating "there is a Y such that ..." gives
%   "for every Y, not ...", so these are what the quantifier will range
%   over — and what a domain annotation has to supply a range for.
%
%   Head is the *abstracted* head, whose arguments are distinct
%   variables, so this is exactly the set the dual must quantify.

body_variables(Head, Body, BodyOnly) :-
    term_variables(Head, HeadVars),
    term_variables(Body, BodyVars),
    exclude_vars(BodyVars, HeadVars, BodyOnly).

exclude_vars([], _, []).
exclude_vars([Var|Vars], Exclude, Rest) :-
    member_eq(Var, Exclude),
    !,
    exclude_vars(Vars, Exclude, Rest).
exclude_vars([Var|Vars], Exclude, [Var|Rest]) :-
    exclude_vars(Vars, Exclude, Rest).

member_eq(X, [Y|T]) :-
    (   X == Y
    ->  true
    ;   member_eq(X, T)
    ).
