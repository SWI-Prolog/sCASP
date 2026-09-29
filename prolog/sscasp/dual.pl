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

:- module(sscasp_dual,
          [ negate_literal/2,           % @Literal, -Negated
            clause_duals/2,             % @Body, -Duals
            body_variables/3            % @Head, @Body, -BodyOnly
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
| `A < B` | `A >= B` |
| `A is B` | `not (A is B)` |

`is/2` is the one with no partner.  It is a *function*, not a relation that
can be turned round, so its negation stays a negation and the compiler emits
it as "evaluate the right side and disagree with it" — which is constructive
in the left one, where `\+ (A is B)` would only fail.

`#=` negates to a *single* literal rather than splitting into `#<` and `#>`.
The complement of a point is two regions, but that is the solver's business:
it expands `#<>` when it applies the constraint.  Keeping the dual to one
literal per literal is what makes the clause count predictable.
*/

%!  negate_literal(@Literal, -Negated) is det.
%
%   Works in both directions, since every pair is symmetric — which is
%   worth having, because the negation of a dual is the original and
%   something will want to read one back.

:- det(negate_literal/2).

negate_literal(Var, _), var(Var) =>
    instantiation_error(Var).
negate_literal(not(Goal), Negated) =>
    Negated = Goal.
negate_literal(Goal, Negated), dual_operator(Goal, Dual) =>
    Negated = Dual.
negate_literal(Goal, Negated) =>
    Negated = not(Goal).

%!  dual_operator(@Goal, -Dual) is semidet.
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

%   Prolog's arithmetic comparisons pair off like the CLP ones.  They
%   are not the same relation — these evaluate and test, where `#<`
%   constrains — but a program may use either and negating one has to
%   give the other.

dual_pair(<,   >=).
dual_pair(>=,  <).
dual_pair(>,   =<).
dual_pair(=<,  >).
dual_pair(=:=, =\=).
dual_pair(=\=, =:=).

dual_pair(=,   \=).
dual_pair(\=,  =).
dual_pair(#=,  #<>).
dual_pair(#<>, #=).
dual_pair(#<,  #>=).
dual_pair(#>=, #<).
dual_pair(#>,  #=<).
dual_pair(#=<, #>).

%   `X ∉ [a, b]` is a constraint and its negation is a *generator* —
%   "one of these" — which is what makes the pair work at all: a dual
%   has to be something to run rather than something to fail.

dual_pair('∉', '∈').
dual_pair('∈', '∉').

		 /*******************************
		 *          A CLAUSE		*
		 *******************************/

%!  clause_duals(@Body, -Duals) is det.
%
%   Duals is one body per literal of Body, each a list of goals: the
%   literals before it, then its negation.  Body is a list of goals, as
%   `abstract.pl` leaves it.
%
%   An **empty** body yields no duals at all, which is right and worth
%   stating: a clause with no body is unconditionally true, so nothing
%   can make it fail and its negation has no way to succeed.  `edge(X,Y).`
%   makes `not edge(X,Y)` fail for every X and Y.

:- det(clause_duals/2).

clause_duals(Body, Duals) :-
    clause_duals(Body, [], Duals).

clause_duals([], _, Duals) =>
    Duals = [].
clause_duals([Literal|Literals], Prefix, Duals), no_dual(Literal) =>
    append(Prefix, [Literal], NextPrefix),
    clause_duals(Literals, NextPrefix, Duals).
clause_duals([Literal|Literals], Prefix, Duals0) =>
    Duals0 = [Dual|Duals],
    negate_literal(Literal, Negated),
    append(Prefix, [Negated], Dual),
    append(Prefix, [Literal], NextPrefix),
    clause_duals(Literals, NextPrefix, Duals).

%   A literal whose negation is `fail` contributes no dual clause: the
%   branch it would begin cannot succeed.  The domain check that
%   `compile.pl:guard_goals/6` writes under `scasp_domain_error` is one —
%   it is `true` or an error — and it still runs at the front of every
%   later branch, which is what makes `not q(30)` raise.

no_dual(Literal) :-
    nonvar(Literal),
    Literal = '$domain_check'(_, _, _).

		 /*******************************
		 *         BODY VARIABLES	*
		 *******************************/

%!  body_variables(@Head, @Body, -BodyOnly) is det.
%
%   BodyOnly is the variables of Body that do not occur in Head, in the
%   order they first appear.  Negating "there is a Y such that ..." gives
%   "for every Y, not ...", so these are what the quantifier will range
%   over — and what a domain annotation has to supply a range for.
%
%   Head is the *abstracted* head, whose arguments are distinct
%   variables, so this is exactly the set the dual must quantify.
%
%   @arg Body is a list of goals, as clause_duals/2 takes it.

:- det(body_variables/3).

body_variables(Head, Body, BodyOnly) :-
    term_variables(Head, HeadVars),
    term_variables(Body, BodyVars),
    quantified(Body, Bound, HeadVars),
    exclude_vars(BodyVars, Bound, BodyOnly).

%!  quantified(@Body, -Bound, @Tail) is det.
%
%   The variables a `forall/2` the *author wrote* already binds.  They
%   are not body-only variables to quantify: the clause says what it
%   means about them, and the dual of `forall(X, g(X))` is
%   "for **some** X, not g(X)" — which is a variable left free, not
%   another quantifier.
%
%   @arg Bound is a difference list ending in Tail; body_variables/3
%       passes the head's variables as Tail, so Bound is everything
%       not to quantify.

quantified(Term, Vars, Tail), var(Term) =>
    Vars = Tail.
quantified(forall(Var, Goal), Vars, Tail) =>
    Vars = [Var|More],
    quantified(Goal, More, Tail).
quantified([Goal|Goals], Vars, Tail) =>
    quantified(Goal, Vars, Middle),
    quantified(Goals, Middle, Tail).
quantified((A, B), Vars, Tail) =>
    quantified(A, Vars, Middle),
    quantified(B, Middle, Tail).
quantified(not(Goal), Vars, Tail) =>
    quantified(Goal, Vars, Tail).
quantified(_, Vars, Tail) =>
    Vars = Tail.

exclude_vars([], _, Rest) =>
    Rest = [].
exclude_vars([Var|Vars], Exclude, Rest), member_eq(Var, Exclude) =>
    exclude_vars(Vars, Exclude, Rest).
exclude_vars([Var|Vars], Exclude, Rest0) =>
    Rest0 = [Var|Rest],
    exclude_vars(Vars, Exclude, Rest).

member_eq(X, [Y|T]) :-
    (   X == Y
    ->  true
    ;   member_eq(X, T)
    ).
