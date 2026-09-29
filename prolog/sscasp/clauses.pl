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

:- module(sscasp_clauses,
          [ clause_parts/3,             % @Clause, -Head, -Body
            clause_head/2,              % @Clause, -Head
            clause_body/2,              % @Clause, -Body
            clause_goals/2,             % @Clause, -Goals
            body_list/2,                % @Body, -Goals
            list_body/2,                % +Goals, -Body
            body_literal/2,             % @Body, -Literal
            body_goal/2,                % @Body, -Goal
            literal_parts/3,            % @Literal, -Polarity, -Goal
            literal_key/2               % @Literal, -Key
          ]).
:- use_module(ops).

/** <module> Taking a clause apart

What every pass of the compiler asks of a clause — its head, its body, the
goals in the body — said once.  The walk over a body in particular: each
pass used to have its own, and they drifted apart in what they looked
through.  The call graph missed an odd loop through a `forall/2` the program
wrote, and one through `not forall(...)`, because its walk stopped where the
closure's went on.

A body here is a **source or translated** body: a conjunction of literals,
`not/1` around a literal or a construct, and the control constructs the
compiler knows — `forall/2`, `for_domain/3`, `findall/3` and Prolog's
`,`, `;`, `->`, `*->` and `\+`, which a program's own expansion may leave.
*/

%!  clause_parts(@Clause, -Head, -Body) is det.
%!  clause_head(@Clause, -Head) is det.
%!  clause_body(@Clause, -Body) is det.
%
%   A fact's body is `true`.

:- det(clause_parts/3).
:- det(clause_head/2).
:- det(clause_body/2).

clause_parts((Head0 :- Body0), Head, Body) =>
    Head = Head0,
    Body = Body0.
clause_parts(Fact, Head, Body) =>
    Head = Fact,
    Body = true.

clause_head(Clause, Head) :-
    clause_parts(Clause, Head, _).

clause_body(Clause, Body) :-
    clause_parts(Clause, _, Body).

%!  clause_goals(@Clause, -Goals) is det.
%
%   The body as a list, `[]` for a fact.

:- det(clause_goals/2).

clause_goals(Clause, Goals) :-
    clause_body(Clause, Body),
    body_list(Body, Goals).

%!  body_list(@Body, -Goals) is det.
%!  list_body(+Goals, -Body) is det.
%
%   A conjunction as a list of its conjuncts and back.  `true` is the
%   empty conjunction both ways: it does not appear in Goals, and `[]`
%   gives `true`.

:- det(body_list/2).
:- det(list_body/2).

body_list(Body, Goals) :-
    phrase(conjuncts(Body), Goals).

conjuncts((A, B)) ==>
    conjuncts(A),
    conjuncts(B).
conjuncts(true) ==>
    [].
conjuncts(Goal) ==>
    [Goal].

list_body([], Body) =>
    Body = true.
list_body([Goal], Body) =>
    Body = Goal.
list_body([Goal|Goals], Body) =>
    Body = (Goal, Rest),
    list_body(Goals, Rest).

%!  body_literal(@Body, -Literal) is nondet.
%
%   Every literal of Body with the polarity it has there: one under an
%   odd number of negations comes out as `not(Literal)`, whatever those
%   negations were written around.  So `not forall(X, r(X))` gives
%   `not r(X)`, and `forall(X, not r(X))` gives the same — the loop the
%   literal closes has one negation on it either way.  A quantifier or a
%   collection is looked through; so is a domain's generator, which is a
%   positive call.

body_literal(Body, Literal) :-
    literal(Body, +, Literal).

literal(Var, _, _), var(Var) =>
    fail.
literal((A, B), P, L) =>
    (   literal(A, P, L)
    ;   literal(B, P, L)
    ).
literal((A ; B), P, L) =>
    (   literal(A, P, L)
    ;   literal(B, P, L)
    ).
literal((A -> B), P, L) =>
    (   literal(A, P, L)
    ;   literal(B, P, L)
    ).
literal((A *-> B), P, L) =>
    (   literal(A, P, L)
    ;   literal(B, P, L)
    ).
literal(\+ A, P, L) =>
    flip(P, P1),
    literal(A, P1, L).
literal(not(A), P, L) =>
    flip(P, P1),
    literal(A, P1, L).
literal(forall(_, A), P, L) =>
    literal(A, P, L).
literal(for_domain(_, Generator, A), P, L) =>
    (   literal(Generator, P, L)
    ;   literal(A, P, L)
    ).
literal(findall(_, A, _), P, L) =>
    literal(A, P, L).
literal(Goal, +, L) =>
    L = Goal.
literal(Goal, -, L) =>
    L = not(Goal).

flip(+, -).
flip(-, +).

%!  body_goal(@Body, -Goal) is nondet.
%
%   Every goal Body calls, whatever its polarity: body_literal/2 without
%   the `not`.

body_goal(Body, Goal) :-
    body_literal(Body, Literal),
    (   Literal = not(Goal0)
    ->  Goal = Goal0
    ;   Goal = Literal
    ).

%!  literal_parts(@Literal, -Polarity, -Goal) is det.
%
%   A positive source literal taken apart: which family of predicates it
%   belongs to and the goal without the wrapper.  `p` and `-p` are
%   different predicates with different duals, and so are the two halves
%   `compile.pl` writes for `#abducible p`.
%
%   @arg Polarity is `plain`, `neg`, `abducible` or `abducibleneg`.

:- det(literal_parts/3).

literal_parts(abducible(-(Goal0)), Polarity, Goal) =>
    Polarity = abducibleneg, Goal = Goal0.
literal_parts(abducible(Goal0), Polarity, Goal) =>
    Polarity = abducible, Goal = Goal0.
literal_parts(-(Goal0), Polarity, Goal) =>
    Polarity = neg, Goal = Goal0.
literal_parts(Goal0, Polarity, Goal) =>
    Polarity = plain, Goal = Goal0.

%!  literal_key(@Literal, -Key) is det.
%
%   @arg Key is `key(Polarity, Name/Arity)`, which names one predicate
%       of the program: the call graph's node, and what the compiler
%       groups a predicate's clauses by.

:- det(literal_key/2).

literal_key(Literal, key(Polarity, Name/Arity)) :-
    literal_parts(Literal, Polarity, Goal),
    (   atom(Goal)
    ->  Name = Goal, Arity = 0
    ;   functor(Goal, Name, Arity)
    ).
