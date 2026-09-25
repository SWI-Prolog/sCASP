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

:- module(sscasp_sld,
          [ sld_proof/3                 % :Goal, +Type, -Children
          ]).
:- use_module(ops).
:- use_module(library(lists), [append/3]).
:- use_module(library(error), [existence_error/2]).

/** <module> Proving a goal by SLD resolution

`:- sld p/2.` says p/2 is proved the way Prolog proves things — by SLD
resolution, left to right, first clause first — rather than the way the rest
of the program is: goal-directed, dualised, coinductive.  It is not dualised,
not closed, and contributes nothing to a model beyond the literal that called
it.  What it *is* is a way to compute — arithmetic, a table lookup, a library
— without writing it as an s(CASP) predicate.

The declaration names the **resolution**, not the language, because that is
the distinction that matters and because it leaves room: a goal proved by SLD
is one thing, a goal proved by tabled resolution or handed to a constraint
solver would be others, and each would want its own word.  It also avoids the
obvious name: a prefix operator called `prolog` makes `prolog:message//1` a
syntax error in every file that can see it, and the old implementation has
exactly that wart.

Strictly this is SLDNF with cut — `\+` and `!` are both handled below — but
SLD is what the procedure is called when nobody is being careful, and a goal
with no negation in it really is SLD.

Two ways to run it, and the difference is only in what the justification
shows:

| `justified` | meta-interpreted, so the justification holds the Prolog proof |
| `opaque` | called directly, so the justification holds the call and no more |

`justified` is the default.  `opaque` is for a goal there is nothing useful to
say about — a foreign predicate, a meta-predicate the interpreter cannot take
apart — and for one where the detail would bury the answer.

## What the interpreter handles

Conjunction, disjunction, if-then-else, negation as failure, cut, and both
kinds of rule (`:-` and `=>`).  A cut cuts to the choice point of the clause
it appears in, which is what `prolog_current_choice/1` and `prolog_cut_to/1`
are for; getting that wrong makes a cut in a called predicate cut the s(CASP)
search, which is a bug that only shows up on backtracking.

A **foreign** or **meta** predicate is called rather than interpreted however
it was declared, because there is nothing to interpret: no clauses to read.
That is `opaque/1` below, and it is why `justified` is a safe default.
*/

:- meta_predicate
    sld_proof(0, +, -).

%!  sld_proof(:Goal, +Type, -Children) is nondet.
%
%   Call Goal.  Children is the justification below it, as the forest of
%   `Literal-Children` the rest of the solver uses — empty for `opaque`,
%   and the shape of the Prolog proof for `justified`.

sld_proof(Module:Goal, opaque, Children) :-
    !,
    call(Module:Goal),
    Children = [].
sld_proof(Module:Goal, _, Children) :-
    prolog_current_choice(Choice),
    interpret(Goal, Module, Tree, Choice),
    below(Goal, Tree, Children).

%   The caller already has a node for the goal — it called a predicate to
%   get here — so what it wants is what is *under* it, not the goal
%   again.  Anything else is passed through: a goal that turned out to be
%   a conjunction or a built-in has no root to strip.

below(Goal, [Root-Children], Stripped) :-
    Root == Goal,
    !,
    Stripped = Children.
below(_, Tree, Tree).

interpret(Goal, _, _, _), var(Goal) =>
    throw(error(instantiation_error, _)).
interpret(true, _, Tree, _) =>
    Tree = [].
interpret(fail, _, _, _) =>
    fail.
interpret(false, _, _, _) =>
    fail.
interpret(Module:Goal, _, Tree, Choice) =>
    interpret(Goal, Module, Tree, Choice).
interpret((A, B), Module, Tree, Choice) =>
    interpret(A, Module, TreeA, Choice),
    interpret(B, Module, TreeB, Choice),
    append(TreeA, TreeB, Tree).
interpret((If -> Then ; Else), Module, Tree, Choice) =>
    (   prolog_current_choice(Inner),
        interpret(If, Module, TreeIf, Inner)
    ->  interpret(Then, Module, TreeThen, Choice),
        append(TreeIf, TreeThen, Tree)
    ;   interpret(Else, Module, TreeElse, Choice),
        Tree = [(not If)-[]|TreeElse]
    ).
interpret((If *-> Then ; Else), Module, Tree, Choice) =>
    (   prolog_current_choice(Inner),
        interpret(If, Module, TreeIf, Inner)
    *-> interpret(Then, Module, TreeThen, Choice),
        append(TreeIf, TreeThen, Tree)
    ;   interpret(Else, Module, TreeElse, Choice),
        Tree = [not(If)-[]|TreeElse]
    ).
interpret((A ; B), Module, Tree, Choice) =>
    (   interpret(A, Module, Tree, Choice)
    ;   interpret(B, Module, Tree, Choice)
    ).
interpret(\+ Goal, Module, Tree, _) =>
    prolog_current_choice(Inner),
    \+ interpret(Goal, Module, _, Inner),
    Tree = [not(Goal)-[]].
interpret(!, _, Tree, Choice) =>
    Tree = [],
    prolog_cut_to(Choice).
interpret(Goal, Module, Tree, _), opaque(Module:Goal) =>
    Tree = [Goal-[]],
    call(Module:Goal).
interpret(Goal, Module, Tree, Choice),
    predicate_property(Module:Goal, imported_from(From)) =>
    interpret(Goal, From, Tree, Choice).
interpret(Goal, Module, Tree, _), predicate_property(Module:Goal, ssu) =>
    most_general_goal(Goal, General),
    prolog_current_choice(Inner),
    (   clause(Module:General, Guard => Body),
        subsumes_term(General, Goal)
    *-> General = Goal,
        interpret((Guard, !, Body), Module, Children, Inner),
        Tree = [Goal-Children]
    ;   existence_error(matching_rule, Module:Goal)
    ).
interpret(Goal, Module, Tree, _) =>
    prolog_current_choice(Inner),
    clause(Module:Goal, Body),
    interpret(Body, Module, Children, Inner),
    Tree = [Goal-Children].

%!  opaque(:Goal) is semidet.
%
%   True when there is nothing to interpret.  A foreign predicate has no
%   clauses to read, and a meta-predicate's arguments are goals in
%   modules the interpreter has no way to track — the old implementation
%   says the same and for the same reason.

opaque(Goal) :-
    predicate_property(Goal, foreign),
    !.
opaque(Goal) :-
    predicate_property(Goal, meta_predicate(_)),
    !.
opaque(Goal) :-
    \+ predicate_property(Goal, number_of_clauses(_)),
    predicate_property(Goal, defined).

most_general_goal(Goal, General) :-
    functor(Goal, Name, Arity),
    functor(General, Name, Arity).
