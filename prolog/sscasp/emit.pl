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

:- module(sscasp_emit,
          [ runnable_program/3,         % +Module, +Program, -Clauses
            body/6,                     % @Body, +Module, @Anc, ?S0, ?S,
                                        %   -Runnable
            checked_kind/1,             % ?Kind
            builtin/2                   % @Goal, -Runnable
          ]).
:- use_module(ops).
:- use_module(mangle, [mangle/2, demangle/2, mangled/1]).
:- use_module(library(lists), [append/3, member/2]).
:- use_module(library(apply), [maplist/3, convlist/3, include/3]).
:- use_module(library(error), [domain_error/2]).
:- use_module(clauses, [clause_parts/3]).

/** <module> Making the compiled program runnable

`compile.pl` produces the program as **logic**: mangled names, duals,
quantifiers, and nothing about how it is solved.  This pass turns that into
clauses Prolog can run, and it is the only place that knows how the solver
carries its state.

Keeping the two apart is worth the extra module.  The logical form is what a
listing shows, what a test reads and what a reimplementation of the *solver*
would keep; the threading below is an implementation of one solver and would
be replaced wholesale by another.

Two transformations:

## Three arguments

Every compiled predicate gains `Anc`, `S0` and `S` — the ancestor chain going
down and the proof coming up.

    m$plain$p$1(A) :- m$plain$q$1(A), m$dual$t$2(A, B).

    m$plain$p$1(A, Anc, S0, S) :- m$plain$q$1(A, Anc, S0, S1),
                                  m$dual$t$2(A, B, Anc, S1, S).

A built-in is left alone and does not advance the state: `A \= 0` is a
constraint, not a literal, and nothing about it belongs in a model.  It does
change: `\=` becomes `apart/2`, which is the same relation stated so that it
constrains rather than merely fails, and is decision 4.

The quantifiers keep their goal as a **term** rather than a threaded call,
because a quantifier re-runs its goal and each run needs its own state.
`for_domain/3` and `forall/2` become calls to the runtime, which threads them.

Those goals are **qualified with the module the program will live in**, which
is why this pass takes one.  A `meta_predicate` declaration would be the usual
way to carry the module across, but it is applied when a clause is *compiled*
and these clauses are `assertz/1`-ed, so the runtime would look for them in
its own module and not find them.

The runtime predicates themselves — `check/7`, `forall/5`, `apart/2` and the
rest — are written **unqualified**.  `run.pl:load_program/2` imports
`sscasp_runtime` into the module before asserting, so the names resolve there;
`runtime.pl` exports exactly what this pass writes, and a goal written here
that it does not export is a bug here.  That is the one qualification and the other: a
goal a quantifier *holds as a term* keeps its `M:`, because nothing resolves
it until the runtime calls it; a goal this pass *writes into a clause body*
does not, because the module it lands in can see it.

## A wrapper per literal

A literal has to be checked against the coinductive hypothesis before its
clauses are tried, so the predicate a caller calls is not the one holding the
clauses:

    m$plain$p$1(A, Anc, S0, S) :-
        check(m$plain$p$1(A), m$dual$p$1(A), +, Anc, S0, Anc1, Action),
        (   Action == expand
        ->  m$body.plain$p$1(A, Anc1, S0, S2),
            proved(Anc1, S0, S2, S)
        ;   done(m$plain$p$1(A), Action, S0, S)
        ).

`proved/4` gets both `S0` and `S2` because the difference between them is the
literal's **children**: that is how the proof comes back as a tree without a
second structure to carry it.

The goal, **its negation** and its polarity are all constants the compiler
knows from the `Kind` field, so the runtime never demangles a name.

Only the four literal kinds get one, and the four abducible kinds that make
an abducible's even loop (`checked_kind/1`).  `dual.2.inner` and the NMR
sub-checks are machinery rather than literals: nothing in a model names them, and
checking them against the hypothesis would be checking something the program
never said.
*/

%!  checked_kind(?Kind) is nondet.
%
%   The kinds that name a **literal** — something that can appear in a
%   model, be assumed, or contradict an assumption.  Everything else in
%   the compiled program is machinery.

checked_kind(plain).
checked_kind(neg).
checked_kind(dual).
checked_kind(dualneg).

%   The two halves of an abducible's even loop are checked too, and that
%   is the whole of what makes an abducible work: `abducible$p` is proved
%   by `not abducible$-p`, which is proved by `not abducible$p`, so the
%   call meets itself across two negations and the coinductive hypothesis
%   lets it stand.  Take the check away and the loop is an ordinary
%   non-terminating one.

checked_kind(abducible).
checked_kind(abducibleneg).
checked_kind(dualabducible).
checked_kind(dualabducibleneg).

%!  polarity(+Kind, -Polarity) is det.
%
%   `+` for a literal, `-` for a negated one.  The number of negations
%   between a goal and an ancestor is the number of times this changes
%   along the path.

polarity(plain,   +).
polarity(neg,     +).
polarity(dual,    -).
polarity(dualneg, -).
polarity(abducible,        +).
polarity(abducibleneg,     +).
polarity(dualabducible,    -).
polarity(dualabducibleneg, -).

%!  opposite(+Kind, -Opposite) is det.
%
%   The kind of the literal a goal of this kind contradicts.

opposite(plain,   dual).
opposite(dual,    plain).
opposite(neg,     dualneg).
opposite(dualneg, neg).
opposite(abducible,        dualabducible).
opposite(dualabducible,    abducible).
opposite(abducibleneg,     dualabducibleneg).
opposite(dualabducibleneg, abducibleneg).

		 /*******************************
		 *        THE PROGRAM		*
		 *******************************/

%!  runnable_program(+Module, +Program, -Clauses) is det.
%
%   Clauses is `Program.clauses` rewritten so that Prolog can run them.
%   That is the whole program — the NMR check is in it already, and
%   `Program.nmr` describes the same clauses rather than adding to them.
%
%   Module is where the clauses will be asserted.  It is *not* the module
%   the names were mangled for: a program compiles once and could be run
%   anywhere, and only the goals a quantifier holds as terms need to say
%   where to look.

:- det(runnable_program/3).

runnable_program(Module, Program, Clauses) :-
    Logical = Program.clauses,
    maplist(runnable_clause(Module), Logical, Rewritten),
    (   get_dict(dcc, Program, Dcc0)
    ->  convlist(runnable_dcc(Module), Dcc0, Dcc)
    ;   Dcc = []
    ),
    findall(Wrapper, wrapper(Logical, Dcc, Wrapper), Wrappers),
    append(Wrappers, Rewritten, Clauses).

%!  runnable_dcc(+Module, @Rule, -Runnable) is semidet.
%
%   A rule of the dynamic consistency check (`compile.pl:dcc_rules/3`)
%   made runnable: a literal among the others is looked for in the proof,
%   `lit(Goal)`, and a built-in is run, `run(Module:Goal)`.  A rule
%   holding a built-in with no runnable form is dropped, which only means
%   less pruning: the NMR check still sees the constraint.

runnable_dcc(Module, dcc(Trigger, Others0), Runnable) =>
    Runnable = dcc(Trigger, Others),
    maplist(runnable_other(Module), Others0, Others).

runnable_other(Module, Goal, Other) :-
    (   callable(Goal),
        functor(Goal, Name, _),
        mangled(Name)
    ->  Other = lit(Goal)
    ;   builtin(Goal, Runnable)
    ->  Other = run(Module:Runnable)
    ).

%!  runnable_clause(+Module, @Clause, -Runnable) is det.

runnable_clause(Module, Clause, Runnable) =>
    Runnable = (Head :- Body),
    clause_parts(Clause, Head0, Body0),
    head(Head0, Head, Anc, S0, S),
    body(Body0, Module, Anc, S0, S, Body).

%!  head(@Head, -Runnable, ?Anc, ?S0, ?S) is det.
%
%   A literal's clauses move to the `body(Kind)` predicate, leaving its
%   own name for the wrapper.
%
%   @arg Anc is the ancestor chain added to the head, as in body/6.
%   @arg S0 is the proof before, as in body/6.
%   @arg S is the proof after, as in body/6.

head(Head0, Head, Anc, S0, S) :-
    Head0 =.. [Name|Args],
    (   demangle(Name, Spec),
        kind_of(Spec, Kind),
        checked_kind(Kind)
    ->  rekind(Spec, body(Kind), Name1)
    ;   Name1 = Name
    ),
    append(Args, [Anc, S0, S], Args1),
    Head =.. [Name1|Args1].

kind_of(pred(_, Kind0, _, _), Kind) => Kind = Kind0.
kind_of(prog(_, Kind0, _), Kind) => Kind = Kind0.

rekind(pred(M, _, N, A), Kind, Name) => mangle(pred(M, Kind, N, A), Name).
rekind(prog(M, _, A), Kind, Name) => mangle(prog(M, Kind, A), Name).

		 /*******************************
		 *          A BODY		*
		 *******************************/

%!  body(@Body, +Module, @Anc, ?S0, ?S, -Runnable) is det.
%
%   @arg Body is a logical body: mangled calls, built-ins in program
%       syntax, and the quantifiers, `findall/3`, `sld/2` and
%       `'$domain_check'/3` the compiler writes.
%   @arg Module is where the clause will be asserted, as in
%       runnable_program/3; it qualifies only the goals held as terms.
%   @arg Anc is the clause's ancestor chain variable, a list of
%       `a(Goal, Neg, Pol)` frames at run time (`solve.pl:check/7`).
%   @arg S0 is the proof before the body, `proof(Forest, Index)` at
%       run time, as in `solve.pl:check/7`.
%   @arg S is the proof after the body, of the same shape.

:- det(body/6).

body(Var, _, _, S0, S, Runnable), var(Var) =>
    S = S0,
    Runnable = call(Var).
body((A, B), M, Anc, S0, S, Runnable) =>
    Runnable = (RA, RB),
    body(A, M, Anc, S0, S1, RA),
    body(B, M, Anc, S1, S, RB).
body(true, _, _, S0, S, Runnable) =>
    S = S0,
    Runnable = true.
body(fail, _, _, S0, S, Runnable) =>
    Runnable = (S = S0, fail).
body(for_domain(Var, Gen, Goal), M, Anc, S0, S, Runnable) =>
    Runnable = for_domain(Var, M:Gen, M:Goal, Anc, S0, S).
body(forall(Vars, Goal), M, Anc, S0, S, Runnable) =>
    Runnable = forall(Vars, M:Goal, Anc, S0, S).
body(findall(Template, Goal, List), M, Anc, S0, S, Runnable) =>
    Runnable = findall_(Template, M:Goal, List, Anc, S0, S).

%   A `findall/3` runs, but not its negation.  `not findall(T, G, L)`
%   asks what the old implementation's `exec_neg_findall/5` asks and
%   does not answer: which of the ways the collection could have gone
%   differently is the one that makes the list wrong.  It is said here
%   rather than left to fail, because failing means `not p` succeeds
%   where p holds.

body(not(Findall), _, _, S0, S, Runnable), Findall = findall(_, _, _) =>
    Runnable = ( S = S0,
                 throw(error(scasp_error(negated_findall(Findall)), _))
               ).
body('$domain_check'(Goal, Domain, PI), M, _, S0, S, Runnable) =>
    Runnable = (domain_check(M:Goal, Domain, PI), S = S0).
body(sld(Goal, Type), _, Anc, S0, S, Runnable) =>
    Runnable = sld_goal(Goal, Type, Anc, S0, S).
body(Goal, _, Anc, S0, S, Runnable),
        Goal =.. [Name|Args],
        mangled(Name) =>
    append(Args, [Anc, S0, S], Args1),
    Runnable =.. [Name|Args1].
body(Goal, _, _, S0, S, Runnable), builtin(Goal, Runnable0) =>
    Runnable = (Runnable0, S = S0).
body(Goal, _, _, _, _, _) =>
    domain_error(scasp_body_goal, Goal).

%!  builtin(@Goal, -Runnable) is semidet.
%
%   A built-in is a constraint rather than a literal: it does not advance
%   the proof, and nothing about it belongs in a model.  What it says is
%   said on the variable instead, which is the point of using solvers
%   that constrain rather than tests that fail.
%
%   `\=` goes into the Herbrand store — the same relation, stated so that
%   it *constrains* an unbound argument rather than merely succeeding on
%   it, which is what makes the dual of `p(0)` answer `not p(X)` with the
%   values X may take rather than with "yes".
%
%   The comparisons become CLP(Q).  Decision 4 keeps two stores because
%   two stores is what s(CASP) is: a dense order for arithmetic and
%   Herbrand disunification for everything else.  Which `{}/1` is meant
%   still matters — `library(clpr)` defines the same one over a different
%   domain, and that is not a difference to discover at run time — but it
%   is said once, by `runtime.pl` re-exporting clpq's, which
%   `run.pl:load_program/2` makes the compiled module import, rather
%   than on every goal here.

builtin(Goal, Runnable), Goal = (_ = _) => Runnable = Goal.
builtin((A \= B), Runnable) => Runnable = apart(A, B).

%   The Herbrand store, said the way a program says it.  `apart/2` and
%   not `dif/2` because `herbrand.pl` owns which of its two backings
%   holds a given disequality, and a post that went round it would put a
%   prohibited value where the set already has one — see there.

builtin('∉'(X, Values), Runnable) => Runnable = not_in_list(X, Values).
builtin(Goal, Runnable), Goal = '∈'(_, _) => Runnable = Goal.

%   `true` and `fail` negate to each other.  `dual.pl` leaves them for
%   here rather than pairing them in `dual_operator/2`, because that
%   table is about operators whose negation is another *operator*.

builtin(not(true), Runnable) => Runnable = fail.
builtin(not(fail), Runnable) => Runnable = true.

%   A domain check is `true` or an error (`compile.pl:guard_goals/6`), so
%   its negation is `fail`.  Where a rule of the dynamic consistency
%   check meets one it is `true`: looking ahead is no place to raise.

builtin(not('$domain_check'(_, _, _)), Runnable) => Runnable = fail.
builtin('$domain_check'(_, _, _), Runnable) => Runnable = true.

%   Prolog arithmetic evaluates and tests, so it passes straight through.
%   `not (A is B)` is the one with something to say: evaluating the right
%   side and disagreeing with the result *constrains* A, where
%   `\+ (A is B)` would only fail on an unbound one.

builtin(Goal, Runnable), Goal = is(_, _) =>
    Runnable = Goal.
builtin(not(is(A, B)), Runnable) =>
    Runnable = (Value is B, apart(A, Value)).
builtin(Goal, Runnable),
        compound(Goal),
        compound_name_arity(Goal, Name, 2),
        arithmetic_comparison(Name) =>
    Runnable = Goal.
builtin(Goal, Runnable),
        compound(Goal),
        compound_name_arguments(Goal, Name, [A, B]),
        comparison(Name, Operator) =>
    Constraint =.. [Operator, A, B],
    Runnable = {Constraint}.
builtin(_, _) =>
    fail.

arithmetic_comparison(<).
arithmetic_comparison(>).
arithmetic_comparison(=<).
arithmetic_comparison(>=).
arithmetic_comparison(=:=).
arithmetic_comparison(=\=).

%!  comparison(?Name, ?Operator) is nondet.
%
%   The source language writes `#<` where CLP(Q) writes `<`.  The `#`
%   forms are the language's, and stay: `<` is Prolog's arithmetic
%   comparison and means something else.

comparison(#=,   =:=).
comparison(#<>,  =\=).
comparison(#<,   <).
comparison(#>,   >).
comparison(#=<,  =<).
comparison(#>=,  >=).

		 /*******************************
		 *        THE WRAPPERS		*
		 *******************************/

%!  wrapper(@Clauses, @Dcc, -Wrapper) is nondet.
%
%   One wrapper per literal the program defines, in the standard order
%   of their mangled names.
%
%   @arg Clauses is the logical program, `Program.clauses`.
%   @arg Dcc is the list of `dcc(Trigger, Others)` rules made runnable
%       by runnable_dcc/3.

wrapper(Clauses, Dcc, Wrapper) :-
    convlist(literal_predicate, Clauses, Specs0),
    sort(Specs0, Specs),
    member(Spec, Specs),
    literal_wrapper(Spec, Dcc, Wrapper).

literal_predicate(Clause, Name-Arity-Spec) :-
    clause_parts(Clause, Head, _),
    functor(Head, Name, Arity),
    demangle(Name, Spec),
    kind_of(Spec, Kind),
    checked_kind(Kind).

%   With dynamic consistency checking (on unless `--no-dcc`), a literal
%   that occurs in a global constraint is checked
%   against it as soon as it is established: `dcc/4` after the rest.  A
%   literal in no constraint gets no such goal, so the check costs
%   nothing where it cannot prune.

literal_wrapper(_-Arity-Spec, Dcc, Wrapper) =>
    kind_of(Spec, Kind),
    rekind(Spec, Kind, Name),
    rekind(Spec, body(Kind), BodyName),
    opposite(Kind, Opposite),
    rekind(Spec, Opposite, NegName),
    polarity(Kind, Polarity),
    length(Args, Arity),
    Goal =.. [Name|Args],
    Neg =.. [NegName|Args],
    append(Args, [Anc1, S0, S2], BodyArgs),
    BodyCall =.. [BodyName|BodyArgs],
    append(Args, [Anc, S0, S], HeadArgs),
    Head =.. [Name|HeadArgs],
    Established =
      ( check(Goal, Neg, Polarity, Anc, S0, Anc1, Action),
        (   Action == expand
        ->  BodyCall,
            proved(Anc1, S0, S2, S)
        ;   done(Goal, Action, S0, S)
        )
      ),
    functor(Goal, GName, _),
    include(dcc_triggered_by(GName/Arity), Dcc, Rules),
    (   Rules == []
    ->  Wrapper = (Head :- Established)
    ;   Wrapper = (Head :- Established, dcc(Goal, Anc, S, Rules))
    ).

dcc_triggered_by(Name/Arity, dcc(Trigger, _)) :-
    functor(Trigger, Name, Arity).


		 /*******************************
		 *          MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(error(scasp_error(negated_findall(Goal)), _)) -->
    [ 's(CASP): the negation of ~p is not implemented'-[Goal] ].
