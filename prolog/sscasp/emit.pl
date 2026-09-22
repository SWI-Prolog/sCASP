:- module(sscasp_emit,
          [ runnable_program/2,         % +Program, -Clauses
            checked_kind/1,             % ?Kind
            polarity/2                  % ?Kind, ?Polarity
          ]).
:- use_module(ops).
:- use_module(mangle, [mangle/2, demangle/2]).
:- use_module(library(lists), [append/3, member/2, memberchk/2]).
:- use_module(library(apply), [maplist/3, foldl/4]).
:- use_module(library(error), [domain_error/2]).

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
change: `\=` becomes `dif/2`, which is the same relation stated so that it
constrains rather than merely fails, and is decision 4.

The quantifiers keep their goal as a **term** rather than a threaded call,
because a quantifier re-runs its goal and each run needs its own state.
`for_domain/3` and `forall/2` become calls to the runtime, which threads them.

## A wrapper per literal

A literal has to be checked against the coinductive hypothesis before its
clauses are tried, so the predicate a caller calls is not the one holding the
clauses:

    m$plain$p$1(A, Anc, S0, S) :-
        check(m$plain$p$1(A), m$dual$p$1(A), +, Anc, S0, Anc1, S1, Action),
        (   Action == done
        ->  S = S1
        ;   m$body.plain$p$1(A, Anc1, S1, S2),
            proved(Anc1, S2, S)
        ).

The goal, **its negation** and its polarity are all constants the compiler
knows from the `Kind` field, so the runtime never demangles a name.

Only the four literal kinds get one.  `dual.2.inner` and the NMR sub-checks
are machinery rather than literals: nothing in a model names them, and
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

%!  polarity(?Kind, ?Polarity) is det.
%
%   `+` for a literal, `-` for a negated one.  The number of negations
%   between a goal and an ancestor is the number of times this changes
%   along the path.

polarity(plain,   +).
polarity(neg,     +).
polarity(dual,    -).
polarity(dualneg, -).

%!  opposite(?Kind, ?Opposite) is det.
%
%   The kind of the literal a goal of this kind contradicts.

opposite(plain,   dual).
opposite(dual,    plain).
opposite(neg,     dualneg).
opposite(dualneg, neg).

		 /*******************************
		 *        THE PROGRAM		*
		 *******************************/

%!  runnable_program(+Program, -Clauses) is det.
%
%   Clauses is `Program.clauses` rewritten so that Prolog can run them.
%   That is the whole program — the NMR check is in it already, and
%   `Program.nmr` describes the same clauses rather than adding to them.

runnable_program(Program, Clauses) :-
    Logical = Program.clauses,
    maplist(runnable_clause, Logical, Rewritten),
    findall(Wrapper, wrapper(Logical, Wrapper), Wrappers),
    append(Wrappers, Rewritten, Clauses).

%!  runnable_clause(+Clause, -Runnable) is det.

runnable_clause(Clause, (Head :- Body)) :-
    clause_parts(Clause, Head0, Body0),
    head(Head0, Head, Anc, S0, S),
    body(Body0, Anc, S0, S, Body).

clause_parts((Head :- Body), Head, Body) :- !.
clause_parts(Fact, Fact, true).

%!  head(+Head, -Runnable, -Anc, -S0, -S) is det.
%
%   A literal's clauses move to the `body(Kind)` predicate, leaving its
%   own name for the wrapper.

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

kind_of(pred(_, Kind, _, _), Kind).
kind_of(prog(_, Kind, _), Kind).

rekind(pred(M, _, N, A), Kind, Name) :- mangle(pred(M, Kind, N, A), Name).
rekind(prog(M, _, A), Kind, Name) :- mangle(prog(M, Kind, A), Name).

		 /*******************************
		 *          A BODY		*
		 *******************************/

%!  body(+Body, +Anc, +S0, -S, -Runnable) is det.

body(Var, _, S, S, call(Var)) :-
    var(Var),
    !.
body((A, B), Anc, S0, S, (RA, RB)) :-
    !,
    body(A, Anc, S0, S1, RA),
    body(B, Anc, S1, S, RB).
body(true, _, S, S, true) :- !.
body(fail, _, S0, S, (S = S0, fail)) :- !.
body(for_domain(Var, Gen, Goal), Anc, S0, S, Runnable) :-
    !,
    Runnable = sscasp_solve:for_domain(Var, Gen, Goal, Anc, S0, S).
body(forall(Var, Goal), Anc, S0, S, Runnable) :-
    !,
    Runnable = sscasp_solve:forall(Var, Goal, Anc, S0, S).
body(Goal, Anc, S0, S, Runnable) :-
    Goal =.. [Name|Args],
    demangle(Name, _),
    !,
    append(Args, [Anc, S0, S], Args1),
    Runnable =.. [Name|Args1].
body(Goal, _, S0, S, Runnable) :-
    builtin(Goal, Runnable0),
    !,
    Runnable = (Runnable0, S = S0).
body(Goal, _, _, _, _) :-
    domain_error(sscasp_body_goal, Goal).

%!  builtin(+Goal, -Runnable) is semidet.
%
%   A built-in is a constraint rather than a literal: it does not advance
%   the proof, and nothing about it belongs in a model.
%
%   `\=` becomes `dif/2` — the same relation, stated so that it
%   *constrains* an unbound argument rather than merely succeeding on it,
%   which is what makes the dual of `p(0)` answer `not p(X)` with the
%   values X may take rather than with "yes".

builtin((A = B), (A = B)).
builtin((A \= B), dif(A, B)).

		 /*******************************
		 *        THE WRAPPERS		*
		 *******************************/

%!  wrapper(+Clauses, -Wrapper) is nondet.
%
%   One wrapper per literal the program defines, in the order the
%   literals first appear.

wrapper(Clauses, Wrapper) :-
    findall(Spec, literal_predicate(Clauses, Spec), Specs0),
    sort(Specs0, Specs),
    member(Spec, Specs),
    literal_wrapper(Spec, Wrapper).

literal_predicate(Clauses, Name-Arity-Spec) :-
    member(Clause, Clauses),
    clause_parts(Clause, Head, _),
    functor(Head, Name, Arity),
    demangle(Name, Spec),
    kind_of(Spec, Kind),
    checked_kind(Kind).

literal_wrapper(_-Arity-Spec, Wrapper) :-
    kind_of(Spec, Kind),
    rekind(Spec, Kind, Name),
    rekind(Spec, body(Kind), BodyName),
    opposite(Kind, Opposite),
    rekind(Spec, Opposite, NegName),
    polarity(Kind, Polarity),
    length(Args, Arity),
    Goal =.. [Name|Args],
    Neg =.. [NegName|Args],
    append(Args, [Anc1, S1, S2], BodyArgs),
    BodyCall =.. [BodyName|BodyArgs],
    append(Args, [Anc, S0, S], HeadArgs),
    Head =.. [Name|HeadArgs],
    Wrapper =
      ( Head :-
          sscasp_solve:check(Goal, Neg, Polarity, Anc, S0, Anc1, S1, Action),
          (   Action == done
          ->  S = S1
          ;   BodyCall,
              sscasp_solve:proved(Anc1, S2, S)
          )
      ).
