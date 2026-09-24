:- module(sscasp_emit,
          [ runnable_program/3,         % +Module, +Program, -Clauses
            body/6,                     % +Body, +Module, +Anc, +S0, -S,
                                        %   -Runnable
            checked_kind/1,             % ?Kind
            polarity/2,                 % ?Kind, ?Polarity
            builtin/2                   % ?Goal, -Runnable
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

Those goals are **qualified with the module the program will live in**, which
is why this pass takes one.  A `meta_predicate` declaration would be the usual
way to carry the module across, but it is applied when a clause is *compiled*
and these clauses are `assertz/1`-ed, so the runtime would look for them in
its own module and not find them.

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

%!  polarity(?Kind, ?Polarity) is det.
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

%!  opposite(?Kind, ?Opposite) is det.
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

runnable_program(Module, Program, Clauses) :-
    Logical = Program.clauses,
    maplist(runnable_clause(Module), Logical, Rewritten),
    findall(Wrapper, wrapper(Logical, Wrapper), Wrappers),
    append(Wrappers, Rewritten, Clauses).

%!  runnable_clause(+Module, +Clause, -Runnable) is det.

runnable_clause(Module, Clause, (Head :- Body)) :-
    clause_parts(Clause, Head0, Body0),
    head(Head0, Head, Anc, S0, S),
    body(Body0, Module, Anc, S0, S, Body).

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

%!  body(+Body, +Module, +Anc, +S0, -S, -Runnable) is det.

body(Var, _, _, S, S, call(Var)) :-
    var(Var),
    !.
body((A, B), M, Anc, S0, S, (RA, RB)) :-
    !,
    body(A, M, Anc, S0, S1, RA),
    body(B, M, Anc, S1, S, RB).
body(true, _, _, S, S, true) :- !.
body(fail, _, _, S0, S, (S = S0, fail)) :- !.
body(for_domain(Var, Gen, Goal), M, Anc, S0, S, Runnable) :-
    !,
    Runnable = sscasp_solve:for_domain(Var, M:Gen, M:Goal, Anc, S0, S).
body(forall(Vars, Goal), M, Anc, S0, S, Runnable) :-
    !,
    Runnable = sscasp_solve:forall(Vars, M:Goal, Anc, S0, S).
body(findall(Template, Goal, List), M, Anc, S0, S, Runnable) :-
    !,
    Runnable = sscasp_solve:findall_(Template, M:Goal, List, Anc, S0, S).
body(not(findall(Template, Goal, List)), _, _, S0, S, Runnable) :-
    !,
    Runnable = ( S = S0,
                 throw(error(sscasp_error(
                          negated_findall(findall(Template, Goal, List))), _))
               ).
body(sld(Goal, Type), _, Anc, S0, S, Runnable) :-
    !,
    Runnable = sscasp_solve:sld_goal(Goal, Type, Anc, S0, S).
body(Goal, _, Anc, S0, S, Runnable) :-
    Goal =.. [Name|Args],
    demangle(Name, _),
    !,
    append(Args, [Anc, S0, S], Args1),
    Runnable =.. [Name|Args1].
body(Goal, _, _, S0, S, Runnable) :-
    builtin(Goal, Runnable0),
    !,
    Runnable = (Runnable0, S = S0).
body(Goal, _, _, _, _, _) :-
    domain_error(sscasp_body_goal, Goal).

%!  builtin(+Goal, -Runnable) is semidet.
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
%   Herbrand disunification for everything else.  `library(clpq)` is
%   named explicitly rather than left to resolve, since `library(clpr)`
%   defines the same `{}/1` over a different domain and the difference is
%   not one to discover at run time.

%   Not its negation, though.  `not findall(T, G, L)` asks what the old
%   implementation's `exec_neg_findall/5` asks and does not answer: which
%   of the ways the collection could have gone differently is the one
%   that makes the list wrong.  It is said here rather than left to fail,
%   because failing means `not p` succeeds where p holds.

builtin((A = B), (A = B)).
builtin((A \= B), sscasp_herbrand:apart(A, B)).

%   The Herbrand store, said the way a program says it.  `apart/2` and
%   not `dif/2` because `herbrand.pl` owns which of its two backings
%   holds a given disequality, and a post that went round it would put a
%   prohibited value where the set already has one — see there.

builtin('∉'(X, Values), sscasp_herbrand:not_in_list(X, Values)).
builtin('∈'(X, Values), sscasp_herbrand:'∈'(X, Values)).

%   `true` and `fail` negate to each other.  `dual.pl` leaves them for
%   here rather than pairing them in `dual_operator/2`, because that
%   table is about operators whose negation is another *operator*.

builtin(not(true), fail).
builtin(not(fail), true).

%   Prolog arithmetic evaluates and tests, so it passes straight through.
%   `not (A is B)` is the one with something to say: evaluating the right
%   side and disagreeing with the result *constrains* A, where
%   `\+ (A is B)` would only fail on an unbound one.

builtin(is(A, B), is(A, B)).
builtin(not(is(A, B)), (Value is B, sscasp_herbrand:apart(A, Value))).
builtin(Goal, Goal) :-
    nonvar(Goal),
    functor(Goal, Name, 2),
    arithmetic_comparison(Name).
builtin(Goal, clpq:{Constraint}) :-
    nonvar(Goal),
    Goal =.. [Name, A, B],
    comparison(Name, Operator),
    Constraint =.. [Operator, A, B].

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
    append(Args, [Anc1, S0, S2], BodyArgs),
    BodyCall =.. [BodyName|BodyArgs],
    append(Args, [Anc, S0, S], HeadArgs),
    Head =.. [Name|HeadArgs],
    Wrapper =
      ( Head :-
          sscasp_solve:check(Goal, Neg, Polarity, Anc, S0, Anc1, Action),
          (   Action == expand
          ->  BodyCall,
              sscasp_solve:proved(Anc1, S0, S2, S)
          ;   sscasp_solve:done(Goal, Action, S0, S)
          )
      ).


		 /*******************************
		 *          MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(error(sscasp_error(negated_findall(Goal)), _)) -->
    [ 's(CASP): the negation of ~p is not implemented'-[Goal] ].
