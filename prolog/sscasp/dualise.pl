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

:- module(sscasp_dualise,
          [ predicate_duals/6,          % +Module, +Annotations, +Polarity,
                                        %   +PI, @Clauses, -Duals
            translate_goal/3,           % +Module, @Goal, -Call
            builtin/1,                  % @Goal
            builtin_indicator/1         % ?Name/Arity
          ]).
:- use_module(ops).
:- use_module(abstract,    [abstract_head/3]).
:- use_module(dual,        [clause_duals/2, body_variables/3,
                            negate_literal/2]).
:- use_module(mangle,      [mangle/2]).
:- use_module(annotations, [argument_domain/4, domain_kind/3]).
:- use_module(domains,     [domain_goal/3]).
:- use_module(unfold,      [no_fact/2]).
:- use_module(library(lists), [append/2, append/3, numlist/3, member/2,
                                nth1/3]).
:- use_module(library(apply), [maplist/3, maplist/4, include/3]).
:- use_module(clauses, [clause_goals/2, list_body/2, literal_parts/3]).

/** <module> Dualising a predicate

Where the pieces meet.  Given every clause of one predicate, this produces the
predicates that prove its negation:

    p(0).
    p(X) :- q(X), not t(X, Y).

    m$dual$p$1(A)          :- m$dual.1$p$1(A), m$dual.2$p$1(A).
    m$dual.1$p$1(A)        :- A \= 0.
    m$dual.2$p$1(A)        :- forall([B], m$dual.2.inner$p$1(A, B)).
    m$dual.2.inner$p$1(A, B) :- m$dual$q$1(A).
    m$dual.2.inner$p$1(A, B) :- m$plain$q$1(A), m$plain$t$2(A, B).

Three levels, as `doc/design.md` describes them: the predicate dual is the
conjunction of the clause duals, a clause dual with body-only variables hands
off to an innermost dual through `forall`, and the innermost dual has one
clause per literal.

## Calls, not wrappers

A dual is a *predicate*, so calling one is an ordinary call: the body literal
`not q(A)` becomes a call to `q`'s dual rather than a `not/1` term for an
interpreter to take apart.  That is decision 1 — the point of compiling to
predicates rather than interpreting `pr_rule/3` — and it is what makes `spy`,
`trace` and per-clause coverage work on the generated program.

Built-ins are left alone.  `=`, `\=` and the CLP comparisons mean what they
mean; only user predicates are renamed.

## Domains

A signature is shorthand for guards, and `compile.pl` writes them in before
the clauses get here — into every clause of the predicate
(`signature_guarded/3`):

    :- pred wrap(shape).
    wrap(f(X)) :- item(X).          % becomes
    wrap(A) :- shape(A), A = f(X), item(X).

and in front of every negated literal of it a body writes
(`use_site_guarded/3`): `not wrap(T)` becomes `shape(T), not wrap(T)`.  So
the dual follows from the guarded clause like any other, outside the domain
`wrap(T)` is false, and a program asking `not wrap(T)` asks only about the
domain (`doc/design-next.md` open question 3).  The guard goes before the
head unification, and that is what the annotation buys.  `not wrap(T)` for an
unbound `T` is otherwise incomplete — it can offer the shapes the head matches
but not the ones it does not, because saying so needs a universally quantified
disequality that no store can express (`doc/bugs.md` #15).  With the guard
first there is nothing to express: the dual enumerates the domain, and for a
ground `T` the head unification is decidable.

### The quantifier

Take one clause, whose `Y` occurs in the body and not in the head:

    p(X) :- q(X), not t(X, Y).

`Y` is what the quantifier ranges over.  With nothing declared there is
nothing to bound it with:

    m$dual$p$1(A)            :- m$dual.1$p$1(A).
    m$dual.1$p$1(A)          :- forall([B], m$dual.1.inner$p$1(A, B)).
    m$dual.1.inner$p$1(A, B) :- m$dual$q$1(A).
    m$dual.1.inner$p$1(A, B) :- m$plain$q$1(A), m$plain$t$2(A, B).

Now say where `t/2`'s second argument lives:

    :- domain thing = [a, b].
    :- pred t(_, thing).

The compiler puts the guard in front of the negated literal, so the clause
that arrives is `p(X) :- q(X), thing(Y), not t(X, Y)`.  `thing(Y)` is a
positive literal of a domain, so `Y` ranges over `thing`, and the quantifier
says so:

    m$dual.1$p$1(A)          :- for_domain(B, m$plain$thing$1(B),
                                           m$dual.1.inner$p$1(A, B)).

A positive `t(X, Y)` would bound it the same way, through `t`'s signature;
a bare `not t(X, Y)` would not, since it holds outside `thing`.

`for_domain(Var, Generator, Goal)` names the domain rather than expanding it.
What to do with it is the solver's business: enumerate a handful of facts, or
post a region and label it.  Expanding here would commit to enumeration at
compile time — right for `[a, b]`, wrong for `0..23`, and the whole reason a
range compiles to a CLP region instead of two dozen facts.  A range domain
gives exactly the same shape:

    :- domain hour = 0..23.

    m$dual.1$busy$1(A)       :- for_domain(B, m$plain$hour$1(B),
                                           m$dual.1.inner$busy$1(A, B)).

That is decision 5's first case, and the reason annotations pay for
themselves.  A variable with no domain keeps `forall/2`, so **`forall/2` in a
compiled program means exactly one thing: a quantifier nothing bounds** —
decision 5's undecidable case, now greppable.

Note where the domain came from.  Not from `p/1`, which is the predicate being
dualised and whose signature says nothing about `Y`; from **`t/2`, where `Y`
is used**.

Where a variable is used in several places, *any* of the domains will do and
the first is taken.  That is not a shortcut: the inner dual negates a
conjunction, so for a value outside the domain of any one positive literal
that literal fails, the conjunction fails, and its negation holds — the instance is true for
free.  Bounding the quantifier by one usage leaves nothing unchecked that
could have failed.
*/

%!  predicate_duals(+Module, +Annotations, +Polarity, +PI, @Clauses, -Duals) is det.
%
%   Duals is every clause of the predicates proving the negation of PI.
%   Clauses are the predicate's clauses, in order; the N-th gets the
%   clause-dual kind `dual(N)`.
%
%   Polarity names the family the predicate belongs to: `plain` for `p`
%   and `neg` for `-p`, which are different predicates with different
%   duals, and `chk(I)` for the NMR check's I-th sub-check.
%
%   PI's name may be `''`, for a predicate deriving from no user
%   predicate — the sub-checks are exactly that case, and `mangle.pl`
%   reserves the empty name field for it.
%
%   A predicate with **no** clauses gets a dual with an empty body: it is
%   false of everything, so its negation is true of everything.
%
%   @arg Annotations is the annotation table of
%       `annotations.pl:program_annotations/2`, with the key
%       `fact_predicates` added by `compile.pl:compile_program/3`: a
%       list of `Name/Arity` whose facts bound a variable.
%   @arg Polarity may also be `abducible` or `abducibleneg`, the two
%       halves of an abducible's even loop.
%   @arg Clauses are source clauses with the polarity wrapper taken
%       off the head (`compile.pl:strip_polarity/2`): a clause of `-p`
%       arrives as one of `p`.

:- det(predicate_duals/6).

predicate_duals(Module, Annotations, Polarity, PI, Clauses, Duals) :-
    dual_kind(Polarity, Base),
    length(Clauses, Count),
    numlist_or_empty(Count, Indices),
    maplist(clause_dual(Module, Annotations, Base, PI), Indices,
            Clauses, ClauseDuals),
    append(ClauseDuals, Flat),
    predicate_dual(Module, Base, PI, Indices, Top),
    Duals = [Top|Flat].

%!  dual_kind(+Polarity, -Base) is det.

dual_kind(plain, Base) => Base = dual.
dual_kind(neg, Base) => Base = dualneg.
dual_kind(chk(I), Base) => Base = dualchk(I).
dual_kind(abducible, Base) => Base = dualabducible.
dual_kind(abducibleneg, Base) => Base = dualabducibleneg.

numlist_or_empty(0, Indices) =>
    Indices = [].
numlist_or_empty(Count, Indices) =>
    numlist(1, Count, Indices).

%!  predicate_dual(+Module, +Base, +PI, +Indices, -Clause) is det.
%
%   The conjunction of the clause duals.  A signature's domain is not a
%   guard here: `compile.pl:signature_guarded/3` has already put it into
%   the clauses, so their duals say what it means for the negation.
%
%   @arg Base is the predicate dual's kind, from dual_kind/2: `dual`,
%       `dualneg`, `dualchk(I)`, ...
%   @arg Indices is `1..N` for the N clauses, `[]` for none.

predicate_dual(Module, Base, PI, Indices, Clause) :-
    PI = Name/Arity,
    functor(Head0, Name, Arity),
    Head0 =.. [_|Args],
    mangle_kind(Module, Base, Name, Arity, DualName),
    Head =.. [DualName|Args],
    maplist(clause_dual_call(Module, Base, PI, Args), Indices, Calls),
    clause_of(Head, Calls, Clause).

clause_dual_call(Module, Base, Name/Arity, Args, Index, Call) :-
    indexed(Base, Index, Kind),
    mangle_kind(Module, Kind, Name, Arity, DualName),
    Call =.. [DualName|Args].

		 /*******************************
		 *         ONE CLAUSE		*
		 *******************************/

%!  clause_dual(+Module, +Annotations, +Base, +PI, +Index, @Clause, -Duals) is det.
%
%   @arg Base is as in predicate_dual/6; Index says which clause.
%   @arg Clause is one clause as predicate_duals/6 receives it, its
%       head not yet abstracted.
%   @arg Duals are the translated clauses of the Index-th clause dual:
%       one per dual body, or, with body-only variables, a bridge
%       holding the quantifier followed by the `inner` clauses.

clause_dual(Module, Annotations, Base, Name/Arity, Index, Clause, Duals) :-
    get_dict(fact_index, Annotations, Facts),
    Facts \== none,
    clause_goals(Clause, Goals),
    member(Literal, Goals),
    no_fact(Facts, Literal),
    !,
    abstract_head(Clause, Head, HeadGuards0),
    Head =.. [_|Args],
    include(closed_guard(Head), HeadGuards0, HeadGuards),
    clause_duals(HeadGuards, GuardBodies),
    append(HeadGuards, [not(Literal)], Absent0),
    append(GuardBodies, [Absent0], DualBodies),
    maplist(translate_body(Module), DualBodies, Translated0),
    map_last(map_last(absent), Translated0, Translated),
    indexed(Base, Index, Kind),
    mangle_kind(Module, Kind, Name, Arity, DualName),
    defined(DualName, Args, Translated, Duals).
clause_dual(Module, Annotations, Base, Name/Arity, Index, Clause, Duals) :-
    abstract_head(Clause, Head, HeadGuards),
    clause_goals(Clause, Body0),
    append(HeadGuards, Body0, Body),
    Head =.. [_|Args],
    body_variables(Head, Body, BodyVars),
    clause_duals(Body, DualBodies),
    maplist(translate_body(Module), DualBodies, Translated),
    indexed(Base, Index, Kind),
    mangle_kind(Module, Kind, Name, Arity, DualName),
    (   BodyVars == []
    ->  defined(DualName, Args, Translated, Duals)
    ;   indexed(Base, Index, inner, InnerKind),
        mangle_kind(Module, InnerKind, Name, Arity, InnerName),
        append(Args, BodyVars, InnerArgs),
        Inner =.. [InnerName|InnerArgs],
        var_domains(Annotations, Body, BodyVars, VarDomains),
        quantify(Module, BodyVars, VarDomains, Inner, Quantified),
        Outer =.. [DualName|Args],
        clause_of(Outer, [Quantified], Bridge),
        defined(InnerName, InnerArgs, Translated, InnerClauses),
        Duals = [Bridge|InnerClauses]
    ).

%   A head unification whose variables are all the head's: `A = cloz-7`
%   but not `A = message{value:V}`.  The dual of the other kind,
%   `A \= message{value:V}`, is about every V, and without the
%   quantifier the clause dual would otherwise have it says something
%   about *some* V instead — a constraint on A and a fresh V that holds
%   for nothing, but which a quantifier over A then reads and covers
%   piece by piece.  The dual of a clause no fact can make true holds
%   wherever its head matches *and* wherever it does not, so leaving a
%   guard out only means the clause is not dismissed that early for a
%   call whose arguments differ there.

closed_guard(Head, Guard) :-
    term_variables(Guard, Vars),
    term_variables(Head, HeadVars),
    \+ ( member(V, Vars),
          \+ ( member(H, HeadVars), H == V )
        ).

%!  map_last(:Goal, +List0, -List) is det.
%
%   List is List0 with Goal applied to its last element.

:- meta_predicate map_last(2, +, -).

map_last(Goal, [X0], List) =>
    call(Goal, X0, X),
    List = [X].
map_last(Goal, [H|T0], List) =>
    List = [H|T],
    map_last(Goal, T0, T).

absent(Call, absent(Call)).

%   **A clause no fact can make true** has a dual that holds wherever
%   its head matches: one of its literals has no clause to prove it
%   (`unfold.pl:no_fact/2`), so the body is false whatever its variables
%   are, head variables included.  The dual is that of the head's unifications followed by the
%   literal alone, without the quantifier over the body-only variables.
%   The head's unifications stay first because they are what rejects a
%   call for another head cheaply — `not rule(r2)` should not
%   look at the facts to dismiss the clause for `r1` — and the literal
%   is the reason the clause does not hold, which the justification
%   shows.
%
%   The literal's negation is **not proved, but recorded**:
%   `absent(NotCall)`, which `emit.pl` makes a proof node without a
%   search.  Proving it would be a search for values its variables do
%   not take, and a fact that agrees in some arguments offers one way
%   to differ per argument — nondeterminism for a negation that was
%   decided at compile time.  Recording it posts no constraint either,
%   which is right: it holds for every value.

%!  indexed(+Base, +Index, -Kind) is det.
%!  indexed(+Base, +Index, +Tag, -Kind) is det.
%
%   `dual` and index 2 give `dual(2)`, and with `inner`, `dual(2,inner)`.

%   Base may itself be compound — `dualchk(1)` gives `dualchk(1,2)` — so
%   the index is appended to its arguments rather than to its name.

indexed(Base, Index, Kind) :-
    Base =.. List,
    append(List, [Index], List1),
    Kind =.. List1.
indexed(Base, Index, Tag, Kind) :-
    Base =.. List,
    append(List, [Index, Tag], List1),
    Kind =.. List1.

%!  mangle_kind(+Module, +Kind, +Name, +Arity, -Mangled) is det.
%
%   An empty Name is a predicate deriving from no user predicate, which
%   `mangle/2` spells `prog/3` rather than `pred/4`.

mangle_kind(Module, Kind, Name, Arity, Mangled) :-
    (   Name == ''
    ->  mangle(prog(Module, Kind, Arity), Mangled)
    ;   mangle(pred(Module, Kind, Name, Arity), Mangled)
    ).

%!  defined(+Name, @Args, @Bodies, -Clauses) is det.
%
%   One clause per body — but never *none*.  A clause with no body is
%   unconditionally true, so it contributes no dual bodies, and leaving
%   the predicate undefined would make the program depend on what an
%   undefined predicate does.  An explicit `fail` says the same thing and
%   says it in the program.
%
%   @arg Name is the mangled name of the predicate being defined.
%   @arg Bodies is a list of bodies, each a list of translated goals.

defined(Name, Args, [], Clauses) =>
    Head =.. [Name|Args],
    Clauses = [(Head :- fail)].
defined(Name, Args, Bodies, Clauses) =>
    maplist(dual_clause(Name, Args), Bodies, Clauses).

dual_clause(Name, Args, Body, Clause) :-
    Head =.. [Name|Args],
    clause_of(Head, Body, Clause).

%!  quantify(+Module, @Variables, @VarDomains, @Goal, -Quantified) is det.
%
%   **One** quantifier over all the body-only variables nothing bounds,
%   as a list — `forall([B, C], Inner)` and not `forall(B, forall(C,
%   Inner))`.  The two say the same thing and cost differently: covering
%   a nest means covering C's domain once per region of B's, so the work
%   is the *product* of the coverings, where covering the pair reads one
%   answer's constraints on both variables at once and the pieces add up
%   instead.  `bec_tap_02` made 83,593 coverings of a three-variable nest
%   where the tuple needs a few dozen.  Upstream's
%   `prolog/scasp/solve.pl:solve_c_forall/8` collects its nest into one `c_forall(Vars, Goal)` for the same
%   reason.
%
%   A variable with a *domain* keeps a quantifier of its own, outside:
%   it has something to enumerate, and each value is a separate search
%   rather than a region to complement.  It gets
%   `for_domain(Var, Generator, Goal)` rather than going in the list: the
%   domain is named in the term, and what to do with it is the solver's
%   business — enumerate a handful of facts, or post a region and label
%   it.  Expanding here instead would commit
%   to enumeration at compile time, which is right for `[a,b]` and wrong
%   for `0..23`.
%
%   So `forall/2` in a compiled program means exactly one thing: the
%   quantifiers nothing bounds.  That is decision 5's undecidable case,
%   and now it is greppable.
%
%   @arg VarDomains is the `Var-Domain` list of var_domains/4.
%   @arg Goal is the call to the inner dual, which the quantifiers
%       wrap.

quantify(Module, Variables, VarDomains, Goal, Quantified) :-
    split_domains(Variables, VarDomains, Bounded, Free),
    (   Free == []
    ->  Inner = Goal
    ;   Inner = forall(Free, Goal)
    ),
    enumerated(Module, Bounded, VarDomains, Inner, Quantified).

split_domains([], _, Bounded, Free) =>
    Bounded = [],
    Free = [].
split_domains([Var|Vars], VarDomains, Bounded, Free) =>
    (   memberchk_eq(Var, VarDomains, _)
    ->  Bounded = [Var|Rest], Free = More
    ;   Bounded = Rest, Free = [Var|More]
    ),
    split_domains(Vars, VarDomains, Rest, More).

enumerated(_, [], _, Goal, Quantified) =>
    Quantified = Goal.
enumerated(Module, [Var|Vars], VarDomains, Goal, Quantified) =>
    memberchk_eq(Var, VarDomains, Domain),
    (   Domain = inferred(Literal)
    ->  same_literal(Vars, VarDomains, Literal, Same, Rest),
        Group = [Var|Same],
        generator(Literal, Group, Generator0),
        (   Same == []
        ->  Quantified0 = Var
        ;   Quantified0 = Group
        )
    ;   Rest = Vars,
        domain_goal(Domain, Var, Generator0),
        Quantified0 = Var
    ),
    enumerated(Module, Rest, VarDomains, Goal, Inner),
    translate_goal(Module, Generator0, Generator),
    Quantified = for_domain(Quantified0, Generator, Inner).

%   **The variables one fact literal bounds are enumerated together**,
%   as tuples of its facts: `happens(E, T)` quantifies `[E, T]` over the
%   pairs `happens/2` holds of, not E over every event and then T over
%   every time for each.  The same argument as a tuple `forall/2`
%   (quantify/5): nested, the coverings multiply, and where the values
%   are not independent most combinations are ones no fact has.
%   `bec_loan` has `happens/2` and `is_due_on_from_to/4`, and nesting
%   their variables one at a time took it from 376 s to no answer in an
%   hour.

same_literal([], _, _, Same, Rest) =>
    Same = [],
    Rest = [].
same_literal([Var|Vars], VarDomains, Literal, Same, Rest) =>
    memberchk_eq(Var, VarDomains, Domain),
    (   Domain = inferred(Other),
        Other == Literal
    ->  Same = [Var|Same1],
        Rest = Rest1
    ;   Same = Same1,
        Rest = [Var|Rest1]
    ),
    same_literal(Vars, VarDomains, Literal, Same1, Rest1).

%   The generator is the literal with every argument that is not one of
%   the group made fresh: every tuple the facts hold at those positions,
%   which is a superset of the ones that matter and so safe.  A
%   **ground** argument is kept: for a fact that disagrees with it the
%   literal is false, so there is nothing to check, and it is what
%   makes `obs(age, A, years)` enumerate the ages rather than the value
%   of every observation.  Arguments holding other variables are made
%   fresh, as they may not be bound yet when the generator runs.

generator(Literal, Group, Generator) :-
    Literal =.. [Name|Args],
    maplist(generator_arg(Group), Args, GenArgs),
    Generator =.. [Name|GenArgs].

generator_arg(Group, Arg, GenArg) :-
    (   var(Arg),
        memberchk_eq_(Arg, Group)
    ->  GenArg = Arg
    ;   ground(Arg)
    ->  GenArg = Arg
    ;   true
    ).

memberchk_eq_(X, [Y|T]) :-
    (   X == Y
    ->  true
    ;   memberchk_eq_(X, T)
    ).

memberchk_eq(Var, [V-Value|T], Found) :-
    (   Var == V
    ->  Found = Value
    ;   memberchk_eq(Var, T, Found)
    ).

		 /*******************************
		 *      A VARIABLE'S DOMAIN	*
		 *******************************/

%!  var_domains(+Annotations, @Body, @Vars, -VarDomains) is det.
%
%   VarDomains pairs each body-only variable that has a domain with that
%   domain's name.  A variable with none is absent, and stays quantified.
%
%   The domain is read off **where the variable is used** — argument 2 of
%   `t/2`, not the signature of the predicate being dualised, which says
%   nothing about it.
%
%   Where a variable is used in several places, *any* of the domains will
%   do, and the first is taken.  That is not a shortcut.  The inner dual
%   negates a conjunction, so for a value outside the domain of any one
%   positive literal that literal fails, the conjunction fails, and its negation
%   holds — the instance is true for free.  Bounding the quantifier by one
%   usage therefore leaves nothing unchecked that could have failed.
%
%   @arg Annotations is as in predicate_duals/6.
%   @arg Body is the abstracted clause body, a list of source goals.
%   @arg VarDomains is a list of `Var-Domain`, Domain a domain name or
%       `inferred(Literal)` for a positive literal of a fact predicate.

var_domains(Annotations, Body, Vars, VarDomains) :-
    var_domains_(Vars, Annotations, Body, VarDomains).

var_domains_([], _, _, VarDomains) =>
    VarDomains = [].
var_domains_([Var|Vars], Annotations, Body, VarDomains) =>
    (   var_domain(Annotations, Body, Var, Domain)
    ->  VarDomains = [Var-Domain|Rest]
    ;   VarDomains = Rest
    ),
    var_domains_(Vars, Annotations, Body, Rest).

var_domain(Annotations, Body, Var, Domain) :-
    member(Literal, Body),
    positive_literal(Literal),
    goal_parts(Literal, Name, Arity, Args),
    nth1(N, Args, Arg),
    Arg == Var,
    argument_domain(Annotations, Name/Arity, N, Domain),
    !.
var_domain(Annotations, Body, Var, Domain) :-
    member(Literal, Body),
    positive_literal(Literal),
    compound(Literal),
    Literal =.. [Domain, Arg],
    Arg == Var,
    domain_name(Annotations, Domain),
    !.
var_domain(Annotations, Body, Var, inferred(Literal)) :-
    get_dict(fact_predicates, Annotations, Facts),
    member(Literal, Body),
    fact_literal(Literal, Facts),
    goal_parts(Literal, _Name, _Arity, Args),
    member(Arg, Args),
    Arg == Var,
    !.

%   **An inferred domain.**  Where nothing is declared, a *positive*
%   literal of a predicate defined by ground facts alone
%   (`compile.pl:fact_predicates/2`) bounds the variable to the values
%   those facts give at its position.  For any other value the literal
%   is false, the conjunction is false and its negation holds, which is
%   the argument above — and it has to be a positive literal for that:
%   outside `q`'s facts `not q(W)` *holds*, so a negated literal bounds
%   nothing.  The domain is the literal itself: enumerated/5 makes the
%   generator, and enumerates every variable it bounds at once.
%
%   It is the `:- domain` a program did not write.  `hamiltonian_scasp`,
%   whose nodes are four facts, answers in 0.06 s this way against 0.7 s
%   searching the universe for them.

%   **A domain's own literal.**  `d(W)` bounds W by `d` for the same
%   reason: it is false outside.  That is what makes the guard a
%   signature puts before `not q(W)` (`compile.pl:use_site_guarded/3`)
%   bound the quantifier its dual needs, whether `d` is an enumeration,
%   a range or a predicate of the program's own.  A domain is a name
%   `:- domain` declares or a signature uses.

domain_name(Annotations, Domain) :-
    Domain \== any,
    (   domain_kind(Annotations, Domain, _)
    ->  true
    ;   dict_pairs(Annotations.signatures, _, Pairs),
        member(_-Arities, Pairs),
        member(_-Domains, Arities),
        memberchk(Domain, Domains)
    ->  true
    ).

fact_literal(Literal, Facts) :-
    positive_literal(Literal),
    \+ builtin(Literal),
    functor(Literal, Name, Arity),
    memberchk(Name/Arity, Facts).

%   **Only a positive literal bounds a variable**, declared or inferred.
%   Outside a signature's domain `q(W)` is false, the guards the
%   signature stands for being in `q`'s clauses; `not q(W)` is *true*
%   there, so a negated literal says nothing about W, and neither does a
%   classically negated one, which the signature does not guard.

positive_literal(Literal) :-
    callable(Literal),
    Literal \= not(_),
    Literal \= -(_).


		 /*******************************
		 *           GOALS		*
		 *******************************/

%!  translate_goal(+Module, @Goal, -Call) is det.
%
%   A user goal becomes a call to its mangled predicate; its negation
%   becomes a call to the dual.  `findall/3` is neither: it is a
%   built-in whose *second argument* is a goal, so the translation goes
%   inside it.  Built-ins are left as they are, and so
%   is the negation of one: `not (A is B)` has no partner to turn into,
%   so it stays and `emit.pl` says what to do with it.  `not findall/3` is
%   in that group and has to be named here, or the clause below would
%   read it as a user predicate called `findall` and dualise *that* —
%   which closes as undefined, so the negation comes out true of
%   everything and `not p` succeeds although p holds.
%
%   `-p(X)` is classical negation, a literal of its own, so it gets the
%   `neg` kind and its negation `dualneg`.  `abducible(p(X))` and
%   `abducible(-p(X))` are the two halves of an abducible's even loop,
%   which are literals of their own for the same reason.

%   A `forall/2` the author wrote is a goal with a goal inside it, so
%   the translation goes in — and the solver's `forall/2` takes a list,
%   so a nest of them collapses into one.

:- det(translate_goal/3).

translate_goal(Module, Quantified, Translated), Quantified = forall(_, _) =>
    quantifier(Quantified, Vars, Body),
    translate_conjunction(Module, Body, Inner),
    Translated = forall(Vars, Inner).

%   Its negation is an **existential**: "not for every X" is "for some X,
%   not", and a dual clause body is where a free variable already means
%   some.  So the quantifier drops and what is left is the negated body.

translate_goal(Module, not(Quantified), Translated),
        Quantified = forall(_, _) =>
    quantifier(Quantified, _, Body),
    (   Body = (_, _)
    ->  throw(error(scasp_error(negated_conjunction(Quantified)), _))
    ;   negate_literal(Body, Negated),
        translate_goal(Module, Negated, Translated)
    ).

translate_goal(Module, not(findall(Template, Goal, List)), Translated) =>
    translate_conjunction(Module, Goal, Inner),
    Translated = not(findall(Template, Inner, List)).
translate_goal(Module, findall(Template, Goal, List), Translated) =>
    translate_conjunction(Module, Goal, Inner),
    Translated = findall(Template, Inner, List).
translate_goal(_, Goal, Translated), builtin(Goal) =>
    Translated = Goal.
translate_goal(Module, not(Goal), Translated), builtin(Goal) =>
    negate_literal(Goal, Negated),
    (   Negated = not(_)
    ->  Translated = Negated	% no partner: emit.pl:builtin/2 says it
    ;   translate_goal(Module, Negated, Translated)
    ).
translate_goal(Module, not(Goal), Call) =>
    goal_kind(Goal, dual, Kind, Name, Arity, Args),
    mangle(pred(Module, Kind, Name, Arity), Mangled),
    Call =.. [Mangled|Args].
translate_goal(Module, Goal, Call) =>
    goal_kind(Goal, plain, Kind, Name, Arity, Args),
    mangle(pred(Module, Kind, Name, Arity), Mangled),
    Call =.. [Mangled|Args].

%!  goal_kind(@Goal, +Positive, -Kind, -Name, -Arity, -Args) is det.
%
%   Positive is the kind wanted for an ordinary goal; a classically
%   negated one takes the matching negative kind.
%
%   @arg Positive is `plain` for a goal, `dual` for its negation.
%   @arg Kind is the mangle kind: Positive itself, or `neg`/`dualneg`,
%       `abducible`/`dualabducible`, `abducibleneg`/`dualabducibleneg`
%       for the wrapped goals.

goal_kind(Literal, Positive, Kind, Name, Arity, Args) :-
    literal_parts(Literal, Polarity, Goal),
    polarity_kind(Polarity, Positive, Kind),
    goal_parts(Goal, Name, Arity, Args).

polarity_kind(plain,        plain, plain).
polarity_kind(plain,        dual,  dual).
polarity_kind(neg,          plain, neg).
polarity_kind(neg,          dual,  dualneg).
polarity_kind(abducible,    plain, abducible).
polarity_kind(abducible,    dual,  dualabducible).
polarity_kind(abducibleneg, plain, abducibleneg).
polarity_kind(abducibleneg, dual,  dualabducibleneg).

goal_parts(Goal, Name, Arity, Args) :-
    (   atom(Goal)
    ->  Name = Goal, Arity = 0, Args = []
    ;   Goal =.. [Name|Args],
        length(Args, Arity)
    ).

translate_body(Module, Body, Translated) :-
    maplist(translate_goal(Module), Body, Translated).

%!  translate_conjunction(+Module, @Body, -Translated) is det.
%
%   A body written as a conjunction rather than a list, which is what
%   `findall/3` carries.

%!  quantifier(@Goal, -Vars, -Body) is det.
%
%   A nest of author-written `forall/2` as the variables it binds and
%   the goal beneath them.

quantifier(forall(Var, Goal), Vars0, Body) =>
    Vars0 = [Var|Vars],
    quantifier(Goal, Vars, Body).
quantifier(Goal, Vars, Body) =>
    Vars = [],
    Body = Goal.

translate_conjunction(Module, (A, B), Translated) =>
    translate_conjunction(Module, A, TA),
    translate_conjunction(Module, B, TB),
    Translated = (TA, TB).
translate_conjunction(Module, Body, Translated) =>
    translate_goal(Module, Body, Translated).

%!  builtin(@Goal) is semidet.
%
%   Goals the compiler passes through untouched.  Deliberately short: a
%   name not on it is a user predicate, so forgetting one shows up as a
%   call to a predicate that does not exist rather than as a silently
%   wrong answer.

builtin(Goal) :-
    nonvar(Goal),
    functor(Goal, Name, Arity),
    builtin_pi(Name, Arity).

%!  builtin_indicator(?PI) is nondet.
%
%   Enumerate the built-ins builtin/1 accepts, as `Name/Arity`.  Each
%   has to have a runnable form in `emit.pl`, and so does its negation;
%   `test_emit.pl` walks this to check that the two tables agree.

builtin_indicator(Name/Arity) :-
    builtin_pi(Name, Arity).

builtin_pi(true, 0).
builtin_pi(fail, 0).
builtin_pi(false, 0).
builtin_pi(=, 2).
builtin_pi(\=, 2).
builtin_pi(#=, 2).
builtin_pi(#<>, 2).
builtin_pi(#<, 2).
builtin_pi(#>, 2).
builtin_pi(#=<, 2).
builtin_pi(#>=, 2).

%   The Herbrand store's own spelling.  `∈` is here because it is what
%   `∉` dualises to, and a dual has to be something to run.

builtin_pi('∉', 2).
builtin_pi('∈', 2).
builtin_pi(forall, 2).
builtin_pi(for_domain, 3).

%   The check `compile.pl:guard_goals/6` puts before a signature's guard
%   when domain errors are on.  Its goal is already translated.

builtin_pi('$domain_check', 3).

%   Prolog arithmetic.  Distinct from the CLP(Q) comparisons above: these
%   evaluate their arguments and test, so they need them bound, where
%   `#<` constrains and does not.  A program may use either.

builtin_pi(is, 2).
builtin_pi(<, 2).
builtin_pi(>, 2).
builtin_pi(=<, 2).
builtin_pi(>=, 2).
builtin_pi(=:=, 2).
builtin_pi(=\=, 2).

		 /*******************************
		 *           CLAUSES		*
		 *******************************/

clause_of(Head, [], Clause) =>
    Clause = Head.
clause_of(Head, Goals, Clause) =>
    Clause = (Head :- Body),
    list_body(Goals, Body).

