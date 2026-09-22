:- module(sscasp_dualise,
          [ predicate_duals/6,          % +Module, +Annotations, +Polarity,
                                        %   +PI, +Clauses, -Duals
            translate_goal/3,           % +Module, +Goal, -Call
            builtin/1                   % @Goal
          ]).
:- use_module(ops).
:- use_module(abstract,    [abstract_head/3]).
:- use_module(dual,        [clause_duals/2, body_variables/3]).
:- use_module(mangle,      [mangle/2]).
:- use_module(annotations, [argument_domain/4]).
:- use_module(domains,     [domain_goal/3]).
:- use_module(library(lists), [append/3, numlist/3, member/2, nth1/3]).
:- use_module(library(apply), [maplist/3, maplist/4, foldl/4]).

/** <module> Dualising a predicate

Where the pieces meet.  Given every clause of one predicate, this produces the
predicates that prove its negation:

    p(0).
    p(X) :- q(X), not t(X, Y).

    m$dual$p$1(A)          :- m$dual.1$p$1(A), m$dual.2$p$1(A).
    m$dual.1$p$1(A)        :- A \= 0.
    m$dual.2$p$1(A)        :- forall(B, m$dual.2.inner$p$1(A, B)).
    m$dual.2.inner$p$1(A, B) :- not q(A).
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

An argument with a declared domain is guarded in the dual:

    :- pred wrap(shape).

    m$dual$wrap$1(A) :- shape(A), m$dual.1$wrap$1(A).

which is the whole point of the annotation.  `not wrap(T)` for an unbound `T`
is otherwise incomplete — it can offer the shapes the head matches but not the
ones it does not, because saying so needs a universally quantified disequality
that no store can express (`doc/bugs.md` #15).  Given a domain there is
nothing to express: the dual enumerates it.

A domain is a restriction of the universe of discourse for that argument, not
a hint, so guarding is sound exactly when the declaration is meant.

### The quantifier

Take one clause, whose `Y` occurs in the body and not in the head:

    p(X) :- q(X), not t(X, Y).

`Y` is what the quantifier ranges over.  With nothing declared there is
nothing to bound it with:

    m$dual$p$1(A)            :- m$dual.1$p$1(A).
    m$dual.1$p$1(A)          :- forall(B, m$dual.1.inner$p$1(A, B)).
    m$dual.1.inner$p$1(A, B) :- m$dual$q$1(A).
    m$dual.1.inner$p$1(A, B) :- m$plain$q$1(A), m$plain$t$2(A, B).

Now say where `t/2`'s second argument lives:

    :- domain thing = [a, b].
    :- pred t(_, thing).

`Y` is used there, so it ranges over `thing`, and the quantifier says so —
the *only* line that changes:

    m$dual.1$p$1(A)          :- for_domain(B, m$plain$thing$1(B),
                                           m$dual.1.inner$p$1(A, B)).

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
conjunction, so for a value outside the domain of any one literal that literal
fails, the conjunction fails, and its negation holds — the instance is true for
free.  Bounding the quantifier by one usage leaves nothing unchecked that
could have failed.
*/

%!  predicate_duals(+Module, +Annotations, +Polarity, +PI, +Clauses, -Duals) is det.
%
%   Duals is every clause of the predicates proving the negation of PI.
%   Clauses are the predicate's clauses, in order; the N-th gets the
%   clause-dual kind `dual(N)`.
%
%   Polarity names the family the predicate belongs to: `plain` for `p`
%   and `neg` for `-p`, which are different predicates with different
%   duals, and `chk(I)` for the NMR check's I-th sub-check.
%
%   Name may be `''`, for a predicate deriving from no user predicate —
%   the sub-checks are exactly that case, and `mangle.pl` reserves the
%   empty name field for it.
%
%   A predicate with **no** clauses gets a dual with an empty body: it is
%   false of everything, so its negation is true of everything.

predicate_duals(Module, Annotations, Polarity, Name/Arity, Clauses, Duals) :-
    dual_kind(Polarity, Base),
    length(Clauses, Count),
    numlist_or_empty(Count, Indices),
    maplist(clause_dual(Module, Annotations, Base, Name/Arity), Indices,
            Clauses, ClauseDuals),
    append(ClauseDuals, Flat),
    predicate_dual(Module, Annotations, Base, Name/Arity, Indices, Top),
    Duals = [Top|Flat].

%!  dual_kind(+Polarity, -Base) is det.

dual_kind(plain, dual).
dual_kind(neg, dualneg).
dual_kind(chk(I), dualchk(I)).

numlist_or_empty(0, []) :- !.
numlist_or_empty(Count, Indices) :- numlist(1, Count, Indices).

%!  predicate_dual(+Module, +Annotations, +PI, +Indices, -Clause) is det.
%
%   The conjunction of the clause duals, guarded by whatever domains the
%   signature gives the arguments.

predicate_dual(Module, Annotations, Base, Name/Arity, Indices, Clause) :-
    functor(Head0, Name, Arity),
    Head0 =.. [_|Args],
    mangle_kind(Module, Base, Name, Arity, DualName),
    Head =.. [DualName|Args],
    domain_guards(Annotations, Name/Arity, Args, Guards0),
    maplist(translate_goal(Module), Guards0, Guards),
    maplist(clause_dual_call(Module, Base, Name/Arity, Args), Indices, Calls),
    append(Guards, Calls, Goals),
    clause_of(Head, Goals, Clause).

clause_dual_call(Module, Base, Name/Arity, Args, Index, Call) :-
    indexed(Base, Index, Kind),
    mangle_kind(Module, Kind, Name, Arity, DualName),
    Call =.. [DualName|Args].

%!  domain_guards(+Annotations, +PI, +Args, -Guards) is det.
%
%   One guard per argument that has a domain, in argument order.

%   Built by walking the arguments rather than with findall/3, which
%   copies: a guard collected that way carries a *fresh* variable and no
%   longer constrains the argument it was made for.

domain_guards(Annotations, PI, Args, Guards) :-
    domain_guards(Args, 1, Annotations, PI, Guards).

domain_guards([], _, _, _, []).
domain_guards([Arg|Args], N, Annotations, PI, Guards) :-
    (   argument_domain(Annotations, PI, N, Domain)
    ->  domain_goal(Domain, Arg, Guard),
        Guards = [Guard|Rest]
    ;   Guards = Rest
    ),
    N1 is N+1,
    domain_guards(Args, N1, Annotations, PI, Rest).

		 /*******************************
		 *         ONE CLAUSE		*
		 *******************************/

%!  clause_dual(+Module, +Annotations, +PI, +Index, +Clause, -Duals) is det.

clause_dual(Module, Annotations, Base, Name/Arity, Index, Clause, Duals) :-
    abstract_head(Clause, Head, HeadGuards),
    clause_body_list(Clause, Body0),
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

%!  defined(+Name, +Args, +Bodies, -Clauses) is det.
%
%   One clause per body — but never *none*.  A clause with no body is
%   unconditionally true, so it contributes no dual bodies, and leaving
%   the predicate undefined would make the program depend on what an
%   undefined predicate does.  An explicit `fail` says the same thing and
%   says it in the program.

defined(Name, Args, [], [Clause]) :-
    !,
    Head =.. [Name|Args],
    Clause = (Head :- fail).
defined(Name, Args, Bodies, Clauses) :-
    maplist(dual_clause(Name, Args), Bodies, Clauses).

dual_clause(Name, Args, Body, Clause) :-
    Head =.. [Name|Args],
    clause_of(Head, Body, Clause).

%!  quantify(+Module, +Variables, +VarDomains, +Goal, -Quantified) is det.
%
%   One quantifier per body-only variable, outermost first, so two of
%   them nest and share one innermost dual rather than taking one each.
%
%   A variable with a domain gets `for_domain(Var, Generator, Goal)`
%   rather than `forall/2`: the domain is named in the term, and what to
%   do with it is the solver's business — enumerate a handful of facts,
%   or post a region and label it.  Expanding here instead would commit
%   to enumeration at compile time, which is right for `[a,b]` and wrong
%   for `0..23`.
%
%   So `forall/2` in a compiled program means exactly one thing: a
%   quantifier nothing bounds.  That is decision 5's undecidable case,
%   and now it is greppable.

quantify(_, [], _, Goal, Goal).
quantify(Module, [Var|Vars], VarDomains, Goal, Quantified) :-
    quantify(Module, Vars, VarDomains, Goal, Inner),
    (   memberchk_eq(Var, VarDomains, Domain)
    ->  domain_goal(Domain, Var, Generator0),
        translate_goal(Module, Generator0, Generator),
        Quantified = for_domain(Var, Generator, Inner)
    ;   Quantified = forall(Var, Inner)
    ).

memberchk_eq(Var, [V-Value|T], Found) :-
    (   Var == V
    ->  Found = Value
    ;   memberchk_eq(Var, T, Found)
    ).

		 /*******************************
		 *      A VARIABLE'S DOMAIN	*
		 *******************************/

%!  var_domains(+Annotations, +Body, +Vars, -VarDomains) is det.
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
%   literal that literal fails, the conjunction fails, and its negation
%   holds — the instance is true for free.  Bounding the quantifier by one
%   usage therefore leaves nothing unchecked that could have failed.

var_domains(Annotations, Body, Vars, VarDomains) :-
    var_domains_(Vars, Annotations, Body, VarDomains).

var_domains_([], _, _, []).
var_domains_([Var|Vars], Annotations, Body, VarDomains) :-
    (   var_domain(Annotations, Body, Var, Domain)
    ->  VarDomains = [Var-Domain|Rest]
    ;   VarDomains = Rest
    ),
    var_domains_(Vars, Annotations, Body, Rest).

var_domain(Annotations, Body, Var, Domain) :-
    member(Literal, Body),
    literal_goal(Literal, Goal),
    goal_parts(Goal, Name, Arity, Args),
    nth1(N, Args, Arg),
    Arg == Var,
    argument_domain(Annotations, Name/Arity, N, Domain),
    !.

literal_goal(not(Goal), Goal) :- !.
literal_goal(-(Goal), Goal) :- !.
literal_goal(Goal, Goal).

		 /*******************************
		 *           GOALS		*
		 *******************************/

%!  translate_goal(+Module, +Goal, -Call) is det.
%
%   A user goal becomes a call to its mangled predicate; its negation
%   becomes a call to the dual.  Built-ins are left as they are.
%
%   `-p(X)` is classical negation, a literal of its own, so it gets the
%   `neg` kind and its negation `dualneg`.

translate_goal(_, Goal, Goal) :-
    builtin(Goal),
    !.
translate_goal(Module, not(Goal), Call) :-
    !,
    goal_kind(Goal, dual, Kind, Name, Arity, Args),
    mangle(pred(Module, Kind, Name, Arity), Mangled),
    Call =.. [Mangled|Args].
translate_goal(Module, Goal, Call) :-
    goal_kind(Goal, plain, Kind, Name, Arity, Args),
    mangle(pred(Module, Kind, Name, Arity), Mangled),
    Call =.. [Mangled|Args].

%!  goal_kind(+Goal, +Positive, -Kind, -Name, -Arity, -Args) is det.
%
%   Positive is the kind wanted for an ordinary goal; a classically
%   negated one takes the matching negative kind.

goal_kind(-(Goal), dual, dualneg, Name, Arity, Args) :- !, goal_parts(Goal, Name, Arity, Args).
goal_kind(-(Goal), plain, neg, Name, Arity, Args) :- !, goal_parts(Goal, Name, Arity, Args).
goal_kind(Goal, Kind, Kind, Name, Arity, Args) :- goal_parts(Goal, Name, Arity, Args).

goal_parts(Goal, Name, Arity, Args) :-
    (   atom(Goal)
    ->  Name = Goal, Arity = 0, Args = []
    ;   Goal =.. [Name|Args],
        length(Args, Arity)
    ).

translate_body(Module, Body, Translated) :-
    maplist(translate_goal(Module), Body, Translated).

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

builtin_pi(true, 0).
builtin_pi(fail, 0).
builtin_pi(=, 2).
builtin_pi(\=, 2).
builtin_pi(#=, 2).
builtin_pi(#<>, 2).
builtin_pi(#<, 2).
builtin_pi(#>, 2).
builtin_pi(#=<, 2).
builtin_pi(#>=, 2).
builtin_pi(forall, 2).
builtin_pi(for_domain, 3).

		 /*******************************
		 *           CLAUSES		*
		 *******************************/

clause_of(Head, [], Head) :- !.
clause_of(Head, Goals, (Head :- Body)) :-
    body_term(Goals, Body).

body_term([Goal], Goal) :- !.
body_term([Goal|Goals], (Goal, Body)) :-
    body_term(Goals, Body).

clause_body_list((_ :- Body), Goals) :-
    !,
    body_list(Body, Goals).
clause_body_list(_, []).

body_list((A, B), Goals) :-
    !,
    body_list(A, GA),
    body_list(B, GB),
    append(GA, GB, Goals).
body_list(true, []) :- !.
body_list(Goal, [Goal]).
