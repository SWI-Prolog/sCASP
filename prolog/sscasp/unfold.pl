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

:- module(sscasp_unfold,
          [ fact_index/4,               % +FactPIs, @Clauses, +SldPIs, -Index
            unfold_facts/3,             % +Index, @Clauses0, -Clauses
            unfold_denials/3,           % +Index, +Bodies0, -Bodies
            no_fact/2                   % +Index, @Literal
          ]).
:- use_module(ops).
:- use_module(clauses, [clause_parts/3, body_list/2, list_body/2]).
:- use_module(dualise, [builtin/1]).
:- use_module(library(lists), [member/2, reverse/2, append/3]).
:- use_module(library(apply), [include/3, foldl/4, partition/4, maplist/3]).
:- use_module(library(solution_sequences), [limit/2]).
:- use_module(library(rbtrees), [rb_new/1, rb_insert_new/4, rb_lookup/3,
                                 rb_update/5, rb_update/4, rb_map/3,
                                 rb_keys/2, rb_visit/2]).

/** <module> Unfolding literals of fact predicates

A body-only variable becomes a quantifier in the dual, and a quantifier is
the expensive part of proving a negation.  `dualise.pl:var_domain/4` already
bounds a variable a positive literal of a *fact predicate* binds — a
predicate defined by ground facts alone (`compile.pl:fact_predicates/2`) —
so the quantifier enumerates those facts rather than searching the
universe.  But the enumeration happens **every time the dual is called**,
and where the program is compiled for the data it runs on, the answer is
already known at compile time.

So before anything is dualised, a positive literal of a fact predicate is
**unfolded** against the facts it unifies with:

| facts that unify | the clause |
|---|---|
| none | is left as it is: its body is false, and the quantifier's generator is empty |
| one | has the literal bound to it |
| up to the limit | is split, one copy per fact |
| more | is left as it is, for the quantifier to enumerate |

```prolog
age(30).
old :- age(A), A > 65.        ==>   old :- age(30), 30 > 65.
```

The dual of the second has no body-only variable left, so it needs no
quantifier: it is `not age(30) ; 30 =< 65`, decided without a search.

## Why it is sound

Unfolding is one of the transformations that preserve stable models, and it
is simple here: a fact predicate has no rules, so under Clark's completion
`q(X)` *is* the disjunction of `X = a` over its facts `q(a)`.  A clause
whose body calls it is therefore equivalent to one clause per fact, with
the unification made — the same argument
`var_domain/4` makes for bounding the quantifier, applied once rather than
at every call.  The literal stays in the body, now ground, so the proof and
the justification still show the fact it used.

Only a literal **all of whose variables are body-only** is unfolded, so the
head is never instantiated.  A literal holding a head variable is left
alone, whatever its facts: binding the head would make `not p(b)` fail on
the head's disequality (`b \= a`) rather than on the literal, and the
justification of a negation would lose its reason — `not abnormal(tweety)`
is what explains why `penguin(tweety)` does not hold.  Such a literal
removes no quantifier either, as the head variable is bound when the dual
is called.

## Why it matters

This is what a large rule base had been doing by hand through the
`scasp_expand/4` hook, and it is the difference between 10 and 250 seconds
on its tests: the data of a request is facts, and every rule reads an
observation into a variable and compares it.  Unfolding does it for any program, and
unlike that hook it does not take the first matching fact where there are
several.

The flag `sscasp_unfold_facts` (default `true`) turns it off, and
`sscasp_unfold_limit` (default 8) is the most facts a clause is split over.
Both are read at compile time.

## And what is not unfolded goes first

Unfolding also **reorders**, without saying so: a literal unfolded against
its facts is resolved at compile time, so in effect it runs before
everything else in the body.  A literal with more facts than the limit
keeps the place the program gave it, and that difference is a cliff.  The
event calculus writes

```prolog
holdsAt(F2, T2) :- initiates(E, F1, T1), happens(E, T1), ...
```

and with `happens/2` unfolded the clause exists only for events that
happen; past the limit `initiates(overflow, spilling, T1)` is tried first,
asks `holdsAt(level(10), T1)` and recurses through every earlier cycle
before `happens(overflow, T1)` fails — 155,568 goals where unfolding needs
34 (`doc/design-next.md` §5e).

So after unfolding, facts_first/3 moves each literal of a fact predicate
that is still there in front of the positive literals of rule-defined
predicates before it — and no further: not past a built-in, which may bind
its arguments (`X is ...`, `=`) so that it is a lookup rather than an
enumeration, nor past a negation or another fact literal.  Only a literal
whose variables are all body-only moves, as for unfolding: one holding a
head variable is selective already when the head is bound, which is how a
dual calls it.  Conjunction commutes, so no answer changes; what changes
is the order of a clause's children in the justification, and which proof
is found first.  The flag `sscasp_facts_first` (default `true`) turns it
off.
*/

:- create_prolog_flag(sscasp_unfold_facts, true, [keep(true)]).
:- create_prolog_flag(sscasp_unfold_limit, 8,    [keep(true)]).
:- create_prolog_flag(sscasp_unfold_derived, true, [keep(true)]).
:- create_prolog_flag(sscasp_facts_first, true, [keep(true)]).
:- create_prolog_flag(sscasp_answer_limit, 256, [keep(true)]).

%!  fact_index(+FactPIs, @Clauses, +SldPIs, -Index) is det.
%
%   What unfolding needs to know about the program, computed once per
%   compile: the facts of each fact predicate, in program order, and
%   which predicates have clauses at all.  Index is `none` when the flag
%   `sscasp_unfold_facts` is off, and every predicate below then leaves
%   the program alone.
%
%   @arg FactPIs is `compile.pl:fact_predicates/2`'s result.
%   @arg Clauses are source clauses, untranslated, domain and abducible
%       clauses included.
%   @arg SldPIs are the predicates `:- sld` declares, which are defined
%       in Prolog rather than by clauses of the program.

:- det(fact_index/4).

fact_index(FactPIs, Clauses, SldPIs, Index) :-
    current_prolog_flag(sscasp_unfold_facts, true),
    !,
    current_prolog_flag(sscasp_unfold_limit, Limit),
    rb_new(Empty),
    rb_new(Empty2),
    foldl(add_pi, FactPIs, Empty, Facts0),
    foldl(add_fact, Clauses, Facts0, Facts1),
    rb_map(Facts1, reverse, Facts),
    foldl(add_defined, Clauses, Empty2, Defined0),
    foldl(add_pi, SldPIs, Defined0, Defined),
    (   current_prolog_flag(sscasp_unfold_derived, true)
    ->  answer_sets(Clauses, SldPIs, Answers)
    ;   rb_map(Facts, fact_answers, Answers)
    ),
    Index = index(Facts, Answers, Defined, Limit).
fact_index(_, _, _, none).

fact_answers(Facts, answers(Facts, [])).

add_pi(PI, Tree0, Tree) :-
    (   rb_insert_new(Tree0, PI, [], Tree)
    ->  true
    ;   Tree = Tree0
    ).

add_fact(Clause, Index0, Index) :-
    Clause \= (_ :- _),
    callable(Clause),
    functor(Clause, Name, Arity),
    rb_update(Index0, Name/Arity, Facts, [Clause|Facts], Index),
    !.
add_fact(_, Index, Index).

add_defined(Clause, Tree0, Tree) :-
    clause_parts(Clause, Head, _),
    callable(Head),
    functor(Head, Name, Arity),
    add_pi(Name/Arity, Tree0, Tree).

%!  unfold_facts(+Index, @Clauses0, -Clauses) is det.
%
%   Unfold the positive literals of fact predicates in the bodies of
%   Clauses0.  The facts themselves come out unchanged.

:- det(unfold_facts/3).

unfold_facts(none, Clauses0, Clauses) =>
    Clauses = Clauses0.
unfold_facts(Index, Clauses0, Clauses) =>
    foldl(unfold_clause(Index), Clauses0, Clauses1, []),
    facts_first(Index, Clauses1, Clauses).

%!  facts_first(+Index, @Clauses0, -Clauses) is det.
%
%   Move each remaining literal of a fact predicate in front of the
%   positive literals of rule-defined predicates that precede it.  See
%   "And what is not unfolded goes first" above.

:- det(facts_first/3).

facts_first(Index, Clauses0, Clauses) :-
    (   current_prolog_flag(sscasp_facts_first, true)
    ->  maplist(facts_first_clause(Index), Clauses0, Clauses)
    ;   Clauses = Clauses0
    ).

facts_first_clause(Index, Clause0, Clause) :-
    clause_parts(Clause0, Head, Body0),
    Body0 \== true,
    body_list(Body0, Goals0),
    foldl(place_goal(Index, Head), Goals0, [], Reversed),
    reverse(Reversed, Goals),
    Goals \== Goals0,
    !,
    list_body(Goals, Body),
    Clause = (Head :- Body).
facts_first_clause(_, Clause, Clause).

%   Done keeps the goals placed so far, last first.  A movable fact
%   literal is put in front of the run of rule literals at the end of it.

place_goal(Index, Head, Goal, Done0, Done) :-
    (   movable_fact(Index, Head, Goal)
    ->  rule_run(Index, Done0, Run, Before),
        append(Run, [Goal|Before], Done)
    ;   Done = [Goal|Done0]
    ).

rule_run(Index, [Goal|Goals], [Goal|Run], Before) :-
    rule_literal(Index, Goal),
    !,
    rule_run(Index, Goals, Run, Before).
rule_run(_, Goals, [], Goals).

movable_fact(index(Facts, _, _, _), Head, Goal) :-
    user_literal(Goal),
    functor(Goal, Name, Arity),
    rb_lookup(Name/Arity, _, Facts),
    body_only(Goal, Head).

rule_literal(index(Facts, _, _, _), Goal) :-
    user_literal(Goal),
    functor(Goal, Name, Arity),
    \+ rb_lookup(Name/Arity, _, Facts).

%!  unfold_denials(+Index, +Bodies0, -Bodies) is det.
%
%   The same for the bodies of global constraints.  A denial is a clause
%   without a head, so all its variables are body-only; splitting it is
%   denying each case, which is what denying their disjunction says.

:- det(unfold_denials/3).

unfold_denials(none, Bodies0, Bodies) =>
    Bodies = Bodies0.
unfold_denials(Index, Bodies0, Bodies) =>
    foldl(unfold_denial(Index), Bodies0, Bodies, []).

unfold_denial(Index, Body0, Bodies, Tail) :-
    unfold_clause(Index, ('$denial' :- Body0), Clauses, []),
    denial_bodies(Clauses, Bodies, Tail).

denial_bodies([], Bodies, Tail) =>
    Bodies = Tail.
denial_bodies([('$denial' :- Body)|T], Bodies, Tail) =>
    Bodies = [Body|Bodies1],
    denial_bodies(T, Bodies1, Tail).

%!  no_fact(+Index, @Literal) is semidet.
%
%   True if Literal is a positive literal that **no clause can prove**:
%   its predicate is defined by facts and none unifies with it, or it is
%   not defined at all, which the compiler closes as false
%   (`compile.pl:close_program/2`).  A clause with such a literal never
%   holds, so its dual holds for every value of its variables and needs
%   no quantifier: `dualise.pl:clause_dual/7` makes it `not Literal`.
%
%   That is the case unfolding leaves: an observation the data does not
%   have.  Rules that read one into a variable and compare it made every
%   `not rule(R)` quantify over the values of every observation that is
%   not there.

no_fact(index(Facts, _, Defined, _), Literal) :-
    user_literal(Literal),
    functor(Literal, Name, Arity),
    (   rb_lookup(Name/Arity, All, Facts)
    ->  \+ ( member(Fact, All),
              unifies(Literal, Fact)
            )
    ;   \+ rb_lookup(Name/Arity, _, Defined)
    ).

%   A call to a predicate of the program: not a built-in, not a
%   negation and not one of the constructs a body may hold.

user_literal(Literal) :-
    callable(Literal),
    \+ builtin(Literal),
    functor(Literal, Name, Arity),
    \+ construct(Name, Arity).

construct(not, 1).
construct(-, 1).
construct(\+, 1).
construct(',', 2).
construct(;, 2).
construct(->, 2).
construct(*->, 2).
construct(forall, 2).
construct(for_domain, 3).
construct(findall, 3).
construct(abducible, 1).
construct('$domain_check', 3).

%!  unfold_clause(+Index, @Clause, -Clauses, ?Tail) is det.
%
%   Clauses-Tail is Clause unfolded: itself if there is nothing to
%   unfold, one clause if every literal it unfolds has one fact, and a
%   copy per combination of facts where it is split.

unfold_clause(Index, Clause, Clauses, Tail) :-
    clause_parts(Clause, Head, Body),
    Body \== true,
    body_list(Body, Goals),
    candidate(Index, Head, Goals, _, _),
    !,
    findall(Head-Goals, unfolded(Index, Head, Goals), Pairs),
    unfolded_clauses(Pairs, Clauses, Tail).
unfold_clause(_, Clause, [Clause|Tail], Tail).

unfolded_clauses([], Clauses, Tail) =>
    Clauses = Tail.
unfolded_clauses([Head-Goals|T], Clauses, Tail) =>
    list_body(Goals, Body),
    Clauses = [(Head :- Body)|Clauses1],
    unfolded_clauses(T, Clauses1, Tail).

%!  unfolded(+Index, ?Head, ?Goals) is nondet.
%
%   Bind Goals to each way of unfolding them.  The literal with the
%   fewest facts goes first, whatever its position: one with a single
%   fact cannot split the clause, and what it binds may narrow a literal
%   that would have.  Every step grounds a literal, which is what ends
%   it.

unfolded(Index, Head, Goals) :-
    (   candidate(Index, Head, Goals, Literal, Facts)
    ->  member(Literal, Facts),
        unfolded(Index, Head, Goals)
    ;   true
    ).

candidate(Index, Head, Goals, Literal, Facts) :-
    findall(N-(Literal0-Facts0),
            candidate_literal(Index, Head, Goals, Literal0, Facts0, N),
            Pairs0),
    Pairs0 \== [],
    keysort(Pairs0, [_-(Literal1-Facts1)|_]),
    first_literal(Goals, Literal1, Literal),
    Facts = Facts1.

%   findall/3 copies, so find the literal in Goals that its copy was
%   taken from: the first with the same shape, which is the one
%   candidate_literal/6 found first too.

first_literal([Goal|Goals], Copy, Literal) :-
    (   Goal =@= Copy
    ->  Literal = Goal
    ;   first_literal(Goals, Copy, Literal)
    ).

candidate_literal(index(_, Answers, _, Limit), Head, Goals, Literal, Facts, N) :-
    member(Literal, Goals),
    fact_literal(Literal, Answers, answers(All, Open)),
    \+ ground(Literal),
    \+ ( member(Pattern, Open),
          unifies(Literal, Pattern)
        ),
    include(unifies(Literal), All, Facts),
    length(Facts, N),
    N > 0,
    N =< Limit,
    body_only(Literal, Head).

fact_literal(Literal, Index, Facts) :-
    callable(Literal),
    Literal \= not(_),
    Literal \= -(_),
    \+ builtin(Literal),
    functor(Literal, Name, Arity),
    rb_lookup(Name/Arity, Facts, Index).

unifies(Literal, Fact) :-
    \+ Literal \= Fact.

%   Only a literal **all** of whose variables are body-only: binding it
%   leaves the head as it is.

body_only(Literal, Head) :-
    term_variables(Head, HeadVars),
    term_variables(Literal, Vars),
    \+ ( member(Var, Vars),
          member(HV, HeadVars),
          HV == Var
        ).


		 /*******************************
		 *      DERIVED PREDICATES	*
		 *******************************/

%!  answer_sets(@Clauses, +SldPIs, -Answers) is det.
%
%   An **over-approximation** of the atoms each predicate can prove,
%   where it is finite: an rb-tree from `Name/Arity` to
%   `answers(Ground, Open)`, Ground a sorted list of ground heads and
%   Open the heads a clause may prove with variables in them.  A literal
%   that unifies with none of Open can only be proved for the answers in
%   Ground that unify with it — `message(state, set, now, Dose)` although
%   another clause of message/4 has a head with variables, which no
%   literal about a dose unifies with.  A predicate that is not in the
%   tree has too many answers to list.  This is how an ASP
%   grounder computes its domains, and for the same reason: a literal of
%   a predicate with a known answer set can be unfolded against it like
%   a literal of a fact predicate — false outside the set, so a clause
%   calling it is the disjunction of its instances for the answers that
%   unify (unfold_clause/4).  A predicate `dose(D, mg)` derived from the
%   current dose of a drug has one answer, and knowing it binds D in every
%   rule that compares it.
%
%   The approximation is the least fixpoint of the program read
%   bottom-up with **every negated literal taken as true**: a stable
%   model is the least model of its reduct, which drops negated literals
%   or whole clauses, so every atom of it is in the least model of the
%   program without them.  Built-ins are evaluated where their arguments
%   are ground and taken as true otherwise, and so is any construct —
%   quantifiers, findall/3, abducibles, classical negation, `:- sld`
%   predicates, whose answers are not in the program: each only admits
%   more.  A predicate with no clauses at all has no answers, the
%   compiler closing it as false.
%
%   Used only to **bind** literals, not to decide that one has no
%   answer (no_fact/2 reads facts only): a clause with a literal whose
%   derived predicate has no answers still has its body proved false,
%   because that proof is the explanation why the rule does not apply.
%
%   The flag `sscasp_unfold_derived` (default `true`) turns this off,
%   and `sscasp_answer_limit` (default 256) is the largest answer set
%   that is kept.

answer_sets(Clauses, SldPIs, Answers) :-
    current_prolog_flag(sscasp_answer_limit, Cap),
    rb_new(Groups0),
    foldl(add_clause, Clauses, Groups0, Groups1),
    rb_map(Groups1, reverse, Groups),
    rb_new(Sld0),
    foldl(add_pi, SldPIs, Sld0, Sld),
    rb_keys(Groups, PIs),
    rb_new(Status00),
    foldl(initial_status, PIs, Status00, Status0),
    Env = env(Groups, Sld, Cap),
    fixpoint(PIs, Env, 0, Status0, Status),
    rb_visit(Status, Pairs),
    rb_new(Answers0),
    foldl(known_answers, Pairs, Answers0, Answers).

add_clause(Clause, Groups0, Groups) :-
    clause_parts(Clause, Head, _),
    callable(Head),
    functor(Head, Name, Arity),
    (   rb_lookup(Name/Arity, Clauses, Groups0)
    ->  rb_update(Groups0, Name/Arity, [Clause|Clauses], Groups)
    ;   rb_insert_new(Groups0, Name/Arity, [Clause], Groups)
    ).

initial_status(PI, Status0, Status) :-
    rb_insert_new(Status0, PI, known([], []), Status).

known_answers(PI-known(List, Open), Answers0, Answers) =>
    rb_insert_new(Answers0, PI, answers(List, Open), Answers).
known_answers(_, Answers0, Answers) =>
    Answers = Answers0.

%!  fixpoint(+PIs, +Env, +Round, +Status0, -Status) is det.
%
%   Recompute every predicate until nothing changes.  Each predicate
%   only grows, from no answers through larger sets to `unknown`, so
%   this reaches the least fixpoint; a recursion that keeps finding new
%   answers after 50 rounds has what it changed set to `unknown`, which
%   cannot change again.  Status is updated in place, so a predicate
%   sees what the ones before it found in the same round.

fixpoint(PIs, Env, Round, Status0, Status) :-
    Force = force(false),
    (   Round >= 50
    ->  nb_setarg(1, Force, true)
    ;   true
    ),
    foldl(recompute(Env, Force), PIs, Status0-false, Status1-Changed),
    (   Changed == true
    ->  Round1 is Round+1,
        fixpoint(PIs, Env, Round1, Status1, Status)
    ;   Status = Status1
    ).

recompute(Env, Force, PI, Status0-Changed0, Status-Changed) :-
    rb_lookup(PI, Old, Status0),
    (   Old == unknown
    ->  Status = Status0,
        Changed = Changed0
    ;   predicate_answers(Env, Status0, PI, New0),
        (   New0 =@= Old
        ->  Status = Status0,
            Changed = Changed0
        ;   (   arg(1, Force, true)
            ->  New = unknown
            ;   New = New0
            ),
            rb_update(Status0, PI, New, Status),
            Changed = true
        )
    ).

%!  predicate_answers(+Env, +Status, +PI, -Answers) is det.
%
%   Answers is `known(SortedGroundHeads, OpenHeads)` or `unknown`.  The
%   search is limited: a join that produces more solutions than a few
%   times the answer limit is `unknown` rather than a long wait, and so
%   are more than 32 distinct heads with variables.

predicate_answers(env(Groups, Sld, Cap), Status, PI, Answers) :-
    rb_lookup(PI, Clauses, Groups),
    Max is Cap*4+1,
    findall(Head,
            limit(Max, ( member(Clause, Clauses),
                         clause_answer(Status, Sld, Clause, Head) )),
            Heads0),
    length(Heads0, Len),
    (   Len >= Max
    ->  Answers = unknown
    ;   partition(ground, Heads0, Ground0, Open0),
        sort(Ground0, Ground),
        foldl(add_variant, Open0, [], Open1),
        reverse(Open1, Open),
        length(Ground, N),
        length(Open, NOpen),
        (   ( N > Cap ; NOpen > 32 )
        ->  Answers = unknown
        ;   Answers = known(Ground, Open)
        )
    ).

add_variant(Term, Set, Set) :-
    member(T, Set),
    T =@= Term,
    !.
add_variant(Term, Set, [Term|Set]).

clause_answer(Status, Sld, Clause, Head) :-
    clause_parts(Clause, Head, Body),
    body_list(Body, Goals),
    join(Goals, Status, Sld),
    builtins(Goals).

%   Pass one: the positive literals, joined with their answers, and
%   unification, which may connect them.  A literal whose predicate has
%   no known answers is taken as true.

join([], _, _).
join([Goal|Goals], Status, Sld) :-
    join_goal(Goal, Status, Sld),
    join(Goals, Status, Sld).

join_goal(Goal, _, _), var(Goal) =>
    true.
join_goal(A = B, _, _) =>
    A = B.
join_goal(Goal, Status, Sld), user_literal(Goal) =>
    functor(Goal, Name, Arity),
    (   rb_lookup(Name/Arity, State, Status)
    ->  (   State = known(Ground, Open)
        ->  (   member(Goal, Ground)
            ;   member(Pattern, Open),
                copy_term(Pattern, Goal)
            )
        ;   true
        )
    ;   rb_lookup(Name/Arity, _, Sld)
    ->  true
    ;   fail                            % undefined: no answers
    ).
join_goal(_, _, _) =>
    true.

%   Pass two: the other built-ins, where their arguments are ground.  An
%   error, `unknown > 2` say, decides nothing and is taken as true.

builtins([]).
builtins([Goal|Goals]) :-
    builtin_goal(Goal),
    builtins(Goals).

builtin_goal(Goal), var(Goal) =>
    true.
builtin_goal(X is Expr), ground(Expr) =>
    (   catch(V is Expr, _, fail)
    ->  (   var(X)
        ->  X = V
        ;   number(X)
        ->  X =:= V
        ;   true
        )
    ;   true
    ).
builtin_goal(A \= B), ground(A-B) =>
    A \= B.
builtin_goal(Goal), comparison(Goal), ground(Goal) =>
    catch(Goal, _, true).
builtin_goal(_) =>
    true.

comparison(_ < _).
comparison(_ > _).
comparison(_ =< _).
comparison(_ >= _).
comparison(_ =:= _).
comparison(_ =\= _).
