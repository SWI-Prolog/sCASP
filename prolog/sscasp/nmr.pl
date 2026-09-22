:- module(sscasp_nmr,
          [ nmr_check/4,                % +Module, +Annotations, +Sources, -Nmr
            denial/2                    % +Source, -Denial
          ]).
:- use_module(ops).
:- use_module(dualise, [predicate_duals/6]).
:- use_module(mangle,  [mangle/2]).
:- use_module(library(lists), [append/3, numlist/3, member/2, memberchk/2]).
:- use_module(library(apply), [maplist/3, maplist/4, maplist/5]).

/** <module> The NMR check

A program's stable models must satisfy every rule read as an implication.
For most rules that is automatic — a model that did not satisfy one would not
be a model — but two kinds have to be *checked*, because a goal-directed
solver proves only what the query reaches:

  - a **global constraint** `:- B`, which is a rule nothing can prove;
  - a rule on an **odd loop over negation**, which the query may reach by a
    route that never tests it;
  - the **consistency** of a classically negated predicate, which is a
    constraint the language imposes rather than the program.

All say the same thing in the end: something must not hold.  So each becomes
a **denial**, and the NMR check is the conjunction of their negations.

    :- B.                   denial   B
    H :- B.     (on a loop) denial   B, not H
    p and -p                denial   p(X), -p(X)

The second reads "the body holds and the head does not", which is exactly the
rule being violated.

A **source** says which of the three a denial came from — `constraint(B)`,
`olon(Clause)` or `consistency(PI, B)` — and that is what the caller passes
in and what comes back out described.  Adding a fourth kind is adding a
clause to `denial/2`.

## Every sub-check has arity zero

A denial's variables are all existentially quantified — "there is an X for
which the rule is violated" — so its negation quantifies them universally.
Writing the sub-check as a predicate of **no arguments**

    m$chk.1$$0 :- B(X, Y), not H(X).

makes X and Y body-only variables of a zero-arity head, and the dual
generator already knows what to do with those: it quantifies them, bounding
each by its domain where one is declared.  Nothing here has to handle
quantification, and the `unbounded` report covers the NMR check for free.

The sub-check the solver calls is the dual, `m$dualchk.1$$0`, which succeeds
exactly when no X and Y violate the rule.  The positive predicate is never
called — the duals are generated *from* it, not through it — so it is not
emitted.  The denial it would have held is in the result instead, where a
listing or a message can reach it without a clause in the running program
being the one term in it that is not compiled.

## The names

A sub-check derives from no user predicate — it is built from a rule, and for
a global constraint from a rule with no head at all — so it takes the empty
name field `mangle.pl` reserves for program-level predicates.  Which rule a
sub-check came from is in the result's `sources` instead, where a report can
use it; putting it in the name would mean naming a zero-arity predicate after
a predicate of another arity.
*/

%!  nmr_check(+Module, +Annotations, +Sources, -Nmr) is det.
%
%   Nmr is
%
%       nmr{ goal:    'm$nmr$$0',       % what the solver calls
%            clauses: [...],            % the check and the sub-check duals
%            checks:  [check(1, p/0, not p), ...] }
%
%   A `check(Index, Source, Denial)` says what the Index-th sub-check is
%   for: Source is the predicate whose rule is on a loop, or the atom
%   `constraint`, and Denial is the body that must not hold.
%
%   A program with nothing to check still gets one — a check that
%   succeeds — so that the solver has one goal to call rather than a case
%   to test.

nmr_check(Module, Annotations, Sources, Nmr) :-
    length(Sources, Count),
    numlist_or_empty(Count, Indices),
    maplist(sub_check(Module, Annotations), Indices, Sources, Clauses, Calls),
    append(Clauses, Flat),
    mangle(prog(Module, nmr, 0), Goal),
    check_clause(Goal, Calls, Check),
    maplist(check_description, Indices, Sources, Checks),
    Nmr = nmr{ goal:    Goal,
               clauses: [Check|Flat],
               checks:  Checks }.

check_description(Index, Source, check(Index, What, Denial)) :-
    denial(Source, Denial),
    described(Source, What).

described(constraint(_), constraint).
described(consistency(PI, _), consistency(PI)).
described(olon(Clause), PI) :-
    clause_head(Clause, Head),
    head_pi(Head, PI).

numlist_or_empty(0, []) :- !.
numlist_or_empty(Count, Indices) :- numlist(1, Count, Indices).

check_clause(Goal, [], Goal) :- !.
check_clause(Goal, Calls, (Goal :- Body)) :-
    comma_list(Calls, Body).

comma_list([Goal], Goal) :- !.
comma_list([Goal|Goals], (Goal, Rest)) :-
    comma_list(Goals, Rest).

%!  sub_check(+Module, +Annotations, +Index, +Source, -Clauses, -Call) is det.
%
%   Clauses are the duals of the Index-th sub-check; Call is what the NMR
%   check calls, which is the outermost of them.

sub_check(Module, Annotations, Index, Source, Duals, Call) :-
    denial(Source, Denial),
    mangle(prog(Module, chk(Index), 0), Name),
    predicate_duals(Module, Annotations, chk(Index), ''/0,
                    [(Name :- Denial)], Duals),
    mangle(prog(Module, dualchk(Index), 0), Call).

%!  denial(+Source, -Denial) is det.
%
%   The body that must not hold.  A global constraint is one already; an
%   OLON rule is violated when its body holds and its head does not.
%
%   A head already denied in the body is not denied twice — `p :- B, not
%   p` is its own denial, and the duplicate would only make the dual
%   generator produce a clause that cannot succeed.

denial(constraint(Body), Body) :- !.
denial(consistency(_, Body), Body) :- !.
denial(olon(Clause), Denial) :-
    clause_head(Clause, Head),
    clause_body(Clause, Body),
    (   body_denies(Body, Head)
    ->  Denial = Body
    ;   Body == true
    ->  Denial = not(Head)
    ;   Denial = (Body, not(Head))
    ).

body_denies(Body, Head) :-
    body_literal(Body, not(Literal)),
    Literal == Head,
    !.

body_literal((A, B), L) :- !, (body_literal(A, L) ; body_literal(B, L)).
body_literal(L, L).

clause_head((Head :- _), Head) :- !.
clause_head(Fact, Fact).

clause_body((_ :- Body), Body) :- !.
clause_body(_, true).

head_pi(-(Head), Name/Arity) :-
    !,
    head_pi(Head, Name/Arity).
head_pi(Head, Name/Arity) :-
    (   atom(Head)
    ->  Name = Head, Arity = 0
    ;   functor(Head, Name, Arity)
    ).
