:- module(sscasp_run,
          [ sscasp/3,                   % +Terms, ?Query, -Model
            sscasp/4,                   % +Terms, ?Query, -Model, -Justification
            sscasp_program/2            % +Terms, -Clauses
          ]).
:- use_module(ops).
:- use_module(compile,  [compile_program/3]).
:- use_module(emit,     [runnable_program/3, body/6]).
:- use_module(dualise,  [translate_goal/3]).
:- use_module(solve,    [call_goal/4, model/2, justification/2,
                         children/3, empty_proof/1]).
:- use_module(library(modules), [in_temporary_module/3]).
:- use_module(library(lists), [member/2]).

/** <module> Answering a query

Compile a program, make it runnable, put it in a module of its own and ask it
something:

    ?- sscasp([ (flies(X) :- bird(X), not penguin(X)),
                bird(tweety) ],
              flies(tweety), Model).
    Model = [bird(tweety), flies(tweety), not penguin(tweety)].

The module is temporary because the mangled names are global to it: two
programs compiled with the same module name would otherwise be the same
predicates.  Nothing is asserted into the caller, so two runs in one process
do not interfere — which is `doc/bugs.md` #5, not reproduced.

## The query runs the NMR check

A query is the literals asked for **followed by** the NMR check, sharing one
proof.  That order matters: the check runs against what the query proved, so a
constraint the query's own answer violates is caught, and the literals the
check needs are part of the same model.

They are the same proof but not the same question, so the justification says
which is which: the query's literals at the top, and everything the check
needed under one `nmr` node.
*/

%!  sscasp(+Terms, ?Query, -Model) is nondet.
%
%   Model is a partial stable model in which Query holds.  Terms is the
%   program as a list, Query a literal or a conjunction of them.

sscasp(Terms, Query, Model) :-
    sscasp(Terms, Query, Model, _).

%!  sscasp(+Terms, ?Query, -Model, -Justification) is nondet.
%
%   Justification is why, as a forest of `Literal-Children`.  It is the
%   same structure the solver threads, so asking for it costs nothing
%   that answering the query did not already cost.

sscasp(Terms, Query, Model, Justification) :-
    compile_program(scasp, Terms, Program),
    in_temporary_module(
        Module,
        load_program(Module, Program),
        answer(Module, Program, Query, Model, Justification)).

%!  sscasp_program(+Terms, -Clauses) is det.
%
%   The clauses `sscasp/3` would run, as they would look in a module
%   called `program`.  For looking at, and for a test that wants to name
%   one.

sscasp_program(Terms, Clauses) :-
    compile_program(scasp, Terms, Program),
    runnable_program(program, Program, Clauses).

%   Emitted here rather than before the module exists, because the goals
%   a quantifier holds as terms have to say which module to call them in
%   and assertz/1 will not add that for us.

load_program(Module, Program) :-
    runnable_program(Module, Program, Clauses),
    forall(member(Clause, Clauses),
           assertz(Module:Clause)).

%!  answer(+Module, +Program, ?Query, -Model) is nondet.

answer(Module, Program, Query, Model, Justification) :-
    query_goals(Query, Goals),
    Nmr = Program.nmr.goal,             % bound here: dict notation expands
    empty_proof(Empty),
    solve_goals(Module, Goals, [], Empty, Proof0),  % into the goal it appears in,
    call_goal(Module:Nmr, [], Proof0, Proof),    % and a meta-argument is one
    model(Proof, Model),
    children(Proof, Proof0, Checked),
    justification(Proof0, Asked),
    justification(Checked, Because),
    append(Asked, [nmr-Because], Justification).

query_goals(Query, Goals) :-
    conjunction(Query, Literals),
    findall(Goal,
            ( member(Literal, Literals),
              translate_goal(scasp, Literal, Goal)
            ),
            Goals0),
    share_variables(Literals, Goals0, Goals).

%   translate_goal/3 is called under findall/3, which copies, so the
%   goals come back with fresh variables and no longer share with the
%   query.  Re-unifying the arguments puts that back — the query must be
%   bound by its own answer.

share_variables([], [], []).
share_variables([Literal|Literals], [Goal0|Goals0], [Goal|Goals]) :-
    translate_goal(scasp, Literal, Goal),
    Goal = Goal0,
    share_variables(Literals, Goals0, Goals).

conjunction((A, B), Literals) :-
    !,
    conjunction(A, As),
    conjunction(B, Bs),
    append(As, Bs, Literals).
conjunction(Literal, [Literal]).

%!  solve_goals(+Module, +Goals, +Anc, +Proof0, -Proof) is nondet.
%
%   The query is a clause body and is made runnable by the same pass, so
%   that whatever may appear in a body may appear in a query.  Calling
%   each literal instead works until one of them is a **constraint**:
%   `?- T #< 40, holds(T, ...)` is an ordinary s(CASP) query, and `#<`
%   is not a predicate to call but something to post.

solve_goals(Module, Goals, Anc, Proof0, Proof) :-
    body_conjunction(Goals, Body),
    body(Body, Module, Anc, Proof0, Proof, Runnable),
    call(Module:Runnable).

body_conjunction([], true).
body_conjunction([Goal|Goals], Body) :-
    (   Goals == []
    ->  Body = Goal
    ;   Body = (Goal, Rest),
        body_conjunction(Goals, Rest)
    ).
