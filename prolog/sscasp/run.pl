:- module(sscasp_run,
          [ sscasp/3,                   % +Terms, ?Query, -Model
            sscasp_program/2            % +Terms, -Clauses
          ]).
:- use_module(ops).
:- use_module(compile,  [compile_program/3]).
:- use_module(emit,     [runnable_program/2]).
:- use_module(dualise,  [translate_goal/3]).
:- use_module(solve,    [call_goal/4, model/2]).
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
*/

%!  sscasp(+Terms, ?Query, -Model) is nondet.
%
%   Model is a partial stable model in which Query holds.  Terms is the
%   program as a list, Query a literal or a conjunction of them.

sscasp(Terms, Query, Model) :-
    compile_program(scasp, Terms, Program),
    runnable_program(Program, Clauses),
    in_temporary_module(
        Module,
        load_program(Clauses, Module),
        answer(Module, Program, Query, Model)).

%!  sscasp_program(+Terms, -Clauses) is det.
%
%   The clauses `sscasp/3` would run.  For looking at, and for a test
%   that wants to name one.

sscasp_program(Terms, Clauses) :-
    compile_program(scasp, Terms, Program),
    runnable_program(Program, Clauses).

load_program(Clauses, Module) :-
    forall(member(Clause, Clauses),
           assertz(Module:Clause)).

%!  answer(+Module, +Program, ?Query, -Model) is nondet.

answer(Module, Program, Query, Model) :-
    query_goals(Query, Goals),
    Nmr = Program.nmr.goal,             % bound here: dict notation expands
    solve_goals(Module, Goals, [], [], Proof0),  % into the goal it appears in,
    call_goal(Module:Nmr, [], Proof0, Proof),    % and a meta-argument is one
    model(Proof, Model).

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

solve_goals(_, [], _, Proof, Proof).
solve_goals(Module, [Goal|Goals], Anc, Proof0, Proof) :-
    call_goal(Module:Goal, Anc, Proof0, Proof1),
    solve_goals(Module, Goals, Anc, Proof1, Proof).
