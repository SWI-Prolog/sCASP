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

:- module(sscasp_run,
          [ scasp/3,                   % +Terms, ?Query, -Model
            scasp/4,                   % +Terms, ?Query, -Model, -Justification
            scasp_run/3,               % +Terms, ?Query, +Options
            scasp_loaded/3,            % +Terms, -Loaded, :Goal
            scasp_loaded/4,            % +Terms, -Loaded, :Goal, +Options
            scasp_ask/4,               % +Loaded, ?Query, -Model, -Justification
            scasp_program/2            % +Terms, -Clauses
          ]).
:- use_module(ops).
:- use_module(compile,  [compile_program/3, guarded_query/3]).
:- use_module(emit,     [runnable_program/3, body/6]).
:- use_module(dualise,  [translate_goal/3]).
:- use_module(solve,    [call_goal/4, model/2, justification/2,
                         children/3, empty_proof/1, table_begin/0]).
:- use_module(herbrand, [normalise/1]).
:- use_module(mangle,   [demangle/2]).
:- use_module(library(modules), [in_temporary_module/3]).
:- use_module(library(lists), [member/2]).
:- use_module(library(apply), [include/3]).
:- use_module(library(error), [instantiation_error/1, must_be/2]).
:- use_module(library(option), [option/2]).
:- use_module(library(gensym), [gensym/2]).

%   Before the first clause, with the imports, because a meta-predicate
%   mode is part of the module's **signature**: a reader — or a tool —
%   taking the interface from the head of the file stops at the first
%   clause, and a declaration below that point changes how callers must
%   pass an argument without saying so where the interface is stated.

:- meta_predicate
    scasp_loaded(+, -, 0),
    scasp_loaded(+, -, 0, +).

/** <module> Answering a query

Compile a program, make it runnable, put it in a module of its own and ask it
something:

    ?- scasp([ (flies(X) :- bird(X), not penguin(X)),
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

%!  scasp(+Terms, ?Query, -Model) is nondet.
%
%   Model is a partial stable model in which Query holds.  Terms is the
%   program as a list, Query a literal or a conjunction of them.

scasp(Terms, Query, Model) :-
    scasp(Terms, Query, Model, _).

%!  scasp(+Terms, ?Query, -Model, -Justification) is nondet.
%
%   Justification is why, as a forest of `Literal-Children`.  It is the
%   same structure the solver threads, so asking for it costs nothing
%   that answering the query did not already cost.

scasp(Terms, Query, Model, Justification) :-
    scasp_run(Terms, Query, [model(Model), tree(Justification)]).

%!  scasp_run(+Terms, ?Query, +Options) is nondet.
%
%   scasp/3,4 with options, which is what `scasp/2` over a Prolog
%   module (`dyncall.pl`) hands its collected terms to.  Options:
%
%     - model(-Model)
%     - tree(-Justification)
%       The answer, as scasp/4 gives it.
%     - module(?Module)
%       Load the compiled program into Module and **leave it there**,
%       rather than in a temporary module destroyed after the query.
%       Unbound, a fresh module is made and Module is bound to it;
%       bound, the predicates a previous run left in it are removed
%       first.  For debugging: the profiler, and anything else that
%       remembers predicates, will not keep track of a module that may
%       vanish, and the compiled program can be looked at with
%       `listing/1` afterwards.  Only the mangled predicates are ever
%       removed, so naming a module that holds anything else is safe.

scasp_run(Terms, Query, Options) :-
    scasp_loaded(Terms, Loaded,
                 scasp_ask(Loaded, Query, Model, Justification),
                 Options),
    ignore(option(model(Model), Options)),
    ignore(option(tree(Justification), Options)).

%!  scasp_loaded(+Terms, -Loaded, :Goal) is nondet.
%!  scasp_ask(+Loaded, ?Query, -Model, -Justification) is nondet.
%
%   Compile and load **once**, then ask any number of questions.  Goal
%   is called with Loaded bound to a handle `scasp_ask/4` takes, and the
%   module is destroyed when Goal is done with — so the handle is good
%   for exactly as long as Goal runs and no longer.
%
%   `scasp/3,4` is the one-question case and is written as this.  The
%   split is for a caller that asks many: a toplevel, where compiling
%   the program again per query is the whole of the cost, and anything
%   serving a loaded program to more than one request.
%
%   Splitting it is all the lifecycle there is.  A `scasp_load/2` that
%   handed back a module the caller had to remember to free would be the
%   other design, and `doc/bugs.md` #5 is what that costs when the
%   caller forgets.

scasp_loaded(Terms, Loaded, Goal) :-
    scasp_loaded(Terms, Loaded, Goal, []).

%!  scasp_loaded(+Terms, -Loaded, :Goal, +Options) is nondet.
%
%   As scasp_loaded/3.  Takes the option `module(?Module)` of
%   scasp_run/3, with which the program outlives Goal.

scasp_loaded(Terms, loaded(Module, Program), Goal, Options) :-
    compile_program(scasp, Terms, Program),
    (   option(module(Module), Options)
    ->  permanent_module(Module),
        load_program(Module, Program),
        call(Goal)
    ;   in_temporary_module(
            Module,
            load_program(Module, Program),
            Goal)
    ).

%!  permanent_module(?Module) is det.
%
%   Module, made up if unbound, with nothing left in it of a program
%   loaded there before.  A compiled program defines only mangled
%   names, so those are what is removed; whatever else the module
%   holds is not ours to touch.

permanent_module(Module) :-
    (   var(Module)
    ->  gensym(sscasp_program_, Module)
    ;   must_be(atom, Module)
    ),
    forall(( current_predicate(Module:Name/Arity),
             demangle(Name, _),
             functor(Head, Name, Arity),
             \+ predicate_property(Module:Head, imported_from(_))
           ),
           abolish(Module:Name/Arity)).

scasp_ask(loaded(Module, Program), Query, Model, Justification) :-
    answer(Module, Program, Query, Model, Justification).

%!  scasp_program(+Terms, -Clauses) is det.
%
%   The clauses `scasp/3` would run, as they would look in a module
%   called `program`.  For looking at, and for a test that wants to name
%   one.

scasp_program(Terms, Clauses) :-
    compile_program(scasp, Terms, Program),
    runnable_program(program, Program, Clauses).

%   Emitted here rather than before the module exists, because the goals
%   a quantifier holds as terms have to say which module to call them in
%   and assertz/1 will not add that for us.
%
%   The imports are what let `emit.pl` write `check(...)` rather than
%   `sscasp_solve:check(...)`: a temporary module imports nothing by
%   itself, so without them the emitted clauses would have to name the
%   module of every goal they call.  Every asserted head is mangled
%   (`scasp$plain$flies$1`), so nothing imported here can be shadowed.
%
%   `clpq` is among them because `library(clpr)` exports `{}/1` too and
%   the emitted comparisons mean the rational one; importing it per
%   module says which without qualifying each goal.

load_program(Module, Program) :-
    add_import_module(Module, sscasp_solve, start),
    add_import_module(Module, sscasp_herbrand, start),
    add_import_module(Module, clpq, start),
    runnable_program(Module, Program, Clauses),
    forall(member(Clause, Clauses),
           assertz(Module:Clause)).

%!  answer(+Module, +Program, ?Query, -Model) is nondet.

answer(Module, Program, Query, Model, Justification) :-
    guarded_query(Program, Query, Guarded),
    query_goals(Guarded, Goals),
    table_begin,
    Nmr = Program.nmr.goal,             % bound here: dict notation expands
    empty_proof(Empty),
    catch(solved(Module, Goals, Nmr, Empty, Proof0, Proof),
          Error,
          rethrown(Error)),
    model(Proof, Model0),
    normalise(Query-Model0),
    shown_model(Program.shown, Model0, Model),
    children(Proof, Proof0, Checked),
    justification(Proof0, Asked),
    justification(Checked, Because),
    append(Asked, [nmr-Because], Justification).

%!  shown_model(+Specs, +Model0, -Model) is det.
%
%   Model is Model0 with only what `#show` asks for, or all of it when
%   the program says nothing: `p/1` shows the literals `p(...)`,
%   `not p/1` shows `not p(...)`, `-p/1` shows `-p(...)` and
%   `not -p/1` shows `not -p(...)`.  The old implementation's
%   `model.pl:filter_shown/3`.  The justification is not filtered: it
%   says why, and a step left out would leave the reason incomplete.

shown_model([], Model0, Model) =>
    Model = Model0.
shown_model(Specs, Model0, Model) =>
    include(shown_literal(Specs), Model0, Model).

shown_literal(Specs, Literal) :-
    literal_spec(Literal, Spec),
    memberchk(Spec, Specs).

literal_spec(not(-(Goal)), Spec) =>
    functor(Goal, Name, Arity),
    Spec = not(-(Name/Arity)).
literal_spec(not(Goal), Spec) =>
    functor(Goal, Name, Arity),
    Spec = not(Name/Arity).
literal_spec(-(Goal), Spec) =>
    functor(Goal, Name, Arity),
    Spec = -(Name/Arity).
literal_spec(Goal, Spec) =>
    functor(Goal, Name, Arity),
    Spec = Name/Arity.

solved(Module, Goals, Nmr, Empty, Proof0, Proof) :-
    solve_goals(Module, Goals, [], Empty, Proof0),  % dict notation expands into
    call_goal(Module:Nmr, [], Proof0, Proof).       % the goal it appears in, and
                                                    % a meta-argument is one

%!  rethrown(+Error) is det.
%
%   A query naming a predicate the program does not have reaches the
%   solver as a call to a mangled name that was never emitted, and the
%   existence error says so in the compiler's language:
%
%       Unknown procedure: 'tmp-1-884':'scasp$plain$nosuchgoal$0'/3
%
%   Which is true and no use to whoever typed it.  The name is put back
%   into the caller's, and the error kind with it — `scasp_predicate` is
%   what `library(scasp)` raises for the same mistake
%   (`scasp/compile.pl:142`), so a caller catching one catches both.
%
%   Only a *mangled* name is rewritten.  Anything else the solver could
%   not find is a bug in this tree rather than a typo in a query, and
%   saying so in the compiler's language is then the right thing.

rethrown(Error) :-
    query_error(Error, Better),
    !,
    throw(Better).
rethrown(Error) :-
    throw(Error).

query_error(error(existence_error(procedure, PI), _),
            error(existence_error(scasp_predicate, Name/Arity), _)) :-
    unqualified(PI, Mangled/_EmittedArity),
    demangle(Mangled, pred(_Module, _Kind, Name, Arity)).

unqualified(_:PI, Plain) :-
    !,
    unqualified(PI, Plain).
unqualified(PI, PI).

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

%   An unbound query has no literals to ask for, and saying so beats
%   what it used to do: `(A, B)` unifies with a variable, so the query
%   grew a conjunct per step and the stack ran out.

conjunction(Query, _) :-
    var(Query),
    !,
    instantiation_error(Query).
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
