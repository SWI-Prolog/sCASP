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
          [ scasp/3,                   % @Terms, ?Query, -Model
            scasp/4,                   % @Terms, ?Query, -Model, -Justification
            scasp_run/3,               % @Terms, ?Query, +Options
            scasp_loaded/4,            % @Terms, -Loaded, :Goal, +Options
            scasp_ask/4,               % +Loaded, ?Query, -Model, -Justification
            scasp_program/2            % @Terms, -Clauses
          ]).
:- use_module(ops).
:- use_module(compile,  [compile_program/3, guarded_query/3]).
:- use_module(emit,     [runnable_program/3, body/6]).
:- use_module(dualise,  [translate_goal/3]).
:- use_module(solve,    [call_goal/4, model/2, justification/2,
                         children/3, empty_proof/1, table_begin/0]).
:- use_module(herbrand, [normalise/1, read_store/2]).
:- use_module(runtime,  []).
:- use_module(mangle,   [demangle/2, mangled/1]).
:- use_module(library(modules), [in_temporary_module/3]).
:- use_module(library(lists), [member/2, append/3]).
:- use_module(library(apply), [include/3, maplist/3]).
:- use_module(library(error), [instantiation_error/1, must_be/2]).
:- use_module(library(option), [option/2]).
:- use_module(library(gensym), [gensym/2]).
:- use_module(library(clpq), [dump/3, entailed/1, {}/1]).

%   Before the first clause, with the imports, because a meta-predicate
%   mode is part of the module's **signature**: a reader — or a tool —
%   taking the interface from the head of the file stops at the first
%   clause, and a declaration below that point changes how callers must
%   pass an argument without saying so where the interface is stated.

:- meta_predicate
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
needed under one `'$nmr'` node — a name no program can give a
predicate, so the node cannot be taken for a literal.
*/

%!  scasp(@Terms, ?Query, -Model) is nondet.
%
%   Model is a partial stable model in which Query holds.  Terms is the
%   program as a list, Query a literal or a conjunction of them.
%
%   @arg Terms is a list of clauses and facts, `(:- Body)` global
%       constraints and `(:- Directive)` annotations, as
%       compile_program/3 takes them.
%   @arg Model is a sorted list of the literals proved: `p(...)`,
%       `not p(...)`, `-p(...)` and `not -p(...)`.

scasp(Terms, Query, Model) :-
    scasp(Terms, Query, Model, _).

%!  scasp(@Terms, ?Query, -Model, -Justification) is nondet.
%
%   Justification is why, as a forest of `Literal-Children`.  It is the
%   same structure the solver threads, so asking for it costs nothing
%   that answering the query did not already cost.

scasp(Terms, Query, Model, Justification) :-
    scasp_run(Terms, Query, [model(Model), tree(Justification)]).

%!  scasp_run(@Terms, ?Query, +Options) is nondet.
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
%       removed, so the module's own predicates survive — but
%       `sscasp_runtime` is added to its import modules and stays
%       there, so name a module of its own.

scasp_run(Terms, Query, Options) :-
    scasp_loaded(Terms, Loaded,
                 scasp_ask(Loaded, Query, Model, Justification),
                 Options),
    ignore(option(model(Model), Options)),
    ignore(option(tree(Justification), Options)).

%!  scasp_loaded(@Terms, -Loaded, :Goal, +Options) is nondet.
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
%
%   Options takes `module(?Module)` of scasp_run/3, with which the
%   program outlives Goal.
%
%   @arg Loaded is `loaded(Module, Program)`, the module holding the
%       program and the `program{...}` dict compile_program/3 made; it
%       is for scasp_ask/4 only.

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

:- det(permanent_module/1).
permanent_module(Module) :-
    (   var(Module)
    ->  gensym(sscasp_program_, Module)
    ;   must_be(atom, Module)
    ),
    forall(( current_predicate(Module:Name/Arity),
             mangled(Name),
             functor(Head, Name, Arity),
             \+ predicate_property(Module:Head, imported_from(_))
           ),
           abolish(Module:Name/Arity)).

scasp_ask(loaded(Module, Program), Query, Model, Justification) :-
    answer(Module, Program, Query, Model, Justification).

%!  scasp_program(@Terms, -Clauses) is det.
%
%   The clauses `scasp/3` would run, as they would look in a module
%   called `program`.  For looking at, and for a test that wants to name
%   one.
%
%   @arg Clauses is the list of clauses runnable_program/3 emits, with
%       mangled heads.

:- det(scasp_program/2).
scasp_program(Terms, Clauses) :-
    compile_program(scasp, Terms, Program),
    runnable_program(program, Program, Clauses).

%   Emitted here rather than before the module exists, because the goals
%   a quantifier holds as terms have to say which module to call them in
%   and assertz/1 will not add that for us.
%
%   The import is what lets `emit.pl` write `check(...)` rather than
%   `sscasp_solve:check(...)`: a temporary module imports nothing by
%   itself, so without it the emitted clauses would have to name the
%   module of every goal they call.  `runtime.pl` is everything they
%   call, and nothing else.

:- det(load_program/2).
load_program(Module, Program) :-
    add_import_module(Module, sscasp_runtime, start),
    runnable_program(Module, Program, Clauses),
    forall(member(Clause, Clauses),
           assert_clause(Module, Clause)).

%   Under `-O` the compiler compiles arithmetic inline, and then refuses
%   a clause whose arithmetic uses a variable nothing can have bound —
%   `hardmath(X) :- X is A+1` — at **assert** time.  Without `-O` the
%   clause is asserted and raises only if it is called, and one such
%   clause in a predicate the query never reaches must not take the
%   program with it.  So it is asserted again with its arithmetic called
%   rather than compiled, which raises where it would have without `-O`.

assert_clause(Module, Clause) :-
    catch(assertz(Module:Clause), error(instantiation_error, _), fail),
    !.
assert_clause(Module, (Head :- Body0)) :-
    called_arithmetic(Body0, Body),
    assertz(Module:(Head :- Body)).

called_arithmetic(Var, Body), var(Var) =>
    Body = Var.
called_arithmetic((A0, B0), Body) =>
    Body = (A, B),
    called_arithmetic(A0, A),
    called_arithmetic(B0, B).
called_arithmetic((A0 ; B0), Body) =>
    Body = (A ; B),
    called_arithmetic(A0, A),
    called_arithmetic(B0, B).
called_arithmetic((A0 -> B0), Body) =>
    Body = (A -> B),
    called_arithmetic(A0, A),
    called_arithmetic(B0, B).
called_arithmetic(\+ A0, Body) =>
    Body = (\+ A),
    called_arithmetic(A0, A).
called_arithmetic(Goal, Body), arithmetic(Goal) =>
    Body = call(Goal).
called_arithmetic(Goal, Body) =>
    Body = Goal.

arithmetic(_ is _).
arithmetic(_ < _).
arithmetic(_ > _).
arithmetic(_ =< _).
arithmetic(_ >= _).
arithmetic(_ =:= _).
arithmetic(_ =\= _).

%!  answer(+Module, +Program, ?Query, -Model, -Justification) is nondet.
%
%   @arg Module is the module the program is loaded into.
%   @arg Program is the `program{...}` dict of compile_program/3.

answer(Module, Program, Query, Model, Justification) :-
    guarded_query(Program, Query, Guarded),
    query_goals(Guarded, Goals),
    table_begin,
    Nmr = Program.nmr.goal,             % bound here: dict notation expands
    empty_proof(Empty),
    seen_answers(Seen),
    catch(solved(Module, Goals, Nmr, Empty, Proof0, Proof),
          Error,
          rethrown(Error)),
    model(Proof, Model0),
    normalise(Query-Model0),
    shown_model(Program.shown, Model0, Model),
    new_answer(Seen, Query-Model),
    children(Proof, Proof0, Checked),
    justification(Proof0, Asked),
    justification(Checked, Because),
    append(Asked, ['$nmr'-Because], Justification).

%!  seen_answers(-Seen) is det.
%!  new_answer(+Seen, @Answer) is semidet.
%
%   **An answer is its bindings and its model**, and one that has
%   already been given is not given again, however differently it was
%   proved.  The proof of the NMR check is where it differs most: a
%   sub-check satisfied by either of two literals is two proofs of one
%   answer, and a large rule base's checks made one query give 72
%   answers that were 36 after `:- sld` goals stopped repeating, and 6
%   different ones.
%   The constraints on the answer's variables are part of it — `X ∉ [a]`
%   and `X ∉ [b]` are two answers.
%
%   The flag `sscasp_distinct_answers` (default `true`) turns this off,
%   giving every proof, as the old implementation does.
%
%   @arg Seen is a trie of the answers given, or `none`.

:- create_prolog_flag(sscasp_distinct_answers, true, [keep(true)]).

seen_answers(Seen) :-
    (   current_prolog_flag(sscasp_distinct_answers, true)
    ->  trie_new(Seen)
    ;   Seen = none
    ).

new_answer(none, _) =>
    true.
new_answer(Seen, Answer) =>
    copy_term(Answer, Copy, Constraints),
    trie_insert(Seen, Copy-Constraints).

%!  shown_model(+Specs, @Model0, -Model) is det.
%
%   Model is Model0 with only what `#show` asks for, or all of it when
%   the program says nothing: `p/1` shows the literals `p(...)`,
%   `not p/1` shows `not p(...)`, `-p/1` shows `-p(...)` and
%   `not -p/1` shows `not -p(...)`.  The old implementation's
%   `model.pl:filter_shown/3`.  The justification is not filtered: it
%   says why, and a step left out would leave the reason incomplete.

:- det(shown_model/3).
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
    (   current_prolog_flag(sscasp_commit_nmr, true) % the goal it appears in,
    ->  term_variables(Goals, Vars),                % and a meta-argument is one
        query_state(Vars, Before),
        committed_nmr(Module, Nmr, Vars, Before, Proof0, Proof)
    ;   call_goal(Module:Nmr, [], Proof0, Proof)
    ).

%!  committed_nmr(+Module, +Nmr, @Vars, +Before, ?Proof0, -Proof) is nondet.
%
%   Prove the NMR check, and commit to its first proof when that proof
%   left the query's variables as it found them.
%
%   The query has been proved when the check runs, so every other proof
%   of the check is the same answer with another justification — and it
%   can be very many: `bec_loan` proves its query once and then its check
%   14,843 times in a minute, 35 different models of one answer and none
%   of the other three answers reached.  With the commitment it gives the
%   four in 0.12 s.
%
%   What may not be thrown away is a proof that says something different
%   about the query.  The check can narrow the query's variables — the
%   consistency check posts a disequality, a quantifier narrows a region
%   — and two proofs narrowing them differently are two answers.  So the
%   commitment is only made when the stores say nothing about Vars after
%   the check that they did not entail before it (entails_state/2): every
%   other proof can then only narrow an answer this one already gives.
%   A check that does narrow keeps its alternatives.
%
%   On by default; the flag `sscasp_commit_nmr` turns it off.  What it
%   loses is alternative models of the same answer — `hamcycle_two`'s
%   second cycle, both through the edge the query asks about.  That is
%   the right default for what s(CASP) is used for: an answer and a
%   justification of it, not an enumeration of the answer sets that
%   contain it — and with the flag off the search may never get past
%   the first answer, as `bec_loan`'s did.

:- create_prolog_flag(sscasp_commit_nmr, true, [keep(true)]).

committed_nmr(Module, Nmr, Vars, Before, Proof0, Proof) :-
    call_goal(Module:Nmr, [], Proof0, Proof),
    (   query_state(Vars, After),
        entails_state(Before, After)
    ->  !
    ;   true
    ).

%!  query_state(@Vars, -State) is det.
%
%   What the stores say about Vars alone, as plain terms over fresh
%   variables: `Bindings-Dense-Herbrand`, the bindings, the CLP(Q) store
%   projected onto them and the Herbrand constraints among them.
%   Projected, because the class the variables are in grows during the
%   check by variables the query never sees.  Read inside a run that
%   `fail` undoes, as `read_store/2` must be.

query_state(Vars, State) :-
    Box = state(-),
    \+ \+ ( include(dense, Vars, Dense),
            (   Dense == []
            ->  Region = []
            ;   dump(Dense, Dense, Region)
            ),
            read_store(Vars, Herbrand0),
            term_variables(Vars, Free),
            include(only_about(Free), Herbrand0, Herbrand1),
            msort(Herbrand1, Herbrand),
            copy_term_nat(Vars-Region-Herbrand, Copy),
            nb_setarg(1, Box, Copy) ),
    arg(1, Box, State).

dense(Var) :-
    var(Var),
    get_attr(Var, clpqr_itf, _).

only_about(Vars, Goal) :-
    term_variables(Goal, GoalVars),
    forall(member(V, GoalVars), memberchk_eq(V, Vars)).

memberchk_eq(X, [Y|Ys]) :-
    (   X == Y
    ->  true
    ;   memberchk_eq(X, Ys)
    ).

%!  entails_state(+Before, +After) is semidet.
%
%   True when Before says all After says: the same bindings, every
%   CLP(Q) constraint of After entailed by Before's region, and every
%   Herbrand constraint of After among Before's or, excluding a number
%   from a variable, entailed by the region — `T > 2` already says
%   `T ∉ [1, 2]`, which the consistency check posts on `bec_loan`.

entails_state(Bindings-Region-Herbrand, After) :-
    After = AfterBindings-_-_,
    Bindings =@= AfterBindings,         % the check bound nothing
    \+ \+ ( After = Bindings-AfterRegion-AfterHerbrand,
            maplist(post, Region),
            forall(member(C, AfterRegion), entailed(C)),
            forall(member(G, AfterHerbrand), herbrand_entailed(G, Herbrand)) ).

post(Constraint) :-
    {Constraint}.

herbrand_entailed(Goal, Herbrand) :-
    (   member(H, Herbrand),
        H =@= Goal
    ->  true
    ;   Goal = apart(X, N), var(X), number(N)
    ->  entailed(X =\= N)
    ;   Goal = apart(N, X), var(X), number(N)
    ->  entailed(X =\= N)
    ).

%!  rethrown(@Error)
%
%   Never returns: raises Error, or the same error in the caller's words.
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
%   (`scasp/compile.pl:143`), so a caller catching one catches both.
%
%   Only a *mangled* name is rewritten.  Anything else the solver could
%   not find is a bug in this tree rather than a typo in a query, and
%   saying so in the compiler's language is then the right thing.

rethrown(Error), query_error(Error, Better) =>
    throw(Better).
rethrown(Error) =>
    throw(Error).

query_error(error(existence_error(procedure, PI), _),
            error(existence_error(scasp_predicate, Name/Arity), _)) :-
    unqualified(PI, Mangled/_EmittedArity),
    demangle(Mangled, pred(_Module, _Kind, Name, Arity)).

unqualified(_:PI, Plain) =>
    unqualified(PI, Plain).
unqualified(PI, Plain) =>
    Plain = PI.

:- det(query_goals/2).
query_goals(Query, Goals) :-
    phrase(conjuncts(Query), Literals),
    maplist(translate_goal(scasp), Literals, Goals).

%   An unbound query has no literals to ask for, and saying so beats
%   what it used to do: `(A, B)` unifies with a variable, so the query
%   grew a conjunct per step and the stack ran out.

conjuncts(Query), var(Query) ==>
    { instantiation_error(Query) }.
conjuncts((A, B)) ==>
    conjuncts(A),
    conjuncts(B).
conjuncts(Literal) ==>
    [Literal].

%!  solve_goals(+Module, ?Goals, ?Anc, ?Proof0, -Proof) is nondet.
%
%   The query is a clause body and is made runnable by the same pass, so
%   that whatever may appear in a body may appear in a query.  Calling
%   each literal instead works until one of them is a **constraint**:
%   `?- T #< 40, holds(T, ...)` is an ordinary s(CASP) query, and `#<`
%   is not a predicate to call but something to post.
%
%   @arg Goals is the list of query literals, translated by
%       translate_goal/3.
%   @arg Anc is the ancestor chain, a list of `a(Goal, Neg, Pol)`
%       frames; `[]` for a query.
%   @arg Proof0 is the proof so far, `proof(Forest, Index)`, and Proof
%       the same extended by what Goals proved.

solve_goals(Module, Goals, Anc, Proof0, Proof) :-
    body_conjunction(Goals, Body),
    body(Body, Module, Anc, Proof0, Proof, Runnable),
    call(Module:Runnable).

body_conjunction([], Body) =>
    Body = true.
body_conjunction([Goal], Body) =>
    Body = Goal.
body_conjunction([Goal|Goals], Body) =>
    Body = (Goal, Rest),
    body_conjunction(Goals, Rest).
