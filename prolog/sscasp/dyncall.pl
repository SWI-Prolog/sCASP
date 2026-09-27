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

:- module(sscasp_dyncall,
          [ scasp/1,                   % :Query
            scasp/2,                   % :Query, +Options
            scasp_query_clauses/2,     % :Query, -Terms
            scasp_query_clauses/3,     % :Query, -Terms, +Options
            scasp_directive/1,         % :Directive
            scasp_constraint/2,        % +Module, +Body
            constraint_clauses/2,      % +Body, -Clauses
            scasp_show/2,              % :Query, +What
            scasp_json/2,              % :Query, -Dict
            scasp_json/3,              % :Query, -Dict, +Options
            (scasp_dynamic)/1,          % :Spec
            scasp_assert/1,             % :Clause
            scasp_assert/2,             % :Clause, +Position
            scasp_retract/1,            % :Clause
            scasp_retractall/1,         % :Head
            scasp_abolish/1,            % :PredicateIndicator
            (sld)/1,                    % :PredicateIndicator
            (describe)/1,               % :Description
            (pred)/1,                   % :Signature or :Description
            (domain)/1,                 % :Definition
            (#=)/2,                     % ?A, ?B
            (#<>)/2,
            (#<)/2,
            (#>)/2,
            (#=<)/2,
            (#>=)/2,

%   Not in `ops.pl`, which is the *language*: this one is a directive for
%   embedding s(CASP) in a Prolog module, and a file front end never sees
%   it.  `library(scasp)` puts it in the same place for the same reason.

            op(1150, fx, scasp_dynamic)
          ]).
:- use_module(ops).
:- use_module(run,    [scasp_run/3, scasp_program/2]).
:- use_module(emit,   [builtin/2]).
:- use_module(describe, [deprecated_description/1]).
:- use_module(annotations, [program_annotations/2]).
:- use_module(listing,  [portray_listing/3, titled/2, commented/2]).
:- use_module(json,     [answer_json/3, answers_json/2]).
:- use_module(library(lists), [member/2, append/3, subtract/3]).
:- use_module(library(apply), [maplist/2, maplist/3, exclude/3, include/3]).
:- use_module(library(option), [option/2, option/3, merge_options/3]).
:- use_module(library(assoc), [empty_assoc/1, get_assoc/3, put_assoc/4]).
:- use_module(library(error), [instantiation_error/1, type_error/2,
                               permission_error/3, domain_error/2]).
:- use_module(library(ordsets), [ord_intersect/2, ord_union/3]).
:- use_module(library(clpq), [{}/1]).

/** <module> Querying ordinary Prolog clauses

`scasp/2` answers an s(CASP) query against the clauses of a Prolog module —
no separate file, no block to open and close.  Write the program as Prolog,
load `library(sscasp)`, and ask:

    :- use_module(library(sscasp)).

    bird(tweety).
    flies(X) :- bird(X), not penguin(X).

    ?- ? flies(X).

## What gets collected

The clauses **reachable from the query**, plus the global constraints whose
call tree overlaps them.  Not the whole module: that is what makes this usable
on a program of any size, and it is why a contradiction, odd loop or
constraint the query cannot reach does not constrain the answer.  Where the
answer must respect something the query cannot see, name it:

    ?- scasp(q, [collect((p, r(_)))]).

Only collection is widened.  The query, the model and the justification are
still about the query alone.

## How a program says the things Prolog has no syntax for

A global constraint is a rule with no head, and `:- Body.` in a Prolog file is
a directive to run at load time.  So it is written

    false :- Body.

which is collected, and `false/0` is never a goal.  Everything else — an
abducible, a domain, a `#pred` template — is a **directive**:

    :- scasp_directive(abducible(maybe)).
    :- scasp_directive(domain(day = [mon, tue])).

recorded per module and added to whatever the query collects.

## The module question

The solver is not module aware: `compile.pl` mangles one module name into
every functor, and `run.pl` fixes it.  So the clauses collected here are
stripped of their module and put in one program.  Two modules defining the
same predicate would collide, and this refuses that rather than silently
choosing — see `check_one_definition/1`.
*/

%   `:` and not `0` throughout.  A query is a *term* this module takes
%   apart, not a goal for the compiler to expand: with `0`, SWI expands
%   the argument where it is written, and `-p(X)` — a goal only s(CASP)
%   knows the meaning of — comes out as a predicate named `'-p'` when
%   `library(scasp)` is loaded alongside.  `:` qualifies it with the
%   caller's module and leaves it alone, which is what
%   `scasp/dyncall.pl` does for the same reason.

:- meta_predicate
    scasp(:),
    scasp(:, +),
    scasp_query_clauses(:, -),
    scasp_query_clauses(:, -, +),
    scasp_show(:, +),
    scasp_json(:, -),
    scasp_json(:, -, +),
    sld(:),
    describe(:),
    pred(:),
    domain(:),
    scasp_directive(:),
    scasp_dynamic(:),
    scasp_assert(:),
    scasp_assert(:, +),
    scasp_retract(:),
    scasp_retractall(:),
    scasp_abolish(:).

%!  scasp_directive(:Directive) is det.
%
%   Record an s(CASP) directive for the calling module: `abducible(Head)`,
%   `domain(Name = Values)`, `pred(Template)`, or a `#show`.  They are
%   added to whatever a query in that module collects.
%
%   A directive rather than a clause because that is what it is — there is
%   nothing to call — and because `:- Body.` is already taken by Prolog.
%
%   It is kept **in the module**, as a clause of `'scasp$directive'/1`,
%   for the reason constraint_clauses/2 gives: so that it has the
%   lifetime of the program.  Called while the module's file is loading
%   it goes through compile_aux_clauses/1, which adds the clause as if
%   the file held it, so a reload wipes it with the file's other clauses
%   and a directive deleted from the source is gone.  Called otherwise it
%   is asserted, and a reload keeps it like any other asserted clause.
%
%   A copy is compiled because the compiler checks singleton-marked
%   variables against the names of the term being loaded, and `:- describe
%   flies(_X) :: ...` stores `_X` twice: in the literal and in its bindings.

scasp_directive(Module:Directive) :-
    (   directive(Module, Seen),
        Seen =@= Directive
    ->  true
    ;   dynamic(Module:'scasp$directive'/1),
        (   loading_module(Module)
        ->  discontiguous(Module:'scasp$directive'/1),
            copy_term(Directive, Clause),   % not the directive's named variables
            compile_aux_clauses(['scasp$directive'(Clause)])
        ;   assertz(Module:'scasp$directive'(Directive))
        )
    ).

loading_module(Module) :-
    prolog_load_context(module, Module),
    prolog_load_context(source, _).

%!  directive(?Module, ?Directive) is nondet.
%
%   Directive was recorded for Module by scasp_directive/1.

directive(Module, Directive) :-
    current_predicate(Module:'scasp$directive'/1),
    \+ predicate_property(Module:'scasp$directive'(_), imported_from(_)),
    Module:'scasp$directive'(Directive).

%!  sld(:Declaration) is det.
%
%   `:- sld p/2.` — prove p/2 by SLD resolution rather than by the
%   solver, and do not collect its clauses.  `:- sld p/2 as opaque.` runs
%   it without interpreting it, so the justification shows the call and
%   no more.
%
%   A predicate rather than a plain `scasp_directive/1` because `sld` is
%   a prefix operator at 1150 and would not fit inside an argument:
%   `:- scasp_directive(sld p/2).` does not parse, and
%   `:- scasp_directive(sld(p/2)).` reads worse than the directive it
%   stands for.  The old implementation exports `(prolog)/1` for the same
%   reason.

sld(Module:Declaration) :-
    qualify_declaration(Module, Declaration, Qualified),
    scasp_directive(Module:sld(Qualified)).

%   The module goes into the declaration, because the goal has to be
%   called where its clauses are and the compiler has no way to find out.

qualify_declaration(Module, (PI as Type), (Module:PI as Type)) :- !.
qualify_declaration(Module, PI, Module:PI).

%!  describe(:Description) is det.
%!  pred(:Specification) is det.
%
%   `:- describe flies(X) :: "@(X) can fly".` — how to say a literal in
%   words, for a reader looking at a model or a justification.  It has no
%   effect on what is proved.
%
%   `pred/1` is the old implementation's spelling and warns.  Here
%   `:- pred p(dom)` already means a *signature*, which says which domain
%   an argument ranges over and does change what is proved, so the two
%   cannot share a directive.
%
%   A predicate rather than a plain `scasp_directive/1` because the
%   template names the literal's variables and a read term does not carry
%   their names: `prolog_load_context/2` does, while this is being
%   loaded, and nothing downstream would.

describe(Module:Description) :-
    load_bindings(Bindings),
    scasp_directive(Module:describe(Description, Bindings)).

%   `pred/1` is two directives sharing a word.  `:- pred p(dom).` is a
%   signature and stays; `:- pred p(X) :: "..."` is a description and
%   warns.  Only the second needs the names.

pred(Module:Spec) :-
    (   nonvar(Spec),
        Spec = (_ :: _)
    ->  deprecated_description(Spec),
        load_bindings(Bindings),
        scasp_directive(Module:pred(Spec, Bindings))
    ;   scasp_directive(Module:pred(Spec))
    ).

%!  domain(:Definition) is det.
%
%   `:- domain day = [mon, tue].` — a domain for a quantifier to range
%   over.  Here because `domain` is a prefix operator and would not fit
%   inside `scasp_directive/1`'s argument, the same reason `sld/1` and
%   `describe/1` exist.

domain(Module:Definition) :-
    scasp_directive(Module:domain(Definition)).

load_bindings(Bindings) :-
    (   prolog_load_context(variable_names, Bindings)
    ->  true
    ;   Bindings = []
    ).

%!  constraint_clauses(+Body, -Clauses) is det.
%!  scasp_constraint(+Module, +Body) is det.
%
%   A global constraint is kept **in its module**, as a clause of the
%   dynamic predicate `'scasp$constraint'/1`.  `false :- Body` in a file
%   expands to Clauses; scasp_assert/1 asserts one at run time.
%
%   In the module rather than in a table here, so that it has the
%   lifetime of the rest of the program.  Reloading a file wipes the
%   clauses it compiled and keeps those asserted since, and destroying a
%   module destroys its predicates; a table keyed by the module's name
%   had neither property.  A constraint deleted from the source survived
%   `make/0`, and in_temporary_module/3, which names its modules from the
%   random state, handed one program the constraints of an earlier one
%   run under the same seed.
%
%   Compiled rather than asserted by a directive, because a reload
%   wipes only what the file compiled: a directive asserting the same
%   clause would add it again on every reload.

constraint_clauses(Body,
                   [ (:- dynamic('scasp$constraint'/1)),
                     (:- discontiguous('scasp$constraint'/1)),
                     'scasp$constraint'(Body)
                   ]).

scasp_constraint(Module, Body) :-
    (   current_predicate(Module:'scasp$constraint'/1),
        Module:'scasp$constraint'(Seen),
        Seen =@= Body
    ->  true
    ;   dynamic(Module:'scasp$constraint'/1),
        assertz(Module:'scasp$constraint'(Body))
    ).

%!  scasp(:Query) is nondet.
%!  scasp(:Query, +Options) is nondet.
%
%   True when Query holds in some partial stable model of the clauses it
%   reaches.  Options:
%
%     - model(-Model)
%       The model that makes it true.
%     - tree(-Justification)
%       Why, as a forest of `Literal-Children`.
%     - collect(+Goal)
%       Collect the clauses Goal reaches as well.  See above.
%     - program(-Terms)
%       The program that was run, as `scasp_query_clauses/2` returns
%       it.  For a caller that needs what came with the clauses.
%     - annotations(-Annotations)
%       The program's annotations — its descriptions, domains and
%       signatures — as the renderers take them: `html_model//2` and
%       `html_justification//2` say a literal in words by its
%       `:- describe` when given `annotations(Annotations)`.  Computed
%       only when asked for.
%     - module(?Module)
%       Leave the compiled program in Module rather than in a temporary
%       module, for the profiler and for looking at.  See
%       `run.pl:scasp_run/3`.

scasp(Query) :-
    scasp(Query, []).

scasp(Module:Query, Options) :-
    scasp_query_clauses(Module:Query, Terms, Options),
    ignore(option(program(Terms), Options)),
    (   option(annotations(Annotations), Options)
    ->  program_annotations(Terms, Annotations)
    ;   true
    ),
    scasp_run(Terms, Query, [model(Model), tree(Justification)|Options]),
    ignore(option(model(Model), Options)),
    ignore(option(tree(Justification), Options)),
    remember(Model, Justification, Terms, Options).

%!  remember(+Model, +Justification, +Terms, +Options) is det.
%
%   Leave the answer where the toplevel can find it.  `sscasp.pl`
%   registers a `residual_goals//0` hook that reads it, so that `?- ? Q.`
%   shows the model as part of *the answer* rather than printing it
%   before the bindings — which is also what makes the model and the
%   bindings name a shared variable the same, since the toplevel names
%   the whole answer at once.
%
%   Backtrackable, so the next answer replaces it and a failed one
%   leaves nothing behind.  The old implementation does the same, in
%   `scasp/embed.pl:save_stack/1`.

remember(Model, Justification, Terms, Options) :-
    b_setval(scasp_answer,
             scasp_answer(Model, Justification, Terms, Options)).

%!  scasp_show(:Query, +What) is det.
%
%   Write the program a query would run.  What is `code` or
%   `code(Option...)` — the options being the arguments, which is how the
%   old implementation's `scasp_show/2` is spelled.  An option given as
%   a bare name means `true`: `code(duals)` is `code(duals(true))`.
%
%     - query(Boolean)
%       The question.  Default `true`.
%     - user(Boolean)
%       The user predicates, as the compiler has them.  Default `true`.
%     - duals(Boolean)
%       The dual rules — what `not p` tries.  Default `false`.
%     - constraints(Boolean)
%       The NMR check, one sub-check at a time, each headed by what it
%       came from.  Default `false`.
%     - source(Boolean)
%       The terms as they were **collected**, verbatim: the clauses, the
%       global constraints and the directives, which is what
%       `scasp_query_clauses/2` returns and the only section that shows
%       a `:- describe` or a `:- domain`.  Default `false`.
%     - compiled(Boolean)
%       The clauses the solver actually runs, in a module called
%       `program`.  Default `false`; there are a lot of them, and they
%       are for when the question is about the emitter rather than about
%       the program.
%   The defaults are the old implementation's: `code` alone is the
%   question and the program, and what the compiler made of it is asked
%   for.
%
%   A listing is written in the **source** spelling — `-p(X)` and not
%   `¬ p(X)` — because that is what a reader compares with their
%   program.  The model and the justification read the other way.
%
%   @error domain_error(scasp_show, What) for anything else.

scasp_show(Query, What) :-
    (   compound(What),
        compound_name_arguments(What, code, Options0)
    ->  true
    ;   What == code
    ->  Options0 = []
    ;   domain_error(scasp_show, What)
    ),
    maplist(show_option, Options0, Options),
    strip_module_goal(Query, Plain),
    scasp_query_clauses(Query, Terms),
    show_source(Terms, Options),
    portray_listing(Terms, Plain, Options),
    show_compiled(Terms, Options).

show_option(Name, Option), atom(Name) =>
    Option =.. [Name, true].
show_option(Option0, Option) =>
    Option = Option0.

show_source(Terms, Options) :-
    (   option(source(true), Options)
    ->  titled('Program as collected', Options),
        forall(member(Term, Terms), show_term(Term))
    ;   true
    ).

show_compiled(Terms, Options) :-
    (   option(compiled(true), Options)
    ->  titled('Emitted clauses', Options),
        commented('run in a module importing sscasp_solve, \c
                   sscasp_herbrand and clpq', Options),
        scasp_program(Terms, Clauses),
        forall(member(Clause, Clauses), show_term(Clause))
    ;   true
    ).

%   With `sscasp/ops.pl`'s operator table, not the caller's.  A caller
%   that has not imported them — a plunit unit, say, or a module that
%   only uses `scasp/2` — would otherwise be shown `not(penguin(X))`
%   where the program says `not penguin(X)`, which is the same term and
%   not the same answer to "what would this run".

show_term(Term) :-
    portray_clause(current_output, Term, [module(sscasp_ops)]).

%!  scasp_json(:Query, -Dict) is det.
%!  scasp_json(:Query, -Dict, +Options) is det.
%
%   Every answer to Query as JSON-able data, for a consumer that is a
%   program rather than a person.  `sscasp/json.pl` says what the shape
%   is; `json_write_dict/3` writes it.
%
%   **Deterministic**: the query is solved to exhaustion, because a JSON
%   document is a whole thing rather than something to backtrack into.
%   Options are `scasp/2`'s, plus
%
%     - limit(Count)
%       Stop after Count answers.  Default `infinite`, which is the
%       right default for a program with a finite answer set and the
%       wrong one for anything else.
%     - variable_names(+Bindings)
%       Report the bindings under the names the query was read with.
%       Without it `bindings` is empty: a query term carries no names.
%
%   Each answer's dict is built **inside** the solve rather than
%   afterwards.  `findall/3` copies, and a copied answer is no longer
%   the one whose query variables the caller holds — the constraints
%   come along but they are about other variables.  A dict is plain
%   data and copies without that question.

scasp_json(Query, Dict) :-
    scasp_json(Query, Dict, []).

scasp_json(Module:Query, Dict, Options) :-
    option(limit(Limit), Options, infinite),
    answer_dicts(Limit, Module:Query, Dicts, Options),
    answers_json(Dicts, Dict).

answer_dicts(infinite, Query, Dicts, Options) :-
    !,
    findall(D, answer_dict(Query, D, Options), Dicts).
answer_dicts(Limit, Query, Dicts, Options) :-
    findnsols(Limit, D, answer_dict(Query, D, Options), Dicts),
    !.

answer_dict(Module:Query, Dict, Options) :-
    scasp(Module:Query, [model(Model), tree(Justification)]),
    answer_json(answer(Query, Model, Justification), Dict, Options).

%!  scasp_query_clauses(:Query, -Terms) is det.
%!  scasp_query_clauses(:Query, -Terms, +Options) is det.
%
%   The program `scasp/2` would run: the clauses reachable from Query,
%   the global constraints overlapping them, and the module's directives.
%   Useful on its own for seeing what a query drags in.

scasp_query_clauses(Query, Terms) :-
    scasp_query_clauses(Query, Terms, []).

scasp_query_clauses(Module:Query, Terms, Options) :-
    (   option(collect(Extra), Options)
    ->  strip_module_goal(Extra, Collect0),
        Collect = (Query, Collect0)
    ;   Collect = Query
    ),
    query_callees(Module:Collect, Callees0),
    include_global_constraints(Module, Callees0, Constraints, Callees),
    check_one_definition(Callees),
    findall(Clause, reachable_clause(Callees, Clause), Clauses),
    contributing_modules(Module, Callees, Modules),
    findall(Directive,
            ( member(Source, Modules),
              directive(Source, Directive)
            ),
            All),
    include(about_collected(Callees, All), All, Kept),
    maplist(as_directive, Kept, Directives),
    append([Directives, Constraints, Clauses], Terms).

as_directive(Directive, (:- Directive)).

%!  about_collected(+Callees, +Directives, +Directive) is semidet.
%
%   A directive is kept when the program being assembled is the one it is
%   about.  A module's directives are all of them; the clauses are only
%   what the query reaches, so a signature or a description for a
%   predicate the query cannot reach does not belong here — and a
%   signature is *checked* against the definitions
%   (`annotations.pl:check_signature_arity/4`), so including one would
%   raise `existence_error(procedure, PI)` on a program that is perfectly
%   well formed.
%
%   Whatever a directive does not name a predicate for is kept: a
%   `domain` declares one rather than describing one, a `#show` is about
%   the output, and an unrecognised directive is somebody else's.

about_collected(Callees, Directives, Directive) :-
    (   subject(Directive, PI)
    ->  (   memberchk(_:PI, Callees)
        ;   declares(Directives, PI)
        )
    ;   true
    ),
    !.

%!  subject(+Directive, -PI) is semidet.
%
%   The predicate a directive is *about*, as opposed to one it declares.
%   Read structurally rather than through `annotations.pl:annotation/2`,
%   which is the same question asked properly: that one *parses* the
%   directive, and parsing `:- pred p(X) :: "..."` prints the
%   deprecation warning, which belongs to compiling the program and not
%   to deciding whether to collect it.

subject(pred(Spec), PI) :-
    !,
    signature_or_description(Spec, PI).
subject(pred(Spec, _), PI) :-
    !,
    signature_or_description(Spec, PI).
subject(describe(Spec), PI) :-
    !,
    described_pi(Spec, PI).
subject(describe(Spec, _), PI) :-
    !,
    described_pi(Spec, PI).

signature_or_description(Spec, PI) :-
    nonvar(Spec),
    (   Spec = (_ :: _)
    ->  described_pi(Spec, PI)
    ;   callable(Spec),
        functor(Spec, Name, Arity),
        PI = Name/Arity
    ).

described_pi(Spec, Name/Arity) :-
    nonvar(Spec),
    Spec = (Literal :: _),
    bare_literal(Literal, Head),
    callable(Head),
    functor(Head, Name, Arity).

bare_literal(Literal, _) :-
    var(Literal),
    !,
    fail.
bare_literal(not(Inner), Head) :- !, bare_literal(Inner, Head).
bare_literal(-(Inner), Head)   :- !, bare_literal(Inner, Head).
bare_literal(Head, Head).

%!  declares(+Directives, ?PI) is nondet.
%
%   A predicate a directive *declares* — one with no clauses to collect,
%   so `goal_callee/3` never reports it and it is never among the
%   callees.  It is still part of the program, and a signature or
%   description for it is still about this program.

declares(Directives, PI) :-
    member(Directive, Directives),
    declared_pi(Directive, PI).

declared_pi(abducible(Head), Name/Arity) :-
    callable(Head),
    functor(Head, Name, Arity).
declared_pi(domain(Name = _), Name/1).
declared_pi(sld(Declaration), PI) :-
    sld_pi(Declaration, PI).
declared_pi(sld(Declaration as _), PI) :-
    sld_pi(Declaration, PI).

sld_pi(_:PI, PI) :- !.
sld_pi(PI, PI).

%!  contributing_modules(+Module, +Callees, -Modules) is det.
%
%   Every module the program is being assembled from, the query's own
%   included.  A constraint and a directive belong with the clauses they
%   are about, not with whoever happens to be asking — and the asker is
%   often neither: plunit runs each test in a module of its own, which
%   inherits the predicates and holds none of the declarations.

%   The query's module is taken with everything it **inherits** from,
%   which is how a declaration in a file is found from a goal that runs
%   somewhere else: a plunit unit is a module of its own that inherits
%   the file's predicates, and a directive in the file is not in it.

contributing_modules(Module, Callees, Modules) :-
    findall(Source, member(Source:_, Callees), Sources),
    findall(Ancestor, default_module(Module, Ancestor), Ancestors),
    append(Sources, Ancestors, All),
    sort(All, Modules).

strip_module_goal(_:Goal, Goal) :- !.
strip_module_goal(Goal, Goal).

%!  check_one_definition(+Callees) is det.
%
%   The solver mangles one module name into every functor, so a program
%   assembled from two modules that both define `p/1` would run with one
%   of them silently discarded.  Refuse instead.
%
%   Asked of the collected set only.  Asking whether *any* module defines
%   the name would refuse a program for using `member/2` next to
%   `library(lists)`, which is not the problem this is about.
%
%   @error permission_error(collect, scasp_predicate, PI)

check_one_definition(Callees) :-
    (   member(A:PI, Callees),
        member(B:PI, Callees),
        A \== B
    ->  permission_error(collect, scasp_predicate, PI)
    ;   true
    ).

%!  reachable_clause(+Callees, -Term) is nondet.
%
%   Each clause of each reachable predicate, stripped of its module.

reachable_clause(Callees, Term) :-
    member(Module:PI, Callees),
    callee_head(PI, Head),
    @(clause(Head, Body0), Module),
    strip_body(Module, Body0, Body),
    clause_term(Head, Body, Term).

%!  callee_head(+PI, -Head) is det.
%
%   A classically negated predicate has no name of its own: `-p(X)` is a
%   clause of `-/1` whose argument is `p(X)`, and `-/1` holds the clauses
%   of *every* such predicate at once.  So it is keyed as `neg(p/1)` and
%   its head built back here, which keeps `-p` and `-q` apart where the
%   functor does not.

callee_head(neg(Name/Arity), -(Inner)) :-
    !,
    functor(Inner, Name, Arity).
callee_head(Name/Arity, Head) :-
    functor(Head, Name, Arity).

clause_term(Head, true, Head) :- !.
clause_term(Head, Body, (Head :- Body)).

%   A multifile clause decompiles as `Module:Body`; the solver does not
%   deal with that.

strip_body(Module, Module:Body, Body) :- !.
strip_body(_, Body, Body).

		 /*******************************
		 *     GLOBAL CONSTRAINTS	*
		 *******************************/

%!  include_global_constraints(+Module, +Callees, -Constraints, -All) is det.
%
%   A `false :- Body` is included when its call tree overlaps what the
%   query reaches, and including it widens that — so a constraint reached
%   only through another constraint comes too, and the walk repeats until
%   nothing new arrives.

include_global_constraints(Module, Callees0, Constraints, Callees) :-
    include_global_constraints(Module, Callees0, Callees, [], Constraints).

include_global_constraints(Module, Callees0, Callees, Have, Constraints) :-
    contributing_modules(Module, Callees0, Modules),
    member(Source, Modules),
    global_constraint(Source, Body),
    \+ ( member((:- Seen), Have),
         Seen =@= Body
       ),
    query_callees(Source:Body, Called),
    ord_intersect(Callees0, Called),
    !,
    ord_union(Callees0, Called, Callees1),
    include_global_constraints(Module, Callees1, Callees,
                               [(:- Body)|Have], Constraints).
include_global_constraints(_, Callees, Callees, Constraints, Constraints).

%!  global_constraint(+Module, -Body) is nondet.
%
%   `false/0` is a built-in and cannot be given clauses, so `false :-
%   Body.` is expanded away at load time.  Two expansions may do it and
%   both are read here, because a process with **both** libraries loaded
%   has two `user:term_expansion/2` clauses for that term and the first
%   one declared wins:
%
%     - `sscasp.pl` records it, which is what happens with only this
%       library loaded;
%     - `scasp/dyncall.pl` turns it into a clause of `-/0`, which is what
%       happens when the old library was loaded first.
%
%   Reading both costs one extra lookup and removes the load order from
%   the answer.

global_constraint(Module, Body) :-
    current_predicate(Module:'scasp$constraint'/1),
    \+ predicate_property(Module:'scasp$constraint'(_), imported_from(_)),
    Module:'scasp$constraint'(Body).
global_constraint(Module, Body) :-
    current_predicate(Module:(-)/0),
    \+ predicate_property(Module:(-), imported_from(_)),
    @(clause(-, Body0), Module),
    strip_body(Module, Body0, Body).

		 /*******************************
		 *        THE CALL TREE		*
		 *******************************/

%!  query_callees(:Goal, -Callees) is det.
%
%   Every predicate Goal can reach, as an ordered set of `Module:Name/Arity`.

query_callees(Module:Goal, Callees) :-
    findall(Callee, goal_callee(Goal, Module, Callee), Direct0),
    sort(Direct0, Direct),
    closure(Direct, Direct, Callees).

closure(Agenda, Reached0, Reached) :-
    empty_assoc(Done),
    closure_(Agenda, Done, Reached0, Reached1),
    sort(Reached1, Reached).

closure_([], _, Reached, Reached).
closure_([PI|Agenda], Done, Reached0, Reached) :-
    (   get_assoc(PI, Done, _)
    ->  closure_(Agenda, Done, Reached0, Reached)
    ;   put_assoc(PI, Done, true, Done1),
        predicate_callees(PI, Called0),
        add_counterpart(PI, Called0, Called),
        exclude(done(Done1), Called, New),
        append(New, Agenda, Agenda1),
        append(New, Reached0, Reached1),
        closure_(Agenda1, Done1, Reached1, Reached)
    ).

%!  add_counterpart(+PI, +Called0, -Called) is det.
%
%   `p/1` and `-p/1` are one predicate as far as **consistency** goes:
%   the compiler writes `:- p(X), -p(X)` for every classically negated
%   predicate, and a program that derives both has no answer set at all.
%   So reaching one reaches the other, and reaching it drags in whatever
%   *it* calls, which is why this happens inside the closure rather than
%   after it.
%
%   Without it a query is answered against a program missing the half
%   that could contradict it:
%
%       q.  p :- q.  -p :- q.
%
%   has no answer set, and `?- scasp(p).` said yes — nothing reachable
%   from `p` mentions `-p`, so its clauses were never collected and the
%   consistency check was never generated.
%
%   This is not "collect the whole program" creeping back.  A global
%   constraint the query cannot reach still does not constrain it; what
%   this says is that the *rest of a predicate* is not somewhere else.

add_counterpart(PI, Called, [Other|Called]) :-
    counterpart(PI, Other),
    !.
add_counterpart(_, Called, Called).

counterpart(Module:neg(Name/Arity), Module:Name/Arity) :-
    !,
    functor(Head, Name, Arity),
    has_clauses(Module, Head).
counterpart(Module:Name/Arity, Where:neg(Name/Arity)) :-
    functor(Inner, Name, Arity),
    defined_somewhere(Module, (-)/1, Where),
    has_clauses(Where, -(Inner)).

has_clauses(Module, Head) :-
    functor(Head, Name, Arity),
    current_predicate(Module:Name/Arity),
    \+ predicate_property(Module:Head, imported_from(_)),
    \+ \+ @(clause(Head, _), Module).

done(Done, PI) :-
    get_assoc(PI, Done, _).

predicate_callees(Module:PI, Callees) :-
    callee_head(PI, Head),
    findall(Callee,
            ( @(clause(Head, Body0), Module),
              strip_body(Module, Body0, Body),
              goal_callee(Body, Module, Callee)
            ),
            Callees0),
    sort(Callees0, Callees).

%!  goal_callee(+Body, +Module, -PI) is nondet.
%
%   The predicates a body calls, skipping what the emitter turns into a
%   constraint or an arithmetic test rather than a call.

goal_callee(Goal, _, _), var(Goal) =>
    instantiation_error(Goal).
goal_callee(true, _, _) => fail.
goal_callee(fail, _, _) => fail.
goal_callee(Goal, Module, _),
        callable(Goal),
        reinterpreted(Goal, PI),
        redefined(Module, PI) =>
    permission_error(define, scasp_control_construct, PI).
goal_callee((A, B), Module, PI) =>
    (   goal_callee(A, Module, PI)
    ;   goal_callee(B, Module, PI)
    ).
goal_callee(not(A), Module, PI) =>
    goal_callee(A, Module, PI).
goal_callee(-(A), Module, PI), callable(A) =>
    functor(A, Name, Arity),
    negation_home(Module, Name/Arity, Where),
    PI = Where:neg(Name/Arity).
goal_callee(findall(_, Goal, _), Module, PI) =>
    goal_callee(Goal, Module, PI).
goal_callee(forall(_, Goal), Module, PI) =>
    goal_callee(Goal, Module, PI).
goal_callee(Module:Goal, _, PI), atom(Module) =>
    goal_callee(Goal, Module, PI).
goal_callee(Goal, _, _), callable(Goal), builtin(Goal, _) =>
    fail.
goal_callee(Goal, Module, _), callable(Goal), declared(Module, Goal) =>
    fail.
goal_callee(Goal, Module, PI), callable(Goal) =>
    functor(Goal, Name, Arity),
    defined_somewhere(Module, Name/Arity, Where),
    PI = Where:Name/Arity.
goal_callee(Goal, _, _) =>
    type_error(callable, Goal).

%!  #=(?A, ?B).
%!  #<>(?A, ?B).
%!  #<(?A, ?B).
%!  #>(?A, ?B).
%!  #=<(?A, ?B).
%!  #>=(?A, ?B).
%
%   The s(CASP) comparisons, as Prolog predicates.  The solver never
%   calls these: goal_callee/3 takes them for the built-ins they are and
%   `emit.pl:builtin/2` compiles them.  They exist so that a module
%   written for s(CASP) is also a well-formed Prolog module, in which
%   check/0 finds nothing undefined and a goal run by Prolog itself
%   means what it means to the solver.  `library(scasp)` defines them
%   for the same reason, so a caller switching library keeps them.

A #=  B :- {A =:= B}.
A #<> B :- {A =\= B}.
A #<  B :- {A < B}.
A #>  B :- {A > B}.
A #=< B :- {A =< B}.
A #>= B :- {A >= B}.

%!  negation_home(+Module, +PI, -Where) is det.
%
%   The module holding the clauses of `-p` for a `-p(...)` called in
%   Module.  Module's own `-/1` if it has clauses for `p`, and otherwise
%   the `-/1` of the module that holds `p`: `p` and `-p` are one
%   predicate, so a module that exports `p` has exported `-p` with it.
%   The old implementation interns `-p` into a predicate `'-p'` that a
%   module exports by name; here `-/1` holds every negated head of its
%   module and cannot be exported per predicate.

negation_home(Module, Name/Arity, Where) :-
    functor(Inner, Name, Arity),
    defined_somewhere(Module, (-)/1, Local),
    (   has_clauses(Local, -(Inner))
    ->  Where = Local
    ;   defined_somewhere(Module, Name/Arity, Home),
        Home \== Local,
        has_clauses(Home, -(Inner))
    ->  Where = Home
    ;   Where = Local
    ).

%!  reinterpreted(+Goal, -PI) is semidet.
%!  redefined(+Module, +PI) is semidet.
%
%   Goal is one the compiler gives a meaning of **its own**, and the
%   module has given it clauses.  Those clauses can never run: the
%   interceptions below take `forall/2` for the quantifier and
%   `findall/3` for the collection before either could be looked up, so
%   the program's own definition is not so much overridden as invisible.
%
%   Not hypothetical, and not loud.  `test/le_programs/subset-scasp.pl`
%   writes its own
%
%       forall(A, B) :- not(A, not(B)).
%
%   and calls `forall(belongs_to(C, A), belongs_to(C, B))` meaning it.
%   Read as the quantifier that is `forall(Vars, Goal)` with a *literal*
%   where the variables go, and the whole program answered **nothing** —
%   which looks exactly like a scenario with no solution.  Its call tree
%   was not followed either, so the `not/2` it also calls, which does not
%   exist, went unmentioned.
%
%   So it is refused, with the term `library(scasp)` refuses it with
%   (`scasp/dyncall.pl`) — a caller catching one catches both.  The check
%   is at the **call**: a definition nothing calls is misread by nobody,
%   and finding one would mean scanning the module rather than the query's
%   call tree.
%
%   `-/1` is deliberately absent.  Every `-p(X)` in a program is a clause
%   of it, which is the representation rather than a redefinition.

reinterpreted(Goal, Name/Arity) :-
    (   construct(Goal)
    ;   builtin(Goal, _)
    ),
    !,
    functor(Goal, Name, Arity).

construct((_, _)).
construct(not(_)).
construct(forall(_, _)).
construct(findall(_, _, _)).
construct(for_domain(_, _, _)).
construct(sld(_, _)).

redefined(Module, Name/Arity) :-
    functor(Head, Name, Arity),
    current_predicate(Module:Name/Arity),
    \+ predicate_property(Module:Head, imported_from(_)),
    \+ predicate_property(Module:Head, built_in),
    predicate_property(Module:Head, number_of_clauses(Clauses)),
    Clauses > 0.

%!  declared(+Module, +Goal) is semidet.
%
%   True when a directive already says what this predicate is, so that
%   whatever clauses happen to be lying around are not it:
%
%     - `:- sld p/2.` — proved by SLD resolution, and the compiler writes
%       a bridge rather than collecting Prolog's clauses;
%     - `:- scasp_directive(abducible(p(X))).` — assumed either way, and
%       an abducible with clauses is not an abducible;
%     - `:- scasp_directive(domain(d = [a, b])).` — the compiler writes
%       the facts, so whatever a `d/1` elsewhere has clauses for is not
%       this one.
%
%   The second is not a nicety.  `maybe/0` is a predicate of
%   `library(random)`, so a program declaring `maybe` abducible and
%   calling it collected *that* — one clause, `random(2) =:= 0` — the
%   moment anything in the process had loaded the library.  Silently: the
%   query still answered, with a coin toss in the model.
%
%   The general hazard remains and is worth knowing about: a program
%   whose predicate has the name of one some loaded library exports gets
%   the library's clauses, because that is what the name resolves to.
%   `library(scasp)` has it too.  A declaration is the way to say
%   otherwise.

declared(Module, Goal) :-
    functor(Goal, Name, Arity),
    default_module(Module, Ancestor),
    (   sld_declaration(Ancestor, Name/Arity)
    ;   directive(Ancestor, abducible(Head)),
        functor(Head, Name, Arity)
    ;   Arity =:= 1,
        directive(Ancestor, domain(Name = _))
    ),
    !.

sld_declaration(Module, Name/Arity) :-
    (   directive(Module, sld(Declaration))
    ;   directive(Module, sld(Declaration as _))
    ),
    (   Declaration = _:PI
    ->  true
    ;   PI = Declaration
    ),
    PI = Name/Arity.

%!  defined_somewhere(+Module, +PI, -Where) is det.
%
%   Which module holds the clauses.  An undefined predicate stays with
%   the caller and collects no clauses — the compiler **closes** the
%   program, so it is false of everything and its negation true of
%   everything, which is what an s(CASP) program means by a predicate it
%   never defines.

%
%   The module's **own** definition is asked for before its ancestors'.
%   Asking an ancestor may autoload the name into it: `distinct/1` is a
%   predicate of library(solution_sequences), so a module defining its
%   own `distinct/1` had it resolved to that library, whose clauses the
%   collector then could not read.

defined_somewhere(Module, Name/Arity, Where) :-
    functor(Head, Name, Arity),
    (   predicate_property(Module:Head, imported_from(From))
    ->  Where = From
    ;   current_predicate(Module:Name/Arity)
    ->  Where = Module
    ;   default_module(Module, Ancestor),
        Ancestor \== Module,
        predicate_property(Ancestor:Head, number_of_clauses(_))
    ->  Where = Ancestor
    ;   Where = Module
    ).


		 /*******************************
		 *    CHANGING THE PROGRAM	*
		 *******************************/

/* Asserting and retracting s(CASP) clauses.

`scasp/2` reads the module's clauses at query time, so a program that
changes between queries needs no invalidation — `query_callees/2`
recomputes and the next answer is about what is there now.  What these
add over plain `assertz/1` is the two things an s(CASP) clause can be
that a Prolog clause cannot:

| `-p(X) :- Body` | classical negation |
| `:- Body` / `false :- Body` | a global constraint |

**The first needs no translation here**, and that is the difference from
`library(scasp)` worth knowing when porting.  The old implementation
interns `-p(X)` into a predicate literally named `'-p'`, so its
`scasp_assert/1` has to rename the functor.  This tree leaves the term
alone: `-p(X)` is a clause of `-/1` carrying `p(X)`, which is what the
compiler writes for a file and what `callee_head/2` looks for, so
`assertz/1` on the term as written is already right.

**The second cannot be asserted at all.** `false/0` is a built-in, so a
global constraint is *recorded* rather than compiled — `scasp_constraint/2`
— exactly as the `false :- Body` term expansion does at load time.
*/

%!  scasp_dynamic(:Spec) is det.
%
%   Declare predicates that clauses will be added to and removed from:
%
%       :- scasp_dynamic p/1.
%       :- scasp_dynamic p/1, q/2.
%       :- scasp_dynamic p/1 as shared.
%
%   `thread_local` by default, so that two threads asking about two
%   different programs do not see each other's clauses; `as shared`
%   makes it `dynamic` instead.
%
%   **The negative half is one predicate.**  Declaring `p/1` also
%   declares `-/1`, which holds `-p(X)` — and `-q(X)`, and every other
%   classically negated head in the module.  So its scope is decided
%   once, by the first declaration that reaches it, rather than per
%   predicate: a later `as shared` cannot make it shared for `-q` alone
%   without making it shared for `-p` too.  The old implementation gives
%   each one its own `'-p'/1` and can scope them separately; that is the
%   price of not interning, and it buys a `-p(X)` that reads as itself
%   everywhere else.

scasp_dynamic(M:Spec) :-
    dynamic_spec(Spec, M).

dynamic_spec(Var, _), var(Var) =>
    instantiation_error(Var).
dynamic_spec((A, B), M) =>
    dynamic_spec(A, M),
    dynamic_spec(B, M).
dynamic_spec((Spec as Scope), M) =>
    dynamic_scoped(Spec, M, Scope).
dynamic_spec(Spec, M) =>
    dynamic_scoped(Spec, M, private).

dynamic_scoped(Var, _, _), var(Var) =>
    instantiation_error(Var).
dynamic_scoped((A, B), M, Scope) =>
    dynamic_scoped(A, M, Scope),
    dynamic_scoped(B, M, Scope).
dynamic_scoped(Name/Arity, M, Scope) =>
    declare(M:Name/Arity, Scope),
    (   scoped_already(M:(-)/1)
    ->  true
    ;   declare(M:(-)/1, Scope)
    ).
dynamic_scoped(Spec, _, _) =>
    type_error(predicate_indicator, Spec).

declare(PI, shared) => dynamic(PI).
declare(PI, private) => thread_local(PI).
declare(_, Scope) => domain_error(scasp_dynamic_scope, Scope).

scoped_already(M:Name/Arity) :-
    functor(Head, Name, Arity),
    (   predicate_property(M:Head, (dynamic))
    ;   predicate_property(M:Head, thread_local)
    ),
    !.

%!  scasp_assert(:Clause) is det.
%!  scasp_assert(:Clause, +Position) is det.
%
%   Add Clause to the calling module's program.  Position is accepted
%   and ignored: the old implementation records it so that a
%   justification can point back at the text, and this solver keeps no
%   source references at all (`doc/semantics.md`).  It is taken rather
%   than refused so that a caller written against `library(scasp)`
%   ports without losing an argument.

scasp_assert(Clause) :-
    scasp_assert(Clause, false).

scasp_assert(M:(:- Body), _Pos) =>
    scasp_constraint(M, Body).
scasp_assert(M:(false :- Body), _Pos) =>
    scasp_constraint(M, Body).
scasp_assert(Clause, _Pos) =>
    assertz(Clause).

%!  scasp_retract(:Clause) is nondet.
%!  scasp_retractall(:Head) is det.
%
%   Take clauses out again.  A global constraint is retracted by the
%   body it was recorded with, since it has no head to name it by.

scasp_retract(M:(:- Body)) =>
    retract(M:'scasp$constraint'(Body)).
scasp_retract(M:(false :- Body)) =>
    retract(M:'scasp$constraint'(Body)).
scasp_retract(Clause) =>
    retract(Clause).

scasp_retractall(M:(:- Body)) =>
    retractall(M:'scasp$constraint'(Body)).
scasp_retractall(M:(false :- Body)) =>
    retractall(M:'scasp$constraint'(Body)).
scasp_retractall(Head) =>
    retractall(Head).

%!  scasp_abolish(:PredicateIndicator) is det.
%
%   Remove every clause of a predicate **and of its classical
%   negation**, which is what makes it more than `retractall/1`: a
%   program that has forgotten `p` but still proves `-p` is not one the
%   caller asked for.
%
%   It empties rather than abolishes, for the reason the old
%   implementation does: abolishing would take the `dynamic` property
%   with it, and the next `scasp_assert/1` would create the predicate
%   afresh in whatever scope it defaulted to.

scasp_abolish(M:(Name/Arity)) =>
    functor(Head, Name, Arity),
    retractall(M:Head),
    retractall(M:(-Head)).
scasp_abolish(_:Spec) =>
    type_error(predicate_indicator, Spec).
