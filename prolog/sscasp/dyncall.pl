:- module(sscasp_dyncall,
          [ sscasp/1,                   % :Query
            sscasp/2,                   % :Query, +Options
            sscasp_query_clauses/2,     % :Query, -Terms
            sscasp_query_clauses/3,     % :Query, -Terms, +Options
            sscasp_directive/1,         % :Directive
            sscasp_constraint/2,        % +Module, +Body
            sscasp_show/2,              % :Query, +What
            sscasp_json/2,              % :Query, -Dict
            sscasp_json/3,              % :Query, -Dict, +Options
            (sld)/1,                    % :PredicateIndicator
            (describe)/1,               % :Description
            (pred)/1,                   % :Signature or :Description
            (domain)/1                  % :Definition
          ]).
:- use_module(ops).
:- use_module(run,    [sscasp/4, sscasp_program/2]).
:- use_module(emit,   [builtin/2]).
:- use_module(describe, [deprecated_description/1]).
:- use_module(listing,  [portray_listing/3, titled/2]).
:- use_module(json,     [answer_json/2, answers_json/2]).
:- use_module(library(lists), [member/2, append/3, subtract/3]).
:- use_module(library(apply), [maplist/2, maplist/3, exclude/3, include/3]).
:- use_module(library(option), [option/2, option/3, merge_options/3]).
:- use_module(library(assoc), [empty_assoc/1, get_assoc/3, put_assoc/4]).
:- use_module(library(error), [instantiation_error/1, type_error/2,
                               permission_error/3, domain_error/2]).
:- use_module(library(ordsets), [ord_intersect/2, ord_union/3]).

/** <module> Querying ordinary Prolog clauses

`sscasp/2` answers an s(CASP) query against the clauses of a Prolog module —
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

    ?- sscasp(q, [collect((p, r(_)))]).

Only collection is widened.  The query, the model and the justification are
still about the query alone.

## How a program says the things Prolog has no syntax for

A global constraint is a rule with no head, and `:- Body.` in a Prolog file is
a directive to run at load time.  So it is written

    false :- Body.

which is collected, and `false/0` is never a goal.  Everything else — an
abducible, a domain, a `#pred` template — is a **directive**:

    :- sscasp_directive(abducible(maybe)).
    :- sscasp_directive(domain(day = [mon, tue])).

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
    sscasp(:),
    sscasp(:, +),
    sscasp_query_clauses(:, -),
    sscasp_query_clauses(:, -, +),
    sscasp_show(:, +),
    sscasp_json(:, -),
    sscasp_json(:, -, +),
    sld(:),
    describe(:),
    pred(:),
    domain(:),
    sscasp_directive(:).

:- dynamic
    directive/2,                        % Module, Directive
    constraint/2.                       % Module, Body

%!  sscasp_directive(:Directive) is det.
%
%   Record an s(CASP) directive for the calling module: `abducible(Head)`,
%   `domain(Name = Values)`, `pred(Template)`, or a `#show`.  They are
%   added to whatever a query in that module collects.
%
%   A directive rather than a clause because that is what it is — there is
%   nothing to call — and because `:- Body.` is already taken by Prolog.

sscasp_directive(Module:Directive) :-
    (   directive(Module, Directive)
    ->  true
    ;   assertz(directive(Module, Directive))
    ).

%!  sld(:Declaration) is det.
%
%   `:- sld p/2.` — prove p/2 by SLD resolution rather than by the
%   solver, and do not collect its clauses.  `:- sld p/2 as opaque.` runs
%   it without interpreting it, so the justification shows the call and
%   no more.
%
%   A predicate rather than a plain `sscasp_directive/1` because `sld` is
%   a prefix operator at 1150 and would not fit inside an argument:
%   `:- sscasp_directive(sld p/2).` does not parse, and
%   `:- sscasp_directive(sld(p/2)).` reads worse than the directive it
%   stands for.  The old implementation exports `(prolog)/1` for the same
%   reason.

sld(Module:Declaration) :-
    qualify_declaration(Module, Declaration, Qualified),
    sscasp_directive(Module:sld(Qualified)).

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
%   A predicate rather than a plain `sscasp_directive/1` because the
%   template names the literal's variables and a read term does not carry
%   their names: `prolog_load_context/2` does, while this is being
%   loaded, and nothing downstream would.

describe(Module:Description) :-
    load_bindings(Bindings),
    sscasp_directive(Module:describe(Description, Bindings)).

%   `pred/1` is two directives sharing a word.  `:- pred p(dom).` is a
%   signature and stays; `:- pred p(X) :: "..."` is a description and
%   warns.  Only the second needs the names.

pred(Module:Spec) :-
    (   nonvar(Spec),
        Spec = (_ :: _)
    ->  deprecated_description(Spec),
        load_bindings(Bindings),
        sscasp_directive(Module:pred(Spec, Bindings))
    ;   sscasp_directive(Module:pred(Spec))
    ).

%!  domain(:Definition) is det.
%
%   `:- domain day = [mon, tue].` — a domain for a quantifier to range
%   over.  Here because `domain` is a prefix operator and would not fit
%   inside `sscasp_directive/1`'s argument, the same reason `sld/1` and
%   `describe/1` exist.

domain(Module:Definition) :-
    sscasp_directive(Module:domain(Definition)).

load_bindings(Bindings) :-
    (   prolog_load_context(variable_names, Bindings)
    ->  true
    ;   Bindings = []
    ).

%!  sscasp_constraint(+Module, +Body) is det.
%
%   Record a global constraint for Module.  Called by the expansion of
%   `false :- Body`, not by a program directly.

sscasp_constraint(Module, Body) :-
    (   constraint(Module, Seen),
        Seen =@= Body
    ->  true
    ;   assertz(constraint(Module, Body))
    ).

%!  sscasp(:Query) is nondet.
%!  sscasp(:Query, +Options) is nondet.
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
%       The program that was run, as `sscasp_query_clauses/2` returns
%       it.  For a caller that needs what came with the clauses — the
%       descriptions, say, which a renderer reads.

sscasp(Query) :-
    sscasp(Query, []).

sscasp(Module:Query, Options) :-
    sscasp_query_clauses(Module:Query, Terms, Options),
    ignore(option(program(Terms), Options)),
    sscasp(Terms, Query, Model, Justification),
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
    b_setval(sscasp_answer,
             sscasp_answer(Model, Justification, Terms, Options)).

%!  sscasp_show(:Query, +What) is det.
%
%   Write the program a query would run.  What is `code` or
%   `code(Option...)` — the options being the arguments, which is how the
%   old implementation's `scasp_show/2` is spelled:
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
%       `sscasp_query_clauses/2` returns and the only section that shows
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
%   @error domain_error(sscasp_show, What) for anything else.

sscasp_show(Query, What) :-
    (   compound(What),
        compound_name_arguments(What, code, Options0)
    ->  true
    ;   What == code
    ->  Options0 = []
    ;   domain_error(sscasp_show, What)
    ),
    Options = Options0,
    strip_module_goal(Query, Plain),
    sscasp_query_clauses(Query, Terms),
    show_source(Terms, Options),
    portray_listing(Terms, Plain, Options),
    show_compiled(Terms, Options).

show_source(Terms, Options) :-
    (   option(source(true), Options)
    ->  titled('Program as collected', Options),
        forall(member(Term, Terms), show_term(Term))
    ;   true
    ).

show_compiled(Terms, Options) :-
    (   option(compiled(true), Options)
    ->  titled('Emitted clauses', Options),
        sscasp_program(Terms, Clauses),
        forall(member(Clause, Clauses), show_term(Clause))
    ;   true
    ).

%   With `sscasp/ops.pl`'s operator table, not the caller's.  A caller
%   that has not imported them — a plunit unit, say, or a module that
%   only uses `sscasp/2` — would otherwise be shown `not(penguin(X))`
%   where the program says `not penguin(X)`, which is the same term and
%   not the same answer to "what would this run".

show_term(Term) :-
    portray_clause(current_output, Term, [module(sscasp_ops)]).

%!  sscasp_json(:Query, -Dict) is det.
%!  sscasp_json(:Query, -Dict, +Options) is det.
%
%   Every answer to Query as JSON-able data, for a consumer that is a
%   program rather than a person.  `sscasp/json.pl` says what the shape
%   is; `json_write_dict/3` writes it.
%
%   **Deterministic**: the query is solved to exhaustion, because a JSON
%   document is a whole thing rather than something to backtrack into.
%   Options are `sscasp/2`'s, plus
%
%     - limit(Count)
%       Stop after Count answers.  Default `infinite`, which is the
%       right default for a program with a finite answer set and the
%       wrong one for anything else.
%
%   Each answer's dict is built **inside** the solve rather than
%   afterwards.  `findall/3` copies, and a copied answer is no longer
%   the one whose query variables the caller holds — the constraints
%   come along but they are about other variables.  A dict is plain
%   data and copies without that question.

sscasp_json(Query, Dict) :-
    sscasp_json(Query, Dict, []).

sscasp_json(Module:Query, Dict, Options) :-
    option(limit(Limit), Options, infinite),
    answer_dicts(Limit, Module:Query, Dicts),
    answers_json(Dicts, Dict).

answer_dicts(infinite, Query, Dicts) :-
    !,
    findall(D, answer_dict(Query, D), Dicts).
answer_dicts(Limit, Query, Dicts) :-
    findnsols(Limit, D, answer_dict(Query, D), Dicts),
    !.

answer_dict(Module:Query, Dict) :-
    sscasp(Module:Query, [model(Model), tree(Justification)]),
    answer_json(answer(Query, Model, Justification), Dict).

%!  sscasp_query_clauses(:Query, -Terms) is det.
%!  sscasp_query_clauses(:Query, -Terms, +Options) is det.
%
%   The program `sscasp/2` would run: the clauses reachable from Query,
%   the global constraints overlapping them, and the module's directives.
%   Useful on its own for seeing what a query drags in.

sscasp_query_clauses(Query, Terms) :-
    sscasp_query_clauses(Query, Terms, []).

sscasp_query_clauses(Module:Query, Terms, Options) :-
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
%   @error permission_error(collect, sscasp_predicate, PI)

check_one_definition(Callees) :-
    (   member(A:PI, Callees),
        member(B:PI, Callees),
        A \== B
    ->  permission_error(collect, sscasp_predicate, PI)
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
    constraint(Module, Body).
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
%   has no answer set, and `?- sscasp(p).` said yes — nothing reachable
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
goal_callee((A, B), Module, PI) =>
    (   goal_callee(A, Module, PI)
    ;   goal_callee(B, Module, PI)
    ).
goal_callee(not(A), Module, PI) =>
    goal_callee(A, Module, PI).
goal_callee(-(A), Module, PI), callable(A) =>
    functor(A, Name, Arity),
    defined_somewhere(Module, (-)/1, Where),
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

%!  declared(+Module, +Goal) is semidet.
%
%   True when a directive already says what this predicate is, so that
%   whatever clauses happen to be lying around are not it:
%
%     - `:- sld p/2.` — proved by SLD resolution, and the compiler writes
%       a bridge rather than collecting Prolog's clauses;
%     - `:- sscasp_directive(abducible(p(X))).` — assumed either way, and
%       an abducible with clauses is not an abducible;
%     - `:- sscasp_directive(domain(d = [a, b])).` — the compiler writes
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

defined_somewhere(Module, Name/Arity, Where) :-
    functor(Head, Name, Arity),
    (   predicate_property(Module:Head, imported_from(From))
    ->  Where = From
    ;   default_module(Module, Ancestor),
        Ancestor \== Module,
        predicate_property(Ancestor:Head, number_of_clauses(_))
    ->  Where = Ancestor
    ;   Where = Module
    ).
