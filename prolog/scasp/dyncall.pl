:- module(scasp_dyncall,
          [ scasp/1,                    % :Query
            scasp/2,                    % :Query, +Options
            scasp_query_clauses/2,      % :Query, -Clauses
            scasp_query_clauses/3,      % :Query, -Clauses, +Options
            scasp_model/1,              % -Model
            scasp_justification/2,      % -Tree, +Options

            scasp_show/2,               % :Query,+What

            (scasp_dynamic)/1,          % :Spec
            scasp_assert/1,             % :Clause
            scasp_assert/2,             % :Clause
            scasp_retract/1,            % :Clause
            scasp_retractall/1,         % :Head
            scasp_abolish/1,            % :PredicateIndicator
            (#)/1,                      % :Directive
            (#)/2,                      % :Directive, +Pos
            (pred)/1,
            (describe)/1,
            (show)/1,
            (abducible)/1,
            (abducible)/2,
            (prolog)/1,                 % :PIs
            (sld)/1,                    % :PIs

            (#=)/2,
            (#<>)/2,
            (#<)/2,
            (#>)/2,
            (#=<)/2,
            (#>=)/2,

            op(900, fy, not),
            op(950, xfx, ::),           % pred not x :: "...".
            op(1200, fx, #),
            op(1150, fx, pred),
            op(1150, fx, describe),
            op(1150, fx, show),
            op(1150, fx, sld),
            op(1150, fx, abducible),
            op(1150, fx, scasp_dynamic),
            op(700, xfx, #=),
            op(700, xfx, #<>),
            op(700, xfx, #<),
            op(700, xfx, #>),
            op(700, xfx, #=<),
            op(700, xfx, #>=)
          ]).
:- use_module(compile).
:- use_module(embed).
:- use_module(common).
:- use_module(modules).
:- use_module(source_ref).
:- use_module(coverage).
:- use_module(listing).
:- use_module(predicates, [scasp_builtin/1]).
:- use_module(clp/clpq, [apply_clpq_constraints/1]).
:- use_module(pr_rules, [process_pr_pred/5]).

:- use_module(library(apply), [maplist/3, exclude/3, maplist/2]).
:- use_module(library(assoc), [empty_assoc/1, get_assoc/3, put_assoc/4]).
:- use_module(library(error),
              [ instantiation_error/1,
                permission_error/3,
                type_error/2,
                must_be/2
              ]).
:- use_module(library(lists), [member/2, append/3]).
:- use_module(library(modules),
              [in_temporary_module/3, current_temporary_module/1]).
:- use_module(library(option), [option/2]).
:- use_module(library(ordsets), [ord_intersect/2, ord_union/3]).
:- use_module(library(prolog_code), [pi_head/2]).

:- meta_predicate
    scasp(0),
    scasp(0, +),
    scasp_show(:, +),
    scasp_query_clauses(:, -),
    scasp_query_clauses(:, -, +),
    scasp_dynamic(:),
    scasp_assert(:),
    scasp_retract(:),
    scasp_retractall(:),
    scasp_abolish(:),
    pred(:),
    describe(:),
    show(:),
    abducible(:),
    prolog(:),
    sld(:).

/** <module>

This predicate assembles the clauses  that   are  reachable from a given
goal.

Issues:

  - Represent classical negation as -Term?  Alternatives:
    - Just use -Term.  Disadvantage is program analysis and module
      dependencies.
    - Provide goal- and term-expansion to intern the - into the functor
      name.   Disadvantage is that we need scasp_assert/1, etc.
*/

%!  scasp(:Query) is nondet.
%!  scasp(:Query, +Options) is nondet.
%
%   Prove query using s(CASP)  semantics.   This  performs the following
%   steps:
%
%     - Collect (transitively) all clauses that are reachable from
%       Query.
%     - Collect all global constraints whose call-tree overlaps with
%       the query call tree.
%     - Establish the compiled s(CASP) representation in a _temporary
%       module_
%     - Run the s(CASP) solver
%     - Optionally extract the model and justification tree.
%
%   Options are passed to scasp_compile/2.  Other options processed:
%
%     - model(-Model)
%       Unify Model with the s(CASP) model, a list of model terms.
%       See scasp_model/1.
%     - tree(-Tree)
%       Unify Tree with the s(CASP) justification tree.  See
%       scasp_justification/2 for details.
%     - source(Boolean)
%       When `false`, do not include source origin terms into the
%       final tree.
%     - collect(:Goal)
%       Collect the program as if Goal were part of the query, without
%       asking it.  s(CASP) gathers the clauses reachable from the query
%       and the global constraints whose call tree overlaps them, so a
%       constraint mentioning nothing the query reaches is left out --
%       and an odd loop or a `p`/`-p` contradiction elsewhere in the
%       program is left out with it.  That is usually what is wanted: it
%       is what makes `scasp/2` usable on a large program.  Where it is
%       not -- where the answer must respect the whole program however
%       unrelated it looks -- name the rest of it here:
%
%           ?- scasp(q, [collect((p, r(_)))]).
%
%       Only collection is affected.  The query, the model and the
%       justification are still about Query alone.

scasp(Query) :-
    scasp(Query, []).

scasp(Query, Options) :-
    Query = SrcModule:_,
    scasp_query_clauses(Query, Clauses0, Options),
    expand_program(SrcModule,  Clauses0, Clauses1, Query, Query1),
    maplist(qualify, Clauses1, Clauses2),
    qualify_query(Query1, SrcModule:QQuery),
    q_expand_program(SrcModule, Clauses2, Clauses, QQuery, QQuery1),
    in_temporary_module(
        Module,
        prepare(Clauses, Module, Options),
        scasp_call_and_results(Module:QQuery1, SrcModule, Options)).

prepare(Clauses, Module, Options) :-
    scasp_compile(Module:Clauses, Options),
    (   option(write_program(_), Options)
    ->  scasp_portray_program(Module:Options)
    ;   true
    ).

qualify_query(M:Q0, M:Q) :-
    qualify_body(Q0, M, Q1),
    intern_negation(Q1, Q).

expand_program(SrcModule, Clauses, Clauses1, QQuery, QQuery1) :-
    current_predicate(SrcModule:scasp_expand/4),
    SrcModule:scasp_expand(Clauses, Clauses1, QQuery, QQuery1),
    !.
expand_program(_, Clauses, Clauses, QQuery, QQuery).

q_expand_program(SrcModule, Clauses, Clauses1, QQuery, QQuery1) :-
    current_predicate(SrcModule:scasp_expand_program/4),
    SrcModule:scasp_expand_program(Clauses, Clauses1, QQuery, QQuery1),
    !.
q_expand_program(_, Clauses, Clauses, QQuery, QQuery).


scasp_call_and_results(Query, SrcModule, Options) :-
    scasp_embed:scasp_call(Query),
    scasp_coverage,
    (   option(model(Model), Options)
    ->  scasp_model(SrcModule:Model)
    ;   true
    ),
    (   option(tree(Tree), Options)
    ->  scasp_justification(SrcModule:Tree, Options)
    ;   true
    ).

%!  scasp_show(:Query, +What)
%
%   Show some aspect of the translated s(CASP) program.  Currently
%   What is one of:
%
%     - code(Options)
%       Show the collected program.  By default shows the query
%       and user program.  To show only the integrity constraints,
%       use:
%
%           ?- scasp_show(Query, code(user(false), constraints(true))).
%
%   An option given as a bare name means `true`: `code(constraints)` is
%   `code(constraints(true))`.

scasp_show(Query, What) :-
    What =.. [Type|Options0],
    maplist(show_option, Options0, Options),
    scasp_show(Query, Type, Options).

show_option(Name, Option), atom(Name) =>
    Option =.. [Name, true].
show_option(Option0, Option) =>
    Option = Option0.

scasp_show(Query, code, Options) =>
    Query = SrcModule:_,
    scasp_query_clauses(Query, Clauses0),
    expand_program(SrcModule,  Clauses0, Clauses1, Query, Query1),
    maplist(qualify, Clauses1, Clauses2),
    qualify_query(Query1, SrcModule:QQuery),
    q_expand_program(SrcModule, Clauses2, Clauses, QQuery, _QQuery1),
    in_temporary_module(
        Module,
        prepare(Clauses, Module, []),
        scasp_portray_program(Module:[source_module(SrcModule)|Options])).

%!  scasp_query_clauses(:Query, -Clauses) is det.
%!  scasp_query_clauses(:Query, -Clauses, +Options) is det.
%
%   Collect the clauses relevant to Query: those reachable from it, plus
%   the global constraints whose call tree overlaps them.  `collect(Goal)`
%   widens the first set without widening the query -- see scasp/2.
%
%   @arg Clauses is a list of source(ClauseRef, Clause).

:- det(scasp_query_clauses/2).
:- det(scasp_query_clauses/3).

scasp_query_clauses(Query, Clauses) :-
    scasp_query_clauses(Query, Clauses, []).

scasp_query_clauses(M:Query, Clauses, Options) :-
    (   option(collect(Extra), Options)
    ->  strip_module_goal(Extra, M, Goal),
        Collect = (Query, Goal)
    ;   Collect = Query
    ),
    query_callees(M:Collect, Callees0),
    maplist(check_not_reserved, Callees0),
    include_global_constraint(Callees0, Constraints, Callees),
    findall(Clause, scasp_clause(Callees, Clause), Clauses, Constraints).

strip_module_goal(_:Goal, _, Goal) :- !.
strip_module_goal(Goal, _, Goal).

%!  check_not_reserved(+PI) is det.
%
%   s(CASP) matches some goals as  control  constructs before considering
%   them as calls to a predicate, so  a program that *defines* one cannot
%   win: the construct is matched first and the definition is never used.
%   Report that rather than letting the  clause  be collected and fail in
%   a way that does not mention the name.
%
%   Only forall/2 needs this.  findall/3 is  a static built-in, so Prolog
%   already refuses to let a program define  it, and shadowing not/1 with
%   s(CASP) negation is intended and documented behaviour.
%
%   @error permission_error(define, scasp_control_construct, PI)

check_not_reserved(_M:PI) :-
    scasp_control_construct(PI),
    !,
    permission_error(define, scasp_control_construct, PI).
check_not_reserved(_).

%!  scasp_control_construct(?PI) is nondet.

scasp_control_construct(forall/2).

scasp_clause(Callees, source(ClauseRef, M:(Head:- Body))) :-
    member(PI, Callees),
    pi_head(PI, M:Head),
    @(clause(Head, Body0, ClauseRef), M),
    unqualify_body(M, Body0, Body).

%!  unqualify_body(+M, :Body0, -Body) is det.
%
%   Multifile clauses use the I_CONTEXT instruction, causing the body to
%   be decompiled as Module:RawBody. s(CASP) does not deal with that.
%
%   Under `-O`, the compiler inlines forall/2, once/1 and ignore/1, and
%   clause/2 returns what it made of them: forall(Cond, Action) becomes
%   `\+ (Cond, \+ Action)` with the conjunctions flattened, once(G)
%   becomes `(G->true)` and ignore(G) `(G->true;true)`, where a variable
%   G is wrapped in call/1.  We put them back, so a program reads the
%   same with or without `-O`.  The same shapes written by hand are read
%   back too, which does not change what they mean to Prolog.

unqualify_body(M, M:Body0, Body) =>
    decompiled(Body0, Body).
unqualify_body(_, Body0, Body) =>
    decompiled(Body0, Body).

decompiled(Var, Goal), var(Var) =>
    Goal = Var.
decompiled(\+ Conj, Goal), last_negated(Conj, Cond0, Action0) =>
    decompiled(Cond0, Cond),
    decompiled(Action0, Action),
    Goal = forall(Cond, Action).
decompiled((G0->true;true), Goal) =>
    inlined_goal(G0, G1),
    decompiled(G1, G),
    Goal = ignore(G).
decompiled((G0->true), Goal) =>
    inlined_goal(G0, G1),
    decompiled(G1, G),
    Goal = once(G).
decompiled((A0,B0), Goal) =>
    decompiled(A0, A),
    decompiled(B0, B),
    Goal = (A,B).
decompiled(not(A0), Goal) =>
    decompiled(A0, A),
    Goal = not(A).
decompiled(findall(T,G0,L), Goal) =>
    decompiled(G0, G),
    Goal = findall(T,G,L).
decompiled(forall(C0,A0), Goal) =>
    decompiled(C0, C),
    decompiled(A0, A),
    Goal = forall(C,A).
decompiled(M:G0, Goal) =>
    decompiled(G0, G),
    Goal = M:G.
decompiled(Goal0, Goal) =>
    Goal = Goal0.

inlined_goal(call(G0), G), var(G0) =>
    G = G0.
inlined_goal(G0, G) =>
    G = G0.

%   `(C1, C2, \+ A)` is `Cond = (C1, C2)`, `Action = A`.

last_negated((C, Rest), Cond, Action) :-
    nonvar(Rest),
    (   Rest = (\+ Action0)
    ->  Cond = C,
        Action = Action0
    ;   last_negated(Rest, Cond1, Action),
        Cond = (C, Cond1)
    ).

qualify(source(Ref, Clause), Q) =>
    Q = source(Ref, QClause),
    qualify(Clause, QClause).
qualify(M:(Head :- true), Q) =>
    qualify_body(Head, M, Q).
qualify(M:(Head :- Body), Q) =>
    qualify_body((Head:-Body), M, Q).
qualify(M:(:- Body), Q) =>
    Q = (:- Constraint),
    qualify_body(Body, M, Constraint).

%!  query_callees(:Query, -Callees) is det.
%
%   True when Callees is a list   of predicate indicators for predicates
%   reachable from Query.
%
%   @arg Callees is an ordered set.

query_callees(M:Query, Callees) :-
    findall(Call, body_calls_pi(Query,M,Call), Calls0),
    sort(Calls0, Calls),
    callee_graph(Calls, Callees).

body_calls_pi(Query, M, PI) :-
    body_calls(Query, M, Call),
    pi_head(PI, Call).

callee_graph(Preds, Nodes) :-
    empty_assoc(Expanded),
    callee_closure(Preds, Expanded, Preds, Nodes0),
    sort(Nodes0, Nodes).

callee_closure([], _, Preds, Preds).
callee_closure([H|T], Expanded, Preds0, Preds) :-
    (   get_assoc(H, Expanded, _)
    ->  callee_closure(T, Expanded, Preds0, Preds)
    ;   put_assoc(H, Expanded, true, Expanded1),
        pi_head(H, Head),
        predicate_callees(Head, Called),
        exclude(expanded(Expanded1), Called, New),
        append(New, T, Agenda),
        append(New, Preds0, Preds1),
        callee_closure(Agenda, Expanded1, Preds1, Preds)
    ).

expanded(Assoc, PI) :-
    get_assoc(PI, Assoc, _).

%!  include_global_constraint(+Callees, -Constraints, -AllCallees) is det

include_global_constraint(Callees0, Constraints, Callees) :-
    include_global_constraint(Callees0, Callees, [], Constraints).

include_global_constraint(Callees0, Callees, Constraints0, Constraints) :-
    global_constraint(Constraint),
    Constraint = source(_, Rule),       % Rule = M:(:-Body)
    \+ ( member(source(_, Rule0), Constraints0),
         Rule =@= Rule0
       ),
    Rule = M:(:-Body),
    query_callees(M:Body, Called),
    ord_intersect(Callees0, Called),
    !,
    ord_union(Callees0, Called, Callees1),
    include_global_constraint(Callees1, Callees,
                              [Constraint|Constraints0], Constraints).
include_global_constraint(Callees, Callees, Constraints, Constraints).


global_constraint(source(Ref, M:(:- Body))) :-
    (   current_temporary_module(M)
    ;   current_module(M)
    ),
    current_predicate(M:(-)/0),
    \+ predicate_property(M:(-), imported_from(_)),
    @(clause(-, Body0, Ref), M),
    unqualify_body(M, Body0, Body).

%!  predicate_callees(:Head, -Callees) is det.
%
%   True when Callees is the list of _direct_ callees from Head.  Each
%   callee is a _predicate indicator_.

:- dynamic predicate_callees_c/4.

predicate_callees(M:Head, Callees) :-
    predicate_callees_c(Head, M, Gen, Callees0),
    predicate_generation(M:Head, Gen),
    !,
    Callees = Callees0.
predicate_callees(M:Head, Callees) :-
    module_property(M, class(temporary)),
    !,
    predicate_callees_nc(M:Head, Callees).
predicate_callees(M:Head, Callees) :-
    retractall(predicate_callees_c(Head, M, _, _)),
    predicate_callees_nc(M:Head, Callees0),
    predicate_generation(M:Head, Gen),
    assertz(predicate_callees_c(Head, M, Gen, Callees0)),
    Callees = Callees0.

predicate_callees_nc(Head, Callees) :-
    findall(Callee, predicate_calls(Head, Callee), Callees0),
    sort(Callees0, Callees).

predicate_calls(Head0, PI) :-
    generalise(Head0, M:Head),
    @(clause(Head, Body0), M),
    unqualify_body(M, Body0, Body),
    body_calls(Body, M, Callee),
    pi_head(PI, Callee).

body_calls(Goal, _M, _), var(Goal) =>
    instantiation_error(Goal).
body_calls(true, _M, _) => fail.
body_calls((A,B), M, Callee) =>
    (   body_calls(A, M, Callee)
    ;   body_calls(B, M, Callee)
    ).
body_calls(not(A), M, Callee) =>
    body_calls(A, M, Callee).
body_calls(findall(_,G,_), M, Callee) =>
    body_calls(G, M, Callee).
body_calls(N, M, Callee), rm_classic_negation(N,A) =>
    body_calls(A, M, Callee).
body_calls(M:A, _, Callee), atom(M) =>
    body_calls(A, M, Callee).
body_calls(G, _M, _CalleePM), callable(G), scasp_builtin(G) =>
    fail.
body_calls(G, M, _CalleePM), callable(G),
    current_predicate(M:pr_prolog_predicate/2),
    M:pr_prolog_predicate(G,_) =>
    fail.
body_calls(G, M, CalleePM), callable(G) =>
    implementation(M:G, Callee0),
    generalise(Callee0, Callee),
    (   predicate_property(Callee, interpreted),
        \+ predicate_property(Callee, meta_predicate(_))
    ->  pm(Callee, CalleePM)
    ;   \+ predicate_property(Callee, _)
    ->  pm(Callee, CalleePM)            % undefined
    ;   pi_head(CalleePI, Callee),
        permission_error(scasp, procedure, CalleePI)
    ).
body_calls(G, _, _) =>
    type_error(callable, G).

rm_classic_negation(-Goal, Goal) :-
    !.
rm_classic_negation(Goal, PGoal) :-
    functor(Goal, Name, _),
    atom_concat(-, Plain, Name),
    Goal  =.. [Name|Args],
    PGoal =.. [Plain|Args].

pm(P, P).
pm(M:P, M:MP) :-
    intern_negation(-P, MP).

generalise(M:Head0, Gen), atom(M) =>
    Gen = M:Head,
    generalise(Head0, Head).
generalise(-Head0, Gen) =>
    Gen = -Head,
    generalise(Head0, Head).
generalise(Head0, Head) =>
    functor(Head0, Name, Arity),
    functor(Head, Name, Arity).

predicate_generation(Head, Gen) :-
    predicate_property(Head, last_modified_generation(Gen0)),
    !,
    Gen = Gen0.
predicate_generation(_, 0).


		 /*******************************
		 *   MANIPULATING THE PROGRAM	*
		 *******************************/

%!  scasp_dynamic(:Spec) is det.
%
%   Declare a predicates as dynamic or thread_local.  Usage patterns:
%
%      :- scasp_dynamic p/1.
%      :- scasp_dynamic p/1 as shared.

scasp_dynamic(M:Spec) :-
    scasp_dynamic(Spec, M, private).
scasp_dynamic(M:(Spec as Scoped)) :-
    scasp_dynamic(Spec, M, Scoped).

scasp_dynamic((A,B), M, Scoped) =>
    scasp_dynamic(A, M, Scoped),
    scasp_dynamic(B, M, Scoped).
scasp_dynamic(Name/Arity, M, Scoped) =>
    atom_concat(-, Name, NName),
    (   Scoped == shared
    ->  dynamic((M:Name/Arity,
                 M:NName/Arity))
    ;   thread_local((M:Name/Arity,
                     M:NName/Arity))
    ).

:- multifile system:term_expansion/2.

%   `ours/0' is below, with the rest of the expansions and the reason
%   they all ask it.  This one is a `system:' expansion rather than a
%   `user:' one, which loses to a `user:' clause -- but losing is not
%   the same as not firing, and without the guard it still claims the
%   directive from a file that never asked this library for it.

system:term_expansion((:- scasp_dynamic(Spec)), Directives) :-
    ours,
    phrase(scasp_dynamic_direcives(Spec), Directives).

scasp_dynamic_direcives(Spec as Scope) -->
    !,
    scasp_dynamic_direcives(Spec, Scope).
scasp_dynamic_direcives(Spec) -->
    !,
    scasp_dynamic_direcives(Spec, private).

scasp_dynamic_direcives(Var, _) -->
    { var(Var), !, fail }.
scasp_dynamic_direcives((A,B), Scope) -->
    scasp_dynamic_direcives(A, Scope),
    scasp_dynamic_direcives(B, Scope).
scasp_dynamic_direcives(Name/Arity, Scope) -->
    { atom_concat(-, Name, NName) },
    (   {Scope == shared}
    ->  [(:- dynamic((Name/Arity, NName/Arity)))]
    ;   [(:- thread_local((Name/Arity, NName/Arity)))]
    ).


%!  scasp_assert(:Clause) is det.
%!  scasp_retract(:Clause) is nondet.
%!  scasp_retractall(:Head) is det.
%
%   Wrappers for assertz/1, retract/1 and   retractall/1  that deal with
%   sCASP terms which may have a head or  body terms that are wrapped in
%   `-(Term)`, indicating classical negation.  Also   deals  with global
%   constraints written in any of these formats:
%
%     - `false :- Constraint`.
%     - `:- Constraint`.

scasp_assert(Clause) :-
    scasp_assert(Clause, false).

scasp_assert(M:(-Head :- Body0), Pos) =>
    intern_negation(-Head, MHead),
    expand_goal(Body0, Body),
    scasp_assert_(M:(MHead :- Body), Pos).
scasp_assert(M:(-Head), Pos) =>
    intern_negation(-Head, MHead),
    scasp_assert_(M:(MHead), Pos).
scasp_assert(M:(:- Body0), Pos) =>
    expand_goal(Body0, Body),
    scasp_assert_(M:((-) :- Body), Pos).
scasp_assert(M:(false :- Body0), Pos) =>
    expand_goal(Body0, Body),
    scasp_assert_(M:((-) :- Body), Pos).
scasp_assert(M:(Head :- Body0), Pos) =>
    expand_goal(Body0, Body),
    scasp_assert_(M:(Head :- Body), Pos).
scasp_assert(Head, Pos) =>
    scasp_assert_(Head, Pos).

scasp_assert_(Clause, false) =>
    assertz(Clause).
scasp_assert_(Clause, Pos) =>
    assertz(Clause, Ref),
    assertz(scasp_dynamic_clause_position(Ref, Pos)).

scasp_assert_into(M, Pos, Rule) :-
    scasp_assert(M:Rule, Pos).

scasp_retract(M:(-Head :- Body0)) =>
    intern_negation(-Head, MHead),
    expand_goal(Body0, Body),
    retract(M:(MHead :- Body)).
scasp_retract(M:(-Head)) =>
    intern_negation(-Head, MHead),
    retract(M:(MHead)).
scasp_retract(M:(:- Body0)) =>
    expand_goal(Body0, Body),
    retract(M:((-) :- Body)).
scasp_retract(M:(false :- Body0)) =>
    expand_goal(Body0, Body),
    retract(M:((-) :- Body)).
scasp_retract(M:(Head :- Body0)) =>
    expand_goal(Body0, Body),
    retract(M:(Head :- Body)).
scasp_retract(Head) =>
    retract(Head).

scasp_retractall(M:(-Head)) =>
    intern_negation(-Head, MHead),
    retractall(M:MHead).
scasp_retractall(Head) =>
    retractall(Head).

%!  scasp_abolish(:PredicateIndicator) is det.
%
%   Remove all facts  for  both   PredicateIndicator  and  its classical
%   negation.

scasp_abolish(M:(Name/Arity)) =>
    pi_head(Name/Arity, Head),
    scasp_retractall(M:Head),
    scasp_retractall(M:(-Head)).


		 /*******************************
		 *          DIRECTIVES		*
		 *******************************/

%!  #(:Directive)
%
%   Handle s(CASP) directives.  Same as ``:- Directive.``.  Provides
%   compatibility with sCASP sources as normally found.

#(Directive) :- #(Directive, false).

#(M:pred(Spec), _)        => pred(M:Spec).
#(M:show(Spec), _)        => show(M:Spec).
#(M:abducible(Spec), Pos) => abducible(M:Spec, Pos).

pred(M:(Spec :: Template)) =>
    process_pr_pred(Spec :: Template, Atom, Children, Cond, Human),
    assertz(M:pr_pred_predicate(Atom, Children, Cond, Human)).

%!  describe(:Template) is det.
%
%   Same as pred/1, using the name of library(sscasp), where `pred` is
%   used for declaring the domain of arguments.

describe(M:Spec) =>
    pred(M:Spec).

show(M:PIs) =>
    phrase(show(PIs), Clauses),
    maplist(assert_show(M), Clauses).

assert_show(M, Clause) :-
    assertz(M:Clause).

show(Var) -->
    { var(Var),
      instantiation_error(Var)
    }.
show((A,B)) -->
    !,
    show(A),
    show(B).
show(not(PI)) -->
    !,
    { pi_head(PI, Head) },
    [ pr_show_predicate(not(Head)) ].
show(-Name/Arity) -->
    !,
    { pi_head(Name/Arity, Head),
      intern_negation(-Head, MHead)
    },
    [ pr_show_predicate(MHead) ].
show(-PI) -->
    !,
    { pi_head(PI, Head),
      intern_negation(-Head, MHead)
    },
    [ pr_show_predicate(MHead) ].
show(PI) -->
    { pi_head(PI, Head) },
    [ pr_show_predicate(Head) ].

%!  abducible(:Spec)
%
%   Declare Spec, a comma list  of   _heads_  to be _abducible_, meaning
%   they can both be in or outside the model.

abducible(Spec) :- abducible(Spec, false).

abducible(M:(A,B), Pos) =>
    abducible(M:A, Pos),
    abducible(M:B, Pos).
abducible(M:Head, Pos), callable(Head) =>
    abducible_rules(Head, Rules),
    maplist(scasp_assert_into(M, Pos), Rules).

abducible_rules(Head,
                [ (Head    :- AHead1),
                  (NegHead :- AHead2),
                  (AHead1  :- not AHead2),
                  (AHead2  :- not AHead1)
                ]) :-
    abducible_head('abducible$',  Head, AHead1),
    abducible_head('abducible$$', Head, AHead2),
    intern_negation(-Head, NegHead).

abducible_head(Prefix, Head, AHead) :-
    format(atom(AHead), '~w~k$', [Prefix, Head]).

abducible(Var) -->
    { var(Var),
      instantiation_error(Var)
    }.
abducible((A,B)) -->
    !,
    abducible(A),
    abducible(B).
abducible(Head) -->
    { must_be(callable, Head),
      abducible_rules(Head, Clauses)
    },
    list(Clauses).

list([]) --> [].
list([H|T]) --> [H], list(T).

%!  prolog(:PIs) is det.
%
%   Declare PIs to be interpreted as   Prolog  predicates. The directive
%   allows for the  following,  where  `Type`   is  one  of  `justified`
%   (default) or `opaque`, meaning that we   call  the predicate without
%   creating a justification tree.
%
%   `prolog` is no longer a prefix operator.  Use `:- sld PIs as Type`,
%   which library(sscasp) understands as well.

prolog(M:PIs) =>
    phrase(prolog_decl(PIs), Clauses),
    maplist(assert_show(M), Clauses).

%!  sld(:PIs) is det.
%
%   Same as prolog/1, using the name of library(sscasp):
%
%       :- sld PIs as Type.
%
%   `prolog` is not an operator, so `:- prolog p/1.` no longer reads.
%   Use `:- sld p/1.`, which works with both libraries.

sld(M:PIs) =>
    prolog(M:PIs).

prolog_decl(Var) -->
    { var(Var),
      !,
      instantiation_error(Var)
    }.
prolog_decl(PIs as Type) -->
    !,
    prolog_decls(PIs, Type).
prolog_decl(PIs) -->
    prolog_decls(PIs, justified).

prolog_decls(Var, _Type) -->
    { var(Var),
      instantiation_error(Var)
    }.
prolog_decls((A,B), Type) -->
    !,
    prolog_decls(A, Type),
    prolog_decls(B, Type).
prolog_decls(PI, Type) -->
    { pi_head(PI, Head),
      must_be(oneof([opaque,justified]), Type)
    },
    [ pr_prolog_predicate(Head, Type) ].


		 /*******************************
		 *            EXPAND		*
		 *******************************/

%!  ours is semidet.
%
%   True when the file being compiled uses **this** library.  Every
%   expansion below asks it first, and that is not tidiness: they are
%   installed in `user`, so without it they fire for every file in the
%   process.
%
%   That matters because `library(sscasp)` is the same language with the
%   same spellings and a different representation.  A file of *its*
%   writing `-dead(X) :- alive(X).` had the head interned here into a
%   predicate named `'-dead'`, where that tree keeps a clause of `-/1`;
%   the clause then existed under a name nothing would look for, and the
%   query raised `existence_error(scasp_predicate, dead/1)` on a program
%   that is perfectly well formed.  Which library was loaded first
%   decided it.
%
%   `scasp/2` is the marker rather than any one directive, because it is
%   what a file imports the library *for*, and the property survives the
%   re-export through `library(scasp)`.  `library(sscasp)` asks the
%   mirror-image question in `sscasp.pl:ours/1`, so a file gets exactly
%   one library's expansions and load order decides nothing.

ours :-
    prolog_load_context(module, Module),
    predicate_property(Module:scasp(_, _), imported_from(scasp_dyncall)).

user:term_expansion(-Fact, MFact) :-
    callable(Fact),
    Fact \= _:_,
    ours,
    intern_negation(-Fact, MFact).
user:term_expansion((-Head :- Body), (MHead :- Body)) :-
    callable(Head),
    Head \= _:_,
    ours,
    intern_negation(-Head, MHead).
user:term_expansion((false :- Body), ((-) :- Body)) :-
    ours.
user:term_expansion((:- describe(SpecIn)), Clauses) :-
    user:term_expansion((:- pred(SpecIn)), Clauses).
user:term_expansion((# describe(SpecIn)), Clauses) :-
    user:term_expansion((# pred(SpecIn)), Clauses).
user:term_expansion((:- sld(SpecIn)), Clauses) :-
    user:term_expansion((:- prolog(SpecIn)), Clauses).
user:term_expansion((# sld(SpecIn)), Clauses) :-
    user:term_expansion((# prolog(SpecIn)), Clauses).
user:term_expansion((:- pred(SpecIn)),
                    [ (:- dynamic(pr_pred_predicate/4)), (:- discontiguous(pr_pred_predicate/4)),
                      pr_pred_predicate(Atom, Children, Cond, Human)
                    ]) :-
    ours,
    process_pr_pred(SpecIn, Atom, Children, Cond, Human).
user:term_expansion((:- show(SpecIn)),
                    [(:- dynamic(pr_show_predicate/1)), (:- discontiguous(pr_show_predicate/1))|Clauses]) :-
    ours,
    phrase(show(SpecIn), Clauses).
user:term_expansion((:- abducible(SpecIn)), Clauses) :-
    ours,
    phrase(abducible(SpecIn), Clauses).
user:term_expansion((# pred(SpecIn)),
                    [ (:- dynamic(pr_pred_predicate/4)), (:- discontiguous(pr_pred_predicate/4)),
                      pr_pred_predicate(Atom, Children, Cond, Human)
                    ]) :-
    ours,
    process_pr_pred(SpecIn, Atom, Children, Cond, Human).
user:term_expansion((# show(SpecIn)),
                    [(:- dynamic(pr_show_predicate/1)), (:- discontiguous(pr_show_predicate/1))|Clauses]) :-
    ours,
    phrase(show(SpecIn), Clauses).
user:term_expansion((# abducible(SpecIn)), Clauses) :-
    ours,
    phrase(abducible(SpecIn), Clauses).
user:term_expansion((# prolog(SpecIn)),
                    [(:- dynamic(pr_prolog_predicate/2)), (:- discontiguous(pr_prolog_predicate/2))|Clauses]) :-
    ours,
    phrase(prolog_decl(SpecIn), Clauses).
user:term_expansion((:- prolog(SpecIn)),
                    [(:- dynamic(pr_prolog_predicate/2)), (:- discontiguous(pr_prolog_predicate/2))|Clauses]) :-
    ours,
    phrase(prolog_decl(SpecIn), Clauses).

user:goal_expansion(-Goal, MGoal) :-
    callable(Goal),
    ours,
    intern_negation(-Goal, MGoal).


		 /*******************************
		 *              CLP		*
		 *******************************/

%!  #=(?A, ?B).
%!  #<>(?A, ?B).
%!  #<(?A, ?B).
%!  #>(?A, ?B).
%!  #>=(?A, ?B).
%!  #=<(?A, ?B).
%
%   Implementation of the s(CASP) constraints.   This  implementation is
%   normally not used and mostly makes the program analysis work.

A #=  B :- apply_clpq_constraints(A #=  B).
A #<> B :- apply_clpq_constraints(A #<> B).
A #<  B :- apply_clpq_constraints(A #<  B).
A #>  B :- apply_clpq_constraints(A #>  B).
A #=< B :- apply_clpq_constraints(A #=< B).
A #>= B :- apply_clpq_constraints(A #>= B).


		 /*******************************
		 *            SOURCE		*
		 *******************************/

:- multifile
    prolog_clause:unify_goal/5.

prolog_clause:unify_goal(scasp(RGoal, Options),
                         scasp(CGoal, Options),
                         _Module,
                         TermPos, TermPos) :-
    intern_negation(RGoal, RGoal2),
    RGoal2 =@= CGoal.


		 /*******************************
		 *            SANDBOX		*
		 *******************************/

:- multifile
    sandbox:safe_meta_predicate/1.

% scasp/1 is safe as it only allows for pure Prolog predicates
% and limited arithmetic.  Note that this does allow calling e.g.
% member/2. s(CASP) does not allow for calling _qualified goals,
% lists:member(...),

sandbox:safe_meta(scasp_dyncall:scasp(_), []).
sandbox:safe_meta(scasp_dyncall:scasp(_, _), []).

