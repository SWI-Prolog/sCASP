:- module(scasp_dyncall,
          [ scasp/2,                    % :Query, +Options
            scasp_query_clauses/2,      % :Query, -Clauses
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
            (show)/1,
            (abducible)/1,
            (abducible)/2,

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
            op(1150, fx, show),
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
    scasp(0, +),
    scasp_show(:, +),
    scasp_query_clauses(:, -),
    scasp_dynamic(:),
    scasp_assert(:),
    scasp_retract(:),
    scasp_retractall(:),
    scasp_abolish(:),
    pred(:),
    show(:),
    abducible(:).

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

scasp(Query, Options) :-
    Query = SrcModule:_,
    scasp_query_clauses(Query, Clauses0),
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

scasp_show(Query, What) :-
    What =.. [Type|Options],
    scasp_show(Query, Type, Options).

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
%
%   @arg Clauses is a list of source(ClauseRef, Clause).

:- det(scasp_query_clauses/2).

scasp_query_clauses(Query, Clauses) :-
    query_callees(Query, Callees0),
    include_global_constraint(Callees0, Constraints, Callees),
    findall(Clause, scasp_clause(Callees, Clause), Clauses, Constraints).

scasp_clause(Callees, source(ClauseRef, M:(Head:- Body))) :-
    member(PI, Callees),
    pi_head(PI, M:Head),
    @(clause(Head, Body, ClauseRef), M).

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
    @(clause(-, Body, Ref), M).

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
    @(clause(Head, Body), M),
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

system:term_expansion((:- scasp_dynamic(Spec)), Directives) :-
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



		 /*******************************
		 *            EXPAND		*
		 *******************************/

user:term_expansion(-Fact, MFact) :-
    callable(Fact),
    Fact \= _:_,
    intern_negation(-Fact, MFact).
user:term_expansion((-Head :- Body), (MHead :- Body)) :-
    callable(Head),
    Head \= _:_,
    intern_negation(-Head, MHead).
user:term_expansion((false :- Body), ((-) :- Body)).
user:term_expansion((:- pred(SpecIn)),
                    pr_pred_predicate(Atom, Children, Cond, Human)) :-
    process_pr_pred(SpecIn, Atom, Children, Cond, Human).
user:term_expansion((:- show(SpecIn)), Clauses) :-
    phrase(show(SpecIn), Clauses).
user:term_expansion((:- abducible(SpecIn)), Clauses) :-
    phrase(abducible(SpecIn), Clauses).
user:term_expansion((# pred(SpecIn)),
                    pr_pred_predicate(Atom, Children, Cond, Human)) :-
    process_pr_pred(SpecIn, Atom, Children, Cond, Human).
user:term_expansion((# show(SpecIn)), Clauses) :-
    phrase(show(SpecIn), Clauses).
user:term_expansion((# abducible(SpecIn)), Clauses) :-
    phrase(abducible(SpecIn), Clauses).

user:goal_expansion(-Goal, MGoal) :-
    callable(Goal),
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

sandbox:safe_meta(scasp_dyncall:scasp(_, _), []).

