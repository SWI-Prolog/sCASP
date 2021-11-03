:- module(scasp_dyncall,
          [ scasp/1,                    % :Query
            scasp/2,                    % :Query, +Options
            scasp_query_clauses/2,      % :Query, -Clauses
            scasp_model/1,              % -Model
            scasp_justification/2,      % -Tree, +Options

            scasp_show/2,               % :Query,+What

            (scasp_dynamic)/1,          % :Spec
            scasp_assert/1,             % :Clause
            scasp_retract/1,            % :Clause
            scasp_retractall/1,         % :Head
            scasp_abolish/1,            % :PredicateIndicator
            (#)/1,                      % :Directive
            (pred)/1,
            (show)/1,
            (abducible)/1,

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
:- use_module(listing).
:- use_module(pr_rules, [process_pr_pred/2]).

:- meta_predicate
    scasp(0),
    scasp(0, +),
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

scasp(Query) :-
    scasp(Query, []).

scasp(Query, Options) :-
    scasp_query_clauses(Query, Clauses),
    qualify(Query, _:QQuery),
    in_temporary_module(
        Module,
        prepare(Clauses, Module, Options),
        scasp_embed:scasp_call(Module:QQuery)).

prepare(Clauses, Module, Options) :-
    scasp_compile(Module:Clauses, Options),
    (   debugging(scasp(code))
    ->  scasp_portray_program(Module:[])
    ;   true
    ).

qualify(M:Q0, M:Q) :-
    qualify(Q0, M, Q1),
    intern_negation(Q1, Q).

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
    scasp_query_clauses(Query, Clauses),
    in_temporary_module(
        Module,
        prepare(Clauses, Module, []),
        scasp_portray_program(Module:Options)).

%!  scasp_query_clauses(:Query, -Clauses) is det.

:- det(scasp_query_clauses/2).

scasp_query_clauses(Query, Clauses) :-
    query_callees(Query, Callees0),
    include_global_constraint(Callees0, Constraints, Callees),
    findall(Clause, scasp_clause(Callees, Clause), Clauses, QConstraints),
    maplist(mkconstraint, Constraints, QConstraints).

scasp_clause(Callees, Clause) :-
    member(PI, Callees),
    pi_head(PI, M:Head),
    @(clause(Head, Body), M),
    mkclause(Head, Body, M, Clause).

mkclause(Head, true, M, Clause) =>
    qualify(Head, M, Clause).
mkclause(Head, Body, M, Clause) =>
    qualify((Head:-Body), M, Clause).

mkconstraint(M:Body, (:- Constraint)) :-
    qualify(Body, M, Constraint).

qualify(-(Head), M, Q) =>
    Q = -QHead,
    qualify(Head, M, QHead).
qualify(not(Head), M, Q) =>
    Q = not(QHead),
    qualify(Head, M, QHead).
qualify((A,B), M, Q) =>
    Q = (QA,QB),
    qualify(A, M, QA),
    qualify(B, M, QB).
qualify((A:-B), M, Q) =>
    Q = (QA:-QB),
    qualify(A, M, QA),
    qualify(B, M, QB).
qualify(G, M, Q), callable(G) =>
    (   built_in(G)
    ->  Q = G
    ;   implementation(M:G, Callee),
        encoded_module_term(Callee, Q)
    ).

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
    global_constraint(Body),
    \+ ( member(Body0, Constraints0),
         Body =@= Body0
       ),
    query_callees(Body, Called),
    ord_intersect(Callees0, Called),
    !,
    ord_union(Callees0, Called, Callees1),
    include_global_constraint(Callees1, Callees,
                              [Body|Constraints0], Constraints).
include_global_constraint(Callees, Callees, Constraints, Constraints).


global_constraint(M:Body) :-
    (   current_temporary_module(M)
    ;   current_module(M)
    ),
    current_predicate(M:(-)/0),
    \+ predicate_property(M:(-), imported_from(_)),
    @(clause(-, Body), M).

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

body_calls(true, _M, _) => fail.
body_calls((A,B), M, Callee) =>
    (   body_calls(A, M, Callee)
    ;   body_calls(B, M, Callee)
    ).
body_calls(not(A), M, Callee) =>
    body_calls(A, M, Callee).
body_calls(N, M, Callee), rm_classic_negation(N,A) =>
    body_calls(A, M, Callee).
body_calls(M:A, _, Callee), atom(M) =>
    body_calls(A, M, Callee).
body_calls(G, _M, _CalleePM), callable(G), built_in(G) =>
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

built_in(_<_).
built_in(_=<_).
built_in(_>_).
built_in(_>=_).
built_in(_=_).

built_in(_#<_).
built_in(_#=<_).
built_in(_#>_).
built_in(_#>=_).
built_in(_#=_).
built_in(_#<>_).

built_in(_\=_).
built_in(_ is _).

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

implementation(M0:Head, M:Head) :-
    predicate_property(M0:Head, imported_from(M1)),
    !,
    M = M1.
implementation(Head, Head).

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

scasp_assert(M:(-Head :- Body0)) =>
    intern_negation(-Head, MHead),
    expand_goal(Body0, Body),
    assertz(M:(MHead :- Body)).
scasp_assert(M:(-Head)) =>
    intern_negation(-Head, MHead),
    assertz(M:(MHead)).
scasp_assert(M:(:- Body0)) =>
    expand_goal(Body0, Body),
    assertz(M:((-) :- Body)).
scasp_assert(M:(false :- Body0)) =>
    expand_goal(Body0, Body),
    assertz(M:((-) :- Body)).
scasp_assert(M:(Head :- Body0)) =>
    expand_goal(Body0, Body),
    assertz(M:(Head :- Body)).
scasp_assert(Head) =>
    assertz(Head).

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

#(M:pred(Spec))      => pred(M:Spec).
#(M:show(Spec))      => show(M:Spec).
#(M:abducible(Spec)) => abducible(M:Spec).

pred(M:(Atom :: Template)) =>
    process_pr_pred(Atom :: Template, Spec),
    assertz(M:pr_pred_predicate(Spec)).

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
    { pi_head(PI, Head) },
    [ pr_show_prdicate(not(Head)) ].
show(PI) -->
    { pi_head(PI, Head) },
    [ pr_show_prdicate(Head) ].

%!  abducible(:Spec)
%
%   Declare Spec, a comma list  of   _heads_  to be _abducible_, meaning
%   they can both be in or outside the model.

abducible(M:(A,B)) =>
    abducible(M:A),
    abducible(M:B).
abducible(M:Head), callable(Head) =>
    abducible_rules(Head, Rules),
    @(maplist(assertz, Rules), M).

abducible_rules(Head,
                [ (Head                 :- not AHead, abducible_1(Head)),
                  (AHead                :- not Head),
                  (abducible_1(Head)    :- not '_abducible_1'(Head)),
                  ('_abducible_1'(Head) :- not abducible_1(Head)),
                  (:- discontiguous((abducible_1/1, '_abducible_1'/1)))
                ]) :-
    Head =.. [F|Args],
    atom_concat('_', F, AF),
    AHead =.. [AF|Args].

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
    intern_negation(-Fact, MFact).
user:term_expansion((-Head :- Body), (MHead :- Body)) :-
    callable(Head),
    intern_negation(-Head, MHead).
user:term_expansion((false :- Body), ((-) :- Body)).
user:term_expansion((:- pred(SpecIn)), pr_pred_predicate(Spec)) :-
    process_pr_pred(SpecIn, Spec).
user:term_expansion((:- show(SpecIn)), Clauses) :-
    phrase(show(SpecIn), Clauses).
user:term_expansion((:- abducible(SpecIn)), Clauses) :-
    phrase(abducible(SpecIn), Clauses).
user:term_expansion((# pred(SpecIn)), pr_pred_predicate(Spec)) :-
    process_pr_pred(SpecIn, Spec).
user:term_expansion((# show(SpecIn)), Clauses) :-
    phrase(show(SpecIn), Clauses).
user:term_expansion((# abducible(SpecIn)), Clauses) :-
    phrase(abducible(SpecIn), Clauses).

user:goal_expansion(-Goal, MGoal) :-
    callable(Goal),
    intern_negation(-Goal, MGoal).


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
