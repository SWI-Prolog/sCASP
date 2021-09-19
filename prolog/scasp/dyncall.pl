:- module(scasp_dyncall,
          [ scasp/1,                    % :Query
            scasp_query_clauses/2,      % :Query, -Clauses
            op(900, fy, not)
          ]).
:- use_module(compile).
:- use_module(embed).

:- meta_predicate
    scasp(0),
    scasp_query_clauses(:, -).

/** <module>

This predicate assembles the clauses  that   are  reachable from a given
goal.


*/

scasp(Query) :-
    scasp_query_clauses(Query, Clauses),
    qualify(Query, _:QQuery),
    in_temporary_module(
        Module,
        scasp_compile(Module:Clauses, []),
        scasp_embed:scasp_call(Module:QQuery)).

qualify(M:Q0, M:Q) :-
    qualify(Q0, M, Q).

%!  scasp_query_clauses(:Query, -Clauses) is det.


scasp_query_clauses(Query, Clauses) :-
    query_callees(Query, Callees),
    findall(Clause, scasp_clause(Callees, Clause), Clauses).

scasp_clause(Callees, Clause) :-
    member(M:Head, Callees),
    @(clause(Head, Body), M),
    mkclause(Head, Body, M, Clause).

mkclause(Head, true, M, Clause) =>
    qualify(Head, M, Clause).
mkclause(Head, Body, M, Clause) =>
    qualify((Head:-Body), M, Clause).

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
    G =.. [Name|Args],
    atomic_list_concat([M,:,Name], QName),
    Q =.. [QName|Args].

query_callees(M:Query, Callees) :-
    findall(Call, body_calls(Query,M,Call), Calls0),
    sort(Calls0, Calls),
    callee_graph(Calls, Callees).

callee_graph(Preds, Nodes) :-
    empty_assoc(Expanded),
    callee_closure(Preds, Expanded, Preds, Nodes0),
    sort(Nodes0, Nodes).

callee_closure([], _, Preds, Preds).
callee_closure([H|T], Expanded, Preds0, Preds) :-
    pi_head(PI, H),
    (   get_assoc(PI, Expanded, _)
    ->  callee_closure(T, Expanded, Preds0, Preds0)
    ;   put_assoc(PI, Expanded, true, Expanded1),
        predicate_callees(H, Called),
        exclude(expanded(Expanded1), Called, New),
        append(New, T, Agenda),
        append(New, Preds0, Preds1),
        callee_closure(Agenda, Expanded1, Preds1, Preds)
    ).

expanded(Assoc, Head) :-
    pi_head(PI, Head),
    get_assoc(PI, Assoc, _).


%!  predicate_callees(:Head, -Callees) is det.
%
%   True when Callees is the list of _direct_ callees from Head.

:- dynamic predicate_callees_c/4.

predicate_callees(M:Head, Callees) :-
    predicate_callees_c(Head, M, Gen, Callees0),
    predicate_generation(M:Head, Gen),
    !,
    Callees = Callees0.
predicate_callees(M:Head, Callees) :-
    retractall(predicate_callees_c(Head, M, _, _)),
    predicate_callees_nc(M:Head, Callees0),
    predicate_generation(M:Head, Gen),
    assertz(predicate_callees_c(Head, M, Gen, Callees0)),
    Callees = Callees0.

predicate_callees_nc(Head, Callees) :-
    findall(Callee, predicate_calls(Head, Callee), Callees0),
    sort(Callees0, Callees).

predicate_calls(Head0, Callee) :-
    generalise(Head0, M:Head),
    @(clause(Head, Body), M),
    body_calls(Body, M, Callee).

body_calls(true, _M, _) => fail.
body_calls((A,B), M, Callee) =>
    (   body_calls(A, M, Callee)
    ;   body_calls(B, M, Callee)
    ).
body_calls(not(A), M, Callee) =>
    body_calls(A, M, Callee).
body_calls(-(A), M, Callee) =>
    body_calls(A, M, Callee).
body_calls(M:A, _, Callee), atom(M) =>
    body_calls(A, M, Callee).
body_calls(G, M, CalleePM), callable(G) =>
    implementation(M:G, Callee0),
    generalise(Callee0, Callee),
    (   predicate_property(Callee, interpreted),
        \+ predicate_property(Callee, meta_predicate(_))
    ->  pm(Callee, CalleePM)
    ;   pi_head(CalleePI, Callee),
        permission_error(scasp, procedure, CalleePI)
    ).
body_calls(G, _, _) =>
    type_error(callable, G).

pm(P, P).
pm(M:P, M:(-P)).

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

