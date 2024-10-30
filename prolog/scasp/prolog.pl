:- module(scasp_prolog,
          [ scasp_prolog/2              % :Goal, -Justification
          ]).
:- use_module(library(apply)).
:- use_module(library(prolog_code)).

:- meta_predicate
    scasp_prolog(0, -).

%!  scasp_prolog(:Goal, -Justification)
%
%   Proof Goal using a  Prolog  meta   interpreter.  Justification  is a
%   s(CASP) compatible justification tree.

scasp_prolog(M:Goal, Tree) :-
    prolog_current_choice(Ch),
    scasp_prolog(Goal, M, Tree, Ch).

scasp_prolog(true, _, Tree, _) =>
    Tree = [].
scasp_prolog(false, _, _, _) =>
    fail.
scasp_prolog(fail, _, _, _) =>
    fail.
scasp_prolog(M:Goal, _, Tree, Ch) =>
    scasp_prolog(Goal, M, Tree, Ch).
scasp_prolog((A,B), M, Children, Ch) =>
    body_list((A,B), Conj),
    maplist(scasp_prolog_ch(Ch, M), Conj, Children).
scasp_prolog((If->Then;Else), M, Tree, Ch) =>
    (   prolog_current_choice(Ch2),
        scasp_prolog(If, M, TreeA, Ch2)
    ->  scasp_prolog(Then, M, TreeB, Ch),
        Tree = [TreeA,TreeB]
    ;   scasp_prolog(Else, M, TreeC, Ch),
        Tree = [not(If)-[], TreeC]
    ).
scasp_prolog((A;B), M, Tree, Ch) =>
    (   scasp_prolog(A, M, Tree, Ch)
    ;   scasp_prolog(B, M, Tree, Ch)
    ).
scasp_prolog(\+ A, M, Tree, _) =>
    prolog_current_choice(Ch),
    \+ scasp_prolog(A, M, _, Ch),
    Tree = not(A)-[].
scasp_prolog(!, _, Tree, Ch) =>
    Tree = !-[],
    prolog_cut_to(Ch).
scasp_prolog(Goal, M, Tree, _),
    predicate_property(M:Goal, foreign) =>
    (   predicate_property(system:Goal, foreign)
    ->  Tree = Goal-[]
    ;   Tree = (M:Goal)-[]
    ),
    call(M:Goal).
scasp_prolog(Goal, M, Tree, Ch),
    predicate_property(M:Goal, imported_from(M2)) =>
    scasp_prolog(Goal, M2, Tree, Ch).
scasp_prolog(Goal, M, Tree, _),
    predicate_property(M:Goal, ssu) =>
    most_general_goal(Goal, Head),
    prolog_current_choice(Ch),
    (   rule(M:Head, Rule, ClauseRef),
        split_rule(Rule, HeadGuard, Body),
        body_list(HeadGuard, [Head|GuardConj]),
        subsumes_term(Head, Goal)
    *-> Head = Goal,
        Tree = goal_origin(M:Goal, ClauseRef)-Children,
        body_list(Body, BodyConj),
        append([GuardConj, [!], BodyConj], Conj),
        maplist(scasp_prolog_ch(Ch, M), Conj, Children0),
        flatten(Children0, Children)
    ;   existence_error(matching_rule, M:Goal)
    ).
scasp_prolog(Goal, M, Tree, _) =>
    Tree = goal_origin(M:Goal, ClauseRef)-Children,
    prolog_current_choice(Ch),
    clause(M:Goal, Body, ClauseRef),
    body_list(Body, Conj),
    maplist(scasp_prolog_ch(Ch, M), Conj, Children0),
    flatten(Children0, Children).

scasp_prolog_ch(Ch, M, Goal, Tree) :-
    scasp_prolog(Goal, M, Tree, Ch).

split_rule(HeadGuard0 => Body0, HeadGuard, Body) =>
    HeadGuard = HeadGuard0,
    Body = Body0.

body_list(Body, List) :-
    phrase(body_list(Body), List).

body_list(A) -->
    { var(A) },
    !,
    [A].
body_list((A,B)) -->
    !,
    body_list(A),
    body_list(B).
body_list(X) -->
    [X].

