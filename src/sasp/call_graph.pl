:- module(call_graph, [
                    build_call_graph/4,
                    destroy_call_graph/0,
                    a/4,
                    ar/2
                  ]).

/** <module> Build the call graph used for NMR check construction and indexing.

Given the input program, build a call graph and assert the components.

@author Kyle Marple
@version 20170127
@license BSD-3
*/

/*
* Copyright (c) 2016, University of Texas at Dallas
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the University of Texas at Dallas nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY OF TEXAS AT DALLAS BE LIABLE FOR
* ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

:-set_prolog_flag(multi_arity_warnings,off).

:- use_module(common).

%! a(?Head:int, ?Goal:int, ?Negation:int, ?ID:int) is det
% Arc in the call graph from Head to Goal. Both head and goal will always be
% positive with Negation indicating whether or not Goal was originally negated.
% ID is the arc ID used to get associated rule IDs from ar/2.
%
% @param Head Rule head.
% @param Goal Absolute value of rule goal.
% @param Negation 1 or 0 indicating if Goal was originally negated.
% @param ID Arc ID. See ar/2.

%! ar(?ArcID:int, ?RuleIDs:list) is det
% Associate an arc ID with a list of rule IDs. Each rule contains the
% associated arc in the call graph; this avoids duplicate arcs.
%
% @param ArcID An integer ID indicating an arc. See a/4.
% @param RuleIDs A list of rule IDs with matching arcs in the call graph.

%! e(Literal:int) is det
% A literal to skip when building the call graph.
%
% @param Literal The literal to skip.
:- dynamic
    a/4, % a(head, goal, negation, id)
    ar/2, % ar(arc_id, rule_ids)
    e/1.

%! build_call_graph(+Rules:list, -Nodes:list, +Excludes:list, +SkipDuals:int)
% Build and assert the call graph. Return a list of nodes. Don't includes rules
% with heads present in the list of exclude. If SkipDuals is 1, rules with
% negative heads won't be included in the call graph.
%
% @param Rules Rules to use in building call graph.
% @param Nodes Nodes in call graph.
% @param Excludes Literals to exclude from call graph. Each member must be of
%        the form e(X), where X is the literal to exclude.
% @param SkipDuals 1 or 0 indicating if rules with negated heads should be
%        included in call graph.
build_call_graph(R, Ns, E, SkipDuals) :-
    R \= [],
    !,
    assert_all(E), % Assert rules to skip for faster access
    get_arcs(R, [], As, SkipDuals),
    retractall(e(_)),
    sort(As, As2),
    merge_arcs(As2, As3, Rs),
    get_nodes(As3, Ns),
    assert_all(As3),
    assert_all(Rs).
build_call_graph([], [], _, _).

%! get_nodes(+Arcs:list, -Nodes:list)
% Get a list of nodes in the graph. These are the positive literals that occur
% as rule heads.
%
% @param Arcs List of arcs in call graph, of the form a(Head, Goal, Neg, ID).
% @param Nodes Nodes in call graph.
get_nodes([X | T], [N | Ns]) :-
    X = a(N, _, _, _),
    get_nodes(T, N, Ns).
get_nodes([], []).

get_nodes([X | T], N, Ns) :-
    X = a(Y, _, _, _),
    Y = N,
    get_nodes(T, N, Ns).
get_nodes([X | T], N, [Y | Ns]) :-
    X = a(Y, _, _, _),
    Y \= N,
    get_nodes(T, Y, Ns).
get_nodes([], _, []).

%! get_arcs(+Rules:list, +ArcsIn:list, -ArcsOut:list, +SkipDuals:int)
% Get call graph edges w/ negation parity from rules. If SkipDuals is 1, don't
% get edges for rules with negative heads. If rule/4 fails (rules have no ID),
% use rule/3 and an ID of -1.
%
% @param Rules The list of rules in the program.
% @param ArcsIn List of arcs of the form a(Head, Goal, Neg, ID).
% @param ArcsOut List of arcs of the form a(Head, Goal, Neg, ID).
% @param SkipDuals 1 or 0 indicating whether or not to skip rules with negated
%        heads.
get_arcs([R | T], Gi, Go, SD) :-
    rule(R, H, I, Y), % Rules have IDs.
    predicate(H, F, _), % get functor of head
    \+e(F),
    (
            SD =:= 0 % Don't skip duals
    ;
            \+is_dual(F)
    ),
    !,
    get_arcs2(F, I, Y, Gi, G1),
    get_arcs(T, G1, Go, SD).
get_arcs([R | T], Gi, Go, SD) :-
    \+rule(R, _, _, _), % Rules do NOT have IDs. Otherwise, dual rules with IDs can trigger an error.
    c_rule(R, H, Y), % rules have no IDs attached.
    I is -1,
    predicate(H, F, _), % get functor of head
    \+e(F),
    (
            SD =:= 0 % Don't skip duals
    ;
            \+is_dual(F)
    ),
    !,
    get_arcs2(F, I, Y, Gi, G1),
    get_arcs(T, G1, Go, SD).
get_arcs([_ | T], Gi, Go, SD) :-
    get_arcs(T, Gi, Go, SD).
get_arcs([], G, G, _) :-
    !.

%! get_arcs2(+Head:ground, +ID:int, +Goals:list, +ArcsIn:list, -ArcsOut:list)
% Get call graph arcs for a single rule. Format is: a(head, goal, negation, id).
%
% @param Head Head of a rule.
% @param ID Rule ID.
% @param Goals Body of rule.
% @param ArcsIn List of arcs of the form a(Head, Goal, Neg, ID).
% @param ArcsOut List of arcs of the form a(Head, Goal, Neg, ID).
get_arcs2(H, I, [Y | T], Gi, [G | Go]) :-
    predicate(Y, Gy, _), % if goal is a predicate, get the functor
    \+is_dual(Gy),
    !,
    G = a(H, Gy, 0, I),
    get_arcs2(H, I, T, Gi, Go).
get_arcs2(H, I, [Y | T], Gi, [G | Go]) :-
    Y = not(Y2), % negated goal
    !,
    predicate(Y2, Gn, _), % get the functor
    G = a(H, Gn, 1, I),
    get_arcs2(H, I, T, Gi, Go).
get_arcs2(H, I, [Y | T], Gi, Go) :-
    \+predicate(Y, _, _), % goal isn't a predicate; skip it
    !,
    get_arcs2(H, I, T, Gi, Go).
get_arcs2(_, _, [], G, G) :-
    !.

%! merge_arcs(+ArcsIn:list, -ArcsOut:list, -IDgroups:list)
% Ensure that at most one arc for each negation exists between two nodes. Create
% a list of IDs for an arc to later associate with rules. Use the first rule ID
% as the ID for the list of rules.
%
% @param ArcsIn List of arcs of the form a(Head, Goal, Neg, ID).
% @param ArcsOut List of arcs of the form a(Head, Goal, Neg, ID).
% @param IDgroups List of ID groups of the form ar(ID, IDlist).
merge_arcs([X | T], [X | As], [Ar | Is]) :-
    X = a(H, G, N, I),
    merge_arcs2(H, G, N, T, To, Rs),
    Ar = ar(I, [I | Rs]),
    merge_arcs(To, As, Is).
merge_arcs([], [], []).

%! merge_arcs2(+Head:int, +Goal:int, +Neg:int, +ArcsIn:list, -ArcsOut:list, -IDs:list)
% Get rules associate with a single arc in the final graph
%
% @param Head Head of rule.
% @param Goal Rule goal. Always positive, with Neg indicating negation.
% @param Neg 1 or 0 indicating if goal is negated.
% @param ArcsIn List of arcs of the form a(Head, Goal, Neg, ID).
% @param ArcsOut List of arcs of the form a(Head, Goal, Neg, ID).
% @param IDs IDs associated with the current arc.
merge_arcs2(H, G, N, [X | T], To, [I | Rs]) :-
    X = a(H, G, N, I),
    merge_arcs2(H, G, N, T, To, Rs).
merge_arcs2(H, G, N, [X | T], [X | T], []) :-
    X \= a(H, G, N, _).
merge_arcs2(_, _, _, [], [], []).

%! assert_all(+List:list)
% Assert each element in a list.
%
% @param List List of facts to assert.
assert_all([X | T]) :-
    assertz(X),
    assert_all(T).
assert_all([]).

%! destroy_call_graph
% Retract the assertions for the call graph.
destroy_call_graph :-
    retractall(a(_,_,_,_)),
    retractall(ar(_,_)).
