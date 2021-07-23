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

:- module(scasp_call_graph,
          [ build_call_graph/2,
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

:- use_module(common).

%!  a(?Head:int, ?Goal:int, ?Negation:int, ?ID:int) is det
%
%   Arc in the call graph from Head  to   Goal.  Both head and goal will
%   always be positive with Negation indicating  whether or not Goal was
%   originally negated. ID is the arc ID used to get associated rule IDs
%   from ar/2.
%
%   @arg Head Rule head.
%   @arg Goal Absolute value of rule goal.
%   @arg Negation 1 or 0 indicating if Goal was originally negated.
%   @arg ID Arc ID. See ar/2.

%!  ar(?ArcID:int, ?RuleIDs:list) is det
%
%   Associate an arc ID with a list of  rule IDs. Each rule contains the
%   associated arc in the call graph; this avoids duplicate arcs.
%
%   @arg ArcID An integer ID indicating an arc. See a/4.
%   @arg RuleIDs A list of rule IDs with matching arcs in the call graph.

%!  e(Literal:int) is det
%
%   A literal to skip when building the call graph.
%
%   @arg Literal The literal to skip.

:- thread_local
    a/4,		% a(head, goal, negation, id)
    ar/2.               % ar(arc_id, rule_ids)

%!  build_call_graph(+Rules:list, -Nodes:list)
%
%   Build and assert the call  graph.  Return   a  list  of nodes. Don't
%   includes rules with  heads  present  in   the  list  of  exclude.
%
%   @arg Rules Rules to use in building call graph.
%   @arg Nodes Nodes in call graph.

build_call_graph([], []) :-
    !.
build_call_graph(R, Ns) :-
    get_arcs(R, [], As),
    sort(As, As2),
    merge_arcs(As2, As3, Rs),
    get_nodes(As3, Ns),
    assert_all(As3),
    assert_all(Rs).

%!  get_nodes(+Arcs:list, -Nodes:list)
%
%   Get a list of nodes in the   graph.  These are the positive literals
%   that occur as rule heads.
%
%   @arg Arcs List of arcs in call graph, of the form a(Head, Goal, Neg, ID).
%   @arg Nodes Nodes in call graph.

get_nodes([X|T], [N|Ns]) :-
    X = a(N, _, _, _),
    get_nodes(T, N, Ns).
get_nodes([], []).

get_nodes([], _, []).
get_nodes([X|T], N, Ns) :-
    X = a(Y, _, _, _),
    (   Y = N
    ->  get_nodes(T, N, Ns)
    ;   Ns = [Y|Ns2],
        get_nodes(T, Y, Ns2)
    ).

%!  get_arcs(+Rules:list, +ArcsIn:list, -ArcsOut:list)
%
%   Get call graph edges w/ negation parity  from rules. If rule/4 fails
%   (rules have no ID), use rule/3 and an ID of -1.
%
%   @arg Rules The list of rules in the program.
%   @arg ArcsIn List of arcs of the form a(Head, Goal, Neg, ID).
%   @arg ArcsOut List of arcs of the form a(Head, Goal, Neg, ID).

get_arcs([], G, G).
get_arcs([R|T], Gi, Go) :-
    (   rule(R, H, I, Y)
    ->  true
    ;   c_rule(R, H, Y)
    ->  I = -1
    ),
    rule(R, H, I, Y), % Rules have IDs.
    predicate(H, F, _), % get functor of head
    \+ ignore_edge(F),
    !,
    get_arcs2(F, I, Y, Gi, G1),
    get_arcs(T, G1, Go).
get_arcs([_|T], Gi, Go) :-
    get_arcs(T, Gi, Go).

ignore_edge('_false_0'). % ignore headless rules
ignore_edge(F) :-        % and duals.
    is_dual(F).


%!  get_arcs2(+Head:ground, +ID:int, +Goals:list, +ArcsIn:list, -ArcsOut:list)
%
%   Get call graph arcs for a  single   rule.  Format  is: a(head, goal,
%   negation, id).
%
%   @arg Head Head of a rule.
%   @arg ID Rule ID.
%   @arg Goals Body of rule.
%   @arg ArcsIn List of arcs of the form a(Head, Goal, Neg, ID).
%   @arg ArcsOut List of arcs of the form a(Head, Goal, Neg, ID).

get_arcs2(_, _, [], G, G) :-
    !.
get_arcs2(H, I, [Y|T], Gi, [G|Go]) :-
    predicate(Y, Gy, _), % if goal is a predicate, get the functor
    \+ is_dual(Gy),
    !,
    G = a(H, Gy, 0, I),
    get_arcs2(H, I, T, Gi, Go).
get_arcs2(H, I, [Y|T], Gi, [G|Go]) :-
    Y = not(Y2),
    !,
    predicate(Y2, Gn, _), % get the functor
    G = a(H, Gn, 1, I),
    get_arcs2(H, I, T, Gi, Go).
get_arcs2(H, I, [Y|T], Gi, Go) :-
    \+ predicate(Y, _, _), % goal isn't a predicate; skip it
    get_arcs2(H, I, T, Gi, Go).

%!  merge_arcs(+ArcsIn:list, -ArcsOut:list, -IDgroups:list) is det.
%
%   Ensure that at most one arc  for   each  negation exists between two
%   nodes. Create a list of  IDs  for   an  arc  to later associate with
%   rules. Use the first rule ID as the ID for the list of rules.
%
%   @arg ArcsIn List of arcs of the form a(Head, Goal, Neg, ID).
%   @arg ArcsOut List of arcs of the form a(Head, Goal, Neg, ID).
%   @arg IDgroups List of ID groups of the form ar(ID, IDlist).

:- det(merge_arcs/3).

merge_arcs([X|T], [X|As], [Ar|Is]) :-
    X = a(H, G, N, I),
    merge_arcs2(H, G, N, T, To, Rs),
    Ar = ar(I, [I|Rs]),
    merge_arcs(To, As, Is).
merge_arcs([], [], []).

%!  merge_arcs2(+Head:int, +Goal:int, +Neg:int,
%!              +ArcsIn:list, -ArcsOut:list, -IDs:list) is det.
%
%   Get rules associate with a single arc in the final graph
%
%   @arg Head Head of rule.
%   @arg Goal Rule goal. Always positive, with Neg indicating negation.
%   @arg Neg 1 or 0 indicating if goal is negated.
%   @arg ArcsIn List of arcs of the form a(Head, Goal, Neg, ID).
%   @arg ArcsOut List of arcs of the form a(Head, Goal, Neg, ID).
%   @arg IDs IDs associated with the current arc.

merge_arcs2(_, _, _, [], [], []) :-
    !.
merge_arcs2(H, G, N, [X|T], To, [I|Rs]) :-
    X = a(H, G, N, I),
    !,
    merge_arcs2(H, G, N, T, To, Rs).
merge_arcs2(_, _, _, [X|T], [X|T], []).

%!  assert_all(+List:list)
%
%   Assert each element in a list.
%
%   @arg List List of facts to assert.

assert_all([X|T]) :-
    assertz(X),
    assert_all(T).
assert_all([]).

%!  destroy_call_graph
%
%   Retract the assertions for the call graph.

destroy_call_graph :-
    retractall(a(_,_,_,_)),
    retractall(ar(_,_)).
