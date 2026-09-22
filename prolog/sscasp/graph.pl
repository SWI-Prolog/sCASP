:- module(sscasp_graph,
          [ call_graph/2,               % +Clauses, -Graph
            odd_components/2,           % +Graph, -Components
            olon_clauses/3              % +Clauses, -Olon, -Ordinary
          ]).
:- use_module(ops).
:- use_module(dualise, [builtin/1]).
:- use_module(library(ugraphs), [vertices_edges_to_ugraph/3, reachable/3,
                                 transpose_ugraph/2, neighbours/3]).
:- use_module(library(lists), [member/2, memberchk/2, append/3, subtract/3]).
:- use_module(library(apply), [maplist/3, partition/4]).
:- use_module(library(ordsets), [ord_intersection/3, ord_memberchk/2]).

/** <module> The call graph, and the loops over negation in it

A program's **call graph** has one node per predicate and one arc per call,
signed by whether the call was negated.  `p :- q, not r` gives a positive arc
to `q` and a negative one to `r`.

Its purpose is to find the **odd loops over negation** — a cycle whose arcs
carry an odd number of negations.  `p :- not p` is the smallest.  Such a loop
has no model in which the rule is simply read as an implication, so the
compiler has to generate an NMR check for the rules on it, and finding them is
this module's job.

## Deciding it by balance rather than by enumerating cycles

A cycle's parity is the sum of its arcs' signs, so the question "does this
component contain an odd cycle?" is the question "can the nodes be assigned
parities consistent with every arc?" — a 2-colouring, exactly like testing a
graph for bipartiteness.  Give one node parity 0 and propagate: a positive arc
keeps the parity, a negative one flips it.  A conflict means an odd cycle, and
no conflict means every cycle is even.

That is linear, and — unlike enumerating paths — it cannot miss one.  It is
also correct to do the propagation *undirectedly* within a strongly connected
component, which is what makes it a colouring rather than a search.  Within an
SCC there is a path back from every node, so two directed paths to the same
node with different parities close into a directed closed walk of odd parity,
which contains an odd directed cycle.  So a consistent assignment exists
exactly when every directed cycle is even.

## Which clauses are on a loop

Every arc *inside* an unbalanced component lies on an odd closed walk: given
an arc and any odd cycle D in the component, the walk through the arc and the
walk through the arc and D have opposite parities, so one of them is odd.  So
the clauses to report are those contributing an arc whose head and callee are
in the same unbalanced component — no finer analysis pays off, and asking for
a *simple* odd cycle through a given arc is NP-hard anyway.

Over-reporting would be safe in any case.  The NMR sub-check a rule gets says
"if the body holds then the head holds", which every stable model of a normal
program satisfies for every rule.  A check for a rule that is not on a loop is
therefore redundant rather than restrictive — it costs time, not answers.
Under-reporting is what would be unsound, and the balance test cannot.

## Classical negation

`p` and `-p` are different nodes, because they are different atoms: nothing
but the consistency constraint relates them, and that constraint is a rule
like any other.
*/

		 /*******************************
		 *         THE GRAPH		*
		 *******************************/

%!  call_graph(+Clauses, -Graph) is det.
%
%   Graph is
%
%       graph{ nodes: [key(Polarity, Name/Arity), ...],
%              arcs:  [arc(From, To, Sign), ...] }
%
%   with Sign one of `+` or `-`.  Nodes include the predicates that are
%   called but not defined: they cannot be on a loop, having no arcs out,
%   but leaving them out would make the graph disagree with the program.
%
%   Built-ins are not nodes.  They call nothing, so an arc to one could
%   not be part of a cycle.

call_graph(Clauses, graph{nodes: Nodes, arcs: Arcs}) :-
    findall(Arc, clause_arc(Clauses, Arc), Arcs0),
    sort(Arcs0, Arcs),
    findall(Node, graph_node(Clauses, Arcs, Node), Nodes0),
    sort(Nodes0, Nodes).

graph_node(Clauses, _, Node) :-
    member(Clause, Clauses),
    head_node(Clause, Node).
graph_node(_, Arcs, Node) :-
    member(arc(From, To, _), Arcs),
    (   Node = From
    ;   Node = To
    ).

clause_arc(Clauses, arc(From, To, Sign)) :-
    member(Clause, Clauses),
    head_node(Clause, From),
    clause_body(Clause, Body),
    body_literal(Body, Literal),
    literal_node(Literal, To, Sign).

%!  head_node(+Clause, -Node) is det.

head_node((Head :- _), Node) :- !, node(Head, Node).
head_node(Fact, Node) :- node(Fact, Node).

clause_body((_ :- Body), Body) :- !.
clause_body(_, true).

%!  body_literal(+Body, -Literal) is nondet.
%
%   Every literal of Body, looking through the control constructs.  At
%   source level there is little to look through, but a body rewritten by
%   a program's own expansion may hold more.

body_literal(Var, _) :-
    var(Var),
    !,
    fail.
body_literal((A, B), L) :- !, (body_literal(A, L) ; body_literal(B, L)).
body_literal((A ; B), L) :- !, (body_literal(A, L) ; body_literal(B, L)).
body_literal((A -> B), L) :- !, (body_literal(A, L) ; body_literal(B, L)).
body_literal(L, L).

%!  literal_node(+Literal, -Node, -Sign) is semidet.
%
%   Fails for a built-in, which is not a node.

literal_node(not(Goal), Node, Sign) :-
    !,
    literal_node(Goal, Node, Sign0),
    flip(Sign0, Sign).
literal_node(Goal, _, _) :-
    builtin(Goal),
    !,
    fail.
literal_node(Goal, Node, +) :-
    node(Goal, Node).

flip(+, -).
flip(-, +).

%!  node(+Literal, -Node) is det.

node(-(Goal), key(neg, PI)) :-
    !,
    pi(Goal, PI).
node(Goal, key(plain, PI)) :-
    pi(Goal, PI).

pi(Goal, Name/Arity) :-
    (   atom(Goal)
    ->  Name = Goal, Arity = 0
    ;   functor(Goal, Name, Arity)
    ).

		 /*******************************
		 *        ODD LOOPS		*
		 *******************************/

%!  odd_components(+Graph, -Components) is det.
%
%   Components is the list of strongly connected components that contain
%   an odd loop over negation, each a sorted list of nodes.  A component
%   of one node is there only if the node calls itself.

odd_components(Graph, Components) :-
    components(Graph, All),
    include_unbalanced(All, Graph.arcs, Components).

include_unbalanced([], _, []).
include_unbalanced([C|Cs], Arcs, Components) :-
    (   cyclic(C, Arcs),
        \+ balanced(C, Arcs)
    ->  Components = [C|Rest]
    ;   Components = Rest
    ),
    include_unbalanced(Cs, Arcs, Rest).

%   A component of one node is a cycle only if the node calls itself;
%   reachability puts every node in a component, cyclic or not.

cyclic([Node], Arcs) :-
    !,
    memberchk(arc(Node, Node, _), Arcs).
cyclic(_, _).

%!  balanced(+Component, +Arcs) is semidet.
%
%   True when the nodes of Component can be assigned parities consistent
%   with every arc inside it — that is, when every cycle in it is even.
%   Propagation is undirected, which is what the module comment argues is
%   sound within a component.

balanced([Node|Nodes], Arcs) :-
    internal_arcs([Node|Nodes], Arcs, Internal),
    colour([Node-0], Internal, [], Colours),
    \+ ( member(N-C1, Colours),
         member(N-C2, Colours),
         C1 \== C2
       ).

internal_arcs(Component, Arcs, Internal) :-
    sort(Component, Set),
    findall(arc(F, T, S),
            ( member(arc(F, T, S), Arcs),
              ord_memberchk(F, Set),
              ord_memberchk(T, Set)
            ),
            Internal).

%!  colour(+Agenda, +Arcs, +Done, -Colours) is det.
%
%   Colours is every (node, parity) pair the propagation reaches.  A node
%   reached twice with different parities appears twice, which is how
%   balanced/2 sees the conflict.

colour([], _, Done, Done).
colour([Node-C|Agenda], Arcs, Done, Colours) :-
    (   memberchk(Node-C, Done)
    ->  colour(Agenda, Arcs, Done, Colours)
    ;   findall(N2-C2, neighbour(Node, C, Arcs, N2, C2), More),
        append(Agenda, More, Agenda1),
        colour(Agenda1, Arcs, [Node-C|Done], Colours)
    ).

%   Undirected: an arc constrains its two ends whichever way it points.

neighbour(Node, C, Arcs, Other, C2) :-
    (   member(arc(Node, Other, Sign), Arcs)
    ;   member(arc(Other, Node, Sign), Arcs)
    ),
    shift(Sign, C, C2).

shift(+, C, C).
shift(-, 0, 1).
shift(-, 1, 0).

%!  components(+Graph, -Components) is det.
%
%   The strongly connected components, as sorted lists of nodes.  By
%   reachability in both directions rather than by Tarjan: a program's
%   call graph is small, and this is four lines.

components(Graph, Components) :-
    maplist(arc_edge, Graph.arcs, Edges),
    vertices_edges_to_ugraph(Graph.nodes, Edges, UGraph),
    transpose_ugraph(UGraph, Reverse),
    scc_of_each(Graph.nodes, UGraph, Reverse, [], Components).

arc_edge(arc(From, To, _), From-To).

scc_of_each([], _, _, _, []).
scc_of_each([Node|Nodes], UGraph, Reverse, Seen, Components) :-
    (   ord_memberchk(Node, Seen)
    ->  scc_of_each(Nodes, UGraph, Reverse, Seen, Components)
    ;   reachable(Node, UGraph, Forward),
        reachable(Node, Reverse, Backward),
        ord_intersection(Forward, Backward, Component),
        append(Seen, Component, Seen0),
        sort(Seen0, Seen1),
        Components = [Component|Rest],
        scc_of_each(Nodes, UGraph, Reverse, Seen1, Rest)
    ).

		 /*******************************
		 *        OLON CLAUSES		*
		 *******************************/

%!  olon_clauses(+Clauses, -Olon, -Ordinary) is det.
%
%   Olon is the clauses that contribute an arc inside a component holding
%   an odd loop over negation; Ordinary is the rest, in order.  These are
%   the clauses the NMR check has to be built from.
%
%   A clause whose head is on a loop but whose body does not lead back
%   into it is *not* on the loop — `p :- b` with `b` outside the
%   component contributes no arc inside it — and is reported as ordinary.

olon_clauses(Clauses, Olon, Ordinary) :-
    call_graph(Clauses, Graph),
    odd_components(Graph, Components),
    append(Components, Nodes0),
    sort(Nodes0, Nodes),
    partition(on_odd_loop(Components, Nodes), Clauses, Olon, Ordinary).

on_odd_loop(Components, Nodes, Clause) :-
    head_node(Clause, Head),
    ord_memberchk(Head, Nodes),
    member(Component, Components),
    ord_memberchk(Head, Component),
    clause_body(Clause, Body),
    body_literal(Body, Literal),
    literal_node(Literal, Callee, _),
    ord_memberchk(Callee, Component),
    !.
