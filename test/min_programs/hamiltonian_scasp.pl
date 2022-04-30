

reachable(V) :- cycle(U, V), reachable(U).
reachable(a) :- cycle(V, a).

% Every node must be reachable.
:- node(U), not reachable(U).

% Choose exactly one edge from each node.
other(U, V) :-
    node(U), node(V), node(W),
    edge(U, W), V \= W, cycle(U, W).
cycle(U, V) :-
    edge(U, V), not other(U, V).

% You cannot choose two edges to the same node
:- cycle(U, W), cycle(V, W), U \= V.


travel_path(Start, Length, Cycle) :- path(Start, Start, Start, Length, [], Cycle).

path(_, X, Y, D, Prev, [X,[D],Y|Prev]) :-
    cycle_dist(X, Y, D).
path(Start, X, Y, D, Prev, Cycle) :-
    D #= D1 + D2,
    cycle_dist(Z, Y, D1), Z \= Start,
    path(Start, X, Z, D2, [([D1],Y)|Prev], Cycle).

edge(X,Y) :- distance(X,Y,D).
cycle_dist(U,V,D) :-
    cycle(U,V), distance(U,V,D).

node(a).
node(b).
node(c).
node(d).

distance(b, c, 31/10).
distance(c, d, L) :-
    L #> 8, L #< 21/2.
distance(d, a, 1).
distance(a, b, 1).
distance(c, a, 1).
distance(a, d, 1).
distance(d, b, 1).


?- travel_path(b, D, Cycle).

#show travel_path/3, cycle_dist/3.
