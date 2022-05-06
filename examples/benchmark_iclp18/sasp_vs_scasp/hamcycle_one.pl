% fact for each vertex(N).
vertex(0).
vertex(1).
vertex(2).

% fact for each edge edge(U, V).
edge(0, 1).
edge(1, 2).
edge(2, 0).

reachable(V) :- chosen(U, V), reachable(U).
reachable(0) :- chosen(V, 0).

% Every vertex must be reachable.
:- vertex(U), not reachable(U).

% Choose exactly one edge from each vertex.
other(U, V) :-
    vertex(U), vertex(V), vertex(W),
    edge(U, W), V \= W, chosen(U, W).
chosen(U, V) :-
    edge(U, V), not other(U, V).

% You cannot choose two edges to the same vertex
:- chosen(U, W), chosen(V, W), U \= V.

?- chosen(1,2).
