% fact for each vertex(N).
vertex(0).
vertex(1).
vertex(2).
vertex(3).

% fact for each edge edge(U, V).
edge(0, 1).
edge(1, 2).
edge(2, 3).
edge(3, 0).

edge(2, 0).
edge(0, 3).
edge(3, 1).

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


ring_length(Length, Ring) :- distance(1,Length, Ring).
distance(X,D,[dist(1,X,D)]) :- reachable(X), chosen(1,X), dist(1,X,D).
distance(Y,D,[dist(X,Y,D2)|Ring]) :- chosen(X,Y), distance(X,D1,Ring), dist(X,Y,D2), D is D1 + D2.

dist(0, 1, 1).
dist(1, 2, 1).
dist(2, 3, 10).
dist(3, 0, 1).

dist(2, 0, 1).
dist(0, 3, 1).
dist(3, 1, 1).



?- distance(3,D,R).