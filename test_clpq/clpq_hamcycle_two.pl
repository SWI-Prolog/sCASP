% fact for each vertex(N).
vertex(0).
vertex(1).
vertex(2).
vertex(3).

% fact for each edge edge(U, V).
edge(0, 1, D) :- D .<. 3.
edge(1, 2, D) :- D .<. 4.
edge(2, 3, D) :- D .<. 5.
edge(3, 0, D) :- D .<. 6.

edge(2, 0, D) :- D .<. 7.
edge(0, 3, D) :- D .<. 8.
edge(3, 1, D) :- D .<. 9.

reachable(V,D) :- D .=. D1 + D2, chosen(U, V, D1), reachable(U, D2).
reachable(0,D) :- chosen(V, 0, D).

% Every vertex must be reachable.
:- vertex(U), not reachable(U,D).

% Choose exactly one edge from each vertex.
other(U, V) :-
    vertex(U), vertex(V), vertex(W),
    edge(U, W, D), V \= W, chosen(U, W, D).
chosen(U, V, D) :-
    edge(U, V, D), not other(U, V).

% You cannot choose two edges to the same vertex
:- chosen(U, W, D1), chosen(V, W, D2), U \= V.


?- chosen(1,2,D).