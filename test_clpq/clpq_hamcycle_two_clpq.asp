% fact for each vertex(N).
vertex(0).
vertex(1).
vertex(2).
vertex(3).

% fact for each edge edge(U, V).
edge(0, 1, 1).
edge(1, 2, 1).
edge(2, 3, 1).
edge(3, 0, 1).

edge(2, 0, 1).
edge(0, 3, 1).
edge(3, 1, 1).

reachable(V,D) :- chosen(U, V, D2), reachable(U,D1), D = D1 + D2.
reachable(0,D) :- chosen(V, 0, D).

reach(U) :- reachable(U,D).

% Every vertex must be reachable.
%:- vertex(U), not reachable(U,D).
:- vertex(U), not reach(U).

% Choose exactly one edge from each vertex.
other(U, V, D) :-
    vertex(U), vertex(V), vertex(W),
    edge(U, W, D1),  V != W, chosen(U, W, D1).
% other(U, V) :-
%     vertex(U), vertex(V), vertex(W),
%     edge(U, W),  V != W, chosen(U, W).
chosen(U, V, D) :-
    edge(U, V, D), not other(U, V, D).

% You cannot choose two edges to the same vertex
:- chosen(U, W, D1), chosen(V, W, D2), U != V.
% ?- chosen(1,2,D).