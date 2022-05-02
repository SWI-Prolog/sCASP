% fact for each vertex(N).
vertex(a).
vertex(b).
vertex(c).
vertex(d).


% fact for each edge(U, V).
edge(b, a).
edge(b, d).
edge(a, c).

edge(a, b).
edge(b, c).
edge(c, d).
edge(d, a).

edge(c, a).
edge(a, d).
edge(d, b).


reachable(V) :- chosen(V, a).
reachable(V) :- chosen(V, U), reachable(U).


% Choose exactly one edge from each vertex.
chosen(U, V) :-
    edge(U, V), not other(U, V).
other(U, V) :-
    edge(U, V), not chosen(U, V).

% Every vertex must be reachable.
:- vertex(U), not reachable(U).
% You cannot choose two edges to the same vertex
:- chosen(U, W), U \= V, chosen(V, W).
:- chosen(W, U), U \= V, chosen(W, V).

?- reachable(a).

#show chosen/2.
