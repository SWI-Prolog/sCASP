% fact for each vertex(N).

vertex(a).
vertex(b).
vertex(c).
vertex(d).

% fact for each edge edge(U, V).
% edge(b, c).

% edge(c, d).
% edge(d, a).
% edge(a, b).

% edge(c, a).
% edge(a, d).
% edge(d, b).


edge(X,Y) :- dist(X,Y,D).
dist(b, c, 3).

dist(c, d, L) :- L .>. 10, L .<. 13.
dist(d, a, 1).
dist(a, b, 1).

dist(c, a, 1).
dist(a, d, 1).
dist(d, b, 1).


reachable(V) :- chosen(U, V), reachable(U).
reachable(a) :- chosen(V, a).

% Every vertex must be reachable.
:- vertex(U), not reachable(U).

% Choose exactly one edge from each vertex.
other(U, V) :-
    vertex(U), vertex(V), vertex(W),
    edge(U, W), V \= W, chosen(U, W).
chosen(U, V) :-
    edge(U, V), not other(U, V).

chosen_dist(U,V,D) :-
	chosen(U,V),
	dist(U,V,D).

% You cannot choose two edges to the same vertex
:- chosen(U, W), chosen(V, W), U \= V.


cycle(Start, Length, Cycle) :- path(Start, Start, Start, Length, [], Cycle).

path(_, X, Y, D, Prev, [X,[D],Y|Prev]) :-
	chosen_dist(X, Y, D).
path(Start, X, Y, D, Prev, Cycle) :-
	D .=. D1 + D2,
	chosen_dist(Z, Y, D1), Z \= Start,
	path(Start, X, Z, D2, [([D1],Y)|Prev], Cycle).

% ring_length(Length, Ring) :- distance(b,Length, Ring).
% distance(X,D,[(X,b)]) :- reachable(X), chosen(b,X), dist(b,X,D).
% distance(Y,D,[(Y|Ring]) :- X \= b, chosen(X,Y), distance(X,D1,Ring), dist(X,Y,D2), D is D1 + D2.


?- cycle(a, Length, Cycle).