




1 { hc(X,Y) : edge(X,Y) } 1 :- node(X).
1 { hc(Z,X) : edge(Z,X) } 1 :- node(X).
reachable(X) :- node(X), hc(1,X).
reachable(Y) :- node(X), node(Y), reachable(X), hc(X,Y).
:- not reachable(X), node(X).


edge(0, 1).
edge(1, 2).
edge(2, 3).
edge(3, 0).

edge(2, 0).
edge(0, 3).
edge(3, 1).

node(0..3).


distance(X,D) :- reachable(X), hc(1,X), dist(1,X,D).
distance(Y,D) :- distance(X,D1), hc(X,Y), dist(X,Y,D2), D = D1 + D2.

dist(0, 1, 1).
dist(1, 2, 1).
dist(2, 3, 1).
dist(3, 0, 1).

dist(2, 0, 1).
dist(0, 3, 1).
dist(3, 1, 1).
