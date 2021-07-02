


%#table path/2.

edge(a,b).
edge(b,c).
edge(a,d) :- not path(c,d).

path(X,Y) :-
    edge(X,Y).
path(X,Y) :-
    edge(X,Z),
    path(Z,Y).


?- path(a,Y).

