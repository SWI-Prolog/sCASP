



#table path/2.

edge(1,2).
edge(2,3) :- not edge(2,4).
edge(2,4) :- not edge(2,3).


path(X,Y) :-
    edge(X,Y).
path(X,Y) :-
    path(X,Z),
    edge(Z,Y).


?- path(X,Y).

