


#table edge/2.

edge(1,2).
edge(2,3).


edge(X,Y) :-
    edge(Y,X).

edge(4,5) :-  not edge(2,5).

?- edge(X,Y).
