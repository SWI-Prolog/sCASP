invalid_value(X) :- not valid_value(X). 

valid_value(V) :-
    coord(X), coord(Y),
    is(V, X+Y).

coord(1).
coord(2).
coord(3). 

?- invalid_value(X). 