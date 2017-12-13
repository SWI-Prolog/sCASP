




p(X,Y) :- X .<. Y.
p(X,Y) :-
	X .<. Y, Y .<. 5.
p(X,Y) :-
	X .>=. Y, Y .<. 5.
p(X,Y) :-
	X .<. Y, Y .>=. 5.
p(X,Y) :-
	X .>=. Y, o(Y).

o(Y) :- Y .>=. 5.

s :- not p(_,_).

query :- not s.

?- query.