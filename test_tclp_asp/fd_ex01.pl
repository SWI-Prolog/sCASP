





go(X,Y) :-
	X in 1 .. 3,
	X #\= 1,
	Y = a,
	label([X]).

p(X) :- not q(X).
q(X) :- not p(X).

?- go(X,Y), p(X).