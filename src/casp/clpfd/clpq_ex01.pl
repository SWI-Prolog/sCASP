




go(X,Y) :-
	X .>. 1,
	X .<>. 4,
	Y = a.

p(X) :- not q(X).
q(X) :- not p(X).

?- go(X,Y), p(X).