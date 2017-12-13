




go(X,Y) :-
	X .>. 1,
	Y = a.

p(X) :- X .>. 3, not q(X).
q(X) :- not p(X).

?- go(X,Y), p(X).