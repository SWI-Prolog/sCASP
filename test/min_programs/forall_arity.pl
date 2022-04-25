% Dual rules for q/2 and q/1 with one body variable used to clash.

p(X) :-
	q(X,_).

q(ha) :-
	q(a,X),
	X > 1.

q(a,2).

?- not p(a).
