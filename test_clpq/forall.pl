


% p(a).
% p(b).
% p(X) :- X \= a, X \= b.

%p(5).
p(X) :-
	X .<. 5.
p(X) :-
	X .>. 7.
 p(X) :-
 	X .>=. 5,
 	X .=<. 7.

s :- not p(_).

query :- not s.

?- query.