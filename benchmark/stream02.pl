query(Pred) :-
	stream(N,Pred),
	not cancelled(N, Pred).

cancelled(N, Pred) :-
	higher_prio(N1, N),
	stream(N1, Pred1),
	incompt(Pred, Pred1).

higher_prio(PHi, PLo) :-
	PHi .>. PLo.

incompt(p(X), q(X)).
incompt(q(X), p(X)).

stream(1,p(X)).
stream(2,q(a)).
stream(3,q(b)).

?- query(X).