


r(stream(N,Pred)) :- stream(N,Pred), not cancelled(N, Pred).

cancelled(N, Pred) :- higher_prio(N1, N), stream(N1, Pred1), incompt(Pred, Pred1).

incompt(p(X), q(X)).
incompt(q(X), p(X)).


% val_stream(N,Pred) :- stream(N,Pred), not neg_val_stream(N,Pred).
% neg_val_stream(N,Pred) :- stream(N,Pred), not val_stream(N,Pred).

% stream(1,p(X)).
% stream(2,q(a)).
% stream(3,q(b)).

stream(1,p(X)).
stream(2,q(a)).
stream(3,p(a)).

higher_prio(PHi, PLo) :- PHi .>. PLo.

?- r(X).