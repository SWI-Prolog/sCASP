


% #table r/1, cancelled/2.

r(stream(N,Pred)) :- val_stream(N,Pred), not cancelled(N, Pred).

cancelled(N, Pred) :- N1 .>. N, val_stream(N1, Pred1), incompt(Pred, Pred1).

incompt(p(A), q(B)) :- A = B.
incompt(q(B), p(A)) :- A = B.


val_stream(N,Pred) :- stream(N,Pred), not neg_val_stream(N,Pred).
%neg_val_stream(N,Pred) :- not stream(N, Pred), not val_stream(N,Pred).
neg_val_stream(N,Pred) :- not val_stream(N,Pred).

% stream(1,p(X)).
% stream(2,q(a)).
% stream(3,q(b)).

% stream(1,p(X)).
% stream(2,q(a)).
% stream(3,p(a)).

stream(P,p(X)) :- P .=<. 1.
stream(P,q(a)) :- P .>. 1, P .=<. 2.
%stream(P,p(a)) :- P .>. 2.


?- r(X).