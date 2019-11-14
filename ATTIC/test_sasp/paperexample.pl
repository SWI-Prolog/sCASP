n(0).
n(s(X)) :- n(X).
p(X, Y) :- not q(X), t(Y, Y).
q(X) :- not p(X, Y).
r(V) :- r(V2).
r(3.14).
s(N) :- N \= 3.14.
t(A, A).
%a(X):- not b(X), t(X,X).
%b(X):- not c(X), not q(X).
%c(X):- not a(X).
