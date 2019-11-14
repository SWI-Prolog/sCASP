p(X) :- not q(X, Y).
q(X, Y) :- not p(X), s(Y).
r :- q(C, C).
s(_).
:- p(X), Z \= 7, q(Z, 0).
