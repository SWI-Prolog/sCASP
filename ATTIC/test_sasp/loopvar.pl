p(X) :- not q(X).
q(X) :- not p(X).
r(X) :- X \= 3, X \= 4, q(X).
?- p(X), r(Y).
