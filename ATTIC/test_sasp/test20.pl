% false.
p(X) :- not q(X).
q(X) :- not p(X).

r(X) :- X \= 1, p(X), not r(X).
s(X) :- X \= 2, p(X), not s(X).

?- p(X).
