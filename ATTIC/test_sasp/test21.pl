% { p(2), ... not p(Var20) ( Var20 \= 1, Var20 \= 2 ), not p(Var40) ( Var40 \= 2 ), not q(2) }
p(X) :- not q(X).
q(X) :- not p(X).

r(X) :- X \= 1, X \= 2, p(X), not r(X).
s(X) :- X \= 2, p(X), not s(X).

?- p(X).
