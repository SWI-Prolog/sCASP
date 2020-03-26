p(X) :- not q(X).
q(X) :- not p(X).

p(1).

?-  q(X).
