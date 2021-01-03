





p(X,Y) :- d(Y), X \= a.
p(X,Y) :- d(Y), X = a.

d(aa).
d(bb).

q(Y) :- not p(X,Y).

?-  not q(Y). %% Should succeed 
