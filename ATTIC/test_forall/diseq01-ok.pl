





p(X,aa) :- X \= a.
p(X,aa) :- X = a.

p(X,bb) :- X \= b.
p(X,bb) :- X = b.


q(Y) :- not p(X,Y).

?-  not q(Y). %% Should succeed 
