p(X):- q(X , Z) , not r(X).
p(Z):- not q(X , Z) , r(X).

q(X , a):- X #> 5.
r(X):- X #< 1.

?- p(X). 