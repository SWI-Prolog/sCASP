max(X) :- not smaller(X).
smaller(X) :- num(X), num(Y), X < Y.

num(1).
num(2).
num(3).
num(5).

?- max(C).
