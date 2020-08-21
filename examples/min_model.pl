



p(X) :- r(X).
p(X) :- not s(X), r(X).

r(2).
r(1).
s(X) :- not t(X).
t(X) :- not s(X).


?- p(X).


