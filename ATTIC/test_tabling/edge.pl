#table p/2.

e(a,b).
e(b,c) :- not e(b,d).
e(b,d) :- not e(b,c).
e(b,a).

p(X,Y) :-
    e(X,Y).
p(X,Y) :-
    p(X,Z),
    e(Z,Y).


?- p(b,X).