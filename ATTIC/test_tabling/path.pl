#table p/2.

p(a,b).
p(b,c) :- not p(b,d).
p(b,d) :- not p(b,c).

p(X,Y) :-
    p(X,Z),
    p(Z,Y).


?- p(a,X).