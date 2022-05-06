% { ancestor(bob,sam,[bob,jill,sam]), ... } x2
%-parent(bob,Y) :- not father(bob,Y), not mother(bob,Y), not new(bob).
%-parent(ted,Z).
test(X) :- not test2(X, Y).
test(X) :- test([]).
test(X).
test2(X, Y) :- Y \= 1, not test3(X).
test2(X, 1) :- not test3(X).
test3(X) :- not test(X).

male(bob).
male(bo).
male(ben).

female(may).
female(jill).
female(sam).

father(bob,jill).
father(bob,bo).
father(ben,sam).

mother(may,jill).
mother(may,bo).
mother(jill,sam).

parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).

% Get lineage from X to Y
ancestor(X,Y,[X,Y]) :- parent(X,Y).
ancestor(X,Y,[X|T]) :- parent(X,Z),ancestor(Z,Y,T).

arith(Z) :- Z is 2 ** 2.2.

?- ancestor(bob,sam,X), arith(Z).
