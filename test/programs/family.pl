% A simple family database

% An unrelated program. As full server this is solved. Using the dynamic
% calling we only consider  predicates  reachable   from  the  query and
% global constraints whose call-tree overlaps with the query.

test1(X) :- not test2(X, Y).
test1(X).
test2(X, Y) :- (Y \= 1, not test3(X)).
test2(X, 1) :- not test3(X).
test3(X) :- not test1(X).

% The family

male(bob).
male(bo).
male(ben).
male(jeff).
male(ron).
male(tim).
male(ray).

female(may).
female(joy).
female(jill).
female(kathy).
female(ali).
female(sam).
female(beth).

father(bob,jill).
father(bob,bo).
%father(bob,kathy).
%father(bo,ron).
father(bo,ali).
father(ben,sam).
%father(jeff,beth).
%father(jeff,tim).
%father(jeff,ray).

mother(may,jill).
mother(may,bo).
%mother(may,kathy).
%mother(joy,ron).
mother(joy,ali).
mother(jill,sam).
%mother(kathy,beth).
%mother(kathy,tim).
%mother(kathy,ray).


parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).

grandparent(X,Y):-parent(X,Z),parent(Z,Y).

ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z),ancestor(Z,Y).

sibling(X,Y):-X\=Y,parent(Z,X),parent(Z,Y).

sister(X,Y):-sibling(X,Y),female(X).

brother(X,Y):-sibling(X,Y),male(X).

cousin(X,Y):-parent(P1,X),parent(P2,Y),P1\=P2.%sibling(P1,P2).

hardmath(X) :- X is A+(B*C-(D/E-F)*G)*H.



?- ancestor(bob,sam).

