
fluent(trotting).
fluent(alive).
fluent(loaded(gun1)).
fluent(loaded(gun2)).

action(shoot(gun1)).
action(shoot(gun2)).

gun(gun1).
gun(gun2).

:- not holds(trotting,s0).
:- not holds(loaded(gun1),s0), not holds(loaded(gun2),s0).
neg_holds(trotting,S) :- neg_holds(alive,S).
neg_holds(alive, result(shoot(X),S)) :- gun(X), holds(loaded(X), S).
holds(F,s0) :- fluent(F), not neg_holds(F,s0).
neg_holds(F,s0) :- fluent(F), not holds(F,s0).
holds(F, result(A,S)) :- action(A), holds(F,S), not neg_holds(F, result(A,S)).
neg_holds(F, result(A,S)) :- action(A), neg_holds(F,S), not holds(F,result(A,S)).