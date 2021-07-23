s(0).
s(s(X)) :-
    s(X).

sum(s(0), X, s(X)).
sum(X, s(0), s(X)).
sum(s(X), Y, s(Z)) :-
    X \= 0,
    sum(X, Y, Z).

sumlist([X | T], [Y | T2], [Z | T3]) :-
    sum(X, Y, Z),
    sumlist(T, T2, T3).
sumlist([], [], []).

?- sumlist([s(s(0)), s(s(s(0)))], [s(s(s(0))), s(s(s(0)))], X).
