% 4 queens
%
% Models: ((1,2),(2,4),(3,1),(4,3)) and ((1,3),(2,1),(3,4),(4,2))
% Retargetable N-queens program.

nqueens(N, Q) :-
    nqueens(N, N, Q).

nqueens(X, N, [q(X, Y) | T]) :-
    X > 0,
    hasqueen(X, Y, N),
    X1 is X - 1,
    nqueens(X1, N, T).
nqueens(0, X, []).

hasqueen(X, Y, N) :-
    X > 0,
    pickqueen(X, Y, N),
    testrow(N, X, Y),
    testcol(N, X, Y),
    testdiag1(X, Y),
    testdiag2(X, Y, N),
    testdiag3(X, Y, N),
    testdiag4(X, Y, N).
hasqueen(0, Y, N).

% pick a queen for each row, ruling out others in the row at the same time
pickqueen(X, Y, Y) :-
    Y > 0,
    q(X, Y).
pickqueen(X, Y, N) :-
    N > 1,
    N1 is N - 1,
    pickqueen(X, Y, N1).

% rule out queens in same row as q(X,Y).
testrow(C, X, Y) :-
    C > 0,
    C \= X,
    not q(C, Y),
    C1 is C - 1,
    testrow(C1, X, Y).
testrow(C, X, Y) :-
    C = X,
    C1 is C - 1,
    testrow(C1, X, Y).
testrow(0, _, _).

% rule out queens in same column as q(X,Y).
testcol(C, X, Y) :-
    C > 0,
    C \= Y,
    not q(X, C),
    C1 is C - 1,
    testcol(C1, X, Y).
testcol(C, X, Y) :-
    C = Y,
    C1 is C - 1,
    testrow(C1, X, Y).
testcol(0, _, _).

% rule out queens in same diagonal as q(X,Y).
testdiag1(X, Y) :-
    X > 1,
    Y > 1,
    X1 is X - 1,
    Y1 is Y - 1,
    not q(X1, Y1),
    testdiag1(X1, Y1).
testdiag1(1, X) :-
    X \= 1.
testdiag1(X, 1).

testdiag2(X, Y, N) :-
    X > 1,
    Y < N,
    X1 is X - 1,
    Y1 is Y + 1,
    not q(X1, Y1),
    testdiag2(X1, Y1, N).
testdiag2(1, X, N) :-
    X \= N.
testdiag2(X, N, N).

testdiag3(X, Y, N) :-
    X < N,
    Y > 1,
    X1 is X + 1,
    Y1 is Y - 1,
    not q(X1, Y1),
    testdiag3(X1, Y1, N).
testdiag3(X, 1, N) :-
    X \= N.
testdiag3(N, X, N).

testdiag4(X, Y, N) :-
    X < N,
    Y < N,
    X1 is X + 1,
    Y1 is Y + 1,
    not q(X1, Y1),
    testdiag4(X1, Y1, N).
testdiag4(N, Y, N) :-
    Y \= N.
testdiag4(X, N, N).

q(X, Y) :- not negq(X, Y).
negq(X, Y) :- not q(X, Y).

?- nqueens(4, Q).
