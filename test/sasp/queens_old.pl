% 4-queens: ((1,2),(2,4),(3,1),(4,3)) and ((1,3),(2,1),(3,4),(4,2))
% Retargetable N-queens program.

nqueens(N, Q) :-
    _nqueens(N, N, Q).

_nqueens(X, N, [q(X, Y) | T]) :-
    X > 0,
    _hasqueen(X, Y, N),
    X1 is X - 1,
    _nqueens(X1, N, T).
_nqueens(0, X, []).

_hasqueen(X, Y, N) :- 
    X > 0,
    _pickqueen(X, Y, N),
    _testrow(N, X, Y),
    _testcol(N, X, Y),
    _testdiag1(X, Y),
    _testdiag2(X, Y, N),
    _testdiag3(X, Y, N),
    _testdiag4(X, Y, N).
_hasqueen(0, Y, N).

% pick a queen for each row, ruling out others in the row at the same time
_pickqueen(X, Y, Y) :-
    Y > 0,
    q(X, Y).
_pickqueen(X, Y, N) :-
    N > 1,
    N1 is N - 1,
    _pickqueen(X, Y, N1).

% rule out queens in same row as q(X,Y).
_testrow(C, X, Y) :-
    C > 0,
    C \= X,
    not q(C, Y),
    C1 is C - 1,
    _testrow(C1, X, Y).
_testrow(C, X, Y) :-
    C = X,
    C1 is C - 1,
    _testrow(C1, X, Y).
_testrow(0, _, _).

% rule out queens in same column as q(X,Y).
_testcol(C, X, Y) :-
    C > 0,
    C \= Y,
    not q(X, C),
    C1 is C - 1,
    _testcol(C1, X, Y).
_testcol(C, X, Y) :-
    C = Y,
    C1 is C - 1,
    _testrow(C1, X, Y).
_testcol(0, _, _).

% rule out queens in same diagonal as q(X,Y).
_testdiag1(X, Y) :-
    X > 1,
    Y > 1,
    X1 is X - 1,
    Y1 is Y - 1,
    not q(X1, Y1),
    _testdiag1(X1, Y1).
_testdiag1(1, X) :-
    X \= 1.
_testdiag1(X, 1).

_testdiag2(X, Y, N) :-
    X > 1,
    Y < N,
    X1 is X - 1,
    Y1 is Y + 1,
    not q(X1, Y1),
    _testdiag2(X1, Y1, N).
_testdiag2(1, X, N) :-
    X \= N.
_testdiag2(X, N, N).

_testdiag3(X, Y, N) :-
    X < N,
    Y > 1,
    X1 is X + 1,
    Y1 is Y - 1,
    not q(X1, Y1),
    _testdiag3(X1, Y1, N).
_testdiag3(X, 1, N) :-
    X \= N.
_testdiag3(N, X, N).

_testdiag4(X, Y, N) :-
    X < N,
    Y < N,
    X1 is X + 1,
    Y1 is Y + 1,
    not q(X1, Y1),
    _testdiag4(X1, Y1, N).
_testdiag4(N, Y, N) :-
    Y \= N.
_testdiag4(X, N, N).

q(X, Y) :- not _negq(X, Y).
_negq(X, Y) :- not q(X, Y).

?- nqueens(4, Q).
