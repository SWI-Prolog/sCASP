% 4-queens: ((1,2),(2,4),(3,1),(4,3)) and ((1,3),(2,1),(3,4),(4,2))
% Retargetable N-queens program.

nqueens(N, Q) :-
    _nqueens(N, N, [], Q).

% Pick queens one at a time and test against all previous queens.
_nqueens(X, N, Qi, Qo) :-
    X > 0,
    _pickqueen(X, Y, N),
    not _attack(X, Y, Qi),
    X1 is X - 1,
    _nqueens(X1, N, [q(X, Y) | Qi], Qo).
_nqueens(0, _, Q, Q).


% pick a queen for row X.
_pickqueen(X, Y, Y) :-
    Y > 0,
    q(X, Y).
_pickqueen(X, Y, N) :-
    N > 1,
    N1 is N - 1,
    _pickqueen(X, Y, N1).

% check if a queen can attack any previously selected queen.
_attack(X, _, [q(X, _) | _]). % same row
_attack(_, Y, [q(_, Y) | _]). % same col
_attack(X, Y, [q(X2, Y2) | _]) :- % same diagonal
    Xd is X2 - X,
    _abs(Xd, Xd2),
    Yd is Y2 - Y,
    _abs(Yd, Yd2),
    Xd2 = Yd2.
_attack(X, Y, [_ | T]) :-
    _attack(X, Y, T).

q(X, Y) :- not _negq(X, Y).
_negq(X, Y) :- not q(X, Y).

_abs(X, X) :- X >= 0.
_abs(X, Y) :- X < 0, Y is X * -1.

?- nqueens(4, Q).
