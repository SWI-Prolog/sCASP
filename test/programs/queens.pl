% 4-queens
%
% Retargetable N-queens program.
% Models ((1,2),(2,4),(3,1),(4,3)) and ((1,3),(2,1),(3,4),(4,2))

nqueens(N, Q) :-
    nqueens(N, N, [], Q).

% Pick queens one at a time and test against all previous queens.
nqueens(X, N, Qi, Qo) :-
    X > 0,
    pickqueen(X, Y, N),
    not attack(X, Y, Qi),
    X1 is X - 1,
    nqueens(X1, N, [q(X, Y) | Qi], Qo).
nqueens(0, _, Q, Q).


% pick a queen for row X.
pickqueen(X, Y, Y) :-
    Y > 0,
    q(X, Y).
pickqueen(X, Y, N) :-
    N > 1,
    N1 is N - 1,
    pickqueen(X, Y, N1).

% check if a queen can attack any previously selected queen.
attack(X, _, [q(X, _) | _]). % same row
attack(_, Y, [q(_, Y) | _]). % same col
attack(X, Y, [q(X2, Y2) | _]) :- % same diagonal
    Xd is X2 - X,
    abs(Xd, Xd2),
    Yd is Y2 - Y,
    abs(Yd, Yd2),
    Xd2 = Yd2.
attack(X, Y, [_ | T]) :-
    attack(X, Y, T).

q(X, Y) :- not negq(X, Y).
negq(X, Y) :- not q(X, Y).

abs(X, X) :- X >= 0.
abs(X, Y) :- X < 0, Y is X * -1.

?- nqueens(4, Q).
