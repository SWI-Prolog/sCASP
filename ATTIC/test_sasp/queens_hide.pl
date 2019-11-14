% 4-queens: ((1,2),(2,4),(3,1),(4,3)) and ((1,3),(2,1),(3,4),(4,2))
% Retargetable N-queens program.

nqueens(N, Q) :-
    hide_nqueens(N, N, [], Q).

% Pick queens one at a time and test against all previous queens.
hide_nqueens(X, N, Qi, Qo) :-
    X > 0,
    hide_pickqueen(X, Y, N),
    not hide_attack(X, Y, Qi),
    X1 is X - 1,
    hide_nqueens(X1, N, [q(X, Y) | Qi], Qo).
hide_nqueens(0, _, Q, Q).


% pick a queen for row X.
hide_pickqueen(X, Y, Y) :-
    Y > 0,
    q(X, Y).
hide_pickqueen(X, Y, N) :-
    N > 1,
    N1 is N - 1,
    hide_pickqueen(X, Y, N1).

% check if a queen can attack any previously selected queen.
hide_attack(X, _, [q(X, _) | _]). % same row
hide_attack(_, Y, [q(_, Y) | _]). % same col
hide_attack(X, Y, [q(X2, Y2) | _]) :- % same diagonal
    Xd is X2 - X,
    hide_abs(Xd, Xd2),
    Yd is Y2 - Y,
    hide_abs(Yd, Yd2),
    Xd2 = Yd2.
hide_attack(X, Y, [_ | T]) :-
    hide_attack(X, Y, T).

q(X, Y) :- not hide_negq(X, Y).
hide_negq(X, Y) :- not q(X, Y).

hide_abs(X, X) :- X >= 0.
hide_abs(X, Y) :- X < 0, Y is X * -1.

?- nqueens(4, Q).
