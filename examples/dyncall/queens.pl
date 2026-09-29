:- use_module(library(scasp)).

% 4-queens: ((1,2),(2,4),(3,1),(4,3)) and ((1,3),(2,1),(3,4),(4,2))
% Retargetable N-queens program.

nqueens(N, Q) :-
    nqueens_(N, N, [], Q).

% Pick queens one at a time and test against all previous queens.
nqueens_(X, N, Qi, Qo) :-
    X > 0,
    pickqueen_(X, Y, N),
    not attack_(X, Y, Qi),
    X1 is X - 1,
    nqueens_(X1, N, [q(X, Y) | Qi], Qo).
nqueens_(0, _, Q, Q).


% pick a queen for row X.
pickqueen_(X, Y, Y) :-
    Y > 0,
    q(X, Y).
pickqueen_(X, Y, N) :-
    N > 1,
    N1 is N - 1,
    pickqueen_(X, Y, N1).

% check if a queen can attack any previously selected queen.
attack_(X, _, [q(X, _) | _]). % same row
attack_(_, Y, [q(_, Y) | _]). % same col
attack_(X, Y, [q(X2, Y2) | _]) :- % same diagonal
    Xd is X2 - X,
    abs_(Xd, Xd2),
    Yd is Y2 - Y,
    abs_(Yd, Yd2),
    Xd2 = Yd2.
attack_(X, Y, [_ | T]) :-
    attack_(X, Y, T).

q(X, Y) :- not negq_(X, Y).
negq_(X, Y) :- not q(X, Y).

abs_(X, X) :- X >= 0.
abs_(X, Y) :- X < 0, Y is X * -1.

% ?- nqueens(4, Q).
