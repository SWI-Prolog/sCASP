% Tower of Hanoi, 7 pegs

% Move N disks in T moves.
hanoi(N, T) :-
    moven(N, 0, T, 1, 2, 3).

% Move N disks from peg A to peg B using peg C. Assign move numbers.
moven(N, Ti, To, A, B, C) :-
    N > 1,
    N1 is N - 1,
    moven(N1, Ti, T2, A, C, B),
    T3 is T2 + 1,
    move(T3, A, B),
    moven(N1, T3, To, C, B, A).
moven(1, Ti, To, A, B, _) :-
    To is Ti + 1,
    move(To, A, B).

% move T: move disk from P1 to P2.
% any move may or may not be selected.
move(T, P1, P2) :-
    not negmove(T, P1, P2).

negmove(T, P1, P2) :-
    not move(T, P1, P2).

?- hanoi(7, T).
