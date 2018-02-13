

% Move N disks in T moves.
hanoi(N, T) :-
        moven(N, 0, T, a, b, c).

% Move N disks from peg A to peg B using peg C.
moven(N, Ti, To, A, B, C) :-
        N .>. 1,
        N1 .=. N - 1,
        T3 .=. T2 + 1,
        moven(N1, Ti, T2, A, C, B),
        move(A, B, T3),
        moven(N1, T3, To, C, B, A).
moven(1, Ti, To, A, B, _) :-
        To .=. Ti + 1,
        move(A, B, To).

% move T: move disk from P1 to P2.
% any move may or may not be selected.
move(P1, P2, T) :-
        not negmove(P1, P2, T).

negmove(P1, P2, T) :-
        not move(P1, P2, T).

?- hanoi(8, T).