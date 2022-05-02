

% Move N disks in T moves.
hanoi(N, T) :-
    move_(N, 0, T, a, b, c).

% Move N disks from peg Pi to peg Pf using peg Paux.
move_(N,Ti,Tf,Pi,Pf,Paux) :-
    N #> 1,
    N1 #= N - 1,
    move_(N1,Ti,T1,Pi,Paux,Pf),
    move_(1,T1,T2,Pi,Pf,Paux),
    move_(N1,T2,Tf,Paux,Pf,Pi).
move_(1,Ti,Tf,Pi,Pf,_) :-
    Tf #= Ti + 1,
    move(Pi,Pf,Tf).

% move T: move disk from Pi to Pf.
% any move may or may not be selected.
move(Pi,Pf,T):- not negmove(Pi,Pf,T).
negmove(Pi,Pf,T):- not move(Pi,Pf,T).

?- hanoi(7, T).

#show move/3.
