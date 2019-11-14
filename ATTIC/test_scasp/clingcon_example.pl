end(20).
stepsize(7).
step(N) :-
    end(E),
    bet(0,E,N).
bet(N, M, N) :- N =< M.
bet(N, M, K) :- N < M, N1 is N+1, bet(N1, M, K).

at(0, S) :- S .=. 0.

move(T) :- step(T), not neg_move(T).
neg_move(T) :- step(T), not move(T).

at(T1, S1) :-
    move(T),
    stepsize(St),
    T1 .=. T - 1,
    S1 .=. St + S,
    at(T, S).

at(T1, S) :-
    not move(T),
    step(T),
    T1 .=. T - 1,
    step(T1),
    at(T, S).

% :- end(E), S .=<. 100, at(E,S).
