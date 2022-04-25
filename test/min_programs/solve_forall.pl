subset(S1, S2) :-
    forall(X, contained(X, S1, S2)). 

contained(X, S1, S2) :-
    member(X, S1), member(X, S2).
contained(X, S1, S2) :-
    member(X, S2), not member(X, S1). 

%subset(S1, S2) :- not -subset(S1, S2).

%-subset(S1, S2) :- member(X, S1), not member(X, S2). 

member(X1, [X2|_]) :- X1 #= X2. 
member(X, [_|R]) :- member(X, R).

?- subset([1], [1,3]). 