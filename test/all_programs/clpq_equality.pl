-mutually_exclusive(S1, S2) :-
    member(X, S1), member(X, S2). 

mutually_exclusive(S1, S2) :-
    not -mutually_exclusive(S1, S2). 

member(X1, [X2|_]) :- X1 #= X2.  
member(X, [_|R]) :- member(X, R). 

?- mutually_exclusive([1,2], [3,4]).