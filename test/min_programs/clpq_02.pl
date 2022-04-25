-mutually_exclusive(S1, S2) :-
    set(S1), set(S2), S1 =\= S2, 
    subset(S1, S2).

mutually_exclusive(S1, S2) :-
    not -mutually_exclusive(S1, S2). 

subset(S1, S2) :-
    not -subset(S1, S2).

-subset(S1, S2) :-
    -member(X, S1), member(X,S2). 

-member(X, S) :- not member(X, S). 

member(X1, [X2|_]) :- X1 #= X2. 
member(X, [_|R]) :- member(X, R). 
    
set([1,2]).
set([2,3]).
set([3,4]). 

?- mutually_exclusive(S1, S2).