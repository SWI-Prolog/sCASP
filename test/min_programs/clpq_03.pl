-mutually_exclusive(S1, S2) :-
    segment(S1), segment(S2), S1 =\= S2, 
    intercept(S1, S2). 

mutually_exclusive(S1, S2) :-
    not -mutually_exclusive(S1, S2). 

intercept([X1, X2], [Y1, Y2]) :-
    Y1 #=< X1, X1 #=< Y2.  
intercept([X1, X2], [Y1, Y2]) :-
    Y1 #=< X2, X2 #=< Y2.
    
segment([1,2]).
segment([2,3]).
segment([3,4]). 

?- mutually_exclusive(S1, S2).