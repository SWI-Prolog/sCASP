validate(L) :-
    findall(X, member(X,L), LL),
    LL = L.

member(X1, [X2|_]) :- X1 #= X2.
member(X, [_|R]) :- member(X, R).

?- validate([1,2,3]).
