member(X, [X |Xs]).
member(X, [_ |Xs]):- member(X, Xs).

list([1,2,3]).

not_in_list(A) :-
    list(A), 
    not member(B, A). 


?- not_in_list(A). 