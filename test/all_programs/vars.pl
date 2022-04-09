% Simple clp(r) constraint

p(a,b,c,d,A) :-
    A #< 5, A #> 1.

?- p(A,B,C,D,E).
