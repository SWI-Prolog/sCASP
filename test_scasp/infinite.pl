

#table even/1, odd/1.

even(0).
even(X) :- not odd(X).
odd(s(X)) :- not odd(X).