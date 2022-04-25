valid(X) :- in_target(X). 
-valid(X) :- not valid(X).

in_target(X) :-  X#>=0, X#=<10. 
in_target(X) :-  X#>4, X#<10. 
in_target(X) :-  X#=5. 

member(X, [X|_]).
member(X, [_|R]) :- member(X, R). 

?- -valid(X).