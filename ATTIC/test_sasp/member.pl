member(X,[X|T]).
member(X,[Y|T]) :- X\=Y, member(X,T).
