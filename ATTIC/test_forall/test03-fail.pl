



p(X,Y) :- X .<. Y.

q :- not p(X,Y).


?-  not q. %% Should fail
