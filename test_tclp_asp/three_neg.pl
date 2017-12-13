

catch(X) :- X = a.
wait(X) :- not catch(X).
%:- catch(X), wait(X).


?- catch(X).
%?- wait(X).


