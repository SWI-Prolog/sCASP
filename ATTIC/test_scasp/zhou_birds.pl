

%OWA-open world assumption(incomplete information)
%conservative about abnormal definition
flies(X):- bird(X), not ab(X).

bird(X):- penguin(X).
bird(X):- wounded_bird(X).

ab(X):- not -penguin(X).  %changed
ab(X):- not -wounded_bird(X).  %changed

-wounded_bird(X):- -bird(X).  %changed
-penguin(X):- -bird(X).  %changed

-flies(X):- penguin(X).
-flies(X):- -bird(X).

%facts
bird(tweety).
-wounded_bird(tweety).
-penguin(tweety).
bird(et).
-wounded_bird(et).
-penguin(et).
-flies(et).

% penguin(X):-not -penguin(X).
% wounded_bird(X):- not -wounded_bird(X).
% -bird(X):-not bird(X).

#compute 0 {flies(X)}.

%?- flies(X).
