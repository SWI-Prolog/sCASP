% subset(S1, S2) :-
%     not -subset(S1, S2).

% -subset(S1, S2) :-
%     -member(X, S1), member(X,S2). 

% -member(X, S) :- not member(X, S). 

% member(X1, [X2|_]) :- X1 #= X2. 
% member(X, [_|R]) :- member(X, R). 

superset(Y, L) :-
    findall(X, subset(X, Y), L). % didn't work

subset([], _).
subset([X|R], S2) :-
    minus(X, S2, [], NS2),
    subset(R, NS2). 

% minus(X, S1, [], NS1)
minus(X, [X|R], P, F) :-
    append(P, R, F). 
minus(X1, [X2|R], P, F) :- 
    X1 #<> X2,
    minus(X1, R, [X2|P], F). 

append([], X, X).
append([U|X], Y, [U|Z]) :- append(X,Y,Z). 

?- subset(X, [1,2,3]).

%?- superset([1,2], L). 