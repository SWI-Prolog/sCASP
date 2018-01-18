


#table path/2.

edge(a,b).
edge(b,a).

%path(c,d) :- not edge(d,e).

path(X,Y) :-
	path(X,Z),
	path(Z,Y).

path(X,Y) :-
	edge(X,Y).


?- path(X,Y).