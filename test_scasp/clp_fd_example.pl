



% :- use_package(library(clpfd)).

% :- use_module(.(clp_disequality_rt)).
% :- op(700, xfx, [(.\=.), (.=.)]).




go(X,Y) :-
	X in 1 .. 5,
	X #\= 1,
	Y = a,
	label([X]).

p(X) :- X #> 3,s not q(X).
q(X) :- not p(X).
