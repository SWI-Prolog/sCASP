



:- use_package(library(clpfd)).

:- use_module(.(clp_disequality_rt)).
:- op(700, xfx, [(.\=.), (.=.)]).





go(X,Y) :-
	X in 1 .. 3,
	X #\= 1,
	Y = a,
	labeling([],[X]).