





planning(Init, Final, Actions,Clears) :-
	length(Init, NumberBlocks),
	not member(Clears,Init),
%	clear(Init,1,NumberBlocks,Clears),
	init(Init,1).


clear(_,Block,NumberBlocks,[]) :-
	Block > NumberBlocks.
clear(Init,Block,NumberBlocks,[Block|Bs]) :-
	Block =< NumberBlocks,
	not member(Block,Init),
	is_clear(0,Block),
	Block1 is Block + 1,
	clear(Init,Block1,NumberBlocks,Bs).
clear(Init,Block,NumberBlocks,Bs) :-
	Block =< NumberBlocks,
	member(Block,Init),
	not is_clear(0,Block),
	Block1 is Block + 1,
	clear(Init,Block1,NumberBlocks,Bs).	

init([],_).
init([B|Rest],Block) :-
	on(0,Block, B),
	Block1 is Block + 1,
	init(Rest, Block1).

is_clear(Step,Block) :- not neg_is_clear(Step,Block).
neg_is_clear(Step,Block) :- not is_clear(Step,Block).

on(Step,A,B) :-	not neg_on(Step,A,B).
neg_on(Step,A,B) :- not on(Step,A,B).

:- on(Step,Block,B1), on(Step,Block,B2), B1 \= B2.

not_member(X, []).
not_member(X, [A | Xs]) :-
	X \= A,
	not_member(X,Xs).

member(X, [A | Xs]) :-
	X \= A,
	member(X,Xs).
member(X, [X | Xs]).

