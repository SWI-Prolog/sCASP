%% USAGE:
% ?- ?= [ab(X)].
% { ab(sam) , penguin(sam) }
% X = sam ? ;
% { ab(john) , wounded_bird(john) }
% X = john ? ;
% no

:- module(tclp_asp, _).


:- use_module(.(clp_call_stack)).
:- op(700, xfx, [(~>), (<~)]).
:- op(700, fx, [(?=)]).
:- use_package(tabling).
:- active_tclp.
:- table query/2.
:- table query2/1.

?= X :-
	query(X,P),
	P ~> -(J,_),
	print_model(J).


print_model([F|J]) :-
	nl,
	print('{ '),
	print(F),
	print_model_(J),
	print(' }'),nl.
	
print_model_([]) :- !.
print_model_([X|Xs]) :-
	print_model_(X), !,
	print_model_(Xs).
print_model_([X]) :- !,
	print(X).
print_model_([X,Y|Xs]) :-
	print(' , '),
	print(X),
	print_model_([Y|Xs]).
	

query([], Att) :-
	Att <~ -([], 0).
query([X|Xs], Att) :-
	query(Xs, AttXs), AttXs ~> -(JXs, NXs),
	query(X,  AttX), AttX ~> -(JX, NX),
	print(just(JXs,'\t',JX)),nl,
	N is 1 + NX + NXs,
	Att <~ -([X, JX|JXs], N).
query(X, Att) :-
	pr_rule(X, Body),
	query(Body, Att).
query(call(X), Att) :-
	\+ pr_rule(X, _),
	print(hola),
	call(X),
	Att <~ -([],0).


query2([]).
query2([X|Xs]) :-
	query2(Xs),
      query2(X).
query2(X) :-
	pr_rule(X, Body),
	query2(Body).






%% A program in prolog $$
path(A, B, [A|Ls]) :-
	edge(A, Z),
	path(Z, B, Ls).

path(A, B, [A, B]) :-
	edge(A, B).

edge(1, 2).
edge(2, 3).
edge(2, 4).


%% A program translated in list format %%
% pr_rules(path(A, B), [path(A, Z), path(Z, B)]).
% pr_rules(path(A, B), [edge(A, B)]).

% pr_rules(edge(1, 2), []).
% pr_rules(edge(2, 1), []).
% pr_rules(edge(1, 1), []).
% %pr_rules(edge(2, C), [call( C .\=. 3 )]).
% pr_rules(edge(2, 4), []).


:- include('pr/birds2_pr.pl').
