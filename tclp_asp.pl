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
:- op(700, fx,  [(?=)]).

:- use_module(.(clp_disequality_rt)).
:- op(700, xfx, [(.\=.), (.=.)]).


:- use_package(tabling).
:- active_tclp.
:- table query/2.
:- table query2/1.

?= X :-
	query(X, P),
	P ~> -(J, _),
	print_model(J).


print_model([F|J]) :-
	nl,
	print('{ '),
	print(F),
	print_model_(J),
	print(' }'), nl.

print_model_([]) :- !.
print_model_([X|Xs]) :-
	print_model_(X), !,
	print_model_(Xs).
print_model_([X]) :- !,
	print(X).
print_model_([X, Y|Xs]) :-
	print(' , '),
	print(X),
	print_model_([Y|Xs]).



query([], Att) :-
	Att <~ -([], 0).
query([X|Xs], Att) :-
	query(X, AttX), AttX ~> -(JX, NX),
	query(Xs, AttXs), AttXs ~> -(JXs, NXs),
	N is 1 + NX + NXs,
	Att <~ -([X, JX|JXs], N).
query(X, Att) :-
	pr_rule(X, Body),
	query(Body, Att).
query(X, Att) :-
	\+ pr_rule(X,_),
	X \= [], X \= [_|_],
	q_exec(X),
	Att <~ -([],0).

q_exec(A .\=. B) :- 
	(
	    call(A .\=. B) ->
	    true
	;
	    print('OK: Disequality fails checking:  '), print(A .\=. B), nl, fail
	).
q_exec(X) :-
	X \= .\=.(_, _),
	call(X).





query2([]).
query2([X|Xs]) :-
	query2(Xs),
	query2(X).
query2(X) :-
	pr_rule(X, Body),
	query2(Body).


%:- table query3/3.
query3([X|Xs], I, O) :-
	format('llama ~w \t with stack = ~w', [X,I]), nl,
	query3(X,  [X|I], O1),
	query3(Xs, O1,    O).
query3([], I, I) :- !.
query3(X,  I, O) :-
	pr_rule(X, Body),
	query3(Body, I, O).





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
% pr_rule(path(A, B), [edge(A, B)]).
% pr_rule(path(A, B), [edge(A, Z), path(Z, B)]).

% pr_rule(edge(1, 2), []).
% pr_rule(edge(2, 3), []).
% % pr_rule(edge(1, 1), []).
% % pr_rule(edge(2, 4), []).


:- include('pr/birds2_pr.pl').
%:- include('pr/pos_loop_simple_pr.pl').
%:- include('pr/pq_loop_pr.pl').
