:- module(tclp_asp, _).

%% USAGE:

%% birds2_pr program
% ?- ?= [flies(X)].
% { model... }
% X = tweety ? 

% ?- ?? [flies(X)].
% { call_stack ...}
% { model ...}
% X = tweety ?

%% path/2 program
% ?- ?= [path(X,Y)].
% ...

% ?- ?? [path(X,Y)].
% ... see justification of results for path(1,4) in README ...

%% It ALSO works for instantiated queries ?= [flies(tweety)]. and
%% returns its model (if there is no model returns 'no')


:- use_module(.(clp_call_stack)).
:- op(700, xfx, [(~>), (<~)]).
:- op(700, fx,  [(?=), (??)]).

:- use_module(.(clp_disequality_rt)).
:- op(700, xfx, [(.\=.), (.=.)]).


:- use_package(tabling).
:- active_tclp.
:- table query/2.
:- table query_stack_predicate/4.
%:- table query_stack/4.
:- table query2/1.

%% simple interpreter only compute the justification but is not able
%% to check the call_stack
?= X :-
	query(X, AttJ),
	print_model(AttJ).

%% this interpreter is based in the previous one and accumulate the
%% call_stack in each iteration to check co-induction...
??(A) :-
	AttI <~ s([], 0),
	query_stack(A, AttI, AttO, AttJ),
	print_stack(AttO), nl,
	print_model(AttJ), nl.

%% Main predicate to compute the model and its justification.
query([], Att) :-
	Att <~ -([], 0).
query([X|Xs], Att) :-
	query_goal(X, AttX), AttX ~> -(JX, NX),
	query(Xs, AttXs), AttXs ~> -(JXs, NXs),
	N is 1 + NX + NXs,
	Att <~ -([X, JX|JXs], N).
query_goal(X, Att) :-
	predicate(X),
	pr_rule(X, Body),
	query(Body, Att).
query_goal(X, Att) :-
	\+ predicate(X),
	X \= [], X \= [_|_],
	q_exec(X),
	Att <~ -([], 0).


%% Main predicate to compute the model, its justification, and the
%% call_stack in each iteration.
query_stack([], AttI, AttI, AttJ) :-
	AttJ <~ -([], 0).
query_stack([X|Xs], AttI, AttO, AttJ) :-
	AttI ~> s(I, NI),
	%	format('Calling ~w \t with stack = ~w', [X, I]), nl,
	check_CHS(X, I, Check), %% Check condition for coinductive success
	(
	    Check == 1, %% coinduction success <- cycles containing
	                %% even loops may succeed
	    AttO1 <~ s([chs(X)|I], NI),
	    -(JX, NX) = -([], 0),
	    AddX = chs
	;
	    Check == 0, %% coinduction does not success or fails <-
                        %% the execution continues inductively
	    NI1 is NI + 1,
	    AttI1 <~ s([X|I], NI1),
	    query_stack(X, AttI1, AttO1, AttJx), AttJx ~> -([X|JX], NX),
	    AddX = X
	;
	    Check == -1, %% coinduction fails <- the negation of a
	                 %% call unifies with a call in the call stack
	    fail
	),
	query_stack(Xs, AttO1, AttO, AttJxs), AttJxs ~> -(JXs, NXs),
	N is 1 + NX + NXs,
	AttJ <~ -([AddX, JX|JXs], N).
query_stack(X, AttI, AttO, AttJ) :-
	X \= [], X \= [_|_],
	predicate(X),
	query_stack_predicate(X, AttI, AttO, AttJ).
query_stack(X, AttI, AttI, XAttJ) :-
	X = forall(_, _),
	% copy_term(X, New),
	query_stack_forall(X),
	% get_justification_forall(New, AttJ), AttJ ~> -(JX, NX),
	% XAttJ <~ -([X|JX], NX).
	XAttJ <~ -([X], 0).
query_stack(X, AttI, AttO, AttJ) :-
	X \= [], X \= [_|_],
	X \= forall(_, _),
	\+ predicate(X),
	query_stack_goal(X, AttI, AttO, AttJ).


%% TABLED to avoid loops and repeated answers
query_stack_predicate(X, AttI, AttO, XAttJ) :-
	pr_rule(X, Body),
	query_stack(Body, AttI, AttO, AttJ), AttJ ~> -(JX, NX),
	XAttJ <~ -([X|JX], NX).
%% It is not tabled to execute the sub-goals and produce the
%% side-effects
query_stack_goal(X, AttI, AttI, AttJ) :-
	q_exec(X),
	AttJ <~ -([X], 0).

%% check_CHS checks conditions for coinductive success or failure
%% coinduction success <- cycles containing even loops may succeed
check_CHS(X, I, 1) :-
	predicate(X),
	\+ \+ even_loop(X, 0, I), !.
%% coinduction fails <- the negation of a call unifies with a call in
%% the call stack
check_CHS(X, I, -1) :-
	predicate(X),
	\+ \+ in_stack(X, I), !.
%% coinduction does not success or fails <- the execution continues
%% inductively
check_CHS(_X, _I, 0) :- true.

%% check if the negation is in the stack -> coinductive failure
in_stack(X, [NegX|_]) :-
	(
	    X == not(NegX)
	;
	    not(X) == NegX
	), !.
in_stack(X, [_|Is]) :-
	in_stack(X, Is).

%% check if it is a even loop -> coinductive success
even_loop(X, N, [I|_]) :- X == I, N > 0, 0 is mod(N, 2).
even_loop(X, N, [I|Is]) :-
	X \== I,
	I = not(_),
	N1 is N + 1,
	even_loop(X, N1, Is).
even_loop(X, N, [I|Is]) :-
	X \== I,
	I \= not(_),
	even_loop(X, N, Is).



%% Check if the goal X is a user defined predicate
predicate(-X) :-
	X =.. [Name|ArgX],
	length(ArgX, N),
	predicate_(Name, N).
predicate(X) :-
	X =.. [Name|ArgX],
	Name \= -,
	length(ArgX, N),
	predicate_(Name, N).
:- table predicate_/2.
predicate_(Name, N) :-
	pr_rule(not(R), _),
	R =.. [Name|ArgR],
	length(ArgR, N).
predicate_(Name, N) :-
	pr_rule(-R, _),
	R =.. [Name|ArgR],
	length(ArgR, N).
predicate_(Name, N) :-
	pr_rule(R, _),
	R =.. [Name|ArgR],
	Name \= -,
	length(ArgR, N).

%% Execute the non user define predicates using Prolog

q_exec(A .\=. B) :- !,
	(
	    call(A .\=. B) ->
	    %	    print('OK'),nl,
	    true
	;
	    print('Note: Disequality fails checking:  '), print(A .\=. B), nl,
	    fail).
q_exec(X) :-
	call(X).


query_stack_forall(forall(Var, Goal)) :-
	AttI <~ s([], 0),
	query_stack([Goal], AttI, _AttO, _AttJ),
	\+ \+ check_unbound(Var, Goal).
check_unbound(Var, Goal) :-
	neg_var(Var, List), !,
	clean(Var),
	exec_with_neg_list(Var, Goal, List).
check_unbound(Var, _Goal) :-
	var(Var).
exec_with_neg_list(_,   _,    []).
exec_with_neg_list(Var, Goal, [Value|Vs]) :-
	AttI <~ s([], 0),
	\+ \+ (
	    Var = Value,
	    query_stack([Goal], AttI, _AttO, _AttJ)
	),
	exec_with_neg_list(Var, Goal, Vs).
get_justification_forall(forall(_, Goal), FAttJ) :-
	AttI <~ s([], 0),
	findall(AttJ, query_stack([Goal], AttI, _AttO, AttJ), AttJList),
	combine(AttJList, FAttJ).
combine([],R) :- R <~ -([],0).
combine([J|Js],R) :-
	combine(Js,R1), R1 ~> -(J1, N1),
	J ~> -(J0,N0),
	N is N1 + N0,
	R <~ -([J0|J1], N).
	


%% The model is obtained from the justification tree.
print_model(AttJ) :-
	AttJ ~> -([F|J], _),
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

%% The stack is generated adding the last calls in the head (to avoid
%% the use of append/3). To print the stack, it is reversed.

%% NOTE that a model could be generated during the search with some
%% calls in the stack which are not present in the model (e.g. the
%% model of path(1,4) for the path/2 program - more details in the
%% file README)
print_stack(AttS) :-
	AttS ~> s(Stack, _),
	reverse(Stack, RStack),
	nl,
	print('{ '),
	print(RStack),
	print(' }'), nl.




%% Initial interpreters...
query2([]).
query2([X|Xs]) :-
	query2(Xs),
	query2(X).
query2(X) :-
	pr_rule(X, Body),
	query2(Body).


%:- table query3/3.
query3([X|Xs], I, O) :-
	format('Calling ~w \t with stack = ~w', [X, I]), nl,
	query3(X,  [X|I], O1),
	query3(Xs, O1,    O).
query3([], I, I) :- !.
query3(X,  I, O) :-
	pr_rule(X, Body),
	query3(Body, I, O).





%% A program in Prolog $$
% path(A, B, [A|Ls]) :-
% 	edge(A, Z),
% 	path(Z, B, Ls).

% path(A, B, [A, B]) :-
% 	edge(A, B).

% edge(1, 2).
% edge(2, 3).
% edge(2, 4).


%% A program translated in list format %%
% r_rule(path(A, B), [edge(A, Z), path(Z, B)]).
% pr_rule(path(A, B), [edge(A, B)]).

% pr_rule(edge(2, 1), []).
% pr_rule(edge(1, 2), []).
% pr_rule(edge(1, 1), []).
% pr_rule(edge(2, 4), []).

% pr_rule(k(X), [s(X)]).
% pr_rule(s(_), []).


%:- include('pr/birds2_pr.pl').
%:- include('pr/pos_loop_simple_pr.pl').
%:- include('pr/pq_loop_pr.pl').
%:- include('pr/forall_pr.pl').
%:- include('pr/loopvar_pr.pl').
%:- include('pr/gpa_pr.pl'). 
:- include('pr/hanoi_pr.pl').
%:- include('pr/hamcycle_pr.pl'). 