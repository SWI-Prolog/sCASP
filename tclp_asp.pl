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

:- use_module(.(clp_disequality_rt)).
:- op(700, xfx, [(.\=.), (.=.)]).


%% Query shortcuts.
:- op(700, fx,  [(?=), (??)]).

:- use_package(tabling).
:- active_tclp.
:- table query/2.
:- table query_stack_predicate/4.
%:- table query_stack/4.
:- table query2/1.


main(_) :-
	?? hanoi(7,P),
	print('?- '), print(P), nl.

%% this interpreter accumulate the call_stack (AttI -> AttO) in each
%% iteration to check co-induction... and the store the justification
%% in AttJ.
??(A) :-
	\+ list(A), !, %% Allows to make a singleton query
	??([A]).
??(A) :-
	AttI = s([], 0),
	append(A, [add_to_query], Query),
	(
	    statistics(runtime, _),
	    query_goals(Query, AttI, AttO, AttJ),
	    statistics(runtime, [_,T]),
	    nl, print(solve_run_time(T,'00 ms')), nl, nl
	),
	print_stack(AttO), nl,
	print_model(AttJ), nl.


%% Main predicate to compute the model, its justification, and the
%% call_stack in each iteration.
query_goals([], AttI, AttI, AttJ) :-
	AttJ = -([], 0).
query_goals([X|Xs], AttI, AttO, AttJ) :-
%	AttI = s(I,_),nl,display('Calling '),print(X),nl,format(' \t with stack = ~w', [I]), nl,
	check_query_goal(X, AttI, AttO1, AttJx), AttJx = -([AddX|JX], NX),
	query_goals(Xs, AttO1, AttO, AttJxs), AttJxs = -(JXs, NXs),
	N is 1 + NX + NXs,
	AttJ = -([AddX, JX|JXs], N).

check_query_goal(X, AttI, AttO, AttJ) :-
	AttI = s(I, NI),
	check_CHS(X, I, Check), %% Check condition for coinductive success
	(
	    Check == 1, %% coinduction success <- cycles containing
	    %% even loops may succeed
	    AttO = s([chs(X)|I], NI),
	    X = '?Var1',
	    -(JX, NX) = -([], 0),
	    AddX = chs
	;
	    Check == 0, %% coinduction does neither success nor fails <-
	    %% the execution continues inductively
	    NI1 is NI + 1,
	    AttI1 = s([X|I], NI1),
	    query_goal(X, AttI1, AttO, AttJx), AttJx = -([X|JX], NX),
	    AddX = X
	;
	    Check == -1, %% coinduction fails <- the negation of a
	    %% call unifies with a call in the call stack
	    fail
	),
	AttJ = -([AddX|JX], NX).


query_goal(X, I, O, J) :-
	X \= [], X \= [_|_],
	predicate(X),
	AttI <~ I,
	query_stack_predicate(X, AttI, AttO, AttJ),
	AttO ~> O,
	AttJ ~> J.
query_goal(X, AttI, AttO, XAttJ) :-
	X = forall(_, _),
	% copy_term(X, New),
	query_stack_forall(X, AttI, AttO),
	% get_justification_forall(New, AttI, AttJ), AttJ ~> -(JX, NX),
	% XAttJ = -([X|JX], NX).
	XAttJ = -([X], 0).
query_goal(X, AttI, AttO, AttJ) :-
	X \= [], X \= [_|_],
	X \= forall(_, _),
	\+ predicate(X),
	query_stack_goal(X, AttI, AttO, AttJ).


%% TABLED to avoid loops and repeated answers
query_stack_predicate(X, AttI, AttO, XAttJ) :-
	AttI ~> I,
	pr_rule(X, Body),
	query_goals(Body, I, O, J), J = -(JX, NX),
	AttO <~ O,
	XAttJ <~ -([X|JX], NX).
%% It is not tabled to execute the sub-goals and produce the
%% side-effects
query_stack_goal(X, AttI, AttI, AttJ) :-
	q_exec(X),
	AttJ = -([X], 0).

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

% q_exec(A .\=. B) :- !,
% 	(
% 	    call(A .\=. B) ->
% 	    %	    print('OK'),nl,
% 	    true
% 	;
% %	    print('Note: Disequality fails checking:  '), print(A .\=. B), nl,
% 	    fail).
q_exec(X) :-
	call(X).


query_stack_forall(forall(Var, Goal), AttI, AttO) :-
	%	AttI = s([], 0),
	query_goals([Goal], AttI, AttO, _AttJ),
	\+ \+ check_unbound(Var, Goal, AttI).
check_unbound(Var, Goal, AttI) :-
	neg_var(Var, List), !,
	clean(Var),
	exec_with_neg_list(Var, Goal, List, AttI).
check_unbound(Var, _Goal, _) :-
	var(Var).
exec_with_neg_list(_,   _,    [],         _).
exec_with_neg_list(Var, Goal, [Value|Vs], AttI) :-
	%	AttI = s([], 0),
	\+ \+ (
	    Var = Value,
	    query_goals([Goal], AttI, _AttO, _AttJ)
	),
	exec_with_neg_list(Var, Goal, Vs, AttI).
get_justification_forall(forall(_, Goal), AttI, FAttJ) :-
	%	AttI = s([], 0),
	findall(AttJ, query_goals([Goal], AttI, _AttO, AttJ), AttJList),
	combine(AttJList, FAttJ).
combine([],     R) :- R = -([], 0).
combine([J|Js], R) :-
	combine(Js, R1), R1 = -(J1, N1),
	J = -(J0, N0),
	N is N1 + N0,
	R = -([J0|J1], N).



%% The model is obtained from the justification tree.
print_model(AttJ) :-
	AttJ = -([F|J], _),
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
	AttS = s(Stack, _),
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


%% PATH/2 recursive %%
% pr_rule(path(A, B), [edge(A, Z), path(Z, B)]).
% pr_rule(path(A, B), [edge(A, B)]).
% pr_rule(edge(2, 1), []).
% pr_rule(edge(1, 2), []).
% pr_rule(edge(1, 1), []).
% pr_rule(edge(2, 4), []).
% pr_rule(add_to_query, []).


%% EXAMPLE with entailment in disequality %%
pr_rule(p(X), [X .\=. 4, q(X)]).
pr_rule(p(X), [X .\=. 4]).
pr_rule(q(X), [X .\=. 5]).
pr_rule(add_to_query, []).


%% Here there are some programs to check TCLP(asp)
:- push_prolog_flag(quiet, error).
%:- include('pr/birds2_pr.pl').                      %% ?? flies(X).
%:- include('pr/pos_loop_simple_pr.pl').             %% ?? p(X). %% the expected result is 'no'
%:- include('pr/pq_loop_pr.pl').                     %% ?? p(X).
%:- include('pr/forall_pr.pl').                      %% ?? not_p.
%:- include('pr/loopvar_pr.pl').                     %% ?? [p(X), r(Y)].
%:- include('pr/gpa_pr.pl').                         %% ?? interview(john).
%:- include('pr/hanoi_pr.pl').                       %% ?? hanoi(3,T).
%:- include('pr/hamcycle_pr.pl').                    %% ?? chosen(1,2).
%:- include('pr/doblenegation_pr.pl').               %% ?? p(X).  %% the expected result is 'no'
%  %:- include('pr/queens_pr.pl').                      %% ?? nqueens(4,Q).
%  %:- include('pr/twomodelshamiltonian_pr.pl').        %% ?? reachable(0).
%:- include('pr/twojustifications_pr.pl').           %% ?? p.  %% it only store one model

:- pop_prolog_flag(quiet).
