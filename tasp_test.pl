


:- use_module(tclp_asp).


:- push_prolog_flag(quiet, error).
%:- include('pr/birds2_pr.pl').                      query(X) :- ?? flies(X).
%:- include('pr/pos_loop_simple_pr.pl').             query(X) :- ?? p(X). %% the expected result s 'no'
%:- include('pr/pq_loop_pr.pl').                     query(X) :- ?? p(X).
%:- include('pr/forall_pr.pl').                      query(X) :- ?? not_p.
%:- include('pr/loopvar_pr.pl').                     query(Y) :- ?? [p(X), r(Y)].
%:- include('pr/gpa_pr.pl').                         query(X) :- ?? interview(john).
%:- include('pr/hanoi_pr.pl').                       query(X) :- ?? hanoi(8,X).
%:- include('pr/hamcycle_pr.pl').                    query(X) :- ?? chosen(1,2).
%:- include('pr/doblenegation_pr.pl').               query(X) :- ?? p(X).  %% the expected result is 'no'
%:- include('pr/chs_disunification_pr.pl').          query(X) :- ?? [p(1), not(p(X))].
%:- include('pr/queens_hide_pr.pl').                      query(X) :- ?? nqueens(4,X).
%:- include('pr/twomodelshamiltonian_pr.pl').        query(X) :- ?? reachable(0).
%:- include('pr/twojustifications_pr.pl').           query(X) :- ?? p.

%% clpfd examples
:- include('tasp_examples/fd_ex01_pr.pl').           query([X,Y]) :- ?? [p(X), go(X,Y)].
:- pop_prolog_flag(quiet).



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
% pr_rule(p(X), [X .\=. 4]).

% pr_rule(p(X), [X .\=. 4, q(X)]).   % 
% pr_rule(q(X), [X .\=. 4]).


% pr_rule(add_to_query, []).
