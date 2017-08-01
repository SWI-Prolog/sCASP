:- module(clp_call_stack, _).


:- use_package(attr).

:- op(700, xfx, [(~>), (<~), (<~>)]).
A ~> Att :- get_attr_local(A, rules(Att)).
A <~ Att :- put_attr_local(A, rules(Att)).
dump_rules([],     [],     []).
dump_rules([X|Xs], [_|Ns], [D|Ds]) :-
	get_attr_local(X, rules(D)),
	dump_rules(Xs, Ns, Ds).
dump_rules([X|Xs], Ns, Ds) :-
	\+ get_attr_local(X, rules(_)),
	dump_rules(Xs, Ns, Ds).


%% Auxiliar predicates %%
attr_unify_hook(rules(Att), B) :- get_attr_local(B, rules(AttB)), Att = AttB.
attribute_goals(X) --> [X ~> G], {get_attr_local(X, rules(G))}.
attr_portray_hook(Att, A) :- format(" ~w  .is  ~w ", [A, Att]).
%% Auxiliar predicates %%


%% TCLP interface %%
call_domain_projection(_, _).
call_entail(_, _, _).
call_store_projection(_, _, _).


answer_domain_projection(X, D) :- answer_domain_projection_(X, D).
answer_domain_projection_([],     []).
answer_domain_projection_([X|Xs], [D|Ds]) :-
	X ~> D, answer_domain_projection_(Xs, Ds).
answer_domain_projection_([X|Xs], [ok|Ds]) :-
	\+ X ~> _, answer_domain_projection_(Xs, Ds).

answer_check_entail(_, D1, D2, R, _) :-
	answer_check_entail_(_, D1, D2, R, _).
answer_check_entail_(_, [],       [],        1, _).
answer_check_entail_(_, [D1|D1s], [_D2|D2s], R, _) :-
	D1 \= -(_, _), answer_check_entail_(_, D1s, D2s, R, _).
answer_check_entail_(_, [-(_, N1)], [-(_, N2)], 1,  _) :- N1 >= N2.
answer_check_entail_(_, [-(_, N1)], [-(_, N2)], -1, _) :- N1 < N2.

answer_store_projection(_, St, St).


apply_answer(Var, Ans) :- apply_answer_(Var, Ans).
apply_answer_([],     []).
apply_answer_([X|Xs], [-(P, N)|Ds]) :- X <~ -(P, N), apply_answer_(Xs, Ds).
apply_answer_([X|Xs], [s(P, N)|Ds]) :-
	\+ X ~> _, X <~ s(P, N), apply_answer_(Xs, Ds).
apply_answer_([X|Xs], [s(_, _)|Ds]) :-
	X ~> _, apply_answer_(Xs, Ds).
%% TCLP interface %%
