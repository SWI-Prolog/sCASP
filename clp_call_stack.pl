:- module(clp_call_stack, _).



:- use_module(.(clp_disequality_rt)).
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
:- multifile attr_unify_hook/2, attribute_goals/3, attr_portray_hook/2.
attr_unify_hook(rules(Att), B) :- get_attr_local(B, rules(AttB)), Att = AttB.
attr_unify_hook(neg(Att), B) :- print(alert), get_attr_local(B, rules(AttB)), Att = AttB.
attribute_goals(X) --> [X ~> G], {get_attr_local(X, rules(G))}.
attr_portray_hook(Att, A) :- format(" ~w  .is  ~w ", [A, Att]).
attr_portray_hook(Att, A) :- format(" ~w  .\=  ~w ", [A, Att]).
%% Auxiliar predicates %%


%% TCLP interface %%
call_domain_projection(Var, Dom) :-
	call_domain_projection_(Var, Dom).
call_domain_projection_([],[]).
call_domain_projection_([X|Xs], [neg(D)|Ds]) :- dump_neg_list(X, D), call_domain_projection_(Xs, Ds).
call_domain_projection_([X|Xs], Ds) :- \+ dump_neg_list(X, _), call_domain_projection_(Xs, Ds).
call_entail(_, Ds, Ds).
call_store_projection(_, St, St).


% answer_domain_projection(X, D) :-
% 	print(X),nl,
% 	answer_domain_projection_(X, D),
% 	display(D),nl.
answer_domain_projection(X, D) :- answer_domain_projection_(X, D).
answer_domain_projection_([],     []).
answer_domain_projection_([X|Xs], [D|Ds]) :-
	X ~> D, answer_domain_projection_(Xs, Ds).
answer_domain_projection_([X|Xs], [neg(List)|Ds]) :-
	\+ X ~> _, dump_neg_list(X,List), answer_domain_projection_(Xs, Ds).

answer_check_entail(_, D1, D2, R, _) :-
	answer_check_entail_(_, D1, D2, R, _).
answer_check_entail_(_, [],       [],        1, _).
answer_check_entail_(_, [s(_,_)|D1s], [_D2|D2s], R, _) :-
	answer_check_entail_(_, D1s, D2s, R, _).
answer_check_entail_(_, [neg(List)|D1s], [neg(List)|D2s], R, _) :-
	answer_check_entail_(_, D1s, D2s, R, _).
answer_check_entail_(_, [neg(List1)|_D1s], [neg(List2)|_D2s], 1, _) :-
	entail_neg_list(List2,List1), !.
answer_check_entail_(_, [neg(List1)|_D1s], [neg(List2)|_D2s], -1, _) :-
	entail_neg_list(List1,List2).
% answer_check_entail_(_, [neg(List1)|_D1s], [neg(List2)|_D2s], _R, _) :-
% 	List1 \= List2, fail.
% answer_check_entail_(_, [-(_,_)|D1s], [_D2|D2s], R, _) :-
% %	fail.
% 	answer_check_entail_(_, D1s, D2s, R, _).
answer_check_entail_(_, [-(_, N1)], [-(_, N2)], 1,  _) :- N1 >= N2.
answer_check_entail_(_, [-(_, N1)], [-(_, N2)], -1, _) :- N1 < N2.

answer_store_projection(_, St, St).


%apply_answer(Var, Ans) :-
%	print(enter(Var,Ans)),nl,
%	apply_answer_(Var, Ans).
%	print(exit(Var,Ans)),nl.
apply_answer(Var, Ans) :-
	apply_answer_(Var, Ans).

apply_answer_([],     []).
apply_answer_([X|Xs], [-(P, N)|Ds]) :-
	X <~ -(P, N), apply_answer_(Xs, Ds).
apply_answer_([X|Xs], [s(P, N)|Ds]) :-
	\+ X ~> _, X <~ s(P, N), apply_answer_(Xs, Ds).
apply_answer_([X|Xs], [s(_, _)|Ds]) :-
	X ~> _, apply_answer_(Xs, Ds).
apply_answer_([X|Xs], [neg(List)|Ds]) :-
	not_unify(X,List), apply_answer_(Xs, Ds).
%% TCLP interface %%
