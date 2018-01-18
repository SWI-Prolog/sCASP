:- module(clp_disequality_rt,_).

%% ------------------------------------------------------------- %%
:- use_package(assertions).
:- doc(title, "Constraint solver for disequalities").
:- doc(author, "Joaquin Arias").
:- doc(filetype, module).

:- doc(module, "

This module contains the code of the constraint solver for
disequalities following the description of the constructive
unification / disunification from the paper @bf{Computing Stable Models
of Normal Logic Programs Without Grounding} by @em{Marple et al. 2017}.

@pred{.=./2} is the predicate used for equality.

@pred{.\=./2} is the predicate used for disequality.

").

%% ------------------------------------------------------------- %%

:- use_package(attr).
:- use_module(library(sets)).
:- use_module(library(terms_check)).
:- use_package(assertions).
:- dynamic disunify/2.

:- op(700, xfx, [(.\=.),(.=.)]).

%% ------------------------------------------------------------- %%
:- doc(section, "Main predicates").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Constructive Unification %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% - Constructive unification of a negatively constrained variable
%% with a non- variable value will succeed if the non-variable value
%% does not constructively unify with any element in the variable’s
%% prohibited value list.
.=.(A,B) :-
	neg_var(A,NegListA),
	non_var(B), !,
	not_unify(B, NegListA),
	clean(A),
	A = B.

.=.(B,A) :-
	neg_var(A,NegListA),
	non_var(B), !,
	not_unify(A, NegListA),
	clean(A),
	A = B.

%% - Constructive unification of two negatively constrained variables
%% will always succeed, setting their shared prohibited value list to
%% the union of their original lists.
.=.(A,B) :-
	neg_var(A,NegListA), 
	neg_var(B,NegListB), !,
	ord_union(NegListA,NegListB,NegList),
	update(A,NegList),
	clean(B),
	B = A.

%% - Constructive unification of two compound terms is performed
%% recursively: first, the functors and arities are tested, then each
%% pair of corresponding arguments is constructively unified.

%% particular case for lists (they are also struct)
.=.([A|As], [B|Bs]) :- true, !,
	length(As,N), length(Bs,N),
	A .=. B,
	As .=. Bs.

.=.(A,B) :-
	struct(A),
	struct(B), !,
	A =.. [Name | La],
	B =.. [Name | Lb],
	La .=. Lb.

%% - In cases where neither argument contains a negatively constrained
%% variable, the result is identical to that of traditional
%% unification.
.=.(A,B) :-
	A = B.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Constructive disunification %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

.\=.(A,B) :- 
	ground(A),
	ground(B), !,
	A \= B.

:- use_module(clp_clpq).
.\=.(A,B) :-
	is_clpq_var(A), !,
	disequality_clpq(A,B).
.\=.(A,B) :-
	is_clpq_var(B), !,
	disequality_clpq(B,A).
% .\=.(A,B) :-
% 	var(A),
% 	num(B),
% 	disequality_clpq(A,B).
% .\=.(A,B) :-
% 	var(B),
% 	num(A),
% 	disequality_clpq(B,A).

	    
%% - Constructive disunification of a negatively constrained variable
%% and a non- variable value will always succeed, adding the
%% "ground" value to the variable’s prohibited value list.
.\=.(A,B) :-
	neg_var(A,NegListA),
	ground(B), !,
	assert(disunify(A,B)),
	insert(NegListA,B,NegList),
	update(A,NegList).

.\=.(B,A) :-
	neg_var(A,NegListA),
	ground(B), !,
	assert(disunify(A,B)),
	insert(NegListA,B,NegList),
	update(A,NegList).


%% - in accordance with the restrictions given in Section 3.1.5,
%% constructive disunification of two non ground variables will
%% produce an error
.\=.(A,B) :-
	var(A), var(B),
	\+ ground(A),
	\+ ground(B), !,
	send_silent_signal(error),
%	format('ERROR: disunification expect at least one argument to be ground, got:\n~p\n~p\n',[A,B]),
	fail.


%% - Constructive disunification of two compound terms is performed by
%% first test- ing functors and arities. If either of these does not
%% match, the operation succeeds deterministically. Otherwise, the
%% pairs of corresponding arguments are handled
%% recursively. Non-deterministic success occurs as soon as the
%% operation succeeds for a pair of arguments, with subsequent pairs
%% tested upon backtracking.

%% particular case for lists (they are also struct)
.\=.(ListA, ListB) :-
	\+ var(ListA),
	\+ var(ListB),
	ListA = [A|As],
	ListB = [B|Bs], !,
	(
	    %	    print(disequality(A,.\=.,B)),nl,
	    A .\=. B
	;
	    %	    print(disequality(As,.\=.,Bs)),nl,
	    As .\=. Bs
	).

.\=.(A,B) :-
	\+ var(A),
	\+ var(B),
	struct(A),
	struct(B), !,
	A =.. [NameA | As],
	B =.. [NameB | Bs],
	(
	    NameA \= NameB ->
	    true
	;
	    As .\=. Bs
	).


% .\=.(A,B) :-
% 	print('vars'),
% 	disequality_clpq(A,B).

%% - In cases where neither argument contains a negatively constrained
%% variable, the result is identical to that of traditional
%% disunification.
.\=.(A,B) :-
	\+ var(A), \+ var(B),
	A \= B.


loop_list_disequality([A|As],[B|Bs]) :-
	(
	    loop_var_disequality(A,B)
	;
	    A .=. B,
	    loop_list_disequality(As,Bs)
	).

loop_var_disequality(A,B) :-
	neg_var(A,ListA),
	neg_var(B,ListB),
	ListA == [],
	ListB \== [],
	loop_var_disequality_(A,ListB).
loop_var_disequality(B,A) :-
	neg_var(A,ListA),
	neg_var(B,ListB),
	ListA == [],
	ListB \== [],
	loop_var_disequality_(A,ListB).
loop_var_disequality(A,B) :-
	A .\=. B.

loop_var_disequality_(A,[NegB|_]) :-
	A .=. NegB.
loop_var_disequality_(A, [_|NegBs]) :-
	loop_var_disequality_(A, NegBs).



not_unify(_A, []) :- !.
not_unify(A, [X|Xs]) :-
	A .\=. X,
	not_unify(A,Xs).



:- use_module(clp_clpq).
loop_list([A|As],[B|Bs]) :-
	(
	    loop_var_disequality(A,B)
	;
	%     loop_var_clpq(A,B)
	% ;
	    A .=. B,
	    loop_list(As,Bs)
	).


%%%%%%%%%%%%%%%%%%%%%%
%% Entailment check %%
%%%%%%%%%%%%%%%%%%%%%%

% entail(A,B) :-
% 	ground(A),
% 	ground(B), !,
% 	A = B.

%% - A negative constrained variable A entails another negative
%% constrained variables B if the prohibited value list of A is a
%% subset of the list of B.
entail(A,B) :-
	neg_var(A,NegListA),
	neg_var(B,NegListB), !,
	ord_subset(NegListA, NegListB).

%% - A negative constrained variable A entails a non-variable value if
%% the non-variable value does not constructively unify with any
%% element in the variable's prohibited value list.
entail(A,B) :-
	neg_var(A, NegListA),
	non_var(B), !,
	not_unify(B, NegListA).

%% - A compound term A entails a compound term B if recursively:
%% first, the functors and arities are equal and then each argument of
%% A entails its pair of B.

%% particular case for lists (they are also struct)
entail([A|As], [B|Bs]) :- true, !,
	length(As,N), length(Bs,N),
	entail(A,B),
	entail(As,Bs).

entail(A,B) :-
	struct(A),
	struct(B), !,
	A =.. [Name | La],
	B =.. [Name | Lb],
	entail(La,Lb).

%% - In cases where neither argument contains a negatively constrained
%% variable, subsumption is used to check entailment.
entail(A,B) :-
	subsumes_term(A,B).


%%%%%%%%%
entail_neg_list(L1, L2) :-
	ord_subset(L1, L2).

%%%%%%%%%%%%%%%%%%%
%% Join operator %%
%%%%%%%%%%%%%%%%%%%

% join(A, B, Join) :-
% 	neg_var(A, NegListA),
% 	neg_var(B, NegListB),
% 	keep_more_particular(NegListA,NegListB,NegListJoin),
% 	add(Join,NegListJoin).


%% ------------------------------------------------------------- %%
:- doc(section, "Auxiliar predicates").
	
%% Auxiliar predicates %%
neg_var(A,List) :-
	(
	    get_attr_local(A,neg(List)), true ->
	    true
	;
	    var(A),
	    List = [],
	    put_attr_local(A,neg(List))
	).
get_neg_var(A,List) :-
	get_attr_local(A,neg(List)).
unbound(A) :-
	(
	    get_attr_local(A,neg(List)), true ->
	    List == []
	;
	    true
	).
non_var(A) :- \+ var(A).
clean(A) :- del_attr_local(A).
update(A,List) :- put_attr_local(A,neg(List)).
add(A,Value) :-
	(
	    neg_var(A,NegListA), true ->
	    insert(NegListA, Value, NegList),
	    update(A,NegList)
	;
	    put_attr_local(A,neg([Value]))
	).
dump_neg_list(A,neg(List)) :-
	get_attr_local(A,neg(List)).


:- multifile attr_unify_hook/2, attribute_goals/3, attr_portray_hook/2.
attr_unify_hook(neg(A),B) :-
	(
	    not_unify(B,A) ->
	    true
	;
	    % print('Fail unification between:  '),
	    % print(B),print('  and the neg list '), print(A),nl,
	    fail
	).

attribute_goals(X) --> 
	[.\=.(X, G)],
	 {get_attr_local(X,neg(G))}.
attr_portray_hook(neg(List), Var) :-
	format(" ~w  .\\=. ~w ",[Var,List]).

:- multifile portray_attribute/2.
portray_attribute(att(_,false,att(clp_disequality_rt,neg(List),_)),Var) :-
	(
	    List == [] ->
	    display(Var)
	;
	    format("{ ~w  .\\=. ~w }",[Var,List])
	).
	
%% Auxiliar predicates %%

%% ------------------------------------------------------------- %%
 
%% Not needed %%
insert_more_general([A|As],B,[A|As]) :-
	entail(A,B), !.
insert_more_general([A|As],B,[B|As]) :-
	entail(B,A), !.
insert_more_general([A|As],B,[A|Rs]) :-
	insert_more_general(As,B,Rs).
insert_more_general([],B,[B]).
%% Not needed %%
