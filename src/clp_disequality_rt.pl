:- module(clp_disequality_rt,
          [ get_neg_var/2,
            not_unify/2,
            loop_list/2,
            (.=.)/2,
            (.\=.)/2,
            loop_term/2,

            op(700, xfx, .=.),
            op(700, xfx, .\=.)
          ]).

%% ------------------------------------------------------------- %%

/** <module> Constraint solver for disequalities

This module contains the code of the constraint solver for disequalities
following   the   description   of   the   constructive   unification  /
disunification from the paper __Computing Stable Models of  Normal Logic
Programs  Without  Grounding__  by _Marple  et al.  2017_. .=./2  is the
predicate  used  for  equality.  .\=./2  is   the  predicate   used  for
disequality.

@author Joaquin Arias
*/


%% ------------------------------------------------------------- %%
:- use_module(scasp_io).

:- use_package(attr).
:- dynamic disunify/2.

:- op(700, xfx, [(.\=.),(.=.)]).

%% ------------------------------------------------------------- %%
		 /*******************************
		 *       MAIN PREDICATES        *
		 *******************************/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Constructive Unification %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% - Constructive unification of a negatively constrained variable
%% with a non- variable value will succeed if the non-variable value
%% does not constructively unify with any element in the variable’s
%% prohibited value list.

:- if(\+((dif(A,b), unifiable(a(A), a(b), _)))).
:- format(user_error, 'ERROR: unifiable/3 validates constraints~n', []),
   halt(1).
:- endif.

.=.(A,B) :-
    unifiable(A,B,U),
    unify(U).

unify([]).
unify([Var=Value|T]) :-
    unify2(Var, Value),
    unify(T).

unify2(A, B) :-
    nonvar(B), !,
    (   get_neg_var(A,NegListA)
    ->  not_unify(B, NegListA),
        clean(A)
    ;   true
    ),
    A = B.
%% - Constructive unification of two negatively constrained variables
%% will always succeed, setting their shared prohibited value list to
%% the union of their original lists.
unify2(A, B) :-
    get_neg_var_or_empty(A,NegListA),
    get_neg_var_or_empty(B,NegListB), !,
    ord_union(NegListA,NegListB,NegList),
    update(A,NegList),
    clean(B),
    B = A.
%% - In cases where neither argument contains a negatively constrained
%% variable, the result is identical to that of traditional
%% unification.
unify2(A,B) :-
    A = B.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Constructive disunification %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

.\=.(A,B) :-
    (   unifiable(A,B,U0)
    ->  reverse(U0, U),
        member(Var=Value, U),
        not_unify2(Var, Value)
    ;   true
    ).

%% - in accordance with the restrictions given in Section 3.1.5,
%% constructive disunification of two non ground variables will
%% produce an error
not_unify2(_,B) :-
    var(B),
    !,
    fail.
not_unify2(A, B) :-
    is_clpq_var(A), !,
    disequality_clpq(A,B).
%% - Constructive disunification of a negatively constrained variable
%% and a non- variable value will always succeed, adding the
%% "ground" value to the variable’s prohibited value list.
not_unify2(A,B) :-
    ground(B),
    neg_var(A,NegListA),
    ord_add_element(NegListA,B,NegList),
    update(A,NegList).

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
    if_user_option(check_calls, format('\t\tLoop_var_disequality( ~p , ~p )\n',[A,B])),
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

loop_term(Goal1, Goal2) :-
    functor(Goal1, Name, Arity),
    functor(Goal2, Name, Arity),
    loop_term(1, Arity, Goal1, Goal2).

loop_term(I, Arity, Goal1, Goal2) :-
    I =< Arity,
    arg(I, Goal1, A),
    arg(I, Goal2, B),
    (   loop_var_disequality(A,B)
    ->  true
    ;   A .=. B,
        I2 is I+1,
        loop_term(I2, Arity, Goal1, Goal2)
    ).


%%%%%%%%%%%%%%%%%%%%%%
%% Entailment check %%
%%%%%%%%%%%%%%%%%%%%%%

% entail(A,B) :-
%       ground(A),
%       ground(B), !,
%       A = B.

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
    nonvar(B), !,
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
    compound(A),
    compound(B), !,
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
%       neg_var(A, NegListA),
%       neg_var(B, NegListB),
%       keep_more_particular(NegListA,NegListB,NegListJoin),
%       add(Join,NegListJoin).


%% ------------------------------------------------------------- %%
		 /*******************************
		 *     AUXILIAR PREDICATES      *
		 *******************************/


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

get_neg_var_or_empty(A,List) :-
    var(A),
    (   get_attr_local(A,neg(List))
    ->  true
    ;   List = []
    ).


unbound(A) :-
    var(A),
    (
        get_attr_local(A,neg(List)), true ->
        List == []
    ;
        true
    ).
clean(A) :- del_attr_local(A).
update(A,List) :- put_attr_local(A,neg(List)).
add(A,Value) :-
    (
        neg_var(A,NegListA), true ->
        ord_add_element(NegListA, Value, NegList),
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
    format("~w.\\=.~w",[Var,List]).

:- multifile portray_attribute/2.
portray_attribute(att(_,false,att(clp_disequality_rt,neg(List),_)),Var) :-
    (
        List == [] ->
        display(Var)
    ;
        format(" {~w ∉ ~w} ",[Var,List])
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
