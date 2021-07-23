:- module(clp_disequality,
          [ get_neg_var/2,
            not_unify/2,
            (.=.)/2,
            (.\=.)/2,
            loop_term/2,

            op(700, xfx, .=.),
            op(700, xfx, .\=.)
          ]).
:- use_module(scasp_options).
:- use_module(clp_clpq).

/** <module> Constraint solver for disequalities

This module contains the code of the constraint solver for disequalities
following   the   description   of   the   constructive   unification  /
disunification from the paper __Computing Stable Models of  Normal Logic
Programs  Without  Grounding__  by _Marple  et al.  2017_. .=./2  is the
predicate  used  for  equality.  .\=./2  is   the  predicate   used  for
disequality.

@author Joaquin Arias
*/


		 /*******************************
		 *   CONSTRUCTIVE UNIFICATION   *
		 *******************************/

%!  A .=. B
%
%   Constructive unification of a negatively constrained variable with a
%   non- variable value will succeed if  the non-variable value does not
%   constructively unify with any element   in the variable’s prohibited
%   value list.

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
    nonvar(B),
    !,
    (   get_neg_var(A,NegListA)
    ->  not_unify(B, NegListA),
        clean(A)
    ;   true
    ),
    A = B.
% - Constructive unification of two negatively constrained variables
% will always succeed, setting their shared prohibited value list to
% the union of their original lists.
unify2(A, B) :-
    get_neg_var_or_empty(A,NegListA),
    get_neg_var_or_empty(B,NegListB),
    !,
    ord_union(NegListA,NegListB,NegList),
    update(A,NegList),
    clean(B),
    B = A.
% - In cases where neither argument contains a negatively constrained
% variable, the result is identical to that of traditional
% unification.
unify2(A,B) :-
    A = B.

		 /*******************************
		 *  CONSTRUCTIVE DISUNIFICATION	*
		 *******************************/

%!  A .\=. B

.\=.(A,B) :-
    (   unifiable(A,B,U0)
    ->  reverse(U0, U),
        member(Var=Value, U),
        not_unify2(Var, Value)
    ;   true
    ).

% - in accordance with the restrictions given in Section 3.1.5,
% constructive disunification of two non ground variables will
% produce an error
not_unify2(_,B) :-
    var(B),
    !,
    fail.
not_unify2(A, B) :-
    is_clpq_var(A), !,
    disequality_clpq(A,B).
% - Constructive disunification of a negatively constrained variable
% and a non- variable value will always succeed, adding the
% "ground" value to the variable’s prohibited value list.
not_unify2(A, B) :-
    ground(B),
    neg_var(A, NegListA),
    ord_add_element(NegListA, B, NegList),
    update(A, NegList).

%!  not_unify(+Term, +List)
%
%   True when Term cannot unify with any of the elements in List

not_unify(_A, []) :- !.
not_unify(A, [X|Xs]) :-
    A .\=. X,
    not_unify(A,Xs).

%!  loop_term(+Goal1, +Goal2)
%
%

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


%!  loop_var_disequality(?A, ?B)
%
%   ???

loop_var_disequality(A, B) :-
    neg_var(A, ListA),
    neg_var(B, ListB),
    (   ListA == [],
        ListB \== []
    ->  loop_var_disequality_(A, ListB)
    ;   ListB == [],
        ListA \== []
    ->  loop_var_disequality_(B, ListA)
    ).
loop_var_disequality(A, B) :-
    if_user_option(check_calls, format('\t\tLoop_var_disequality( ~p , ~p )\n',[A,B])),
    A .\=. B.

loop_var_disequality_(A, [NegB|_]) :-
    A .=. NegB.
loop_var_disequality_(A, [_|NegBs]) :-
    loop_var_disequality_(A, NegBs).


		 /*******************************
		 *     AUXILIAR PREDICATES      *
		 *******************************/

neg_var(A, List) :-
    (   get_attr(A, clp_disequality, neg(List))
    ->  true
    ;   var(A)
    ->  List = [],
        put_attr(A, clp_disequality, neg(List))
    ).

get_neg_var(A,List) :-
    get_attr(A, clp_disequality, neg(List)).

get_neg_var_or_empty(A,List) :-
    var(A),
    (   get_attr(A,clp_disequality,neg(List))
    ->  true
    ;   List = []
    ).

clean(A) :-
    del_attr(A, clp_disequality).

update(A,List) :-
    put_attr(A,clp_disequality,neg(List)).

attr_unify_hook(neg(A),B) :-
    not_unify(B, A).

attribute_goals(X) -->
    { get_neg_var(X, G)
    },
    [.\=.(X, G)].

attr_portray_hook(neg(List), Var) :-
    format("~p .\\=. ~p",[Var,List]).
