:- module(diseq_oracle,
          [ (..=..)/2,
            (..\=..)/2,
            v/0,

            op(700, xfx, ..=..),
            op(700, xfx, ..\=..)
          ]).

:- op(700, xfx, .=.).
:- op(700, xfx, .\=.).

v :-
    wrap_predicate(clp_disequality_rt:(A .=. B), diseq,
                   Wrapped1,
                   diseq_oracle:v1(A,B,Wrapped1)),
    wrap_predicate(clp_disequality_rt:(A2 .\=. B2), diseq,
                   Wrapped2,
                   diseq_oracle:v2(A2,B2,Wrapped2)).


v1(A,B,Wrapped) :-
    copy_term(A+B, CA+CB),
    (   Wrapped
    ->  (   CA ..=.. CB
        ->  (   A+B =@= CA+CB
            ->  true
            ;   format(user_error, 'Different constraints for ~p~n', [A .=. B])
            )
        ;   format(user_error, 'Should fail ~p~n', [A .=. B]),
            break
        )
    ;   (   CA ..=.. CB
        ->  format(user_error, 'Should succeed ~p~n', [A .=. B])
        ;   true
        )
    ).


v2(A,B,Wrapped) :-
    copy_term(A+B, CA+CB),
    (   Wrapped
    ->  (   CA ..\=.. CB
        ->  copy_term(A+B,A1+B1,C1a),
            copy_term(CA+CB,CA1+CB1,CC1a),
            sort(C1a, C1),
            sort(CC1a, CC1),
            (   A1+B1+C1 =@= CA1+CB1+CC1
            ->  true
            ;   format(user_error, 'Different constraints for ~q~n', [(A1 .\=. B1 -> C1)]),
                format(user_error, 'Different constraints for ~q~n', [(CA1 .\=. CB1 -> CC1)])
            )
        ;   format(user_error, 'Should fail ~p~n', [A .\=. B]),
            break
        )
    ;   (   CA ..\=.. CB
        ->  format(user_error, 'Should succeed ~p~n', [A .\=. B])
        ;   true
        )
    ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Constructive Unification %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% - Constructive unification of a negatively constrained variable
%% with a non- variable value will succeed if the non-variable value
%% does not constructively unify with any element in the variable’s
%% prohibited value list.
..=..(A,B) :-
    neg_var(A,NegListA),
    nonvar(B), !,
    not_unify(B, NegListA),
    clean(A),
    A = B.

..=..(B,A) :-
    neg_var(A,NegListA),
    nonvar(B), !,
    not_unify(A, NegListA),             % JW: BUG!
    clean(A),
    A = B.

%% - Constructive unification of two negatively constrained variables
%% will always succeed, setting their shared prohibited value list to
%% the union of their original lists.
..=..(A,B) :-
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
..=..([A|As], [B|Bs]) :- true, !,
    length(As,N), length(Bs,N),
    A ..=.. B,
    As ..=.. Bs.

..=..(A,B) :-
    compound(A),
    compound(B), !,
    A =.. [Name | La],
    B =.. [Name | Lb],
    La ..=.. Lb.

%% - In cases where neither argument contains a negatively constrained
%% variable, the result is identical to that of traditional
%% unification.
..=..(A,B) :-
    A = B.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Constructive disunification %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

..\=..(A,B) :-
    ground(A),
    ground(B), !,
    A \= B.

/*
..\=..(A,B) :-
    is_clpq_var(A), !,
    disequality_clpq(A,B).
..\=..(A,B) :-
    is_clpq_var(B), !,
    disequality_clpq(B,A).
*/
% ..\=..(A,B) :-
%       var(A),
%       num(B),
%       disequality_clpq(A,B).
% ..\=..(A,B) :-
%       var(B),
%       num(A),
%       disequality_clpq(B,A).


%% - Constructive disunification of a negatively constrained variable
%% and a non- variable value will always succeed, adding the
%% "ground" value to the variable’s prohibited value list.
..\=..(A,B) :-
    neg_var(A,NegListA),
    ground(B), !,
    ord_add_element(NegListA,B,NegList),
    update(A,NegList).

..\=..(B,A) :-
    neg_var(A,NegListA),
    ground(B), !,
    ord_add_element(NegListA,B,NegList),
    update(A,NegList).


%% - in accordance with the restrictions given in Section 3.1.5,
%% constructive disunification of two non ground variables will
%% produce an error
..\=..(A,B) :-
    var(A), var(B),
    \+ ground(A),
    \+ ground(B), !,
%       send_silent_signal(error),
%       format('ERROR: disunification expect at least one argument to be ground, got:\n~p\n~p\n',[A,B]),
    fail.


%% - Constructive disunification of two compound terms is performed by
%% first test- ing functors and arities. If either of these does not
%% match, the operation succeeds deterministically. Otherwise, the
%% pairs of corresponding arguments are handled
%% recursively. Non-deterministic success occurs as soon as the
%% operation succeeds for a pair of arguments, with subsequent pairs
%% tested upon backtracking.

%% particular case for lists (they are also struct)
% ..\=..([],[]) :- !, fail.
% ..\=..(ListA, ListB) :-
%       \+ var(ListA),
%       \+ var(ListB),
%       ListA = [A|As],
%       ListB = [B|Bs], !,
%       format('disequality ~p \= ~p\n',[ListA,ListB]),
%       (
%           format('\tdisequality ~p \= ~p\n',[A,B]),
%           A ..\=.. B
%       ;
%           format('\tdisequality ~p \= ~p\n',[A,B]),
%           As ..\=.. Bs
%       ).

% ..\=..(A,B) :-
%       \+ var(A),
%       \+ var(B),
%       struct(A),
%       struct(B), !,
%       A =.. [NameA | As],
%       B =.. [NameB | Bs],
%       (
%           NameA \= NameB ->
%           true
%       ;
%           As ..\=.. Bs
%       ).

..\=..(A,B) :-
    \+ var(A),
    \+ var(B),
    compound(A),
    compound(B), !,
%       format('check_dual(~p \= ~p)\n',[A,B]),
    (
        unifiable(A,B,Unifier) ->
%           format('check_dual(~p)\n',[Unifier]),
        check_dual(Unifier)
    ;
        true
    ).

% ..\=..(A,B) :-
%       print('vars'),
%       disequality_clpq(A,B).

%% - In cases where neither argument contains a negatively constrained
%% variable, the result is identical to that of traditional
%% disunification.
..\=..(A,B) :-
    \+ var(A), \+ var(B),
    A \= B.

check_dual([A=B|_]) :-
    A ..\=.. B.
check_dual([_|Ds]) :-
    check_dual(Ds).

not_unify(_A, []) :- !.
not_unify(A, [X|Xs]) :-
    A ..\=.. X,
    not_unify(A,Xs).

neg_var(A,List) :-
    (
        get_attr(A,diseq_oracle,neg(List)), true ->
        true
    ;
        var(A),
        List = [],
        put_attr(A,diseq_oracle,neg(List))
    ).

update(A,List) :- put_attr(A,diseq_oracle,neg(List)).

clean(A) :- del_attr(A, diseq_oracle).

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
    {get_attr(X,diseq_oracle,neg(G))}.

