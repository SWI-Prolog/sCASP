:- module(scasp_predicates,
          [ table_predicate/1,          % ?Goal
            shown_predicate/1,
            scasp_builtin/1,            % ?Goal
            prolog_builtin/1,           % ?Goal
            clp_builtin/1,              % ?Goal
            clp_interval/1,             % ?Goal
            user_predicate/1,           % ?Goal
            scasp_compiled/1            % ?Head
          ]).
:- use_module(ops, [op(_,_,_)]).
:- meta_predicate
    user_predicate(:),
    shown_predicate(:),
    table_predicate(:).

/** <module> Basic information about sCASP predicates
*/

%!  user_predicate(:Goal)
%
%   Success if Goal is a user predicate

user_predicate(_:builtin(_)) => fail.
user_predicate(_:not(_ is _)) => fail.
user_predicate(_:not(true)) => fail.
user_predicate(_:not(fail)) => fail.
user_predicate(_:not(_)) => true.
user_predicate(M:Goal) =>
    functor(Goal, Name, Arity),
    M:pr_user_predicate(Name/Arity), !.

%!  table_predicate(:Goal)
%
%   Success if Goal is defined as a tabled predicate with  the directive
%   `:- table pred/n.`

table_predicate(M:Goal) =>
    functor(Goal, Name, Arity),
    M:pr_table_predicate(Name/Arity).

shown_predicate(M:not(Goal)) :-
    !,
    user_predicate(M:Goal).
shown_predicate(Goal) :-
    user_predicate(Goal).

%!  scasp_builtin(?Goal)
%
%   True when Goal is built-in

scasp_builtin(Head) :-
    prolog_builtin(Head).
scasp_builtin(Head) :-
    clp_builtin(Head).
scasp_builtin(Head) :-
    clp_interval(Head).
scasp_builtin(_ is _).
scasp_builtin(findall(_,_,_)).

%!  prolog_builtin(?Goal)
%
%   Success  if  Goal  is  a  builtin  prolog  predicate  (the  compiler
%   introduced its dual)

prolog_builtin(true).
prolog_builtin(fail).
prolog_builtin(false).
prolog_builtin(_ = _).
prolog_builtin(_ \= _).
prolog_builtin(_ < _).
prolog_builtin(_ > _).
prolog_builtin(_ >= _).
prolog_builtin(_ =< _).

%!  clp_builtin(?Goal)
%
%   Success if Goal is a builtin constraint predicate

clp_builtin(_ #= _).
clp_builtin(_ #<> _).
clp_builtin(_ #< _).
clp_builtin(_ #> _).
clp_builtin(_ #>= _).
clp_builtin(_ #=< _).

%!  clp_interval(?Goal)
%
%   Success  if  Goal  is  a  builtin  constraint  predicate  to extract
%   interval limits

clp_interval(inf(_Expr, _Inf)).
clp_interval(sup(_Expr, _Inf)).


%!  scasp_compiled(?Head)
%
%   True when Head is part of the transformed representation.

scasp_compiled(pr_rule(_Head, _Body, _Origin)).
scasp_compiled(pr_query(_Query)).
scasp_compiled(pr_user_predicate(_Pred)).
scasp_compiled(pr_table_predicate(_Pred)).
scasp_compiled(pr_show_predicate(_Pred)).
scasp_compiled(pr_pred_predicate(_Atom, _Children, _Cond, _Human)).


