:- module(scasp_predicates,
          [ table_predicate/1,          % ?Goal
            shown_predicate/1,
            prolog_builtin/1,           % ?Goal
            clp_builtin/1,              % ?Goal
            clp_builtin_translate/2,    % ?Goal,?Goal_T
            clp_interval/1,             % ?Goal
            user_predicate/1            % ?Goal
          ]).
:- use_module(scasp_ops).
:- use_module(sasp/output).           % the pr_* predicates

/** <module> Basic information about sCASP predicates
*/

%!  user_predicate(?Goal)
%
%   Success if Goal is a user predicate

user_predicate(builtin(_)) => fail.
user_predicate(not(_ is _)) => fail.
user_predicate(not(true)) => fail.
user_predicate(not(fail)) => fail.
user_predicate(not(_)) => true.
user_predicate(Goal) =>
    functor(Goal, Name, Arity),
    pr_user_predicate(Name/Arity), !.

%!  table_predicate(?Goal)
%
%   Success if Goal is defined as a tabled predicate with  the directive
%   `:- table pred/n.`

table_predicate(Goal) :-
    functor(Goal, Name, Arity),
    pr_table_predicate(Name/Arity).

shown_predicate(not(Goal)) :-
    !,
    user_predicate(Goal).
shown_predicate(Goal) :-
    user_predicate(Goal).


%!  prolog_builtin(?Goal)
%
%   Success  if  Goal  is  a  builtin  prolog  predicate  (the  compiler
%   introduced its dual)

prolog_builtin(true).
prolog_builtin(fail).
prolog_builtin(_ = _).
prolog_builtin(_ \= _).
prolog_builtin(_ < _).
prolog_builtin(_ > _).
prolog_builtin(_ >= _).
prolog_builtin(_ =< _).

%!  clp_builtin(?Goal)
%
%   Success if Goal is a builtin constraint predicate

clp_builtin(_ .=. _).
clp_builtin(_ .<>. _).
clp_builtin(_ .<. _).
clp_builtin(_ .>. _).
clp_builtin(_ .>=. _).
clp_builtin(_ .=<. _).

%!  clp_builtin_translate(?Goal, ?Goal_T)
%
%   Translate s(CASP) constraints into CLP(Q/R) syntax

clp_builtin_translate(A #=  B, A .=.  B).
clp_builtin_translate(A #<> B, A .<>. B).
clp_builtin_translate(A #<  B, A .<.  B).
clp_builtin_translate(A #>  B, A .>.  B).
clp_builtin_translate(A #>= B, A .>=. B).
clp_builtin_translate(A #=< B, A .=<. B).

%!  clp_interval(?Goal)
%
%   Success  if  Goal  is  a  builtin  constraint  predicate  to extract
%   interval limits

clp_interval(inf(_Expr, _Inf)).
clp_interval(sup(_Expr, _Inf)).
