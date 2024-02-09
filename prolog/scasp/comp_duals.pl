/*
* Copyright (c) 2016, University of Texas at Dallas
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the University of Texas at Dallas nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY OF TEXAS AT DALLAS BE LIABLE FOR
* ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

:- module(scasp_comp_duals,
          [ comp_duals/0,
            comp_duals3/2,
            define_forall/3
          ]).

/** <module> Dual rule computation

Computation of dual rules (rules for the negation of a literal).

@author Kyle Marple
@version 20170127
@license BSD-3
*/

:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(common).
:- use_module(program).
:- use_module(variables).
:- use_module(options).

:- create_prolog_flag(scasp_plain_dual, false, []).

%!  comp_duals is det
%
%   Compute rules for the negations of   positive literals (dual rules),
%   even if there are no clauses for the positive literal (negation will
%   be a fact). Wrapper for comp_duals2/1.

:- det(comp_duals/0).

comp_duals :-
    debug(scasp(compile), 'Computing dual rules...', []),
    defined_predicates(Preds),
    maplist(comp_dual, Preds).

%!  comp_dual(+Predicate) is det
%
%   get matching rules and call comp_duals3/2.

comp_dual('_false_0') :-	% Headless rules are handled by NMR check
    !.
comp_dual(X) :-
    scasp_builtin_encoded(X),   % skip dual of scasp builtins
    !.
comp_dual(X) :-
    findall(R, (defined_rule(X, H, B, _), c_rule(R, H, B)), Rs), % get rules for a single predicate
    comp_duals3(X, Rs).

scasp_builtin_encoded('call_1').
scasp_builtin_encoded('findall_3').
scasp_builtin_encoded('inf_2').
scasp_builtin_encoded('sup_2').

%!  comp_duals3(+Predicate:atom, +Rules:list) is det
%
%   Compute the dual for  a  single   positive  literal.  Make sure that
%   Predicate is used for the dual head  instead of taking the head from
%   one of the rules. This allows a  new   head  to be passed during NMR
%   sub-check creation.
%
%   @arg Predicate The head of each rule in Rules, of the form Head/arity.
%   @arg Rules The list of rules for a single predicate.

:- det(comp_duals3/2).

comp_duals3(P, []) :-
    !,		% Predicate is called but undefined. Dual will be a fact.
    predicate(H, P, []), % create a dummy predicate for outer_dual_head/2.
    outer_dual_head(H, Hd),
    c_rule(Rd, Hd, []),
    assert_rule(dual(Rd)).
comp_duals3(P, R) :- % predicate is defined by one or more rules.
    predicate(H, P, []), % create a dummy predicate for P.
    outer_dual_head(H, Hd),
    comp_dual(Hd, R, Db, 1),
    c_rule(Rd, Hd, Db),
    assert_rule(dual(Rd)).

%!  comp_dual(+DualHead:compound, +Rules:list, -DualBody:list, +Count:int) is det
%
%   Compute the dual for a predicate with multiple rules. First, compute
%   the dual of each individual rule, replacing   each  head with a new,
%   unique one. Then create the overall  dual   using  the  heads of the
%   individual duals as goals. When finished, assert the overall dual.
%
%   @arg DualHead The head of the dual rule.
%   @arg Rules The list of rules.
%   @arg DualBody The body of the outer dual rule.
%   @arg Count Counter used to ensure that new heads are unique.

comp_dual(_, [], [], _) :-
    !.
comp_dual(Hn, [X|T], [Dg|Db], C) :-
    c_rule(X, H, B),
    % get unique head with Hn2 including original args and Hn3 using variable args
    predicate(H, _, A1),
    predicate(Hn, F, A2),
    replace_prefix(F, n_, n__, F2),       % add underscore to make it non-printing.
    create_unique_functor(F2, C, F3),
    abstract_structures(A1, A3, 0, G),
    append(G, B, B2),
    prep_args(A3, A2, [], A4, [], 0, G2), % get var list for inner dual clause heads and inner unifiability goals
    append(G2, B2, B3), % combine all goals
    predicate(Dh, F3, A4), % inner dual clause head
    body_vars(Dh, B2, Bv),
    predicate(Dg, F3, A2), % outer dual goal for inner dual
    comp_dual2(Dh, B3, Bv), % create inner dual clauses
    C2 is C + 1,
    !,
    comp_dual(Hn, T, Db, C2).

%!  comp_dual2(+DualHead:compound, +Body:list, +BodyVars:list) is det
%
%   Compute the dual for a single clause.   Since  any body variables in
%   the original rule  are  existentially,   they  must  be  universally
%   quantified in the dual.  This  is   accomplished  by  creating a new
%   predicate with both  the  original  head   variables  and  the  body
%   variables in the head, which will contain  the duals of the original
%   goals. The "inner dual" will  then  call   this  new  predicate in a
%   forall over the body variables.
%
%   @arg DualHead The head of the dual rule.
%   @arg Body The body of the original rule.
%   @arg BodyVars The list of variables in the body but not the head.

comp_dual2(Hn, Bg, []) :-
    !,				% no body variables
    comp_dual3(Hn, Bg, []).
comp_dual2(Hn, Bg, Bv) :-
    Hn =.. [F|A1],
    append(A1, Bv, A2),
    length(A2, Arity),
    split_functor(F, Base0, _), % remove arity
    atomic_list_concat([Base0, '_vh', Arity], Base),
    join_functor(F2, Base, Arity),
    Hn2 =.. [F2|A2], % add body variables to innermost head.
    define_forall(Hn2, G, Bv), % get the call to the innermost dual
    comp_dual3(Hn2, Bg, []), % create innermost duals
    c_rule(Rd, Hn, [G]), % create dual
    assert_rule(dual(Rd)).

%!  comp_dual3(+DualHead:compound, +Body:list, +UsedGoals:list) is det
%
%   Compute the innermost dual for a single   rule by negating each goal
%   in turn. Unlike grounded ASP, it is not enough to have a single-goal
%   clause for each original goal. Because   side  effects are possible,
%   the sub-dual for a given goal must   include  all previous goals and
%   retain program order.
%
%   @arg DualHead The head of the dual rule.
%   @arg Body The body goals to negate.
%   @arg UsedGoals The goals that have already been processed, in original
%        order.

comp_dual3(_, [], _) :-
    !.
comp_dual3(Hn, [X|T], U) :-
    X = builtin_1(_), % handle built-ins specially
    !,
    (   current_prolog_flag(scasp_plain_dual, true)
    ->  U2 = [X]
    ;   append(U, [X], U2)
    ),
    comp_dual3(Hn, T, U2).
comp_dual3(Hn, [X|T], U) :-
    dual_goal(X, X2),
    (   current_prolog_flag(scasp_plain_dual, true)
    ->  Db = [X2]
    ;   append(U, [X2], Db) % Keep all goals prior to the dual one.
    ),
    c_rule(Rd, Hn, Db), % Clause for negation of body goal
    assert_rule(dual(Rd)),
    append(U, [X], U2),
    comp_dual3(Hn, T, U2).

%!  dual_goal(+GoalIn:compound, -GoalOut:compound) is det
%
%   Given a goal, negated it.
%
%   @arg GoalIn The original goal.
%   @arg GoalOut The negated goal.

% constraint
dual_goal(#=(A, B), #<>(A,B)).
dual_goal(#<>(A, B), #=(A,B)).
dual_goal(#>(A, B), #=<(A,B)).
dual_goal(#<(A, B), #>=(A,B)).
dual_goal(#>=(A, B), #<(A,B)).
dual_goal(#=<(A, B), #>(A,B)).
% clpq/r
dual_goal(#=(A, B), #<>(A,B)).
dual_goal(#<>(A, B), #=(A,B)).
dual_goal(#>(A, B), #=<(A,B)).
dual_goal(#<(A, B), #>=(A,B)).
dual_goal(#>=(A, B), #<(A,B)).
dual_goal(#=<(A, B), #>(A,B)).

dual_goal(=\=(A, B), =:=(A,B)).
dual_goal(=:=(A, B), =\=(A,B)).
dual_goal(<(A, B), >=(A,B)).
dual_goal(>(A, B), =<(A,B)).
dual_goal(=<(A, B), >(A,B)).
dual_goal(>=(A, B), <(A,B)).
dual_goal(@<(A, B), @>=(A,B)).
dual_goal(@>(A, B), @=<(A,B)).
dual_goal(@=<(A, B), @>(A,B)).
dual_goal(@>=(A, B), @<(A,B)).
%dual_goal(=(A, B), '#<>'(A,B)).
dual_goal(=(A, B), \=(A,B)).
dual_goal(\=(A, B), =(A,B)).
%dual_goal('#<>'(A, B), =(A,B)).
dual_goal(is(A, B), not(is(A,B))). % special case for is
dual_goal(not(X), X) :-
    predicate(X, _, _),
    !.
dual_goal(X, not(X)) :-
    predicate(X, _, _),
    !.

%!  define_forall(+GoalIn:compound, -GoalOut:compound, +BodyVars:list) is det
%
%   If BodyVars is empty, just  return   the  original  goal. Otherwise,
%   define a forall for  the  goal.   For  multiple  body variables, the
%   forall will be nested, with each layer containing a single variable.
%
%   @arg GoalIn Input goal.
%   @arg GoalOut Output goal.
%   @arg BodyVars Body variables present in GoalIn.

define_forall(G, G, []) :-
    !.
define_forall(Gi, forall(X, G2), [X|T]) :-
    define_forall(Gi, G2, T).

%!  outer_dual_head(+Head:atom, -DualHead:compound) is det
%
%   Create the dual version of a  rule   head  by negating the predicate
%   name and replacing the args with a variable list of the same arity.
%
%   @arg Head The initial, positive head, a predicate name.
%   @arg DualHead The dual head, a predicate struct.

outer_dual_head(H, D) :-
    predicate(H, P, _Args),
    negate_functor(P, Pd),
    split_functor(Pd, _, A),		% get the arity
    var_list(A, Ad),			% get the arg list
    predicate(D, Pd, Ad).

%!  abstract_structures(+ArgsIn:list, -ArgsOut:list, +Counter:int,
%!                      -Goals:list) is det
%
%   Given a list of args, abstract any structures by replacing them with
%   variables  and  adding  a  goal  unifying   the  variable  with  the
%   structure.
%
%   @arg ArgsIn The original args from a clause head.
%   @arg ArgsOut Output new args.
%   @arg Counter Input counter.
%   @arg Goals Goals unifying non-variables with the variables replacing them.

abstract_structures([], [], _, []).
abstract_structures([X|T], [$Y|T2], C, [G|Gt]) :-
    compound(X),
    \+ is_var(X),
    !,
    atom_concat('_Z', C, Y),
    C1 is C + 1,
    G = ($Y = X),
    abstract_structures(T, T2, C1, Gt).
abstract_structures([X|T], [X|T2], C, G) :-
    abstract_structures(T, T2, C, G).


%!  prep_args(+OrigArgs:list, +VarArgs:list,
%!            +NewArgsIn:list, -NewArgsOut:list,
%!            +VarsSeen:list, +Counter:int, -Goals:list) is det
%
%   Given two sets of args, check if  each   of  the  original args is a
%   variable. If so, check if it's a   member of NewArgsIn. If it's not,
%   add it to output args. If it isn't   a  variable, or the variable is
%   present in NewArgsIn, add the   corresponding variable from VarArgs.
%   The result is a list of variables   that  keeps any variables in the
%   original  head.  When  an  element  from  VarArgs  is  used,  add  a
%   unification (=) goal to Goals.
%
%   @arg OrigArgs The original args from a clause head.
%   @arg VarArgs A list of unique variable names of the same length as OrigArgs.
%   @arg NewArgsIn Input new args.
%   @arg NewArgsOut Output new args.
%   @arg VarsSeen List of variables encountered in original args.
%   @arg Counter Input counter.
%   @arg Goals Goals unifying non-variables with the variables replacing them.

:- det(prep_args/7).

prep_args([], _, Ai, Ao, _, _, []) :-
    reverse(Ai, Ao). % Restore proper ordering
prep_args([X|T], [Y|T2], Ai, Ao, Vs, C, [G|Gt]) :-
    is_var(X),
    memberchk(X, Vs), % X has already been seen
    !,
    G = (Y=X),     % create unification goal
    prep_args(T, T2, [Y|Ai], Ao, Vs, C, Gt).
prep_args([X|T], [_|T2], Ai, Ao, Vs, C, G) :-
    is_var(X),
    !,
    prep_args(T, T2, [X|Ai], Ao, [X|Vs], C, G).
prep_args([X|T], [Y|T2], Ai, Ao, Vs, C, Go) :-
    X =.. [F|X2], % X is a compound term; process it.
    !,
    prep_args2(X2, X3, Vs, Vs2, C, C2, Gs),
    Xo =.. [F|X3],
    G = (Y=Xo), % create unification goal
    !,
    prep_args(T, T2, [Y|Ai], Ao, Vs2, C2, Gt),
    append([G|Gs], Gt, Go).
prep_args([X|T], [Y|T2], Ai, Ao, Vs, C, [G|Gt]) :-
    G = (Y=X), % create unification goal
    prep_args(T, T2, [Y|Ai], Ao, Vs, C, Gt).

%!  prep_args2(+ArgsIn:list, -ArgsOut:list,
%!             +VarsSeenIn:list, -VarsSeenOut:list,
%!             +CounterIn:int, -CounterOut:int, -UniGoals:list) is det
%
%   Given the arguments from a compound  term, create unifiability goals
%   to be used in the dual.
%
%   @arg ArgsIn Input args.
%   @arg ArgsOut Output args.
%   @arg VarsSeenIn Input vars seen.
%   @arg VarsSeenOut Output vars seen.
%   @arg CounterIn Input counter.
%   @arg CounterOut Output counter.
%   @arg UniGoals List of unification goals.

:- det(prep_args2/7).

prep_args2([], [], Vs, Vs, C, C, []).
prep_args2([X|T], [Y|T2], Vsi, Vso, Ci, Co, [G|Gt]) :-
    is_var(X),
    !,
    (   memberchk(X, Vsi) % X has been seen
    ->  Vs1 = Vsi
    ;   Vs1 = [X|Vsi]
    ),
    atom_concat('_Y', Ci, Y),
    C1 is Ci + 1,
    G = (Y=X), % create unification goal
    prep_args2(T, T2, Vs1, Vso, C1, Co, Gt).
prep_args2([X|T], [Y|T2], Vsi, Vso, Ci, Co, Go) :-
    X =.. [F|X2], % X is a compound term
    !,
    atom_concat('_Y', Ci, Y),
    C1 is Ci + 1,
    prep_args2(X2, X3, Vsi, Vs1, C1, C2, Gs),
    Xo =.. [F|X3],
    G = (Y=Xo), % create unification goal
    prep_args2(T, T2, Vs1, Vso, C2, Co, Gt),
    append([G|Gs], Gt, Go).
prep_args2([X|T], [Y|T2], Vsi, Vso, Ci, Co, [G|Gt]) :-
    % X isn't a variable or compound term
    atom_concat('_Y', Ci, Y),
    C1 is Ci + 1,
    G = (Y=X), % create unification goal
    prep_args2(T, T2, Vsi, Vso, C1, Co, Gt).
