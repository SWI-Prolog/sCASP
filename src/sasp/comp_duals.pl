:- module(comp_duals, [
                    comp_duals/0,
                    comp_duals3/2,
                    define_forall/3,
                    plain_dual/1
                  ]).

/** <module> Dual rule computation

Computation of dual rules (rules for the negation of a literal).

@author Kyle Marple
@version 20170127
@license BSD-3
*/

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

:- use_module(library(lists)).
:- use_module(common).
:- use_module(program).
:- use_module(variables).

%! comp_duals is det
% Compute rules for the negations of positive literals (dual rules), even if
% there are no clauses for the positive literal (negation will be a fact).
% Wrapper for comp_duals2/1.
comp_duals :-
    write_verbose(0, 'Computing dual rules...\n'),
    defined_predicates(Preds),
    comp_duals2(Preds),
    !.
comp_duals :-
    write_error('could not compute dual rules'),
    !,
    fail.

scasp_builtin('call_1').
scasp_builtin('findall_3').
%! comp_duals2(+Predicates:list) is det
% For each predicate in Predicates, get matching rules and call comp_duals3/2.
%
% @param Predicates List of predicates in the program.
comp_duals2([X | T]) :-
    X = '_false_0', % skip headless rules; handled by NMR check
    !,
    comp_duals2(T).
comp_duals2([X | T]) :-
    scasp_builtin(X),  % skip dual of scasp builtins
    !,
    comp_duals2(T).
comp_duals2([X | T]) :-
    findall(R, (defined_rule(X, H, B), rule(R, H, B)), Rs), % get rules for a single predicate
    comp_duals3(X, Rs),
    !,
    comp_duals2(T).
comp_duals2([]) :-
    !.
    
%! comp_duals3(+Predicate:ground, +Rules:list) is det
% Compute the dual for a single positive literal. Make sure that Predicate is
% used for the dual head instead of taking the head from one of the rules. This
% allows a new head to be passed during NMR sub-check creation.
%
% @param Predicate The head of each rule in Rules, of the form Head/arity.
% @param Rules The list of rules for a single predicate.
comp_duals3(P, R) :-
    R \= [], % predicate is defined by one or more rules.
    !,
    predicate(H, P, []), % create a dummy predicate for P.
    outer_dual_head(H, Hd),
    comp_dual(Hd, R, [], 1),
    !.
comp_duals3(P, R) :-
    R = [], % Predicate is called but undefined. Dual will be a fact.
    !,
    predicate(H, P, []), % create a dummy predicate for outer_dual_head/2.
    outer_dual_head(H, Hd),
    rule(Rd, Hd, []),
    assert_rule(Rd),
    !.

%! comp_dual(+DualHead:compound, +Rules:list, +DualBody:list, +Count:int) is det
% Compute the dual for a predicate with multiple rules. First, compute the dual
% of each individual rule, replacing each head with a new, unique one. Then
% create the overall dual using the heads of the individual duals as goals. When
% finished, assert the overall dual.
%
% @param DualHead The head of the dual rule.
% @param Rules The list of rules.
% @param DualBody The body of the outer dual rule.
% @param Count Counter used to ensure that new heads are unique.
comp_dual(Hn, [X | T], Db, C) :-
    rule(X, H, B),
    % get unique head with Hn2 including original args and Hn3 using variable args
    predicate(H, _, A1),
    predicate(Hn, F, A2),
    atom_chars(F, ['n', '_' | Fc]),
    atom_chars(F2, ['n', '_', '_' | Fc]), % add underscore to make it non-printing.
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
    comp_dual(Hn, T, [Dg | Db], C2).
comp_dual(Hn, [], Db, _) :-
    reverse(Db, Db2), % restore proper goal order
    rule(Rd, Hn, Db2),
    assert_rule(Rd),
    !.

%! comp_dual2(+DualHead:compound, +Body:list, +BodyVars:list) is det
% Compute the dual for a single clause. Since any body variables in the original
% rule are existentially, they must be universally quantified in the dual. This
% is accomplished by creating a new predicate with both the original head
% variables and the body variables in the head, which will contain the duals of
% the original goals. The "inner dual" will then call this new predicate in a
% forall over the body variables.
%
% @param DualHead The head of the dual rule.
% @param Body The body of the original rule.
% @param BodyVars The list of variables in the body but not the head.
comp_dual2(Hn, Bg, Bv) :-
    Bv \= [],
    !,
    Hn =.. [F | A1],
    append(A1, Bv, A2),
    length(A2, L),
    number_chars(L, Lc),
    split_functor(F, Fc, _), % remove arity
    append(Fc, ['_' | Lc], Fc2), % add new arity
    atom_chars(F2, Fc2), % Create functor for innermost dual
    Hn2 =.. [F2 | A2], % add body variables to innermost head.
    define_forall(Hn2, G, Bv), % get the call to the innermost dual
    comp_dual3(Hn2, Bg, []), % create innermost duals
    rule(Rd, Hn, [G]), % create dual
    assert_rule(Rd),
    !.
comp_dual2(Hn, Bg, []) :-
    !, % no body variables
    comp_dual3(Hn, Bg, []).

:- dynamic plain_dual/1. % see scasp_io
%! comp_dual3(+DualHead:compound, +Body:list, +UsedGoals:list) is det
% Compute the innermost dual for a single rule by negating each goal in turn.
% Unlike grounded ASP, it is not enough to have a single-goal clause for each
% original goal. Because side effects are possible, the sub-dual for a given
% goal must include all previous goals and retain program order.
%
% @param DualHead The head of the dual rule.
% @param Body The body goals to negate.
% @param UsedGoals The goals that have already been processed, in original
%        order.
comp_dual3(Hn, [X | T], U) :-
    X = builtin_1(_), % handle built-ins specially
    (  plain_dual(on) ->  
        append([], [X], U2)
    ;
        append(U, [X], U2)
    ),
    !,
    comp_dual3(Hn, T, U2).
comp_dual3(Hn, [X | T], U) :-
    dual_goal(X, X2),
    (  plain_dual(on) ->
        append([], [X2], Db)
    ;
        append(U, [X2], Db) % Keep all goals prior to the dual one.
    ),
    rule(Rd, Hn, Db), % Clause for negation of body goal
    assert_rule(Rd),
    append(U, [X], U2),
    !,
    comp_dual3(Hn, T, U2).
comp_dual3(_, [], _) :-
    !.

%! dual_goal(+GoalIn:compound, -GoalOut:compound) is det
% Given a goal, negated it.
%
% @param GoalIn The original goal.
% @param GoalOut The negated goal.

%% constraint 
dual_goal(#=(A, B), #<>(A,B)).
dual_goal(#<>(A, B), #=(A,B)).
dual_goal(#>(A, B), #=<(A,B)).
dual_goal(#<(A, B), #>=(A,B)).
dual_goal(#>=(A, B), #<(A,B)).
dual_goal(#=<(A, B), #>(A,B)).
%% clpq/r
dual_goal(.=.(A, B), .<>.(A,B)).
dual_goal(.<>.(A, B), .=.(A,B)).
dual_goal(.>.(A, B), .=<.(A,B)).
dual_goal(.<.(A, B), .>=.(A,B)).
dual_goal(.>=.(A, B), .<.(A,B)).
dual_goal(.=<.(A, B), .>.(A,B)).

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
%dual_goal(=(A, B), '.\=.'(A,B)).
dual_goal(=(A, B), \=(A,B)).
dual_goal(\=(A, B), =(A,B)).
%dual_goal('.\=.'(A, B), =(A,B)).
dual_goal(is(A, B), not(is(A,B))). % special case for is
dual_goal(not(X), X) :-
    predicate(X, _, _),
    !.
dual_goal(X, not(X)) :-
    predicate(X, _, _),
    !.

%! define_forall(+GoalIn:compound, -GoalOut:compound, +BodyVars:list) is det
% If BodyVars is empty, just return the original goal. Otherwise, define a
% forall for the goal. For multiple body variables, the forall will be nested,
% with each layer containing a single variable.
%
% @param GoalIn Input goal.
% @param GoalOut Output goal.
% @param BodyVars Body variables present in GoalIn.
define_forall(Gi, Go, [X | T]) :-
    define_forall(Gi, G2, T), % get inner portion
    Go = forall(X, G2). % build outer portion
define_forall(G, G, []) :-
    !.
    
%! outer_dual_head(+Head:compound, -DualHead:ground) is det
% Create the dual version of a rule head by negating the predicate name and
% replacing the args with a variable list of the same arity.
%
% @param Head The initial, positive head, a predicate struct.
% @param DualHead The dual head, a predicate struct.
outer_dual_head(H, D) :-
    predicate(H, P, _),
    negate_functor(P, Pd),
    split_functor(Pd, _, A), % get the arity
    var_list(A, 0, [], Ad), % get the arg list
    predicate(D, Pd, Ad),
    !.

%! abstract_structures(+ArgsIn:list, -ArgsOut:list, +Counter:int, -Goals:list) is det
% Given a list of args, abstract any structures by replacing them with variables
% and adding a goal unifying the variable with the structure.
%
% @param ArgsIn The original args from a clause head.
% @param ArgsOut Output new args.
% @param Counter Input counter.
% @param Goals Goals unifying non-variables with the variables replacing them.
abstract_structures([X | T], [Y | T2], C, [G | Gt]) :-
    X =.. [_ | Xt], % X is a compound term; process it.
    Xt \= [],
    !,
    number_chars(C, Cc), % get chars from counter
    append(['_', 'Z'], Cc, Vc), % _Z# for unique names at this point
    atom_chars(Y, Vc),
    C1 is C + 1,
    G =.. [=, Y, X], % create unification goal
    !,
    abstract_structures(T, T2, C1, Gt).
abstract_structures([X | T], [X | T2], C, G) :-
    !, % X is not a compound term
    abstract_structures(T, T2, C, G).
abstract_structures([], [], _, []) :-
    !.

%! prep_args(+OrigArgs:list, +VarArgs:list, +NewArgsIn:list, -NewArgsOut:list, +VarsSeen:list, +Counter:int, -Goals:list) is det
% Given two sets of args, check if each of the original args is a variable. If
% so, check if it's a member of NewArgsIn. If it's not, add it to output args.
% If it isn't a variable, or the variable is present in NewArgsIn, add the
% corresponding variable from VarArgs. The result is a list of variables that
% keeps any variables in the original head. When an element from VarArgs is
% used, add a unification (=) goal to Goals.
%
% @param OrigArgs The original args from a clause head.
% @param VarArgs A list of unique variable names of the same length as OrigArgs.
% @param NewArgsIn Input new args.
% @param NewArgsOut Output new args.
% @param VarsSeen List of variables encountered in original args.
% @param Counter Input counter.
% @param Goals Goals unifying non-variables with the variables replacing them.
prep_args([X | T], [Y | T2], Ai, Ao, Vs, C, [G | Gt]) :-
    is_var(X),
    member(X, Vs), % X has already been seen
    G =.. [=, Y, X], % create unification goal
    !,
    prep_args(T, T2, [Y | Ai], Ao, Vs, C, Gt).
prep_args([X | T], [_ | T2], Ai, Ao, Vs, C, G) :-
    is_var(X),
    !,
    prep_args(T, T2, [X | Ai], Ao, [X | Vs], C, G).
prep_args([X | T], [Y | T2], Ai, Ao, Vs, C, Go) :-
    X =.. [F | X2], % X is a compound term; process it.
    !,
    prep_args2(X2, X3, Vs, Vs2, C, C2, Gs),
    Xo =.. [F | X3],
    G =.. [=, Y, Xo], % create unification goal
    !,
    prep_args(T, T2, [Y | Ai], Ao, Vs2, C2, Gt),
    append([G | Gs], Gt, Go).
prep_args([X | T], [Y | T2], Ai, Ao, Vs, C, [G | Gt]) :-
    !, % X is not a variable or a compound term
    G =.. [=, Y, X], % create unification goal
    prep_args(T, T2, [Y | Ai], Ao, Vs, C, Gt).
prep_args([], _, Ai, Ao, _, _, []) :-
    reverse(Ai, Ao), % Restore proper ordering
    !.

%! prep_args2(+ArgsIn:list, -ArgsOut:list, +VarsSeenIn:list, -VarsSeenOut:list, +CounterIn:int, -CounterOut:int, -UniGoals:list) is det
% Given the arguments from a compound term, create unifiability goals to be
% used in the dual.
%
% @param ArgsIn Input args.
% @param ArgsOut Output args.
% @param VarsSeenIn Input vars seen.
% @param VarsSeenOut Output vars seen.
% @param CounterIn Input counter.
% @param CounterOut Output counter.
% @param UniGoals List of unification goals.
prep_args2([X | T], [Y | T2], Vsi, Vso, Ci, Co, [G | Gt]) :-
    is_var(X),
    !,
    (member(X, Vsi) -> % X has been seen
            Vs1 = Vsi
    ;
            Vs1 = [X | Vsi]
    ),
    number_chars(Ci, Cc), % get chars from counter
    append(['_', 'Y'], Cc, Vc),
    atom_chars(Y, Vc),
    C1 is Ci + 1,
    G =.. [=, Y, X], % create unification goal
    !,
    prep_args2(T, T2, Vs1, Vso, C1, Co, Gt).
prep_args2([X | T], [Y | T2], Vsi, Vso, Ci, Co, Go) :-
    X =.. [F | X2], % X is a compound term
    !,
    number_chars(Ci, Cc), % get chars from counter
    append(['_', 'Y'], Cc, Vc),
    atom_chars(Y, Vc),
    C1 is Ci + 1,
    prep_args2(X2, X3, Vsi, Vs1, C1, C2, Gs),
    Xo =.. [F | X3],
    G =.. [=, Y, Xo], % create unification goal
    !,
    prep_args2(T, T2, Vs1, Vso, C2, Co, Gt),
    append([G | Gs], Gt, Go).
prep_args2([X | T], [Y | T2], Vsi, Vso, Ci, Co, [G | Gt]) :-
    % X isn't a variable or compound term
    number_chars(Ci, Cc), % get chars from counter
    append(['_', 'Y'], Cc, Vc),
    atom_chars(Y, Vc),
    C1 is Ci + 1,
    G =.. [=, Y, X], % create unification goal
    !,
    prep_args2(T, T2, Vsi, Vso, C1, Co, Gt).
prep_args2([], [], Vs, Vs, C, C, []) :-
    !.
