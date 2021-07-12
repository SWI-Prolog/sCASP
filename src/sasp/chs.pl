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

:- module(chs,
          [ chs_entry/5,
            new_chs/1,
            add_to_chs/10,
            remove_from_chs/3,
            check_chs/8
          ]).

/** <module> Handle operations related to the Coinductive Hypothesis Set (CHS).

Handle CHS-related operations.

@author Kyle Marple
@version 20170510
@license BSD-3
*/

:- use_module(library(lists)).
:- use_module(library(rbtrees)).
:- use_module(common).
:- use_module(debug).
:- use_module(output). % for fill_in_variable_values/5
:- use_module(solve).
:- use_module(variables).

%!  chs_entry(+Entry:compound, ?Functor:ground, ?Args:list,
%!            ?Success:int, ?InNMR:int) is det.
%!  chs_entry(?Entry:compound, +Functor:ground, +Args:list,
%!            +Success:int, +InNMR:int) is det.
%
%   Convert a CHS entry to its components and vice-versa.
%
%   @arg Entry The entry struct.
%   @arg Functor The predicate functor.
%   @arg Args The list of args.
%   @arg Success A 1 or 0 indicating whether or not the predicate has succeeded
%        yet (either via inductive success or recursive call). If 0, intervening
%        negations will need to be checked before coinductive success occurs.
%   @arg InNMR 1 if the goal was first added after entering the NMR check, 0
%        otherwise.
chs_entry(-(F, A, S, N), F, A, S, N).

%!  new_chs(-CHS:compound) is det
%
%   Create an empty CHS.
%
%   @arg CHS The new CHS.

new_chs(CHS) :-
    rb_empty(CHS),
    !.

%!  check_chs(+Functor:ground, +Args:list, +VarsIn:compound,
%!            -VarsOut:compound, +CHS:list, +CallStack:list, -Code:int,
%!            -EvenLoop:compound) is nondet
%
%   Check the CHS for a predicate or its negation. Be sure that the args
%   are unifiable, but don't actually  unify   them.  Set  code based on
%   result: -1 for failure (negation present), 1  for success, 0 for not
%   present. If not present, add to CHS with success flag set to 0. Note
%   that if a match is found, Args  are   unified  with  the args of the
%   match. Also, only use  cuts  after   coinductive  failure  and exact
%   matches. For other cases, we want to   be  able to backtrack and try
%   other matches. Finally,  check  for   intervening  negations  before
%   allowing coinductive success, failing on positive loops.
%
%   @arg Functor The name/arity of the predicate to check.
%   @arg Args Args of the call being checked.
%   @arg VarsIn Input variables.
%   @arg VarsOut Output variables.
%   @arg CHS The CHS.
%   @arg CallStack The list of ancestor calls. Used to ensure that positive
%        loops fail.
%   @arg Code Return value. -1, 0, 1. See above.
%   @arg EvenLoop Struct containing goals and vars for an even loop, if one is
%        found.

check_chs(F, A, V, V, C, _, _, _) :- % coinductive failure
    negate_functor(F, Fn),
    chs_entry(E, Fn, A, _, _),
    rb_lookup(Fn, Esn, C),
    once(exact_match(E, Esn, V, _)), % don't bother with advanced checks if an exact match is in the CHS
    !,
    fail.
check_chs(F, A, Vi, Vo, C, Cs, Flag, Eloop) :- % constructive coinductive failure
    chs_entry(E, F, A, _, _),
    (   rb_lookup(F, Es, C)
    ->  \+ exact_match(E, Es, Vi, _) % don't bother if an exact match is in the CHS
    ;   true
    ),
    once(get_dual_entries(E, Ens, C, Vi)),
    Ens \= [],
    G =.. [F | A],
    once(fill_in_variable_values(G, G2, [], _, Vi)),
    G2 =.. [_ | A2],
    once(replace_vars_in_all(Ens, Ens2, Vi, V1, 1)),
    match_neg(A2, Ens2, V1, V2, Rval),
    (   Rval = 0
    ->  !,
        fail
    ;   true
    ),
    once(check_chs2(F, A2, V2, Vo, C, Cs, Flag, Eloop)). % test new values
check_chs(F, A, Vi, Vo, C, Cs, Flag, Eloop) :- % coinductive success; success flag set
    check_chs2(F, A, Vi, Vo, C, Cs, Flag, Eloop).

%!  check_chs2(+Functor:ground, +Args:list, +VarsIn:compound,
%!             -VarsOut:compound, +CHS:list, +CallStack:list, -Code:int,
%!             -EvenLoop:compound) is nondet
%
%   Check the CHS for a  predicate.   This  handles  coinductive success
%   only. Coinductive failure is handled by check_chs/8.
%
%   @arg Functor The name/arity of the predicate to check.
%   @arg Args Args of the call being checked.
%   @arg VarsIn Input variables.
%   @arg VarsOut Output variables.
%   @arg CHS The CHS.
%   @arg CallStack The list of ancestor calls. Used to ensure that positive
%        loops fail.
%   @arg Code Return value. -1, 0, 1. See above.
%   @arg EvenLoop Struct containing goals and vars for an even loop, if one is
%        found.

check_chs2(F, A, V, V, C, _, 1, -([], [])) :- % coinductive success; success flag set
    chs_entry(E, F, A, 1, _),
    rb_lookup(F, Es, C), % get entries for predicate
    exact_match(E, Es, V, 1),
    !. % don't allow backtracking when we have an exact match
check_chs2(F, A, Vi, Vo, _, Cs, Rval, Eloop) :- % coinductive success; success flag not set
    G =.. [F | A],
    check_negations(G, Cs, Vi, Vo, 0, Rval2, Cyc, Cvars), % Rval2 = -1, 1 or 2
    (   Rval2 > 0                       % cycle
    ->  Eloop = -(Cyc, Cvars)
    ;   Eloop = -([], [])
    ),
    (   member(Rval2, [-1, 2]) % on exact match or coinductive failure, don't backtrack
    -> !
    ;  true
    ),
    (   Rval2 = 2
    ->  Rval = 1
    ;   Rval = Rval2
    ).
check_chs2(_, _, V, V, _, _, 0, -([], [])). % not present, or any matches consumed

%!  get_dual_entries(+Entry: compound, -DualEntries: list,
%!                   +CHS:compound, +VarStruct:compound)
%
%   Given a CHS entry, get CHS  entries   from  any  duals with which it
%   unifies. As we  know  the  functors   and  arity,  just  return  the
%   arguments instead of the complete entries.
%
%   @arg Entry The entry to test.
%   @arg DualEntries The list of argument lists for  matching dual entries.
%   @arg CHS The CHS
%   @arg VarStruct The variable structure to use in unification testing.

get_dual_entries(E, Ens, C, V) :-
    chs_entry(E, F, A, _, _),
    once(negate_functor(F, Fn)),
    rb_lookup(Fn, Ens2, C), % get matches for negation
    chs_entry(En, Fn, _, 1, _), % dummy entry for negation
    findall(An, (member(En, Ens2), chs_entry(En, _, An, _, _)), Ens3), % get args for entries matching negated functor
    once(replace_vars(A, A2, V, V2, [], _, 0)), % strip unifiability flags
    Gn =.. [Fn | A2],
    check_unifiability(Gn, Fn, Ens3, Ens, V2),
    !.

%!  check_unifiability(+Goal:compound, +NegFunctor:atom, +DualArgsIn:list,
%!                     -DualArgsOut:list, +VarStruct:compound)
%
%   Given a list of argument lists,  create   a  term  with each element
%   using NegFunctor and test unifiability with Goal.
%
%   @arg Goal The goal to test against.
%   @arg NegFunctor The functor to use in testing.
%   @arg DualArgsIn The input list of lists.
%   @arg DualArgsOut The output list of lists.
%   @arg VarStruct The variable struct to use in testing.

check_unifiability(G, F, [A | Asi], [A | Aso], V) :- % unifies
    once(replace_vars(A, A2, V, V2, [], _, 0)), % strip unifiability flags
    G2 =.. [F | A2],
    solve_unify(G, G2, V2, _, 0),
    !,
    check_unifiability(G, F, Asi, Aso, V).
check_unifiability(G, F, [_ | Asi], Aso, V) :- % does not unify
    !,
    check_unifiability(G, F, Asi, Aso, V).
check_unifiability(_, _, [], [], _) :-
    !.

%!  match_neg(+Args:list, +NegArgs:list, +VarsIn:compound,
%!            -VarsOut:compound, -Rval:int) is nondet
%
%   For each arg in Args, bind or constrain it using the values from the
%   corresponding elements in each entry of newargs. For bound values in
%   NewArgs, the arg cannot take those   values. For constrained values,
%   it must take one of those values.   If any is completely unbound, we
%   fail. Since there might be multiple possible values for each arg, we
%   have to be able to backtrack and select others.
%
%   @arg Args The input args.
%   @arg NegArgs List of lists of args from CHS entries for the predicate dual.
%   @arg VarsIn Input variables.
%   @arg VarsOut Output variables.
%   @arg RVal 1 or 0 indicating if values remain to be tested.

match_neg(_, [], V, V, 1).
match_neg(_, [], V, V, 0) :-
    !.
match_neg(A, As, Vi, Vo, R) :-
    As \= [],
    A \= [],
    once(select_heads(As, _, Hs)), % get heads from each entry in As
    match_neg2(A, Hs, As, Vi, Vo, R).
match_neg(_, _, _, _, 0) :- % out of values
    !.

%!  match_neg2(+Args:list, +OrigConstraints:list, +Duals:list
%!             +VarsIn:compound, -VarsOut:compound, -Rval:int) is nondet
%
%   Test the first arg. If the tests fail:
%
%     - If the arg is ground, remove any constraints that it satisfies.
%     - Test the next arg.
%
%   Succeed when all constraints are satisfied. Fail if no args remain.
%
%   @arg Args The input args.
%   @arg OrigConstraints The list of constraints on the first arg.
%   @arg Duals Lists of dual args to be used in testing subsequent input args.
%   @arg VarsIn Input variables.
%   @arg VarsOut Output variables.
%   @arg RVal 1 or 0 indicating if values remain to be tested.

match_neg2([X | _], Cs, _, Vi, Vo, 1) :-
    once(split_neg_matches(Cs, Cc, B, Lv, Vi)), % also handles failure on unbound non-loop variable
    once(intersect_all(Cc, C)),
    match_neg_test(X, C, B, Lv, Vi, Vo), % tests passed if this succeeds
    (   \+ is_unbound(X, Vi, _, _, _)
    ->  !
    ;   true
    ).
match_neg2([X | T], _, D, Vi, Vo, R) :-
    is_ground(X, Vi),
    !,
    once(remove_satisfied_constraints(X, D, D2, Vi, _)), % we can ignore variable changes, since they only affect the heads we're removing
    once(select_heads(D2, D3, _)), % remove entries for current arg
    match_neg(T, D3, Vi, Vo, R),
    !.
match_neg2([_ | T], _, D, Vi, Vo, R) :-
    once(select_heads(D, D2, _)), % remove entries for current arg
    match_neg(T, D2, Vi, Vo, R).

%!  match_neg_test(+Arg:compound, +Constraints:list, +BoundDuals:list,
%!                 +LoopVars:list, +VarsIn:compound,
%!                 -VarsOut:compound) is nondet
%
%   Test an arg against constraints applied by duals. If unbound, try to
%   find a value that satisfies all constraints.
%
%   @arg Arg The arg to test.
%   @arg Constraints The intersection of any constraints on the arg
%        (except loop vars).
%   @arg BoundDuals The set of bound values from duals corresponding to the arg.
%   @arg LoopVars Bound or constrained loop variables that should be constrained
%        against Arg, even if it's a variable.
%   @arg VarsIn Input variables.
%   @arg VarsOut Output variables.

match_neg_test(X, C, B, Lv, Vi, Vo) :-
    C \= [], % At least one constraint
    !,
    member(Y, C),
    solve_unify(X, Y, Vi, V1, 0), % X satisfies all constraints by unifying with a member of the intersection
    dne_all(X, B, V1, V1), % X doesn't match any bound duals
    constrain_loop_vars(Lv, X, V1, Vo). % constrain any loop vars against X
match_neg_test(X, [], B, Lv, Vi, Vo) :-
    !, % no constraints
    dne_all(X, B, Vi, V1), % no constraints, so test or constrain X against all bound duals
    constrain_loop_vars(Lv, X, V1, Vo). % constrain any loop vars against X

%!  constrain_loop_vars(+LoopVars:list, +Arg:compound,
%!                      +VarsIn:compound, -VarsOut:compound)
%
%   Constrain each loop variable in LoopVars against Arg, even if Arg is
%   a variable.
%
%   @arg LoopVars Bound or constrained loop variables that should be constrained
%        against Arg, even if it's a variable.
%   @arg Arg The arg to constrain against.
%   @arg VarsIn Input variables.
%   @arg VarsOut Output variables.

constrain_loop_vars([X | T], Y, Vi, Vo) :-
    add_var_constraint(X, Y, Vi, V1), % MUST BE BACKTRACKABLE!
    constrain_loop_vars(T, Y, V1, Vo).
constrain_loop_vars([], _, V, V) :-
    !.

%!  remove_satisfied_constraints(+Value:compound,
%!                               +DualArgsIn:list, -DualArgsOut:list,
%!                               +VarsIn:compound, -VarsOut:compound) is det
%
%   Keep only those constraint lists  whose   head  unifies  with Value.
%   Value will always be ground.
%
%   @arg Value The value to check against.
%   @arg DualArgsIn Input list of dual args lists.
%   @arg DualArgsOut List of dual args lists that aren't satisfied by Value.
%   @arg VarsIn Input variables.
%   @arg VarsOut Output variables.

remove_satisfied_constraints(V, [[X | Tx] | T], [[X | Tx] | T2], Vi, Vo) :-
    once(solve_unify(V, X, Vi, V1, 0)),
    !,
    remove_satisfied_constraints(V, T, T2, V1, Vo).
remove_satisfied_constraints(X, [_ | T], Do, Vi, Vo) :-
    !, % first arg doesn't unify
    remove_satisfied_constraints(X, T, Do, Vi, Vo).
remove_satisfied_constraints(_, [], [], V, V).

%!  intersect_all(+ListofLists:list, -Intersection:list) is det
%
%   Take the intersection of all lists in ListofLists.
%
%   @arg ListofLists Input lists.
%   @arg Intersection The intersection.

intersect_all([X | T], I) :-
    T \= [],
    !,
    intersect_all(T, I2),
    !,
    list_intersection(X, I2, I).
intersect_all([X], X) :-
    !.
intersect_all([], []) :-
    !.

%!  dne_all(+Goal:compound, +ListofGoals:list,
%!          +VarsIn:compound, -VarsOut:compound) is det
%
%   Succeed if Goal doesn't unify with any member of ListofGoals.
%
%   @arg Goal Input goal.
%   @arg ListofGoals A list of goals.
%   @arg VarsIn Input variables.
%   @arg VarsOut Output variables.

dne_all(X, [Y | T], Vi, Vo):-
    solve_dnunify(X, Y, Vi, V1, _),
    dne_all(X, T, V1, Vo).
dne_all(_, [], V, V).

%!  select_heads(+ListIn:list, -ListOut:list, -HeadsOut:list) is det
%
%   Remove the head  from  each  list   in  ListIn,  returning  them  in
%   HeadsOut.
%
%   @arg ListIn Input list of lists.
%   @arg ListOut Output list of lists. Lists from ListIn with heads removed.
%   @arg HeadsOut List of the heads from each entry of ListIn.

select_heads([[X | T] | Tt], [T | T2], [X | Ht]) :-
    !,
    select_heads(Tt, T2, Ht).
select_heads([], [], []) :-
    !.

%!  split_neg_matches(+Matches:list, -ConstraintMatches:list,
%!                    -BoundMatches:list, -LoopVars:list,
%!                    +Vars:compound) is det

%   Split variables in Matches depending on   whether  they are bound or
%   constrained. If completely unbound AND NOT   A  LOOP VARIABLE, fail.
%   For loop variables that  are  unbound   or  constrained,  return the
%   VARIABLE in LoopVars.
%
%   @arg Matches Input matches.
%   @arg ConstraintMatches Constrained matches. List of constraint lists for non-loop variables.
%   @arg BoundMatches Bound matches.
%   @arg LoopVars Bound or constrained loop variables.
%   @arg Vars Variable struct to look up values.

split_neg_matches([X | T], Cm, [X | Bm], Lv, V) :-
    \+is_var(X),
    !,
    split_neg_matches(T, Cm, Bm, Lv, V).
split_neg_matches([X | T], Cm, [Val | Bm], Lv, V) :-
    is_var(X),
    var_value(X, V, val(Val)), % not constrained or unbound
    !,
    split_neg_matches(T, Cm, Bm, Lv, V).
split_neg_matches([X | T], [Val | Cm], Bm, Lv, V) :-
    is_var(X),
    is_unbound(X, V, Val, _, Vl),
    Vl =\= 1, % non-loop variable
    Val = [_ | _], % constrained
    !,
    split_neg_matches(T, Cm, Bm, Lv, V).
split_neg_matches([X | T], Cm, Bm, [X | Lv], V) :-
    is_var(X),
    is_unbound(X, V, _, _, 1), % loop variable
    !,
    split_neg_matches(T, Cm, Bm, Lv, V).
split_neg_matches([], [], [], [], _) :-
    !.

%!  exact_match(+CHSentry:compound, +CHSentries:list,
%!              +Vars:compound, -Success:int) is det
%
%   Check to see if there is an  exact   match  for CHSentry in the CHS.
%   Basically, unify, but only allow  unbound   variables  to match with
%   other unbound variables and  constrained   variables  to  match with
%   variables that have the exact same constraints.
%
%   @arg CHSentry The entry to check.
%   @arg CHSentries The CHS entries matching the functor of CHSentry.
%   @arg Vars The var struct to get values from.
%   @arg Success The success flag of the matching entry.

exact_match(E, [X | _], V, S) :-
    chs_entry(E, F, A1, _, _),
    chs_entry(X, F, A2, S, _), % functors match
    exact_match2(A1, A2, V),
    !.
exact_match(E, [_ | T], V, S) :-
    !,
    exact_match(E, T, V, S).

%!  exact_match2(+Args1:list, +Args2:list, +Vars:compound) is det
%
%   Check to see if  two  lists   of  arguments  match  exactly. Unbound
%   variables only match other unbound  variables, constrained variables
%   only match variables with exactly the same constraints.
%
%   @arg Args1 First list of arguments.
%   @arg Args2 Second list of arguments.
%   @arg Vars The var struct to get values from.

exact_match2([X | T], [X | T2], V) :- % values match exactly; continue
    !,
    exact_match2(T, T2, V).
exact_match2([X | T], [Y | T2], V) :-
    is_var(X),
    is_var(Y),
    !,
    var_value(X, V, Xv),
    var_value(Y, V, Yv),
    (
            var_con(Xv, Val, _, _),
            var_con(Yv, Val, _, _)
    ;
            Xv = val(Xv2),
            Yv = val(Yv2),
            exact_match2([Xv2], [Yv2], V)
    ),
    !,
    exact_match2(T, T2, V).
exact_match2([X | T], [Y | T2], V) :-
    is_var(X),
    !,
    var_value(X, V, val(Xv)),
    exact_match2([Xv], [Y], V),
    !,
    exact_match2(T, T2, V).
exact_match2([X | T], [Y | T2], V) :-
    is_var(Y),
    !,
    var_value(Y, V, val(Yv)),
    exact_match2([X], [Yv], V),
    !,
    exact_match2(T, T2, V).
exact_match2([X | T], [Y | T2], V) :-
    X =.. [F | A],
    Y =.. [F | A2],
    length(A, L),
    length(A2, L), % same functor and arity
    !,
    exact_match2(A, A2, V),
    !,
    exact_match2(T, T2, V).
exact_match2([], [], _) :-
    !.

%!  check_negations(+Goal:compound, +CallStack:list, +VarsIn:compound,
%!                  -VarsOut:compound, +NegSeen:int,
%!                  -Rval:int, -Cycle:list, -CVars:list)
%
%   Check the call stack for negations between  a call and its ancestor.
%   This should only be called after   chs:check_chs/5 returns a flag of
%   2. This ensures that an  ancestor   call  *exists*. Additionally, we
%   don't need to count the negations  to   be  sure  that they're even,
%   since dual rules handle that for us.
%
%   @arg Goal The call being tested.
%   @arg CallStack The list of ancestor calls. Must NOT include the current
%        call.
%   @arg VarsIn Input variable struct.
%   @arg VarsOut Output variable struct.
%   @arg NegSeen 1 or 0 indicating if a negation has been seen. Call with 0
%        initially.
%   @arg Rval 1 Goal unifies with an ancestor with at least one intervening
%        negation. -1 if it unifies with an ancestor with NO intervening
%        negation.
%   @arg Cycle List of goals between the current call and its ancestor in the
%        call stack, if one exists.
%   @arg CVars List of value IDs for non-ground variables present in both the
%        ancestor and recursive call, by value ID.

check_negations(G, [-(X, _) | T], Vi, Vo, _, R, [X | C], Cv) :- % Intervening negation
    predicate(X, F2, _),
    is_dual(F2),
    predicate(G, F, _),
    F \= F2, % not a match
    !,
    check_negations(G, T, Vi, Vo, 1, R, C, Cv).
check_negations(G, [-(X, _) | T], Vi, Vo, _, R, [X | C], Cv) :- % Intervening negation
    predicate(X, F, _),
    is_dual(F),
    \+solve_unify(G, X, Vi, _, 1), % not a match
    !,
    check_negations(G, T, Vi, Vo, 1, R, C, Cv).
check_negations(G, [-(X, _) | T], Vi, Vo, N, R, C, Cv) :- % Match found
    predicate(G, F, A1),
    predicate(X, F, A2),
    solve_unify(G, X, Vi, V2, 1),
    !,
    chs_entry(E1, F, A1, _, _),
    chs_entry(E2, F, A2, _, _),
    (   N = 1                    % intervening negation, we're in an even loop
    ->  C = [X],
        Vo = V2,
        variable_intersection(G, X, Vi, Cv), % get variables that are unbound throughout the loop
        (   exact_match(E1, [E2], Vi, _)
        ->  R = 2 % exact match in call stack; succeed R = 2
        ;   R = 1 % intervening negation; succeed R = 1
        )
    ;   (   exact_match(E1, [E2], Vi, _)
        ->  R = -1, % positive loop; fail
            Vo = Vi, % failure: drop changes
            C = [],
            Cv = []
        ;   % ignore changes from unification; keep going
            check_negations(G, T, Vi, Vo, N, R, [X | C], Cv)
        )
    ),
    !.
check_negations(G, [-(X, _) | T], Vi, Vo, N, R, [X | C], Cv) :- % Neither a match nor a negation; keep going
    !,
    check_negations(G, T, Vi, Vo, N, R, C, Cv).



%!  add_to_chs(+Functor:ground, +Args:list, +Success:compound,
%!             +InNMR:int, -Entry: compound, +SubVars:list,
%!             +VarsIn:compound, -VarsOut:compound,
%!             +CHSin:compound, -CHSout:compound) is det
%
%   Insert an entry into the CHS.  Typically,   this  will be done after
%   check_chs/8 has reported that a  tested   predicate  is not present.
%   Note that check_chs/8 uses the same two   cases:  when a goal is not
%   present, and when all matches  have   been  exhausted. Therefore, we
%   need to check for matches before adding  the entry, so that we don't
%   end up with duplicates.
%
%   @arg Functor The name/arity of the predicate to check.
%   @arg Args Args of the call being checked.
%   @arg Success Value of the success flag to use.
%   @arg InNMR 1 if the goal was first added after entering the NMR check, 0
%        otherwise.
%   @arg Entry The entry added to the CHS. Used to remove old values.
%   @arg SubVars The list of substitution variable structs from even loops.
%   @arg VarStructIn Input var struct.
%   @arg VarStructOut Output var struct.
%   @arg CHSin Input CHS.
%   @arg CHSout Output CHS.

add_to_chs(F, A, S, N, E, Ev, Vi, Vo, CHSi, CHSo) :- % not already present
    once(get_sub_pairs(F, A, Ev, Ev2, Vi)),
    once(replace_vars(A, A2, Vi, Vo, Ev2, _, 1)), % perform substitution
    chs_entry(E, F, A2, S, N),
    (   rb_lookup(F, Es, CHSi) % existing entry for predicate
    ->  \+ exact_match(E, Es, Vo, _),
        rb_update(CHSi, F, [E | Es], CHSo)
    ;   rb_insert(CHSi, F, [E], CHSo)
    ),
    !.
add_to_chs(F, A, S, _, E, _, V, V, CHS, CHS) :- % already present; skip
    chs_entry(E, F, A, S, _).

%!  get_sub_pairs(+Functor:ground, +Args:list, +SubVarsIn:list,
%!                -SubVarsOut:list, +VarStruct:compound)
%
%   Get variable substitution pairs  for  even   loop  variables  in the
%   current goal.
%
%   @arg Functor The name/arity of the predicate to check.
%   @arg Args Args of the call being checked.
%   @arg SubVarsIn Input list of substitution variable structs.
%   @arg SubVarsOut Output list of substitution variable structs.
%   @arg VarStruct Var struct to lookup IDs.

get_sub_pairs(_, _, [], [], _) :-
    !.
get_sub_pairs(F, A, Svi, Svo, V) :-
    get_sub_ids(Svi, Sv2, V),
    G =.. [F | A],
    body_vars2([G], [], [], Gv), % get vars from goal
    get_sub_pairs2(Gv, Sv2, Svo, V),
    !.

%!  get_sub_pairs2(+GoalVars:list, +SubVarsIn:list, -SubVarsOut:list,
%!                 +VarStruct:compound)
%
%   Get variable substitution pairs by checking  each body variable's ID
%   against those of the input pairs.
%
%   @arg GoalVars Body variables to test.
%   @arg SubVarsIn Input ID / subst. variable pairs.
%   @arg SubVarsOut Output var/subst var pairs.
%   @arg VarStruct Var struct to lookup IDs.

get_sub_pairs2([X | T], Svi, [-(X, Y) | Svo], Vs) :- % match
    get_value_id(X, Xi, Vs),
    member(-(Xi, Y), Svi), % ID match
    !,
    get_sub_pairs2(T, Svi, Svo, Vs).
get_sub_pairs2([_ | T], Svi, Svo, Vs) :- % no match
    !,
    get_sub_pairs2(T, Svi, Svo, Vs).
get_sub_pairs2([], _, [], _) :-
    !.

%!  get_sub_ids(+SubVarsIn:list, -SubVarsOut:list, +VarStruct:compound)
%
%   Given a list of variable and   substitution  variable pairs, replace
%   the variables with their value IDs.
%
%   @arg SubVarsIn Input sub var structs.
%   @arg SubVarsOut Output sub var structs.
%   @arg VarStruct Variable struct to look up IDs.

get_sub_ids([X | T], [X2 | T2], Vs) :-
    X = -(A, B),
    get_value_id(A, Ai, Vs),
    X2 = -(Ai, B),
    !,
    get_sub_ids(T, T2, Vs).
get_sub_ids([], [], _) :-
    !.

%!  remove_from_chs(+Entry:compound, +CHSin:compound, -CHSout:compound) is det
%
%   Given a CHS entry, remove it from the  CHS. If not found, return the
%   original CHS.
%
%   @arg Entry The entry to remove.
%   @arg CHSin Input CHS.
%   @arg CHSout Output CHS.

remove_from_chs(E, CHSi, CHSo) :-
    chs_entry(E, F, _, _, _),
    rb_lookup(F, Es, CHSi),
    select(E, Es, Es2),
    rb_update(CHSi, F, Es2, CHSo),
    !.
remove_from_chs(_, CHS, CHS) :- % not present.
    !.

%!  replace_vars_in_all(+GoalsIn:list, -GoalsOut:list, +VarStructIn:int,
%!                      -VarStructOut:int, +Flag:int) is det
%
%   Given a list of lists of goals,   replace NON-LOOP variables in each
%   with unique ones. Ignore unbound or constrained loop variables.
%
%   @arg GoalsIn Input goals.
%   @arg GoalsOut Output goals.
%   @arg VarStructIn Input var struct.
%   @arg VarStructOut Output var struct.
%   @arg Flag 0 or 1 indicating if non-loop variables should be set as
%        non-unifiable.

replace_vars_in_all([X | T], [X2 | T2], Vsi, Vso, F) :-
    replace_vars(X, X2, Vsi, Vs1, [], _, F),
    !,
    replace_vars_in_all(T, T2, Vs1, Vso, F).
replace_vars_in_all([], [], Vs, Vs, _) :-
    !.

%!  replace_vars(+GoalsIn:list, -GoalsOut:list, +VarStructIn:int,
%!               -VarStructOut:int, +VarsIn:compound, -VarsOut:compound,
%!               +Flag:int) is det
%
%   Given a list of goals, replace every   bound variable with its value
%   and every constrained or unbound NON-LOOP variable with a unique one
%   that has the same constraints.  If   the  variable  has already been
%   replaced,  use  the  same  replacement   value.  Ignore  unbound  or
%   constrained loop variables.
%
%   @arg GoalsIn Input goals.
%   @arg GoalsOut Output goals.
%   @arg VarStructIn Input var struct.
%   @arg VarStructOut Output var struct.
%   @arg VarsIn Input vars.
%   @arg VarsOut Output vars.
%   @arg Flag 0 or 1 indicating if non-loop variables should be set as
%        non-unifiable.

replace_vars([Xi | Ti], [Xo | To], Vsi, Vso, Vi, Vo, F) :-
    replace_vars2(Xi, Xo, Vsi, Vs1, Vi, V1, F),
    !,
    replace_vars(Ti, To, Vs1, Vso, V1, Vo, F).
replace_vars([], [], Vs, Vs, V, V, _).

%!  replace_vars2(+Goal:compound, -GoalOut:compound,
%!                +VarStructIn:int, -VarStructOut:int,
%!                +VarsIn:compound, -VarsOut:compound, +Flag:int)
%
%   Given a goal, replace every bound variable  with its value and every
%   constrained or unbound NON-LOOP variable with  a unique one that has
%   the same constraints. If the variable has already been replaced, use
%   the same replacement  value.  Ignore   unbound  or  constrained loop
%   variables.
%
%   @arg GoalIn Input goal.
%   @arg GoalOut Output goal.
%   @arg VarStructIn Input var struct.
%   @arg VarStructOut Output var struct.
%   @arg VarsIn Input vars.
%   @arg VarsOut Output vars.
%   @arg Flag 0 or 1 indicating if non-loop variables should be set as
%        non-unifiable.

replace_vars2(Gi, Go, Vs, Vs, V, V, _) :-
    is_var(Gi),
    member(-(Gi, Go), V), % already encountered, use the same value
    !.
replace_vars2(Gi, Gi, Vsi, Vsi, Vi, Vo, _) :-
    is_unbound(Gi, Vsi, _, _, 1), % unbound or constrained variable, loop variable
    !,
    Vo = [-(Gi, Gi) | Vi].
replace_vars2(Gi, Go, Vsi, Vso, Vi, Vo, 0) :-
    is_unbound(Gi, Vsi, Con, _, Vl), % unbound or constrained variable, non-loop variable
    Vl =\= 1,
    !,
    var_con(Val, Con, 0, Vl), % ALLOW further constraints or instantiation.
    generate_unique_var(Go, Vsi, Vs1, Gi),
    update_var_value(Go, Val, Vs1, Vso), % copy value to new variable
    Vo = [-(Gi, Go) | Vi].
replace_vars2(Gi, Go, Vsi, Vso, Vi, Vo, 1) :-
    is_unbound(Gi, Vsi, Con, _, Vl), % unbound or constrained variable, non-loop variable
    Vl =\= 1,
    !,
    var_con(Val, Con, 2, Vl), % DISALLOW further constraints or instantiation.
    generate_unique_var(Go, Vsi, Vs1, Gi),
    update_var_value(Go, Val, Vs1, Vso), % copy value to new variable
    Vo = [-(Gi, Go) | Vi].
replace_vars2(Gi, Val2, Vsi, Vso, Vi, Vo, Flag) :-
    is_var(Gi),
    var_value(Gi, Vsi, val(Val)),
    Val =.. [F | A], % compound term
    !,
    replace_vars(A, A2, Vsi, Vso, Vi, V1, Flag),
    Val2 =.. [F | A2],
    Vo = [-(Gi, Val2) | V1].
replace_vars2(Gi, Val, Vs, Vs, Vi, Vo, _) :-
    is_var(Gi),
    !,
    var_value(Gi, Vs, val(Val)), % not unbound, constrained or a compound term
    Vo = [-(Gi, Val) | Vi].
replace_vars2(Gi, Go, Vsi, Vso, Vi, Vo, Flag) :-
    Gi =.. [F | A], % compound term, process args
    !,
    replace_vars(A, A2, Vsi, Vso, Vi, Vo, Flag),
    Go =.. [F | A2].
replace_vars2(G, G, Vs, Vs, V, V, _) :-
    % not a variable or compound term; keep original.
    !.
