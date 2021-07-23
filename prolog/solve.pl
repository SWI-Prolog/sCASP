:- module(casp_solve,
          [ solve/4                   % :Goals, +StackIn, -StackOut, -Model
          ]).
:- use_module(clp_call_stack).
:- use_module(scasp_options).
:- use_module(predicates).
:- use_module(clp_disequality).
:- use_module(clp_clpq).
:- use_module(sasp/output).           % the pr_* predicates
:- use_module(scasp_io).

/** <module> The sCASP solver
*/

%!  solve(:Goals, +StackIn, -StackOut, -Model)
%
%   Solve the list of sub-goals `Goal`  where   StackIn  is  the list of
%   goals already visited and returns  in   StackOut  the  list of goals
%   visited to prove the sub-goals and in  Model the model that supports
%   the sub-goals.

solve([], StackIn, [[]|StackIn], []).
solve([Goal|Goals], StackIn, StackOut, Model) :-
    if_user_option(check_calls, print_check_calls_calling(Goal, StackIn)),
    check_goal(Goal, StackIn, StackMid, Modelx), Modelx = [AddGoal|JGoal],
    if_user_option(check_calls, format('Success ~@\n', [print_goal(Goal)])),
    solve(Goals, StackMid, StackOut, Modelxs), Modelxs = JGoals,
    (   shown_predicate(Goal)
    ->  Model = [AddGoal, JGoal|JGoals]
    ;   Model = [JGoal|JGoals]
    ).


%!  check_goal(?Goal, ?StackIn, ?StackOut, ?Model)
%
%   Call  check_CHS/3 to  check the  sub-goal Goal  against the  list of
%   goals already visited StackIn to  determine if  it is  a coinductive
%   success, a coinductive failure, an already proved sub-goal, or if it
%   has to be evaluated

check_goal(Goal, StackIn, StackOut, Model) :-
    check_CHS(Goal, StackIn, Check), %% Check condition for coinductive success
    check_goal_(Check, Goal, StackIn, StackOut, Model).

% coinduction success <- cycles containing even loops may succeed
check_goal_(co_success, Goal, StackIn, StackOut, Model) :-
    (   current_option(assume,on)
    ->  mark_prev_goal(Goal,StackIn, StackMark),
        StackOut = [[],chs(Goal)|StackMark]
    ;   StackOut = [[],chs(Goal)|StackIn]
    ),
    JGoal = [],
    AddGoal = chs(Goal),
    Model = [AddGoal|JGoal].
% already proved in the stack
check_goal_(proved, Goal, StackIn, StackOut, Model) :-
    StackOut = [[], proved(Goal)|StackIn],
    JGoal = [],
    AddGoal = proved(Goal),
    Model = [AddGoal|JGoal].
% coinduction does neither success nor fails <- the execution continues inductively
check_goal_(cont, Goal, StackIn, StackOut, Model) :-
    solve_goal(Goal, StackIn, StackOut, Modelx), Modelx = [Goal|JGoal],
    AddGoal = Goal,
    Model = [AddGoal|JGoal].
% coinduction fails <- the negation of a call unifies with a call in the call stack
check_goal_(co_failure, _Goal, _StackIn, _StackOut, _Model) :-
    fail.

mark_prev_goal(Goal,[I|In],[assume(Goal)|In]) :- Goal == I, !.
mark_prev_goal(Goal,[I|In],[I|Mk]) :- mark_prev_goal(Goal,In,Mk).
mark_prev_goal(_Goal,[],[]).


%!  solve_goal(?Goal, ?StackIn, ?StackOut, ?GoalModel)
%
%   Solve a  simple sub-goal  Goal where  StackIn is  the list  of goals
%   already visited and returns in StackOut the list of goals visited to
%   prove  the  sub-goals  and  in  `Model` the  model with  support the
%   sub-goals

solve_goal(Goal, StackIn, StackOut, GoalModel) :-
    Goal = forall(_, _),
    !,
    (   current_option(prev_forall, on)
    ->  solve_goal_forall(Goal, [Goal|StackIn], StackOut, Model)
    ;   solve_c_forall(Goal, [Goal|StackIn], StackOut, Model)
    ),
    GoalModel = [Goal|Model].
solve_goal(Goal, StackIn, [[], Goal|StackIn], GoalModel) :-
    Goal = not(is(V, Expresion)),
    !,
    NV is Expresion,
    V .\=. NV,
    GoalModel = [Goal].
solve_goal(Goal, _, _, _) :-
    Goal = not(true),
    !,
    fail.
solve_goal(Goal, StackIn, StackOut, Model) :-
    Goal \= [], Goal \= [_|_], Goal \= builtin(_),
    table_predicate(Goal),
    if_user_option(check_calls, format('Solve the tabled goal ~p\n', [Goal])),
    AttStackIn <~ stack([Goal|StackIn]),
    solve_goal_table_predicate(Goal, AttStackIn, AttStackOut, AttModel),
    AttStackOut ~> stack(StackOut),
    AttModel ~> model(Model).
solve_goal(Goal, StackIn, StackOut, Model) :-
    Goal \= [], Goal \= [_|_], Goal \= builtin(_),
    \+ table_predicate(Goal),
    user_predicate(Goal),
    (   solve_goal_predicate(Goal, [Goal|StackIn], StackOut, Model)
    *-> true
    ;   shown_predicate(Goal),
        if_user_option(
            trace_failures,
            ( if_user_option(show_tree,
                             print_check_calls_calling(Goal, [Goal|StackIn])),
              format("\nFAILURE to prove the literal: ~@\n\n", [print_goal(Goal)])
            )
        ),
        fail
    ).
solve_goal(call(Goal),StackIn,StackOut,[call(Goal)|Model]) :- !,
    solve_goal(Goal,StackIn,StackOut,Model).
solve_goal(not(call(Goal)),StackIn,StackOut,[not(call(Goal))|Model]) :- !,
    solve_goal(not(Goal),StackIn,StackOut,Model).
solve_goal(Goal, StackIn, StackOut, [Goal|Model]) :-
    Goal=findall(_, _, _), !,
    exec_findall(Goal, StackIn, StackOut, Model).
solve_goal(not(Goal), StackIn, StackIn, [not(Goal)]) :-
    Goal=findall(_, _, _), !,
    exec_neg_findall(Goal, StackIn).
solve_goal(Goal, StackIn, [[], Goal|StackOut], Model) :-
    Goal \= [], Goal \= [_|_], \+ user_predicate(Goal),
    \+ table_predicate(Goal),
    solve_goal_builtin(Goal, StackIn, StackOut, Model).


%!  solve_goal_forall(+Forall, ?StackIn, ?StackOut, ?GoalModel)
%
%   Solve a sub-goal of the form `forall(Var,Goal)`  and success  if Var
%   success in all its domain for the goal Goal. It calls solve/4
%
%   @arg Forall is a term forall(?Var, ?Goal)

solve_goal_forall(forall(Var, Goal), StackIn, [[]|StackOut], Model) :-
    my_copy_term(Var, Goal, NewVar, NewGoal),
    my_copy_term(Var, Goal, NewVar2, NewGoal2),
    solve([NewGoal], StackIn, [[]|StackMid], ModelMid),
    if_user_option(check_calls,
                   format('\tSuccess solve ~@\n\t\t for the ~@\n',
                          [print_goal(NewGoal), print_goal(forall(Var, Goal))])),
    check_unbound(NewVar, List),
    (   List == ground
    ->  if_user_option(check_calls,
                       format('The var ~p is grounded so try with other clause\n',
                              [NewVar])),
        fail
    ;   List == []
    ->  StackOut = StackMid,
        Model = ModelMid
    ;   List = clpq(NewVar3, Constraints)
    ->  findall(dual(NewVar3, ConDual),
                dual_clpq(Constraints, ConDual),
                DualList),
        if_user_option(check_calls,
                       format('Executing ~@ with clpq ConstraintList = ~p\n',
                              [print_goal(Goal), DualList])),
        exec_with_clpq_constraints(NewVar2, NewGoal2,
                                   entry(NewVar3, []),
                                   DualList, StackMid, StackOut, ModelList), !,
        append(ModelMid, ModelList, Model)
    ;   !,   %% Position of the cut in s(CASP) - remove answers in max.lp
        if_user_option(check_calls,
                       format('Executing ~@ with clp_disequality list = ~p\n',
                              [print_goal(Goal), List])),
        exec_with_neg_list(NewVar2, NewGoal2,
                           List, StackMid, StackOut, ModelList),
        append(ModelMid, ModelList, Model)
    ).

check_unbound(Var, ground) :-
    ground(Var), !.
check_unbound(Var, List) :-
    get_neg_var(Var, List), !.
check_unbound(Var, 'clpq'(NewVar, Constraints)) :-
    dump_clpq_var([Var], [NewVar], Constraints),
    Constraints \== [], !.
check_unbound(Var, []) :-
    var(Var), !.

exec_with_clpq_constraints(_, _, _, [], StackIn, StackIn, []).
exec_with_clpq_constraints(Var, Goal, entry(ConVar, ConEntry),
                           [dual(ConVar, ConDual)|Duals],
                           StackIn, StackOut, Model) :-
    my_copy_term(Var, [Goal, StackIn], Var01, [Goal01, StackIn01]),
    append(ConEntry, ConDual, Con),
    my_copy_term(ConVar, Con, ConVar01, Con01),
    my_copy_term(Var, Goal, Var02, Goal02),
    my_copy_term(ConVar, ConEntry, ConVar02, ConEntry02),
    Var01 = ConVar,
    (   apply_clpq_constraints(Con)
    ->  if_user_option(check_calls,
                       format('Executing ~p with clpq_constrains ~p\n',
                              [Goal01, Con])),
        solve([Goal01], StackIn01, [[]|StackOut01], Model01),
        if_user_option(check_calls,
                       format('Success executing ~p with constrains ~p\n',
                              [Goal01, Con])),
        if_user_option(check_calls,
                       format('Check entails Var = ~p with const ~p and ~p\n',
                              [Var01, ConVar01, Con01])),
        (   entails([Var01], ([ConVar01], Con01))
        ->  if_user_option(check_calls, format('\tOK\n', [])),
            StackOut02 = StackOut01,
            Model03 = Model01
        ;   if_user_option(check_calls, format('\tFail\n', [])),
            dump_clpq_var([Var01], [ConVar01], ExitCon),
            findall(dual(ConVar01, ConDual01),
                    dual_clpq(ExitCon, ConDual01),
                    DualList),
            if_user_option(check_calls,
                           format('Executing ~p with clpq ConstraintList = ~p\n',
                                  [Goal, DualList])),
            exec_with_clpq_constraints(Var, Goal, entry(ConVar01, Con01),
                                       DualList, StackOut01, StackOut02, Model02),
            append(Model01, Model02, Model03)
        )
    ;   if_user_option(check_calls,
                       format('Skip execution of an already checked \c
                              constraint ~p (it is inconsitent with ~p)\n',
                              [ConDual, ConEntry])),
        StackOut02 = StackIn01,
        Model03 = []
    ),
    if_user_option(check_calls,
                   format('Executing ~p with clpq ConstraintList = ~p\n',
                          [Goal, Duals])),
    exec_with_clpq_constraints(Var02, Goal02,
                               entry(ConVar02, ConEntry02),
                               Duals, StackOut02, StackOut, Model04),
    append(Model03, Model04, Model).

exec_with_neg_list(_, _, [], StackIn, StackIn, []).
exec_with_neg_list(Var, Goal, [Value|Vs], StackIn, StackOut, Model) :-
    my_copy_term(Var, [Goal, StackIn], NewVar, [NewGoal, NewStackIn]),
    NewVar = Value,
    if_user_option(check_calls,
                   format('Executing ~p with value ~p\n', [NewGoal, Value])),
    solve([NewGoal], NewStackIn, [[]|NewStackMid], ModelMid),
    if_user_option(check_calls,
                   format('Success executing ~p with value ~p\n',
                          [NewGoal, Value])),
    exec_with_neg_list(Var, Goal, Vs, NewStackMid, StackOut, Models),
    append(ModelMid, Models, Model).

%!  solve_goal_table_predicate(?Goal, ?AttStackIn, ?AttStackOut, ?AttModel)
%
%   Used to evaluate predicates under tabling. This predicates should be
%   defined in the program using the directive _#table pred/n._

solve_goal_table_predicate(Goal, AttStackIn, AttStackOut, AttModel) :-
    pr_rule(Goal, Body),
    AttStackIn ~> stack(StackIn),
    solve(Body, StackIn, StackOut, Model),
    AttStackOut <~ stack(StackOut),
    AttModel <~ model([Goal|Model]).

%!  solve_goal_predicate(?Goal, ?StackIn, ?StackOut, ?GoalModel)
%
%   Used to evaluate a user predicate

solve_goal_predicate(Goal, StackIn, StackOut, GoalModel) :-
    pr_rule(Goal, Body),
    solve(Body, StackIn, StackOut, BodyModel),
    GoalModel = [Goal|BodyModel].

%!  solve_goal_builtin(?Goal, ?StackIn, ?StackOut, ?AttModel)
%
%   Used to evaluate builtin predicates predicate

solve_goal_builtin(is(X, Exp), StackIn, StackIn, Model) :-
    capture_rational(Exp, CaptExp), !, %% If it fails later the call(Goal) will also fail...
    exec_goal(is(X, CaptExp)),
    Model = [is(X, Exp)]. %% the Model should 'Start' with the Goal
solve_goal_builtin(builtin(Goal), StackIn, StackIn, Model) :- !,
    exec_goal(Goal),
    Model = [builtin(Goal)].
solve_goal_builtin(Goal, StackIn, StackIn, Model) :-
    clp_builtin(Goal), !,
    exec_goal(apply_clpq_constraints(Goal)),
    Model = [Goal].
solve_goal_builtin(Goal, StackIn, StackIn, Model) :-
    clp_interval(Goal), !,
    exec_goal(Goal),
    Model = [Goal].
solve_goal_builtin(not(Goal), StackIn, StackIn, _Model) :-
    clp_interval(Goal), !,
    if_user_option(warning,format("\nWARNING s(CASP): Failure calling negation of ~p\n",[Goal])),
    fail.
solve_goal_builtin(Goal, StackIn, StackIn, Model) :-
    clp_builtin_translate(Goal, Goal_T), !,
    exec_goal(apply_clpq_constraints(Goal_T)),
    Model = [Goal].
solve_goal_builtin(Goal, StackIn, StackIn, Model) :-
    prolog_builtin(Goal), !,
    exec_goal(Goal),
    Model = [Goal].
% The predicate is not defined as user_predicates neither builtin
solve_goal_builtin(Goal, StackIn, StackIn, Model) :-
    if_user_option(check_calls,
        format('The predicate ~p is not user_defined / builtin\n', [Goal])),
    (   Goal = not(_)
    ->  Model = [Goal] %% the negation of a not defined predicate success.
    ;   fail %% a not defined predicate allways fails.
    ).

exec_goal(A \= B) :- !,
    if_user_option(check_calls, format('exec ~@\n', [print_goal(A \= B)])),
    A .\=. B,
    if_user_option(check_calls, format('ok   ~@\n', [print_goal(A \= B)])).
exec_goal(Goal) :-
    (   current_option(check_calls, on)
    ->  E = error(_,_),
        format('exec goal ~@ \n', [print_goal(Goal)]),
        catch(call(Goal), E, (print_message(warning, E), fail)),
        format('ok   goal ~@ \n', [print_goal(Goal)])
    ;   catch(call(Goal), error(_,_), fail)
    ).

capture_rational(G, A/B) :- ground(G), G=rat(A, B), !.
capture_rational(St, NSt) :-
    St =.. [Op, A, B], !,
    capture_rational(A, Na),
    capture_rational(B, Nb),
    NSt =.. [Op, Na, Nb].
capture_rational(A, A) :- ground(A).


% TODO: Peding StackOut to carry the literal involved in the findall (if needed)
exec_findall(findall(Var, Call, List), StackIn, StackOut, Model) :-

    if_user_option(check_calls, format('execution of findall(~p, ~p, _) \n', [Var, Call])),

    findall(t(Var, S, M), (
            solve([Call], StackIn, S0, M),
            append(S, StackIn, S0)
        ), VSMList),

    process_vsmlist(VSMList, List, SOut, Model),
    append(SOut, [findall(Var, Call, List)|StackIn], StackOut),

    if_user_option(check_calls, format('Result execution = ~p \n', [List])).

process_vsmlist(VSMList, List, [[]|StackOut], Model) :-
    process_vsmlist_(VSMList, List, StackOut, Model).

process_vsmlist_([], [], [], []).
process_vsmlist_([t(V, [[]|S], M)|Rs], [V|Vs], S1, M1) :-
    process_vsmlist_(Rs, Vs, S0, M0),
    append(S0, S, S1),
    append(M, M0, M1).

% TODO: What to do with the negation of findall/3 (if required)
exec_neg_findall(Goal, _) :-
    if_user_option(check_calls, format('PENDING: execution of not ~p \n', [Goal])),
    fail.


%!  check_CHS(+Goal, +StackIn, -Result) is semidet.
%
%   Checks the StackIn and returns  in  Result   if  the  goal Goal is a
%   coinductive success, a coinductive  failure   or  an  already proved
%   goal. Otherwise it is constraint against  its negation atoms already
%   visited.

check_CHS(Goal, I, Result) :-
    (   user_predicate(Goal)
    ->  check_CHS_(Goal, I, Result)
    ;   Result = cont
    ).

% inmediate success if the goal has already been proved.
check_CHS_(Goal, I, proved) :-
    ground(Goal),
    \+ \+ proved_in_stack(Goal, I), !.
% coinduction success <- cycles containing even loops may succeed
check_CHS_(Goal, I, co_success) :-
    type_loop(Goal, I, even), !.
% coinduction fails <- the goal is entailed by its negation in the
% call stack
check_CHS_(Goal, I, co_failure) :-
    \+ \+ neg_in_stack(Goal, I), !,
    if_user_option(check_calls, format('Negation of the goal in the stack, failling (Goal = ~w)\n', [Goal])).
% coinduction fails <- cycles containing positive loops can be solve
% using tabling
check_CHS_(Goal, I, co_failure) :-
    \+ table_predicate(Goal),
    if_user_option(no_fail_loop, fail),
    \+ \+ (
        type_loop(Goal, I, fail_pos(S)),
        if_user_option(check_calls, format('Positive loop, failling (Goal == ~w)\n', [Goal])),
        if_user_option(pos_loops, format('\nWarning: positive loop failling (Goal ~w == ~w)\n', [Goal, S]))
    ), !.
check_CHS_(Goal, I, _Cont) :-
    \+ table_predicate(Goal),
    \+ \+ (
        type_loop(Goal, I, pos(S)),
        if_user_option(check_calls, format('Positive loop, continuing (Goal = ~w)\n', [Goal])),
        if_user_option(pos_loops, format('\nNote: positive loop continuing (Goal ~w = ~w)\n', [Goal, S]))
    ), fail.
% coinduction does not succeed or fail <- the execution continues inductively
check_CHS_(Goal, I, cont) :-
    (   ground(Goal)
    ->  (   constrained_neg_in_stack(Goal, I)
        *-> true
        ;   true
        )
    ;   (   ground_neg_in_stack(Goal, I)
        *-> true
        ;   true
        )
    ).

% check if the negation is in the stack -> coinductive failure
% extended to check if the negation in the stack entails the current call -> failure
neg_in_stack(not(Goal), [NegGoal|_]) :-
    Goal == NegGoal, !.
neg_in_stack(Goal, [not(NegGoal)|_]) :-
    Goal == NegGoal, !.
neg_in_stack(not(Goal), [NegGoal|_]) :-
    variant(Goal, NegGoal), !,
    if_user_option(warning,
                   format("~NWARNING: Co-Failing in a negated loop due \c
                           to a variant call (extension clp-disequality required).\n\c
                           \tCurrent call:\t~@\n\tPrevious call:\t~@\n",
                          [print_goal(Goal), print_goal(NegGoal)])).
neg_in_stack(Goal, [not(NegGoal)|_]) :-
    variant(Goal, NegGoal), !,
    if_user_option(warning,
                   format("~NWARNING: Co-Failing in a negated loop due \c
                          to a variant call (extension clp-disequality required).\n\c
                          \tCurrent call:\t~@\n\tPrevious call:\t~@\n",
                          [print_goal(Goal), print_goal(NegGoal)])).
neg_in_stack(Goal, [_|Ss]) :-
    neg_in_stack(Goal, Ss).

% ground_neg_in_stack
ground_neg_in_stack(Goal, S) :-
    if_user_option(check_calls, format('Enter ground_neg_in_stack for ~@\n',
                                       [print_goal(Goal)])),
    ground_neg_in_stack_(Goal, S, 0, 0, Flag),
    Flag == found,
    %       ( Flag == found_dis ; Flag == found_clpq ),
    if_user_option(check_calls, format('\tThere exit the negation of ~@\n\n',
                                       [print_goal(Goal)])).

ground_neg_in_stack_(_, [], _, _, _Flag) :- !.
ground_neg_in_stack_(Goal, [[]|Ss], Intervening, MaxInter, Flag) :- !,
    NewInter is Intervening - 1,
    ground_neg_in_stack_(Goal, Ss, NewInter, MaxInter, Flag).
ground_neg_in_stack_(Goal, [chs(not(NegGoal))|Ss], Intervening, MaxInter, found) :-
    Intervening =< MaxInter,
    same_functor(Goal, NegGoal), % limit output
    if_user_option(check_calls,
                   format('\t\tCheck disequality of ~@ and ~@\n',
                          [print_goal(Goal), print_goal(chs(not(NegGoal)))])),
    \+ \+ Goal = NegGoal,
    loop_term(Goal, NegGoal), !,
    max(MaxInter, Intervening, NewMaxInter),
    NewInter is Intervening + 1,
    ground_neg_in_stack_(Goal, Ss, NewInter, NewMaxInter, found).
ground_neg_in_stack_(not(Goal), [chs(NegGoal)|Ss], Intervening, MaxInter, found) :-
    Intervening =< MaxInter,
    same_functor(Goal, NegGoal),
    if_user_option(check_calls,
                   format('\t\tCheck disequality of ~@ and ~@\n',
                          [print_goal(not(Goal)), print_goal(chs(NegGoal))])),
    \+ \+ Goal = NegGoal,
    loop_term(Goal, NegGoal), !,
    max(MaxInter, Intervening, NewMaxInter),
    NewInter is Intervening + 1,
    ground_neg_in_stack_(not(Goal), Ss, NewInter, NewMaxInter, found).
ground_neg_in_stack_(not(Goal), [NegGoal|Ss], Intervening, MaxInter, found) :-
    Intervening =< MaxInter,
    same_functor(Goal, NegGoal),
    if_user_option(check_calls,
                   format('\t\tCheck disequality of ~@ and ~@\n',
                          [print_goal(not(Goal)), print_goal(NegGoal)])),
    \+ \+ Goal = NegGoal,
    loop_term(Goal, NegGoal), !,
    max(MaxInter, Intervening, NewMaxInter),
    NewInter is Intervening + 1,
    ground_neg_in_stack_(not(Goal), Ss, NewInter, NewMaxInter, found).
ground_neg_in_stack_(Goal, [_|Ss], Intervening, MaxInter, Flag) :- !,
    max(MaxInter, Intervening, NewMaxInter),
    NewInter is Intervening + 1,
    ground_neg_in_stack_(Goal, Ss, NewInter, NewMaxInter, Flag).


% restrict even more the constrained in the stack
constrained_neg_in_stack(_, []).
constrained_neg_in_stack(not(Goal), [NegGoal|Ss]) :-
    same_functor(Goal, NegGoal),
    if_user_option(check_calls,
                   format('\t\tCheck if not(~@) is consistent with ~@\n',
                          [print_goal(Goal), print_goal(NegGoal)])), !,
    loop_term(Goal, NegGoal), !,
    if_user_option(check_calls, format('\t\tOK\n', [])),
    constrained_neg_in_stack(not(Goal), Ss).
constrained_neg_in_stack(Goal, [not(NegGoal)|Ss]) :-
    same_functor(Goal, NegGoal),
    if_user_option(check_calls,
                   format('\t\tCheck if not(~@) is consistent with ~@\n',
                          [print_goal(Goal), print_goal(NegGoal)])), !,
    loop_term(Goal, NegGoal), !,
    if_user_option(check_calls, format('\t\tOK\n', [])),
    constrained_neg_in_stack(Goal, Ss).
constrained_neg_in_stack(Goal, [_|Ss]) :-
    constrained_neg_in_stack(Goal, Ss).



% proved_in_stack
proved_in_stack(Goal, S) :-
    proved_in_stack_(Goal, S, 0, -1),
    if_user_option(check_calls,
                   format('\tGoal ~@ is already in the stack\n',
                          [print_goal(Goal)])).

proved_in_stack_(Goal, [Top|Ss], Intervening, MaxInter) :-
    (   Top == []
    ->  NewInter is Intervening - 1,
        proved_in_stack_(Goal, Ss, NewInter, MaxInter)
    ;   Goal == Top
    ->  Intervening =< MaxInter
    ;   Top == chs(Goal)
    ->  Intervening =< MaxInter
    ;   NewMaxInter is max(MaxInter, Intervening),
        NewInter is Intervening + 1,
        proved_in_stack_(Goal, Ss, NewInter, NewMaxInter)
    ).

max(A, B, M) :-
    (   A >= B
    ->  M = A
    ;   M = B
    ).

% check if it is a even loop -> coinductive success
type_loop(not(Goal), Stack, Type) :-
    !,
    Intervening = 0,
    NumberNegation = 1,
    type_loop_(not(Goal), Intervening, NumberNegation, Stack, Type).
type_loop(Goal, Stack, Type) :-
    Intervening = 0,
    NumberNegation = 0,
    type_loop_(Goal, Intervening, NumberNegation, Stack, Type).

type_loop_(Goal, Iv, N, [[]|Ss], Type) :- !,
    NewIv is Iv - 1,
    type_loop_(Goal, NewIv, N, Ss, Type).
type_loop_(Goal, Iv, N, [_S|Ss], Type) :-
    Iv < 0,
    NewIv is Iv + 1,
    type_loop_(Goal, NewIv, N, Ss, Type).

type_loop_(Goal, 0, 0, [S|_],fail_pos(S)) :-  \+ \+ type_loop_fail_pos(Goal, S).
type_loop_(Goal, 0, 0, [S|_],pos(S)) :- \+ \+ Goal = S.

% avoid loops due to repeated negated goal... this is not the right solution ->
% It should be solved using tabling !!
% type_loop_(not(Goal), 0, N, [not(S)|_],fail_pos(S)) :- Goal == S, N > 0, 0 is mod(N, 2).

type_loop_(not(Goal), 0, _N, [not(S)|_], even) :- \+ \+ Goal == S. %% redundant, for debugging proposals
type_loop_(not(Goal), 0, _N, [not(S)|_], even) :- variant(Goal, S), Goal = S.
type_loop_(Goal, 0, N, [S|_], even) :- Goal \= not(_), Goal == S, N > 0, 0 is mod(N, 2).

type_loop_(Goal, 0, N, [S|Ss], Type) :-
    Goal \== S,
    S = not(_),
    NewN is N + 1,
    type_loop_(Goal, 0, NewN, Ss, Type).
type_loop_(Goal, 0, N, [S|Ss], Type) :-
    Goal \== S,
    S \= not(_),
    type_loop_(Goal, 0, N, Ss, Type).

type_loop_fail_pos(Goal, S) :-
    Goal == S, !.
type_loop_fail_pos(Goal, S) :-
    variant(Goal, S), !,
    if_user_option(warning,format("\nWARNING: Failing in a positive loop due to a variant call (tabling required).\n\tCurrent call:\t~@\n\tPrevious call:\t~@\n",
                                  [print_goal(Goal),print_goal(S)])).
type_loop_fail_pos(Goal, S) :-
    entail_terms(Goal, S),
    if_user_option(warning, format("\nWARNING: Failing in a positive loop due to a subsumed call under clp(q).\n\tCurrent call:\t~@\n\tPrevious call:\t~@\n", [print_goal(Goal), print_goal(S)])).


%!  solve_c_forall(+Forall, +StackIn, -StackOut, -GoalModel)
%
%   Solve a sub-goal of the form c_forall(Vars,Goal) and succeeds if the
%   goal `Goal` succeeds covering the domain of all the vars in the list
%   of vars `Vars. It calls solve/4
%
%   @arg Forall is a term forall(Var, Goal).
%   @tbd Improve the efficiency by removing redundant justifications w.o.
%   losing solutions.

solve_c_forall(Forall, StackIn, [[]|StackOut], Model) :-
    collect_vars(Forall, c_forall(Vars0, Goal0)),    % c_forall([F,G], not q_1(F,G))

    if_user_option(check_calls,format('\nc_forall(~p,\t~p)\n\n',[Vars0, Goal0])),

    my_copy_vars(Vars0, Goal0, Vars1, Goal1),        % Vars should remain free
    my_diff_term(Goal1, Vars1, OtherVars),
    Initial_Const = [],                              % Constraint store = top
    (   current_option(all_forall,on)
    ->  solve_var_forall_(Goal1,
                          entry(Vars1, Initial_Const),
                          dual(Vars1, [Initial_Const]),
                          OtherVars, StackIn, StackOut, Model)
    ;   solve_other_forall(Goal1,
                           entry(Vars1, Initial_Const),
                           dual(Vars1, [Initial_Const]),
                           OtherVars, StackIn, StackOut, Model)
    ).

solve_other_forall(Goal,
                   entry(Vars, Initial_Const),
                   dual(Vars, [Initial_Const]),
                   OtherVars, StackIn, StackOutExit, ModelExit) :-
    append(Vars,OtherVars,AllVars),
    my_copy_vars(AllVars,   [Goal,StackIn,OtherVars,Vars],
                 _AllVars1, [Goal1,StackIn1,OtherVars1,Vars1]),
    my_copy_vars(AllVars, [Goal,StackIn,OtherVars,Vars],
                 _AllVars2, [Goal2,StackIn2,OtherVars2,Vars2]),

    if_user_option(check_calls,
                   format("solve other forall:\n\c
                           \t Goal \t~p\n\c
                           \t Vars1       \t~p\n\c
                           \t OtherVars   \t~p\n\c
                           \t StackIn    \t~p\n\n",
                          [Goal,Vars1,OtherVars,StackIn])),

    % disequality and clp for numbers
    dump_constraint(OtherVars, OtherVars1, Dump, []-[], Pending-Pending1), !,
    clpqr_dump_constraints(Pending, Pending1, CLP),
    append(CLP, Dump, Constraints1),
    my_copy_vars(OtherVars1, Constraints1, OtherVars2, Constraints2),

    if_user_option(check_calls,
                   format("solve other forall:\n\c
                          \t OtherVars1   \t~p\n\c
                          \t OtherVars2   \t~p\n\c
                          \t Constraints1   \t~p\n\c
                          \t Constraints2 \t~p\n\n",
                          [OtherVars, OtherVars1, Constraints1, Constraints2])),

    apply_const_store(Constraints1),!,

    solve_var_forall_(Goal1,
                      entry(Vars1, Initial_Const),
                      dual(Vars1, [Initial_Const]), OtherVars1,
                      StackIn1, StackOut, Model),
    !,
    (   OtherVars = OtherVars1,
        StackOutExit = StackOut,
        ModelExit = Model
    ;   \+ ground(OtherVars),
        apply_const_store(Constraints2),
        % disequality and clp for numbers
        dump_constraint(OtherVars1, OtherVars2, Dump1, []-[], Pend-Pend1), !,
        clpqr_dump_constraints(Pend, Pend1, CLP1),
        append(CLP1, Dump1, AnsConstraints2),
        make_duals(AnsConstraints2, Duals),
        member(Dual, Duals),
        apply_const_store(Dual),
        solve_other_forall(Goal2,
                           entry(Vars2, Initial_Const),
                           dual(Vars2, [Initial_Const]),
                           OtherVars2, StackIn2, StackOutExit, ModelExit), !,
        OtherVars = OtherVars2
    ).

solve_var_forall_(_Goal, _, dual(_, []), _OtherVars, StackIn, StackIn, []) :- !.
solve_var_forall_(Goal,
                  entry(C_Vars, Prev_Store),
                  dual(C_Vars, [C_St|C_Stores]),
                  OtherVars, StackIn, StackOut, Model) :-
    if_user_option(check_calls,
                   format("solve forall:\n\c
                          \tPrev_Store \t~p\n\c
                          \tC_St       \t~p\n\c
                          \tC_Stores   \t~p\n\c
                          \tStackIn    \t~p\n\n",
                          [Prev_Store,C_St,C_Stores,StackIn])),

    my_copy_vars(C_Vars, [Goal, Prev_Store, C_St], C_Vars1, [Goal1, Prev_Store1, C_St1]),
    my_copy_vars(C_Vars, [Goal, Prev_Store, C_Stores], C_Vars2, [Goal2, Prev_Store2, C_Stores2]),

    apply_const_store(Prev_Store),
    (   if_user_option(check_calls,format('apply_const_store ~@\n',[print_goal(C_St)])),
        apply_const_store(C_St) % apply a Dual
    ->  solve([Goal], StackIn, [[]|StackOut1], Model1),
        find_duals(C_Vars-C_Vars1, OtherVars, Duals),       %% New Duals
        if_user_option(check_calls,format('Duals = \t ~p\n',[Duals])),
        append_set(Prev_Store1, C_St1, Current_Store1),
        solve_var_forall_(Goal1,
                          entry(C_Vars1, Current_Store1),
                          dual(C_Vars1, Duals),
                          OtherVars, StackOut1, StackOut2, Model2),
        append(Model1,Model2,Model3)
    ;   if_user_option(check_calls,format('Entail: Fail  applying \t ~@\n',
                                         [print_goal(C_St)])),
        %% The dual C_St is not consistent with Prev_Store -> already checked (entails)
        StackOut2 = StackIn,
        Model3 = []
    ),
    solve_var_forall_(Goal2,
                      entry(C_Vars2, Prev_Store2),
                      dual(C_Vars2, C_Stores2),
                      OtherVars, StackOut2, StackOut, Model4),
    append(Model3, Model4, Model).

append_set([],X,X):- !.
append_set([A|As],Bs,Cs) :-
    \+ \+ memberchk_oc(A, Bs),
    !,
    append_set(As,Bs,Cs).
append_set([A|As],Bs,[A|Cs]) :-
    append_set(As,Bs,Cs).

memberchk_oc(Term, [H|T]) :-
    (   unify_with_occurs_check(Term, H)
    ->  true
    ;   memberchk_oc(Term, T)
    ).

apply_const_store([]) :- !.
apply_const_store([C|Cs]) :-
    apply_constraint(C),
    apply_const_store(Cs).

apply_constraint(A \= B) =>
    A .\=. B.
apply_constraint(A = B) =>
    A .=. B.
apply_constraint(CLPConstraint) =>
    apply_clpq_constraints(CLPConstraint).

find_duals(C_Vars-C_Vars1, OtherVars, Duals) :-
    % disequality and clp for numbers
    dump_constraint(C_Vars, C_Vars1, Dump, []-[], Pending-Pending1), !,
    clp_vars_in(OtherVars, OtherCLPVars),
    append(Pending, OtherCLPVars, CLPVars),
    append(Pending1, OtherCLPVars, CLPVars1),
    clpqr_dump_constraints(CLPVars, CLPVars1, CLP),
    append(CLP, Dump, Constraints),
    make_duals(Constraints,Duals), !.

make_duals(Ls,Ds) :-
    make_duals_([],Ls,[],Ds).

make_duals_(_,[],Ds,Ds).
make_duals_(Prev,[A|As],D0,Ds) :-
    append(Prev,[A],Prev1),
    make_duals_(Prev1,As,D0,D1),
    dual(A,Duals_A),
    combine(Duals_A,Prev,As,Ls),
    append(Ls,D1,Ds).

combine([A],Prev,Post,[Rs]) :-
    append(Prev,[A|Post],Rs).
combine([A,B|As],Prev,Post,[RA|RAs]) :-
    append(Prev,[A|Post],RA),
    combine([B|As],Prev,Post,RAs).

:- det(dual/2).

dual(.=.(A,B), [.<.(A,B), .>.(A,B)]).
dual(.<.(A,B), [.>=.(A,B)]).
dual(.>.(A,B), [.=<.(A,B)]).
dual(.=<.(A,B), [.>.(A,B)]).
dual(.>=.(A,B), [.<.(A,B)]).

dual(=(A,B), [\=(A,B)]).
dual(\=(A,B), [=(A,B)]).


dump_constraint([], [], [], Pending, Pending).
dump_constraint([V|Vs], [V1|V1s], [V1 = V | Vs_Dump], P0, P1) :-
    ground(V),
    \+ number(V), !,
    dump_constraint(Vs, V1s, Vs_Dump, P0, P1).
dump_constraint([V|Vs], [V1|V1s], [V1 = V | Vs_Dump], P0, P1) :-
    ground(V),
    number(V), !,
    dump_constraint(Vs, V1s, Vs_Dump, P0, P1).
dump_constraint([V|Vs], [V1|V1s], Rs_Dump, P0, P1) :-
    get_neg_var(V, List),
    List \== [], !,
    dump_neg_list(V1, List, V_Dump),
    dump_constraint(Vs, V1s, Vs_Dump, P0, P1),
    append(V_Dump, Vs_Dump, Rs_Dump).
dump_constraint([V|Vs], [V1|V1s], Vs_Dump, PV-PV1, P1) :-
    dump_constraint(Vs, V1s, Vs_Dump, [V|PV]-[V1|PV1], P1).

dump_neg_list(_,[],[]) :- !.
dump_neg_list(V,[L|Ls],[V \= L|Rs]) :- dump_neg_list(V,Ls,Rs).


clp_vars_in(Vars, ClpVars) :-
    include(is_clpq_var, Vars, ClpVars).

%!  collect_vars(?Forall, ?CollectForall)
%
%   Forall Vars in a list

collect_vars(Forall, c_forall(Vars, Goal)) :-
    collect_vars_(Forall, [], Vars, Goal).

collect_vars_(forall(Var, Goal), Vars, [Var|Vars], Goal) :-
    Goal \= forall(_, _), !.
collect_vars_(forall(Var, Forall), V0, V1, Goal) :-
    collect_vars_(Forall, [Var|V0], V1, Goal).


		 /*******************************
		 *     AUXILIAR PREDICATES      *
		 *******************************/

%!  my_copy_term(+Var, +Term, -NewVar, -NewTerm) is det.
%
%   Its behaviour is similar to  copy_term/2.   It  returns in NewTerm a
%   copy of the term Term but  it   only  replaces with a fresh variable
%   NewVar the occurrences of Var

my_copy_term(Var0, Term0, Var, Term) :-
    term_variables(Term0, AllVars),
    delete_var(AllVars, Var0, Share0),
    copy_term_nat(t(Var0,Share0,Term0), t(Var,Share,Term)),
    Share = Share0.

delete_var([], _, []).
delete_var([H|T0], V, List) :-
    (   H == V
    ->  List = T0
    ;   List = [H|T],
        delete_var(T0, V, T)
    ).

my_copy_vars(Vars0, Term0, Vars, Term) :-
    term_variables(Term0, AllVars),
    sort(AllVars, AllVarsSorted),
    sort(Vars0, Vars0Sorted),
    ord_subtract(AllVarsSorted, Vars0Sorted, Share0),
    copy_term_nat(t(Vars0,Share0,Term0), t(Vars,Share,Term)),
    Share = Share0.

my_diff_term(Term, Vars, Others) :-
    term_variables(Term, Set),
    diff_vars(Set, Vars, Others).

%!  diff_vars(+VarsIn, -Subtract, -VarsOut) is det.

diff_vars([], _, []).
diff_vars([H|T0], Vars, List) :-
    (   member_var(Vars, H)
    ->  diff_vars(T0, Vars, List)
    ;   List = [H|T],
        diff_vars(T0, Vars, T)
    ).

member_var(Vars, Var) :-
    member(V, Vars),
    Var == V,
    !.