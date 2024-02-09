:- module(scasp_solve,
          [ solve/4                   % :Goals, +StackIn, -StackOut, -Model
          ]).
:- use_module(clp/call_stack, [(<~)/2, (~>)/2, op(_,_,_)]).
:- use_module(clp/disequality, [(.\=.)/2, get_neg_var/2, loop_term/2, op(_,_,_)]).
:- use_module(predicates,
              [ shown_predicate/1, user_predicate/1, table_predicate/1,
                clp_builtin/1, clp_interval/1, prolog_builtin/1 ]).
:- use_module(clp/clpq,
              [ dual_clpq/2, dump_clpq_var/3, apply_clpq_constraints/1,
                entails/2, entail_terms/2, clpqr_dump_constraints/3, is_clpq_var/1 ]).
:- use_module(verbose,
              [ verbose/1, scasp_trace_goal/2, scasp_trace_event/2,
                scasp_warning/1, scasp_warning/2, scasp_info/2,
                print_check_calls_calling/2
              ]).

:- autoload(library(apply), [maplist/2, include/3]).
:- autoload(library(assoc),
            [get_assoc/3, empty_assoc/1, get_assoc/5, put_assoc/4]).
:- autoload(library(lists), [append/3, member/2]).
:- autoload(library(terms), [variant/2]).

:- meta_predicate
    solve(:, +, -, -).

/** <module> The sCASP solver
*/

% Note:  The  Ciao  original  controls   forall  algorithm  using  three
% current_option/2 facts, `prev_forall`, `sasp_forall` and `all_forall`:
%
%   <no option>:    no current_option/2
%   --prev_forall:  prev_forall=on, sasp_forall=off
%   --all_c_forall: all_forall=on
%   --sasp_forall:  prev_forall=on sasp_forall=on

:- create_prolog_flag(scasp_no_fail_loop, false, [keep(true)]).
:- create_prolog_flag(scasp_assume,       false, [keep(true)]).
:- create_prolog_flag(scasp_forall,       all,   [keep(true)]).
:- create_prolog_flag(scasp_dcc,	  false, [keep(true)]).
:- create_prolog_flag(scasp_trace_dcc,	  false, [keep(true)]).

%!  solve(:Goals, +StackIn, -StackOut, -Model)
%
%   Solve the list of sub-goals `Goal`  where   StackIn  is  the list of
%   goals already visited and returns  in   StackOut  the  list of goals
%   visited to prove the sub-goals and in  Model the model that supports
%   the sub-goals.

solve(M:Goals, StackIn, StackOut, Model) :-
    stack_parents(StackIn, Parents),
    stack_proved(StackIn, ProvedIn),
    solve(Goals, M, Parents, ProvedIn, _ProvedOut, StackIn, StackOut, Model).

solve([], _, _, Proved, Proved, StackIn, [[]|StackIn], []).
solve([Goal|Goals], M, Parents, ProvedIn, ProvedOut, StackIn, StackOut, Model) :-
    verbose(print_check_calls_calling(Goal, StackIn)),
    check_goal(Goal, M, Parents, ProvedIn, ProvedMid, StackIn, StackMid, Modelx),
    Modelx = [AddGoal|JGoal],
    verbose(format('Success ~@\n', [print_goal(Goal)])),
    solve(Goals, M, Parents, ProvedMid, ProvedOut, StackMid, StackOut, Modelxs),
    Modelxs = JGoals,
    (   shown_predicate(M:Goal)
    ->  Model = [AddGoal, JGoal|JGoals]
    ;   Model = [JGoal|JGoals]
    ).


proved_relatives(not(Goal), Proved, Relatives) =>
    proved_relatives(Goal, Proved, Relatives).
proved_relatives(Goal, Proved, Relatives) =>
    functor(Goal, Name, Arity),
    get_assoc(Name/Arity, Proved, Relatives).

%!  check_goal(+Goal, +Module, +Parents, +ProvedIn, -ProvedOut,
%!              +StackIn, -StackOut, -Model)
%
%   Call  check_CHS/3 to  check the  sub-goal Goal  against the  list of
%   goals already visited StackIn to  determine if  it is  a coinductive
%   success, a coinductive failure, an already proved sub-goal, or if it
%   has to be evaluated.
%
%   @arg StackOut is updated by prepending one or more elements to
%   StackIn.
%
%	  - [], chs(Goal)		Proved by co-induction
%	  - [], proved(Goal)		Proved in a completed subtree
%	  - From solve_goal/8		Continued execution

check_goal(Goal, M, Parents, ProvedIn, ProvedOut, StackIn, StackOut, Model) :-
    check_CHS(Goal, M, Parents, ProvedIn, StackIn, Check),
    check_goal_(Check, Goal, M,
                Parents, ProvedIn, ProvedOut, StackIn, StackOut,
                Model),
    (   current_prolog_flag(scasp_dcc, true),
        (   Check == co_success
        ;   Check == cont
        )
    ->  dynamic_consistency_check(Goal, M, StackIn)
    ;   true
    ).

% coinduction success <- cycles containing even loops may succeed
check_goal_(co_success, Goal, _M,
            _Parents, ProvedIn, ProvedOut, StackIn, StackOut,
            [AddGoal]) :-
    AddGoal = chs(Goal),
    add_proved(AddGoal, ProvedIn, ProvedOut),
    (   current_prolog_flag(scasp_assume, true)
    ->  mark_prev_goal(Goal, StackIn, StackMark),
        StackOut = [[],AddGoal|StackMark]
    ;   StackOut = [[],AddGoal|StackIn]
    ).
% already proved in the stack
check_goal_(proved, Goal, _M,
            _Parents, ProvedIn, ProvedOut, StackIn, StackOut,
            [AddGoal]) :-
    AddGoal = proved(Goal),
    add_proved(AddGoal, ProvedIn, ProvedOut),
    StackOut = [[], proved(Goal)|StackIn].
% coinduction does neither success nor fails <- the execution continues inductively
check_goal_(cont, Goal, M,
            Parents, ProvedIn, ProvedOut, StackIn, StackOut,
            Model) :-
    scasp_trace_goal(
        M:Goal,
        solve_goal(Goal, M,
                   [Goal|Parents], ProvedIn, ProvedOut, StackIn, StackOut,
                   Model)).
% coinduction fails <- the negation of a call unifies with a call in the call stack
check_goal_(co_failure, _Goal, _M,
            _Parents, _ProvedIn, _ProvedOut, _StackIn, _StackOut,
            _Model) :-
    fail.

mark_prev_goal(Goal, [I|In], [assume(Goal)|In]) :-  Goal == I, !.
mark_prev_goal(Goal, [I|In], [I|Mk]) :- mark_prev_goal(Goal,In,Mk).
mark_prev_goal(_Goal,[],[]).

%!  dynamic_consistency_check(+Goal, +Module, +StackIn) is semidet.
%
%   Check that the resulting literal is consistent with the nmr.

dynamic_consistency_check(Goal, M, StackIn) :-
    user_predicate(M:Goal),
    ground(Goal),
    M:pr_dcc_predicate(dcc(Goal), Body),
%   scasp_trace_event(scasp_trace_dcc, dcc_call(Goal, StackIn)),
    dynamic_consistency_eval(Body, M, StackIn),
    !,
    scasp_trace_event(scasp_trace_dcc, dcc_discard(Goal, Body)),
    fail.
dynamic_consistency_check(_, _, _).


dynamic_consistency_eval([], _, _).
dynamic_consistency_eval([SubGoal|Bs], M, StackIn) :-
    dynamic_consistency_eval_(SubGoal, M, StackIn),
    dynamic_consistency_eval(Bs, M, StackIn).

dynamic_consistency_eval_(not(SubGoal), M, StackIn) :-
    user_predicate(M:SubGoal), !,
    member_stack(not(SubGoal), StackIn).
dynamic_consistency_eval_(SubGoal, M, StackIn) :-
    user_predicate(M:SubGoal), !,
    member_stack(SubGoal, StackIn).
dynamic_consistency_eval_(SubGoal, _, _) :-
    solve_goal_builtin(SubGoal, _).


%!  solve_goal(+Goal, +Module,
%!             +Parents, +ProvedIn, -ProvedOut, +StackIn, -StackOut,
%!             -Model)
%
%   Solve a  simple sub-goal  Goal where  StackIn is  the list  of goals
%   already visited and returns in StackOut the list of goals visited to
%   prove  the  sub-goals  and  in  `Model` the  model with  support the
%   sub-goals

solve_goal(Goal, M, Parents, ProvedIn, ProvedOut, StackIn, StackOut, GoalModel) :-
    Goal = forall(_, _),
    !,
    (   current_prolog_flag(scasp_forall, Algo),
        ( Algo == prev -> true ; Algo == sasp )
        % Ciao version --prev_forall or --sasp_forall
    ->  solve_goal_forall(Goal, M,
                          Parents, ProvedIn, ProvedOut, [Goal|StackIn], StackOut,
                          Model)
    ;   solve_c_forall(Goal, M,
                       Parents, ProvedIn, ProvedOut, [Goal|StackIn], StackOut,
                       Model)
    ),
    GoalModel = [Goal|Model].
solve_goal(Goal, _M,
           _Parents, ProvedIn, ProvedOut, StackIn, [[], Goal|StackIn],
           GoalModel) :-
    Goal = not(is(V, Expresion)),
    add_proved(Goal, ProvedIn, ProvedOut),
    !,
    NV is Expresion,
    V .\=. NV,
    GoalModel = [Goal].
solve_goal(Goal, _, _, _, _, _, _, _) :-
    Goal = not(true),
    !,
    fail.
solve_goal(Goal, M, Parents, ProvedIn, ProvedOut, StackIn, StackOut, Model) :-
    table_predicate(M:Goal),
    !,
    verbose(format('Solve the tabled goal ~p\n', [Goal])),
    AttStackIn <~ stack([Goal|StackIn]),
    solve_goal_table_predicate(Goal, M, Parents, ProvedIn, ProvedOut,
                               AttStackIn, AttStackOut, AttModel),
    AttStackOut ~> stack(StackOut),
    AttModel ~> model(Model).
solve_goal(call(Goal), M, Parents, ProvedIn, ProvedOut, StackIn, StackOut,
           [call(Goal)|Model]) :-
    !,
    solve_goal(Goal, M, Parents, ProvedIn, ProvedOut, StackIn, StackOut, Model).
solve_goal(not(call(Goal)), M, Parents, ProvedIn, ProvedOut, StackIn, StackOut,
           [not(call(Goal))|Model]) :-
    !,
    solve_goal(not(Goal), M, Parents, ProvedIn, ProvedOut, StackIn, StackOut,
               Model).
solve_goal(Goal, M, Parents, ProvedIn, ProvedOut, StackIn, StackOut,
           [Goal|Model]) :-
    Goal = findall(_, _, _),
    !,
    exec_findall(Goal, M, Parents, ProvedIn, ProvedOut, StackIn, StackOut, Model).
solve_goal(not(Goal), M, Parents, ProvedIn, ProvedIn, StackIn, StackIn,
           [not(Goal)]) :-
    Goal = findall(_, _, _),
    !,
    exec_neg_findall(Goal, M, Parents, ProvedIn, StackIn).
solve_goal(Goal, M, Parents, ProvedIn, ProvedOut, StackIn, StackOut, Model) :-
    user_predicate(M:Goal),
    !,
    (   solve_goal_predicate(Goal, M, Parents, ProvedIn, ProvedOut,
                             StackIn, StackOut, Model)
    *-> true
    ;   verbose(format(' FAIL~n')),
        shown_predicate(M:Goal),
        scasp_trace_event(scasp_trace_failures,
                          trace_failure(Goal, [Goal|StackIn])),
        fail
    ).
solve_goal(Goal, _M, _Parents, ProvedIn, ProvedIn, StackIn, [[], Goal|StackIn],
           Model) :-
    solve_goal_builtin(Goal, Model).


mkgoal(Goal, generated(_), Goal                     ) :- !.
mkgoal(Goal, Origin      , goal_origin(Goal, Origin)).

%!  solve_goal_forall(+Forall, +Module,
%!                    +Parents, +ProvedIn, -ProvedOut, +StackIn, -StackOut,
%!                    -Model)
%
%   Solve a sub-goal of the form `forall(Var,Goal)`  and success  if Var
%   success in all its domain for the goal Goal. It calls solve/4
%
%   @arg Forall is a term forall(?Var, ?Goal)

solve_goal_forall(forall(Var, Goal), M,
                  Parents, ProvedIn, ProvedOut, StackIn, [[]|StackOut],
                  Model) :-
    my_copy_term(Var, Goal, NewVar, NewGoal),
    my_copy_term(Var, Goal, NewVar2, NewGoal2),
    solve([NewGoal], M, Parents, ProvedIn, ProvedMid, StackIn, [[]|StackMid],
          ModelMid),
    verbose(format('\tSuccess solve ~@\n\t\t for the ~@\n',
                   [print_goal(NewGoal), print_goal(forall(Var, Goal))])),
    check_unbound(NewVar, List),
    (   List == ground
    ->  verbose(format('The var ~p is grounded so try with other clause\n',
                       [NewVar])),
        fail
    ;   List == []
    ->  ProvedOut = ProvedMid,
        StackOut = StackMid,
        Model = ModelMid
    ;   List = clpq(NewVar3, Constraints)
    ->  findall(dual(NewVar3, ConDual),
                dual_clpq(Constraints, ConDual),
                DualList),
        verbose(format('Executing ~@ with clpq ConstraintList = ~p\n',
                       [print_goal(Goal), DualList])),
        exec_with_clpq_constraints(NewVar2, NewGoal2, M,
                                   entry(NewVar3, []),
                                   DualList,
                                   Parents, ProvedMid, ProvedOut,
                                   StackMid, StackOut, ModelList),
        !,
        append(ModelMid, ModelList, Model)
    ;   verbose(format('Executing ~@ with clp_disequality list = ~p\n',
                       [print_goal(Goal), List])),
        (   current_prolog_flag(scasp_forall, prev)
        ->  !  % Ciao --prev_forall: remove answers in max.lp
        ;   true
        ),
        exec_with_neg_list(NewVar2, NewGoal2, M,
                           List, Parents, ProvedMid, ProvedOut,
                           StackMid, StackOut, ModelList),
        (   current_prolog_flag(scasp_forall, sasp)
        ->  !  % Ciao --sasp_forall: remove answers in hamcycle_two.lp
               % Without cuts the evaluation may loop - e.g. queens.lp
        ;   true
        ),
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

exec_with_clpq_constraints(_Var, _Goal, _M, _, [],
                           _Parents, ProvedIn, ProvedIn, StackIn, StackIn, []).
exec_with_clpq_constraints(Var, Goal, M, entry(ConVar, ConEntry),
                           [dual(ConVar, ConDual)|Duals],
                           Parents, ProvedIn, ProvedOut, StackIn, StackOut,
                           Model) :-
    my_copy_term(Var,   [Goal,   Parents,   StackIn,   ProvedIn],
                 Var01, [Goal01, Parents01, StackIn01, ProvedIn01]),
    append(ConEntry, ConDual, Con),
    my_copy_term(ConVar, Con, ConVar01, Con01),
    my_copy_term(Var, Goal, Var02, Goal02),
    my_copy_term(ConVar, ConEntry, ConVar02, ConEntry02),
    Var01 = ConVar,
    (   apply_clpq_constraints(Con)
    ->  verbose(format('Executing ~p with clpq_constrains ~p\n',
                       [Goal01, Con])),
        solve([Goal01], M, Parents01, ProvedIn01, ProvedOut01, StackIn01, [[]|StackOut01], Model01),
        verbose(format('Success executing ~p with constrains ~p\n',
                       [Goal01, Con])),
        verbose(format('Check entails Var = ~p with const ~p and ~p\n',
                       [Var01, ConVar01, Con01])),
        (   entails([Var01], ([ConVar01], Con01))
        ->  verbose(format('\tOK\n', [])),
            StackOut02 = StackOut01,
            Model03 = Model01
        ;   verbose(format('\tFail\n', [])),
            dump_clpq_var([Var01], [ConVar01], ExitCon),
            findall(dual(ConVar01, ConDual01),
                    dual_clpq(ExitCon, ConDual01),
                    DualList),
            verbose(format('Executing ~p with clpq ConstraintList = ~p\n',
                           [Goal, DualList])),
            exec_with_clpq_constraints(Var, Goal, M, entry(ConVar01, Con01),
                                       DualList,
                                       Parents01, ProvedOut01, ProvedOut02,
                                       StackOut01, StackOut02, Model02),
            append(Model01, Model02, Model03)
        )
    ;   verbose(format('Skip execution of an already checked \c
                        constraint ~p (it is inconsitent with ~p)\n',
                       [ConDual, ConEntry])),
        StackOut02 = StackIn01,
        Model03 = []
    ),
    verbose(format('Executing ~p with clpq ConstraintList = ~p\n',
                   [Goal, Duals])),
    exec_with_clpq_constraints(Var02, Goal02, M,
                               entry(ConVar02, ConEntry02),
                               Duals, Parents, ProvedOut02, ProvedOut,
                               StackOut02, StackOut, Model04),
    append(Model03, Model04, Model).

exec_with_neg_list(_, _, _, [], _, ProvedIn, ProvedIn, StackIn, StackIn, []).
exec_with_neg_list(Var, Goal, M, [Value|Vs],
                   Parents, ProvedIn, ProvedOut, StackIn, StackOut,
                   Model) :-
    my_copy_term(Var, [Goal, StackIn], NewVar, [NewGoal, NewStackIn]),
    NewVar = Value,
    verbose(format('Executing ~p with value ~p\n', [NewGoal, Value])),
    solve([NewGoal], M, Parents,
          ProvedIn, ProvedMid, NewStackIn, [[]|NewStackMid], ModelMid),
    verbose(format('Success executing ~p with value ~p\n',
                   [NewGoal, Value])),
    exec_with_neg_list(Var, Goal, M, Vs, Parents,
                       ProvedMid, ProvedOut, NewStackMid, StackOut, Models),
    append(ModelMid, Models, Model).

%!  solve_goal_table_predicate(+Goal, +Module, +Parents, +ProvedIn, -ProvedOut,
%!                             +AttStackIn, -AttStackOut, -AttModel)
%
%   Used to evaluate predicates under tabling. This predicates should be
%   defined in the program using the directive _#table pred/n._

solve_goal_table_predicate(Goal, M, Parents, ProvedIn, ProvedOut, AttStackIn, AttStackOut, AttModel) :-
    M:pr_rule(Goal, Body, _Origin),
    AttStackIn ~> stack(StackIn),
    solve(Body, M, Parents, ProvedIn, ProvedOut, StackIn, StackOut, Model),
    AttStackOut <~ stack(StackOut),
    AttModel <~ model([Goal|Model]).

%!  solve_goal_predicate(+Goal, +Module,
%!                       +Parents, +ProvedIn, -ProvedOut, +StackIn, -StackOut,
%!                       -Model)
%
%   Used to evaluate a user predicate

solve_goal_predicate(Goal, M, Parents, ProvedIn, ProvedOut, StackIn, StackOut,
                     GoalModel) :-
    M:pr_rule(Goal, Body, Origin),
    mkgoal(Goal, Origin, StackGoal),
    solve(Body, M, Parents, ProvedIn, ProvedMid, [StackGoal|StackIn], StackOut, BodyModel),
    add_proved(Goal, ProvedMid, ProvedOut),
    GoalModel = [Goal|BodyModel].

%!  solve_goal_builtin(+Goal, -Model)
%
%   Used to evaluate builtin predicates predicate

solve_goal_builtin(is(X, Exp), Model) :-
    exec_goal(is(X, Exp)),
    Model = [is(X, Exp)]. %% the Model should 'Start' with the Goal
solve_goal_builtin(builtin(Goal), Model) :- !,
    exec_goal(Goal),
    Model = [builtin(Goal)].
solve_goal_builtin(Goal, Model) :-
    clp_builtin(Goal), !,
    exec_goal(apply_clpq_constraints(Goal)),
    Model = [Goal].
solve_goal_builtin(Goal, Model) :-
    clp_interval(Goal), !,
    exec_goal(Goal),
    Model = [Goal].
solve_goal_builtin(not(Goal), _Model) :-
    clp_interval(Goal), !,
    scasp_warning(scasp(failure_calling_negation(Goal))),
    fail.
solve_goal_builtin(Goal, Model) :-
    clp_builtin(Goal),
    !,
    exec_goal(apply_clpq_constraints(Goal)),
    Model = [Goal].
solve_goal_builtin(Goal, Model) :-
    prolog_builtin(Goal), !,
    exec_goal(Goal),
    Model = [Goal].
% The predicate is not defined as user_predicates neither builtin
solve_goal_builtin(Goal, Model) :-
    verbose(format('The predicate ~p is not user_defined / builtin\n', [Goal])),
    (   Goal = not(_)
    ->  Model = [Goal] %% the negation of a not defined predicate success.
    ;   fail %% a not defined predicate allways fails.
    ).

exec_goal(A \= B) :- !,
    verbose(format('exec ~@\n', [print_goal(A \= B)])),
    A .\=. B,
    verbose(format('ok   ~@\n', [print_goal(A \= B)])).
exec_goal(Goal) :-
    (   current_prolog_flag(scasp_verbose, true)
    ->  E = error(_,_),
        verbose(format('exec goal ~@ \n', [print_goal(Goal)])),
        catch(call(Goal), E, (print_message(warning, E), fail)),
        verbose(format('ok   goal ~@ \n', [print_goal(Goal)]))
    ;   catch(call(Goal), error(_,_), fail)
    ).

% TODO: Pending StackOut to carry the literal involved in the findall
% (if needed)
% TODO: Handling of ProvedIn/ProvedOut

%!  exec_findall(+Findall, +Module,
%!		 +ProvedIn, -ProvedOut, +StackIn, -StackOut, -Model)

exec_findall(findall(Var, Call, List), M,
             Parents, ProvedIn, ProvedOut, StackIn, StackOut, Model) :-
    verbose(format('execution of findall(~p, ~p, _) \n', [Var, Call])),
    findall(t(Var, S, Mdl), (
            solve([Call], M, Parents, ProvedIn, _ProvedOut, StackIn, S0, Mdl),
            append(S, StackIn, S0)
        ), VSMList),
    process_vsmlist(VSMList, List, SOut, Model),
    append(SOut, [findall(Var, Call, List)|StackIn], StackOut),
    stack_proved(StackOut, ProvedOut),
    verbose(format('Result execution = ~p \n', [List])).

process_vsmlist(VSMList, List, [[]|StackOut], Model) :-
    process_vsmlist_(VSMList, List, StackOut, Model).

process_vsmlist_([], [], [], []).
process_vsmlist_([t(V, [[]|S], M)|Rs], [V|Vs], S1, M1) :-
    process_vsmlist_(Rs, Vs, S0, M0),
    append(S0, S, S1),
    append(M, M0, M1).

% TODO: What to do with the negation of findall/3 (if required)
exec_neg_findall(Goal, _, _, _, _) :-
    verbose(format('PENDING: execution of not ~p \n', [Goal])),
    fail.


%!  check_CHS(+Goal, +Module, +Parents, +ProvedIn, +StackIn, -Result) is det.
%
%   Checks the StackIn and returns  in  Result   if  the  goal Goal is a
%   coinductive success, a coinductive  failure   or  an  already proved
%   goal. Otherwise it is constraint against  its negation atoms already
%   visited.

:- det(check_CHS/6).

check_CHS(Goal, M, Parents, Proved, I, Result) :-
    (   user_predicate(M:Goal)
    ->  check_CHS_(Goal, M, Parents, Proved, I, Result)
    ;   Result = cont
    ).

% inmediate success if the goal has already been proved.
check_CHS_(Goal, _M, _Parents, Proved, _Stack, proved) :-
    ground(Goal),
    proved_in_stack(Goal, Proved),
    !.
% coinduction success <- cycles containing even loops may succeed
check_CHS_(Goal, _M, Parents, _Proved, _I, co_success) :-
    check_parents(Goal, Parents, even),
    !.
% coinduction fails <- the goal is entailed by its negation in the
% call stack
check_CHS_(Goal, _M, Parents, Proved, _Stack, co_failure) :-
    neg_in_stack(Goal, Parents, Proved), !,
    verbose(format('Negation of the goal in the stack, failling (Goal = ~w)\n', [Goal])).
% coinduction fails <- cycles containing positive loops can be solved
% using tabling
check_CHS_(Goal, M, Parents, _Proved, _Stask, co_failure) :-
    \+ table_predicate(M:Goal),
    \+ current_prolog_flag(scasp_no_fail_loop, true),
    \+ \+ (
        check_parents(Goal, Parents, fail_pos(S)),
        verbose(format('Positive loop, failing (Goal == ~w)\n', [Goal])),
        scasp_warning(scasp_warn_pos_loops, pos_loop(fail, Goal, S))
    ), !.
check_CHS_(Goal, M, Parents, _Proved, _Stack, _Cont) :-
    \+ table_predicate(M:Goal),
    \+ \+ (
        check_parents(Goal, Parents, pos(S)),
        verbose(format('Positive loop, continuing (Goal = ~w)\n', [Goal])),
        scasp_info(scasp_warn_pos_loops, pos_loop(continue, Goal, S))
    ), fail.
% coinduction does not succeed or fail <- the execution continues inductively
check_CHS_(Goal, _M, Parents, Proved, _Stack, cont) :-
    (   ground(Goal)
    ->  constrained_neg_in_stack(Goal, Parents, Proved)
    ;   ground_neg_in_stack(Goal, Parents, Proved)
    ).

%!  neg_in_stack(+Goal, +Parents, +Proved) is semidet.
%
%   True when the nagation of  Goal  is  in   Stack.  If  so  we  have a
%   coinductive failure. Check on variants   which  requires tabling for
%   proper results.

neg_in_stack(Goal, _Parents, Proved) :-
    proved_relatives(Goal, Proved, Relatives),
    member(Relative, Relatives),
    is_negated_goal(Goal, Relative),
    !.
neg_in_stack(Goal, Parents, _) :-
    member(Relative, Parents),
    is_negated_goal(Goal, Relative),
    !.

is_negated_goal(Goal, Head) :-
    (   Goal = not(G)
    ->  (   G == Head
        ->  true
        ;   G =@= Head
        ->  scasp_warning(co_failing_in_negated_loop(G, Head))
        )
    ;   Head = not(NegGoal)
    ->  (   Goal == NegGoal
        ->  true
        ;   Goal =@= NegGoal
        ->  scasp_warning(co_failing_in_negated_loop(Goal, NegGoal))
        )
    ).

%!  ground_neg_in_stack(++Goal, +Parents, +Proved) is det.
%
%   Propagate disequality constraints of Goal  through matching goals on
%   the stack.

:- det(ground_neg_in_stack/3).

ground_neg_in_stack(Goal, Parents, Proved) :-
    verbose(format('Enter ground_neg_in_stack for ~@\n', [print_goal(Goal)])),
    (   proved_relatives(Goal, Proved, Relatives)
    ->  maplist(ground_neg_in_stack_(Flag, Goal), Relatives)
    ;   true
    ),
    maplist(ground_neg_in_stack_(Flag, Goal), Parents),
    (   Flag == found
    ->  verbose(format('\tThere exit the negation of ~@\n\n', [print_goal(Goal)]))
    ;   true
    ).

ground_neg_in_stack_(found, TGoal, SGoal) :-
    gn_match(TGoal, SGoal, Goal, NegGoal),
    \+ Goal \= NegGoal,
    verbose(format('\t\tCheck disequality of ~@ and ~@\n',
                   [print_goal(TGoal), print_goal(SGoal)])),
    loop_term(Goal, NegGoal),
    !.
ground_neg_in_stack_(_, _, _).

gn_match(Goal, chs(not(NegGoal)), Goal, NegGoal) :- !.
gn_match(not(Goal), chs(NegGoal), Goal, NegGoal) :- !.
gn_match(not(Goal), NegGoal,      Goal, NegGoal) :- !.


%!  constrained_neg_in_stack(+Goal, +Parents, +Proved) is det.
%
%   Propagate the fact that we accept Goal into all other accepted goals
%   in the stack.

:- det(constrained_neg_in_stack/3).

constrained_neg_in_stack(Goal, Parents, Proved) :-
    (   proved_relatives(Goal, Proved, Relatives)
    ->  maplist(contrained_neg(Goal), Relatives)
    ;   true
    ),
    maplist(contrained_neg(Goal), Parents).

contrained_neg(not(Goal), NegGoal) :-
    is_same_functor(Goal, NegGoal),
    verbose(format('\t\tCheck if not(~@) is consistent with ~@\n',
                   [print_goal(Goal), print_goal(NegGoal)])), !,
    loop_term(Goal, NegGoal),
    !,
    verbose(format('\t\tOK\n', [])).
contrained_neg(Goal, not(NegGoal)) :-
    is_same_functor(Goal, NegGoal),
    verbose(format('\t\tCheck if not(~@) is consistent with ~@\n',
                   [print_goal(Goal), print_goal(NegGoal)])), !,
    loop_term(Goal, NegGoal),
    !,
    verbose(format('\t\tOK\n', [])).
contrained_neg(_,_).

is_same_functor(Term1, Term2) :-
    functor(Term1, Name, Arity, Type),
    functor(Term2, Name, Arity, Type).

%!  proved_in_stack(+Goal, +Proved) is semidet.
%
%   True when Goal appears in one of  the finished branches of the proof
%   tree, i.e., it appears in Stack, but not as direct parent.

proved_in_stack(Goal, Proved) :-
    proved_relatives(Goal, Proved, Relatives),
    member(Relative, Relatives),
    (   Goal == Relative
    ;   Goal == chs(Relative)
    ),
    !.

%!  check_parents(+Goal, +Parents, -Type) is semidet.
%
%   Type is the coinductive result. This is   `even`  if we have an even
%   loop through negation or a simple positive match.

check_parents(not(Goal), Parents, Type) :-
    !,
    check_parents(not(Goal), 1, Parents, Type).
check_parents(Goal, Parents, Type) :-
    check_parents(Goal, 0, Parents, Type).

check_parents(Goal, 0, [Parent|_Parents], Type) :-
    (   \+ \+ type_loop_fail_pos(Goal, Parent)
    ->  Type = fail_pos(Parent)
    ;   \+ Goal \= Parent
    ->  Type = pos(Parent)
    ),
    !.
check_parents(Goal, N, [Parent|Parents], Type) :-
    (   even_loop(Goal, Parent, N)
    ->  Type = even
    ;   Goal \== Parent
    ->  (  Parent = not(_)
        ->  NewN is N + 1,
            check_parents(Goal, NewN, Parents, Type)
        ;   check_parents(Goal, N, Parents, Type)
        )
    ).

even_loop(not(Goal), not(Parent), _) :-
    Goal =@= Parent,
    !,
    Goal = Parent.
even_loop(Goal, Parent, N) :-
    Goal \= not(_),
    Goal == Parent,
    N > 0,
    0 =:= mod(N, 2).

type_loop_fail_pos(Goal, S) :-
    Goal == S, !.
type_loop_fail_pos(Goal, S) :-
    variant(Goal, S), !,
    scasp_warning(variant_loop(Goal, S)).
type_loop_fail_pos(Goal, S) :-
    entail_terms(Goal, S),
    scasp_warning(subsumed_loop(Goal, S)).

%!  stack_parents(+Stack, -Parents) is det.
%
%   Get the direct callers from  Stack.   Stack  contains  both previous
%   proved stacks as well as the current list of parents.

:- det(stack_parents/2).
stack_parents(Stack, Parents) :-
    stack_parents(Stack, 0, Parents).

stack_parents([], _, []).
stack_parents([[]|T], N, Parents) :-
    !,
    N1 is N-1,
    stack_parents(T, N1, Parents).
stack_parents([_|T], N, Parents) :-
    N < 0,
    !,
    N1 is N+1,
    stack_parents(T, N1, Parents).
stack_parents([goal_origin(H, _)|T], N, [H|TP]) :-
    !,
    stack_parents(T, N, TP).
stack_parents([H|T], N, [H|TP]) :-
    stack_parents(T, N, TP).

%!  stack_proved(Stack, Proved:assoc) is det.
%
%   True when Proved is an assoc  holding   all  goals that have already
%   been proved in Stack. This excludes  the   direct  parents of in the
%   stack, i.e. only adds goals from already completed branches.
%
%   The code is based on  the   old  proved_in_stack/2. Effectively this
%   extracts the other half than stack_parents/2,  so possibly we should
%   sync the code with that.

:- det(stack_proved/2).

stack_proved(Stack, Proved) :-
    empty_assoc(Proved0),
    stack_proved(Stack, 0, -1, Proved0, Proved).

stack_proved([], _, _, Proved, Proved).
stack_proved([Top|Ss], Intervening, MaxInter, Proved0, Proved) :-
    (   Top == []
    ->  NewInter is Intervening - 1,
        stack_proved(Ss, NewInter, MaxInter, Proved0, Proved)
    ;   Intervening > MaxInter
    ->  NewMaxInter is max(MaxInter, Intervening),
        NewInter is Intervening + 1,
        stack_proved(Ss, NewInter, NewMaxInter, Proved0, Proved)
    ;   add_proved(Top, Proved0, Proved1),
        stack_proved(Ss, Intervening, MaxInter, Proved1, Proved)
    ).

add_proved(goal_origin(Goal, _), Assoc0, Assoc) =>
    add_proved(Goal, Goal, Assoc0, Assoc).
add_proved(Goal, Assoc0, Assoc) =>
    add_proved(Goal, Goal, Assoc0, Assoc).

add_proved(not(Term), Goal, Assoc0, Assoc) =>
    add_proved(Term, Goal, Assoc0, Assoc).
add_proved(chs(Term), Goal, Assoc0, Assoc) =>
    add_proved(Term, Goal, Assoc0, Assoc).
add_proved(forall(_,_), _Goal, Assoc0, Assoc) =>
    Assoc = Assoc0.
add_proved(Term, Goal, Assoc0, Assoc) =>
    functor(Term, Name, Arity),
    (   get_assoc(Name/Arity, Assoc0, List, Assoc, [Goal|List])
    ->  true
    ;   put_assoc(Name/Arity, Assoc0, [Goal], Assoc)
    ).

%!  solve_c_forall(+Forall, +Module,
%!                 +Parents, +ProvedIn, -ProvedOut, +StackIn, -StackOut,
%!                 -Model)
%
%   Solve a sub-goal of the form c_forall(Vars,Goal) and succeeds if the
%   goal `Goal` succeeds covering the domain of all the vars in the list
%   of vars `Vars. It calls solve/4
%
%   @arg Forall is a term forall(Var, Goal).
%   @tbd Improve the efficiency by removing redundant justifications w.o.
%   losing solutions.

solve_c_forall(Forall, M, Parents, ProvedIn, ProvedOut, StackIn, [[]|StackOut],
               Model) :-
    collect_vars(Forall, c_forall(Vars0, Goal0)),    % c_forall([F,G], not q_1(F,G))

    verbose(format('\nc_forall(~p,\t~@)\n\n',[Vars0, print_goal(Goal0)])),

    my_copy_vars(Vars0, Goal0, Vars1, Goal1),        % Vars should remain free
    my_diff_term(Goal1, Vars1, OtherVars),
    Initial_Const = [],                              % Constraint store = top
    (   current_prolog_flag(scasp_forall, all_c)     % Ciao --all_c_forall
    ->  solve_var_forall_(Goal1, M, Parents, ProvedIn, ProvedOut,
                          entry(Vars1, Initial_Const),
                          dual(Vars1, [Initial_Const]),
                          OtherVars, StackIn, StackOut, Model)
    ;   solve_other_forall(Goal1, M, Parents, ProvedIn, ProvedOut,
                           entry(Vars1, Initial_Const),
                           dual(Vars1, [Initial_Const]),
                           OtherVars, StackIn, StackOut, Model)
    ).

solve_other_forall(Goal, M, Parents, ProvedIn, ProvedOutExit,
                   entry(Vars, Initial_Const),
                   dual(Vars, [Initial_Const]),
                   OtherVars, StackIn, StackOutExit, ModelExit) :-
    append(Vars,OtherVars,AllVars),
    my_copy_vars(AllVars,   [Goal,  Parents,  ProvedIn,  StackIn,  OtherVars,  Vars],
                 _AllVars1, [Goal1, Parents1, ProvedIn1, StackIn1, OtherVars1, Vars1]),
    my_copy_vars(AllVars,   [Goal,  Parents,  ProvedIn,  StackIn,  OtherVars,  Vars],
                 _AllVars2, [Goal2, Parents2, ProvedIn2, StackIn2, OtherVars2, Vars2]),

    verbose(format("solve other forall:\n\c
                           \t Goal \t~p\n\c
                           \t Vars1       \t~p\n\c
                           \t OtherVars   \t~p\n\c
                           \t StackIn     \t~p\n\n",
                          ['G'(Goal),'G'(Vars1),
                           'G'(OtherVars),'G'(StackIn)])),

    % disequality and clp for numbers
    dump_constraint(OtherVars, OtherVars1, Dump, []-[], Pending-Pending1), !,
    clpqr_dump_constraints(Pending, Pending1, CLP),
    append(CLP, Dump, Constraints1),
    my_copy_vars(OtherVars1, Constraints1, OtherVars2, Constraints2),

    verbose(format("solve other forall:\n\c
                          \t OtherVars1   \t~p\n\c
                          \t OtherVars2   \t~p\n\c
                          \t Constraints1 \t~p\n\c
                          \t Constraints2 \t~p\n\n",
                          ['G'(OtherVars1), 'G'(OtherVars2),
                           'G'(Constraints1), 'G'(Constraints2)])),

    apply_const_store(Constraints1),
    !,

    solve_var_forall_(Goal1, M, Parents1, ProvedIn1, ProvedOut,
                      entry(Vars1, Initial_Const),
                      dual(Vars1, [Initial_Const]), OtherVars1,
                      StackIn1, StackOut, Model),
    !,
    (   OtherVars = OtherVars1,
        StackOutExit = StackOut,
        ModelExit = Model,
        ProvedOutExit = ProvedOut
    ;   \+ ground(OtherVars),
        apply_const_store(Constraints2),
        % disequality and clp for numbers
        dump_constraint(OtherVars1, OtherVars2, Dump1, []-[], Pend-Pend1), !,
        clpqr_dump_constraints(Pend, Pend1, CLP1),
        append(CLP1, Dump1, AnsConstraints2),
        make_duals(AnsConstraints2, Duals),
        member(Dual, Duals),
        apply_const_store(Dual),
        solve_other_forall(Goal2, M, Parents2, ProvedIn2, ProvedOutExit,
                           entry(Vars2, Initial_Const),
                           dual(Vars2, [Initial_Const]),
                           OtherVars2, StackIn2, StackOutExit, ModelExit), !,
        OtherVars = OtherVars2
    ).

%!  solve_var_forall_(+Goal, +Module, +Parents, +ProvedIn, -ProvedOut,
%!                    +Entry, +Duals, +OtherVars,
%!                    +StackIn, -StackOut, -Model) is nondet.
%
%   Implements the _forall_ algorithm as described   in section 2.3 from
%   "Constraint Answer Set Programming  without   Grounding"  by Joaquin
%   Arias et all. It works different though.
%
%     - For each step  we  copy  the   Goal,  solve  it  and compute the
%       instantiations created by Goal. From these instantiations we
%       compute the _duals_.
%     - Continue until all duals are proved.
%
%   Note that the constraints on the   _forall variables_ are maintained
%   explicitly.

solve_var_forall_(_Goal, _M, _Parents, Proved, Proved, _, dual(_, []),
                  _OtherVars, StackIn, StackIn, []) :- !.
solve_var_forall_(Goal, M, Parents, ProvedIn, ProvedOut,
                  entry(C_Vars, Prev_Store),
                  dual(C_Vars, [C_St|C_Stores]),
                  OtherVars, StackIn, StackOut, Model) :-
    verbose(format("solve forall:\n\c
                          \tPrev_Store \t~p\n\c
                          \tC_St       \t~p\n\c
                          \tC_Stores   \t~p\n\c
                          \tStackIn    \t~p\n\n",
                          ['G'(Prev_Store),'G'(C_St),
                           'G'(C_Stores),'G'(StackIn)])),

    my_copy_vars(C_Vars,  [Goal,Prev_Store,C_St],
                 C_Vars1, [Goal1,Prev_Store1,C_St1]),
    my_copy_vars(C_Vars,  [Goal,Prev_Store,C_Stores],
                 C_Vars2, [Goal2,Prev_Store2,C_Stores2]),

    apply_const_store(Prev_Store),
    (   %verbose(format('apply_const_store ~@\n',[print_goal(C_St)])),
        apply_const_store(C_St) % apply a Dual
    ->  solve([Goal], M, Parents, ProvedIn, ProvedOut1, StackIn, [[]|StackOut1], Model1),
        find_duals(C_Vars, C_Vars1, OtherVars, Duals),       %% New Duals
        verbose(format('Duals = \t ~p\n',[Duals])),
        append_set(Prev_Store1, C_St1, Current_Store1),
        solve_var_forall_(Goal1, M, Parents, ProvedOut1, ProvedOut2,
                          entry(C_Vars1, Current_Store1),
                          dual(C_Vars1, Duals),
                          OtherVars, StackOut1, StackOut2, Model2),
        append(Model1,Model2,Model3)
    ;   verbose(format('Entail: Fail  applying \t ~p\n', ['G'(C_St)])),
        %% The dual C_St is not consistent with Prev_Store -> already checked (entails)
        ProvedOut2 = ProvedIn,
        StackOut2 = StackIn,
        Model3 = []
    ),
    solve_var_forall_(Goal2, M, Parents, ProvedOut2, ProvedOut,
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
    A = B.
apply_constraint(CLPConstraint) =>
    apply_clpq_constraints(CLPConstraint).

%!  find_duals(+C_Vars, +C_Vars1, +OtherVars, -Duals)
%
%   C_Vars is the list of forall variables   after solve/4. C_Vars1 is a
%   copy of this list before calling solve/4.   Our  task is to create a
%   _dual_ model from the instantiation. If   subsequently  we can prove
%   the dual model to be true then  we   proved  the forall is true. For
%   example, if solve  succeeded  with  [X]   -->  [q],  it  created the
%   instantiation X=q. It we now can prove X\=q, we proved solve is true
%   for all X.
%
%   @see Section 2.3 from  "Constraint   Answer  Set Programming without
%   Grounding" by Joaquin Arias et all.
%   @tbd JW: it is not clear to me why OtherVars is needed and why it is
%   not copied.

find_duals(C_Vars, C_Vars1, OtherVars, Duals) :-
    % disequality and clp for numbers
    dump_constraint(C_Vars, C_Vars1, Dump, []-[], Pending-Pending1), !,
    clp_vars_in(OtherVars, OtherCLPVars),		% clp(Q) vars
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

dual(#=(A,B), [#<(A,B), #>(A,B)]).
dual(#<(A,B), [#>=(A,B)]).
dual(#>(A,B), [#=<(A,B)]).
dual(#=<(A,B), [#>(A,B)]).
dual(#>=(A,B), [#<(A,B)]).

dual(=(A,B), [\=(A,B)]).
dual(\=(A,B), [=(A,B)]).


%!  dump_constraint(+C_Vars, +C_Vars1, -Dump, +Pending0, -Pending) is det
%
%   @arg Dump is a list of V1=B and V1\=B, where V1 is a variable from
%   C_Vars1.
%   @arg Pending is a pair of lists with variables from C_Vars and
%   C_Vars1 that are not processed (in reverse order, why?)

:- det(dump_constraint/5).
dump_constraint([], [], [], Pending, Pending).
dump_constraint([V|Vs], [V1|V1s], [V1 = V | Vs_Dump], P0, P1) :-
    ground(V), !,
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

:- if(current_predicate(copy_term_nat/4)).

my_copy_term(Var0, Term0, Var, Term) :-
    copy_term_nat(Var0, Term0, Var, Term).

my_copy_vars(Vars0, Term0, Vars, Term) :-
    copy_term_nat(Vars0, Term0, Vars, Term).

:- else.

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

:- endif.

%!  my_diff_term(+Term, +Vars, -Others) is det.
%
%   Others are variables in Term that do not appear in Vars.

my_diff_term(Term, Vars, Others) :-
    term_variables(Term, Set),
    diff_vars(Set, Vars, Others).

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

member_stack(Goal, Stack) :- member(goal_origin(Goal, _), Stack).
member_stack(Goal, Stack) :- member(Goal, Stack).
