:- module(scasp, [
    scasp_test/2,
    main/1,
    load/1,
    run_defined_query/0,
    '?'/1,
    '??'/1,
    solve/4,
    check_goal/4,
    solve_goal/4,
    solve_goal_forall/4,
    solve_goal_table_predicate/4,
    solve_goal_predicate/4,
    solve_goal_builtin/4,
    check_CHS/3,
    predicate/1,
    table_predicate/1,
    my_copy_term/4,
    clear_flags/0,
    check_calls/0,
    pos_loops/0,
    print_on/0
                ]).

%% ------------------------------------------------------------- %%
:- use_package(assertions).
:- doc(title, "Meta interpreter under stable model semantics").
:- doc(author, "Joaquin Arias").
:- doc(filetype, module).

:- doc(module, "

This module contains the main functionality of @apl{scasp}.

@pred{load/1} is the predicate used to load the constraint logic
program.

@pred{solve/4} is the predicate used to evaluate the query of a
program under the stable model semantic.

").

%% ------------------------------------------------------------- %%
:- use_module(scasp_io).
:- reexport(scasp_io, [
    pr_rule/2,
    pr_query/1,
    pr_user_predicate/1,
    pr_table_predicate/1,
    pr_show_predicate/1,
    pr_pred_predicate/1,
    set/2,
    write_program/0
                     ]).

:- use_module(clp_call_stack).
:- op(700, xfx, ['~>', '<~']).
:- reexport(clp_call_stack, [
    '~>'/2,
    '<~'/2
                        ]).

:- use_module(clp_disequality_rt).
:- op(700, xfx, [(.\=.),(.=.)]).

:- use_module(clp_clpq).

:- op(700, fx,  [not,(?=), (??), (?)]).

%% ------------------------------------------------------------- %%
:- doc(section, "Main predicates").

%% :- use_package(tabling).  
%% :- active_tclp.
%% :- table solve_goal_table_predicate/4.% , solve_goal_predicate/4.

:- pred load(Files) : list(Files) #"Loads a list of files".

load(X) :-
%       abolish_all_tables,
    load_program(X),
    true.

:- pred scasp_test(Args,Result) : list(Args) #"Used to test s(CASP) from
test.pl module".

scasp_test(Args,Results) :-
    parse_args(Args, Options, Sources),
    set_options(Options),
    load(Sources),
    defined_query(Q),
    process_query(Q,_,Query),
    findall(
        Result,
        ( solve(Query, [], StackOut, _Model),
          pretty_term([],_,StackOut, Result)
        ),
        Results).


:- pred main(Args) : list(Args) #"Used when calling from command line
by passing the command line options and the input files".

main(Args) :-
    print_on,
    retractall(current_option(_,_)),
    retractall(counter(_,_)),
    parse_args(Args, Options, Sources),
    set_options(Options),
    load(Sources),
    if_user_option(write_program, ( write_program, halt )),
    (
        current_option(interactive, on) ->
        main_loop
    ;
        defined_query(Q),
        main_solve(Q)
    ).
main(_).

main_loop :-
    print('\n?- '),
    catch(read(R),_,fail_main_loop),
    conj_to_list(R,Q),
    (
        member(exit, Q) ->
        halt
    ;
        (
            main_solve(Q) ->
            nl, main_loop
        ;
            % print('\nfalse'),
            main_loop
        )
    ).

fail_main_loop :-
    print('\nno'),
    main_loop.

:- use_module(library(terms_vars)).
:- use_module(library(formulae)).
main_solve(Q0) :-
    current_option(answers,Number),
    init_counter,

    process_query(Q0,Q,Query), varset(Q,Vars),

    pretty_term([],D1,par(Vars,Q),par(PVars,PQ)),

    print_query(PQ),

    statistics(runtime,_),
    if(solve(Query, [], StackOut, Model),nl,(print('\nno models\n\n'),fail)),
    statistics(runtime, [_|[T]]),

    increase_counter,
    answer_counter(Counter),
    format('\tANSWER:\t~w (in ~w ms)\n',[Counter,T]),

    pretty_term(D1,D2,par(Q,Vars,Model),par(PAnswer,Bindings,P_Model)),

    if_user_option(process_stack,(
        reverse(StackOut, Reverse_StackOut),
        pretty_term(D2,_D3,Reverse_StackOut,P_StackOut)
    )),
    
    if_user_option(html,print_html([PQ,PAnswer,Bindings,PVars],P_Model,P_StackOut)),
    if_user_option(print_tree,print_justification_tree(P_StackOut)),
    print_model(P_Model),nl,
    
    print_unifier(Bindings,PVars),

    (
        Number = -1,
        allways_ask_for_more_models,nl,nl
    ;
        Number = 0,
        nl,nl,
        statistics(runtime,_),
        fail
    ;
        Number > 0,
        nl,nl,
        statistics(runtime,_),
        Counter = Number
    ).
    

:- pred run_defined_query #"Used from the interactive mode to run
the defined query".

:- use_module(library(write)).
:- use_module(library(terms_check)).

run_defined_query :-
    defined_query(A),
    solve_query(A),
    print(A),
    allways_ask_for_more_models,nl,nl.

defined_query(_) :-
    pr_query([not(o_false)]), !,
    format('\nQuery not defined\n',[]),
    fail.
defined_query(Q) :-
    pr_query(Q).

%% ------------------------------------------------------------- %%
:- doc(section, "Top Level Predicates").

:- pred check_calls/0 #"Turn on the flag @var{check_calls}".
:- pred pos_loops/0 #"Turn on the flag @var{pos_loops}".
:- pred print_on/0 #"Turn on the flag @var{print}".
:- pred print_tree_on/0 #"Turn on the flag @var{print_tree}".

clear_flags :-
    set(check_calls,off),
    set(pos_loops,off),
    set(print,off),
    set(print_tree,off).
check_calls :-  set(check_calls,on).
pos_loops :-    set(pos_loops,on).
print_on :-     set(print,on).
print_tree_on :- set(print_tree,on).

:- pred ??(Query) : list(Query) #"Shorcut predicate to ask queries in
the top-level returning also the justification tree. It calls solve_query/1".
:- pred ?(Query) : list(Query) #"Shorcut predicate to ask queries in
the top-level. It calls solve_query/1".

?? Q :-
    set(print,on),
    solve_query(Q).

? Q :-
    set(print,off),
    solve_query(Q).

solve_query(Q) :-
    init_counter,

    process_query(Q,_,Query),

    statistics(runtime,_),
    solve(Query, [], StackOut, Model),
    statistics(runtime, [_|T]),

    increase_counter,
    answer_counter(Counter),
    format('\nAnswer ~w\t(in ~w ms):',[Counter,T]),nl,

    reverse(StackOut, Reverse_StackOut),
    if_user_option(print_tree,print_justification_tree(Reverse_StackOut)),
    print_model(Model),nl,nl,

    ask_for_more_models.

%% ------------------------------------------------------------- %%
:- doc(section, "Predicates to solve the query").

:- pred solve(Goals, StackIn, StackOut, Model) #"Solve the list of
sub-goals @var{Goal} where @var{StackIn} is the list of goals already
visited and returns in @var{StackOut} the list of goals visited to
prove the sub-goals and in @var{Model} the model with support the
sub-goals".

solve([], StackIn, [[]|StackIn], []).
solve([Goal|Goals], StackIn, StackOut, Model) :-
    if_user_option(check_calls, print_check_calls_calling(Goal,StackIn)),
    check_goal(Goal, StackIn, StackMid, Modelx), Modelx = [AddGoal|JGoal],
    if_user_option(check_calls, format('Succes  ~p\n',[Goal])),
    solve(Goals, StackMid, StackOut, Modelxs), Modelxs = JGoals,
    (
        shown_predicate(Goal) ->
        Model = [AddGoal, JGoal|JGoals]
    ;
%           Model = [AddGoal, JGoal|JGoals]
        Model = [JGoal|JGoals]
    ).

:- pred check_goal(Goal, StackIn, StackOut, Model) #"Call
@pred{check_CHS/3} to check the sub-goal @var{Goal} against the list
of goals already visited @var{StackIn} to determine if it is a
coinductive success, a coinductive failure, an already proved
sub-goal, or if it has to be evaluated".

check_goal(Goal, StackIn, StackOut, Model) :-
    check_CHS(Goal, StackIn, Check),  %% Check condition for coinductive success
    check_goal_(Check, Goal, StackIn, StackOut, Model).

%% coinduction success <- cycles containing even loops may succeed
check_goal_(co_success, Goal, StackIn, StackOut, Model) :-
    StackOut = [[],chs(Goal)|StackIn],
    JGoal = [],
    AddGoal = chs(Goal),
    Model = [AddGoal|JGoal].
%% already proved in the stack
check_goal_(proved, Goal, StackIn, StackOut, Model) :-
    StackOut = [[],proved(Goal)|StackIn],
    JGoal = [],
    AddGoal = proved(Goal),
    Model = [AddGoal|JGoal].
%% coinduction does neither success nor fails <- the execution continues inductively
check_goal_(cont, Goal, StackIn, StackOut, Model) :-
    solve_goal(Goal, StackIn, StackOut, Modelx), Modelx = [Goal|JGoal],
    AddGoal = Goal,
    Model = [AddGoal|JGoal].
%% coinduction fails <- the negation of a call unifies with a call in the call stack
check_goal_(co_failure, _Goal, _StackIn, _StackOut, _Model) :-
    fail.

:- pred solve_goal(Goal, StackIn, StackOut, GoalModel) #"Solve a
simple sub-goal @var{Goal} where @var{StackIn} is the list of goals
already visited and returns in @var{StackOut} the list of goals
visited to prove the sub-goals and in @var{Model} the model with
support the sub-goals".

solve_goal(Goal, StackIn, StackOut, GoalModel) :-
    Goal = forall(_,_),
    solve_goal_forall(Goal, [Goal|StackIn], StackOut, Model),
    GoalModel = [Goal|Model].
solve_goal(Goal, StackIn, [[],Goal|StackIn], GoalModel) :-
    Goal = not(is(V,Expresion)), 
    NV is Expresion,
    V .\=. NV,
    GoalModel = [Goal].
solve_goal(Goal, StackIn, StackOut, Model) :-
    Goal \= [], Goal \= [_|_], Goal \= forall(_, _), Goal \= not(is(_,_)),Goal \= builtin(_),
    table_predicate(Goal),
    if_user_option(check_calls, format('Solve the tabled goal ~p\n',[Goal])),
    AttStackIn <~ stack([Goal|StackIn]),
    solve_goal_table_predicate(Goal, AttStackIn, AttStackOut, AttModel),
    AttStackOut ~> stack(StackOut),
    AttModel ~> model(Model).
solve_goal(Goal, StackIn, StackOut, Model) :-
    Goal \= [], Goal \= [_|_], Goal \= forall(_, _), Goal \= not(is(_,_)),Goal \= builtin(_),
    \+ table_predicate(Goal),
    predicate(Goal), 
    solve_goal_predicate(Goal, [Goal|StackIn], StackOut, Model).
solve_goal(Goal, StackIn, [[],Goal|StackOut], Model) :-
    Goal \= [], Goal \= [_|_], Goal \= forall(_, _), Goal \= not(is(_,_)), \+ predicate(Goal),
    \+ table_predicate(Goal),
    solve_goal_builtin(Goal, StackIn, StackOut, Model).

:- pred solve_goal_forall(forall(Var,Goal), StackIn, StackOut,
GoalModel) #"Solve a sub-goal of the form @var{forall(Var,Goal)} and
success if @var{Var} success in all its domain for the goal
@var{Goal}. It calls @pred{solve/4}".

solve_goal_forall(forall(Var, Goal), StackIn, [[]|StackOut], Model) :-
    my_copy_term(Var,Goal,NewVar,NewGoal),
    my_copy_term(Var,Goal,NewVar2,NewGoal2),
    solve([NewGoal], StackIn, [[]|StackMid], ModelMid),
    if_user_option(check_calls, format('\tSuccess solve ~p\n\t\t for the ~p\n',[NewGoal,forall(Var,Goal)])),
    check_unbound(NewVar, List),
    (
        List == ground ->
        if_user_option(check_calls, format('The var ~p is grounded so try with other clause\n', [NewVar])),
        fail
    ;
        List == [] ->
        StackOut = StackMid,
        Model = ModelMid
    ;
        List = 'clpq'(NewVar3,Constraints) ->
        findall('dual'(NewVar3,ConDual), dual_clpq(Constraints, ConDual), DualList),
        %       dual_clpq(Constraints, ConDual),
        if_user_option(check_calls, format('Executing ~p with clpq ConstraintList = ~p\n', [Goal, DualList])),
        exec_with_clpq_constraints(NewVar2, NewGoal2, 'entry'(NewVar3,[]), DualList, StackMid, StackOut, ModelList), !,
        append(ModelMid, ModelList, Model)
    ;
        !,
        if_user_option(check_calls, format('Executing ~p with clp_disequality list = ~p\n', [Goal, List])),
        exec_with_neg_list(NewVar2, NewGoal2, List, StackMid, StackOut, ModelList), 
        append(ModelMid, ModelList, Model)
    ).

check_unbound(Var, ground) :-
    ground(Var), !.
check_unbound(Var, List) :-
    get_neg_var(Var, List), !.
check_unbound(Var, 'clpq'(NewVar,Constraints)) :-
    dump_clpq_var([Var],[NewVar],Constraints),
    Constraints \== [], !.
check_unbound(Var, []) :-
    var(Var), !.

exec_with_clpq_constraints(_, _, _, [], StackIn, StackIn, []).
exec_with_clpq_constraints(Var, Goal, 'entry'(ConVar, ConEntry),['dual'(ConVar, ConDual)|Duals], StackIn, StackOut, Model) :-
    my_copy_term(Var, [Goal, StackIn], Var01, [Goal01,StackIn01]),
    append(ConEntry, ConDual, Con),
    my_copy_term(ConVar, Con, ConVar01, Con01),
    my_copy_term(Var, Goal, Var02, Goal02),
    my_copy_term(ConVar, ConEntry, ConVar02, ConEntry02),
    Var01 = ConVar,
    (
        apply_clpq_constraints(Con) ->
        if_user_option(check_calls, format('Executing ~p with clpq_constrains ~p\n',[Goal01, Con])),
        solve([Goal01], StackIn01, [[]|StackOut01], Model01),
        if_user_option(check_calls, format('Success executing ~p with constrains ~p\n',[Goal01, Con])),
        if_user_option(check_calls, format('Check entails Var = ~p with const ~p and ~p\n',[Var01, ConVar01, Con01])),
        (
            entails([Var01], ([ConVar01], Con01)) ->
            if_user_option(check_calls, format('\tOK\n',[])),
            StackOut02 = StackOut01,
            Model03 = Model01
        ;
            if_user_option(check_calls, format('\tFail\n',[])),
            dump_clpq_var([Var01], [ConVar01], ExitCon),
            findall('dual'(ConVar01, ConDual01), dual_clpq(ExitCon, ConDual01), DualList),
            %               dual_clpq(ExitCon, ConDual01),
            if_user_option(check_calls, format('Executing ~p with clpq ConstraintList = ~p\n', [Goal, DualList])),
            exec_with_clpq_constraints(Var, Goal, 'entry'(ConVar01, Con01), DualList, StackOut01, StackOut02, Model02),
            append(Model01, Model02, Model03)
        )
    ;
        if_user_option(check_calls, format('Skip execution of an already checked constraint ~p (it is inconsitent with ~p)\n',[ConDual, ConEntry])),
        StackOut02 = StackIn01,
        Model03 = []
    ),
    if_user_option(check_calls, format('Executing ~p with clpq ConstraintList = ~p\n', [Goal, Duals])),
    exec_with_clpq_constraints(Var02, Goal02, 'entry'(ConVar02, ConEntry02), Duals, StackOut02, StackOut, Model04),
    append(Model03, Model04, Model).

exec_with_neg_list(_,   _,    [],         StackIn, StackIn, []).
exec_with_neg_list(Var, Goal, [Value|Vs], StackIn, StackOut, Model) :-
    my_copy_term(Var, [Goal,StackIn], NewVar, [NewGoal,NewStackIn]),
    NewVar = Value,
    if_user_option(check_calls, format('Executing ~p with value ~p\n',[NewGoal,Value])),
    solve([NewGoal], NewStackIn, [[]|NewStackMid], ModelMid),
    if_user_option(check_calls, format('Success executing ~p with value ~p\n',[NewGoal,Value])),
    exec_with_neg_list(Var, Goal, Vs, NewStackMid, StackOut, Models),
    append(ModelMid,Models,Model).

:- pred solve_goal_table_predicate(Goal, AttStackIn, AttStackOut,
AttModel) #"Used to evaluate predicates under tabling. This predicates
should be defined in the program using the directive @em{#table
pred/n.}".

%% TABLED to avoid loops and repeated answers
solve_goal_table_predicate(Goal, AttStackIn, AttStackOut, AttModel) :-
    pr_rule(Goal, Body),
    AttStackIn ~> stack(StackIn),
    solve(Body, StackIn, StackOut, Model),
    AttStackOut <~ stack(StackOut),
    AttModel <~ model([Goal|Model]).
%% TABLED to avoid loops and repeated answers

:- pred solve_goal_predicate(Goal, StackIn, StackOut, GoalModel)
#"Used to evaluate a user predicate".

solve_goal_predicate(Goal, StackIn, StackOut, GoalModel) :-
    pr_rule(Goal, Body),
    solve(Body, StackIn, StackOut, BodyModel),
    GoalModel = [Goal|BodyModel].

:- pred solve_goal_builtin(Goal, StackIn, StackOut, AttModel) #"Used
to evaluate builtin predicates predicate".

solve_goal_builtin(is(X,Exp), StackIn, StackIn, Model) :- 
    capture_rational(Exp,CaptExp), !, %% If it fails later the call(Goal) will also fail...
    exec_goal(is(X,CaptExp)),
    Model = [is(X,Exp)].  %% the Model should 'Start' with the Goal
solve_goal_builtin(builtin(Goal), StackIn, StackIn, Model) :- !,
    exec_goal(Goal),
    Model = [builtin(Goal)].
solve_goal_builtin(Goal, StackIn, StackIn, Model) :-
    Goal =.. [Op|_],
    clp_builtin(Op), !,
    exec_goal(apply_clpq_constraints(Goal)),
    Model = [Goal].
solve_goal_builtin(Goal, StackIn, StackIn, Model) :-
    Goal =.. [Op|Operands],
    clp_builtin_translate(Op,Op_T), !,
    Goal_T =.. [Op_T|Operands],
    exec_goal(apply_clpq_constraints(Goal_T)),
    Model = [Goal].
solve_goal_builtin(Goal, StackIn, StackIn, Model) :-
    Goal =.. [Op|_],
    prolog_builtin(Op), !,
    exec_goal(Goal),
    Model = [Goal].
%% The predicate is not defined as user_predicates neither builtin
solve_goal_builtin(Goal, StackIn, StackIn, Model) :-
    if_user_option(check_calls,
    format('The predicate ~p is not user_defined / builtin\n',[Goal])),
    (
        Goal = not(_) ->
        Model = [Goal]     %% the negation of a not defined predicate success.
    ;
        fail               %% a not defined predicate allways fails.
    ).

exec_goal(A \= B) :- !,
    if_user_option(check_calls, format('exec ~p \\= ~p\n',[A,B])),
    .\=.(A, B),
    if_user_option(check_calls, format('ok   ~p \\= ~p\n', [A,B])).
exec_goal(Goal) :-
    if_user_option(check_calls, format('exec goal ~p \n',[Goal])),
    catch(call(Goal),_,fail),
    if_user_option(check_calls, format('ok   goal ~p \n', [Goal])).

capture_rational(G, A/B) :- ground(G), G=rat(A,B),!.
capture_rational(St, NSt) :-
    St =.. [Op,A,B], !,
    capture_rational(A,Na),
    capture_rational(B,Nb),
    NSt =.. [Op,Na,Nb].
capture_rational(A, A) :- ground(A).

:- pred check_CHS(Goal, StackIn, Result) #"Checks the @var{StackIn}
and returns in @var{Result} if the goal @var{Goal} is a coinductive
success, a coinductive failure or an already proved goal. Otherwise it
is constraint against its negation atoms already visited".

%% inmediate success if the goal has already been proved.
check_CHS(Goal, I, proved) :-
    predicate(Goal),
    ground(Goal),
    \+ \+ proved_in_stack(Goal, I), !.
%% coinduction success <- cycles containing even loops may succeed
check_CHS(Goal, I, co_success) :-
    predicate(Goal),
    type_loop(Goal, I, even), !.
%% coinduction fails <- the goal is entailed by its negation in the
%% call stack
check_CHS(Goal, I, co_failure) :-
    predicate(Goal),
    \+ \+ neg_in_stack(Goal, I), !,
    if_user_option(check_calls, format('Negation of the goal in the stack, failling (Goal = ~w)\n',[Goal])).
%% coinduction fails <- cycles containing positive loops can be solve
%% using tabling
check_CHS(Goal, I, co_failure) :-
    predicate(Goal),
    \+ table_predicate(Goal),
    if_user_option(no_fail_loop, fail),
    \+ \+ (
              type_loop(Goal, I, fail_pos(S)),
              if_user_option(check_calls, format('Positive loop, failling (Goal == ~w)\n',[Goal])),
              if_user_option(pos_loops, format('\nWarning: positive loop failling (Goal ~w == ~w)\n',[Goal,S]))
          ), !.
check_CHS(Goal, I, _cont) :-
    predicate(Goal),
    \+ table_predicate(Goal),
    \+ \+ (
              type_loop(Goal, I, pos(S)),
              if_user_option(check_calls, format('Positive loop, continuing (Goal = ~w)\n',[Goal])),
              if_user_option(pos_loops, format('\nNote: positive loop continuing (Goal ~w = ~w)\n',[Goal,S]))
          ), fail.
%% coinduction does not success or fails <- the execution continues
%% inductively
check_CHS(Goal, I, cont) :-
    if(
          (
              predicate(Goal),
              \+ ground(Goal),    %% the current goal is restricted
              ground_neg_in_stack(Goal, I)
          ),
          true,true
      ),
    if(
          (
              predicate(Goal),
              ground(Goal),   %% the current goal restrict previous results
              constrained_neg_in_stack(Goal, I)
          ),
          true,true
      ).

%% check if the negation is in the stack -> coinductive failure
%% extended to check if the negation in the stack entails the current call -> failure
neg_in_stack(not(Goal), [NegGoal|_]) :-
    Goal == NegGoal, !.
neg_in_stack(Goal, [not(NegGoal)|_]) :-
    Goal == NegGoal, !.
neg_in_stack(not(Goal), [NegGoal|_]) :-
    variant(Goal, NegGoal),
    instance(Goal, NegGoal),
    instance(NegGoal, Goal), !,
    if_user_option(warning,format("\nWARNING: Co-Failing in a negated loop due to a variant call (extension clp-disequality required).\n\tCurrent call:\t~p\n\tPrevious call:\t~p\n",[Goal,NegGoal])).
neg_in_stack(Goal, [not(NegGoal)|_]) :-
    variant(Goal, NegGoal),
    instance(Goal, NegGoal),
    instance(NegGoal, Goal), !,
    if_user_option(warning,format("\nWARNING: Co-Failing in a negated loop due to a variant call (extension clp-disequality required).\n\tCurrent call:\t~p\n\tPrevious call:\t~p\n",[Goal,NegGoal])).
%% neg_in_stack(not(Goal), [NegGoal|_]) :-
%%       Goal =.. [Name|ArgGoal],
%%       NegGoal =.. [Name|NegArgGoal],
%%       if_user_option(check_calls, format('\t\tCheck if not(~p) entails (is more particular than) ~p\n',[Goal,NegGoal])),
%%       entail_list(ArgGoal, NegArgGoal), !,
%%       if_user_option(check_calls, format('\t\tOK\n',[])).
% neg_in_stack(Goal, [not(NegGoal)|_]) :-
%       Goal =.. [Name|ArgGoal],
%       NegGoal =.. [Name|NegArgGoal],
%       if_user_option(check_calls, format('\t\tCheck if ~p entails (is more particular than) not(~p)\n',[Goal,NegGoal])),
%       entail_list(ArgGoal, NegArgGoal), !,
%       if_user_option(check_calls, format('\t\tOK\n',[])).
neg_in_stack(Goal, [_|Ss]) :-
    neg_in_stack(Goal, Ss).

%% ground_neg_in_stack
ground_neg_in_stack(Goal, S) :-
    if_user_option(check_calls, format('Enter ground_neg_in_stack for ~p\n',[Goal])),
    ground_neg_in_stack_(Goal, S, 0, 0, Flag),
    Flag == found,
%       ( Flag == found_dis ; Flag == found_clpq ),
    if_user_option(check_calls, format('\tThere exit the negation of ~p\n\n',[Goal])).
    
ground_neg_in_stack_(_,[],_,_, _Flag) :- !.
ground_neg_in_stack_(Goal, [[]|Ss], Intervening, MaxInter, Flag) :- !,
    NewInter is Intervening - 1,
    ground_neg_in_stack_(Goal, Ss, NewInter, MaxInter, Flag).
ground_neg_in_stack_(Goal, [chs(not(NegGoal))|Ss], Intervening, MaxInter, found) :-
%ground_neg_in_stack_(Goal, [not(NegGoal)|Ss], Intervening, MaxInter, found) :-
    Intervening =< MaxInter,
    Goal =.. [Name|ArgGoal],
    NegGoal =.. [Name|ArgNegGoal], 
    if_user_option(check_calls, format('\t\tCheck disequality of ~p and ~p\n',[Goal,chs(not(NegGoal))])),
    %               if_user_option(check_calls, format('\t\tCheck disequality of ~p and ~p\n',[Goal,not(NegGoal)])),
    \+ \+ Goal = NegGoal,
    loop_list(ArgGoal, ArgNegGoal), !,
    max(MaxInter, Intervening, NewMaxInter),
    NewInter is Intervening + 1,
    ground_neg_in_stack_(Goal, Ss, NewInter, NewMaxInter, found).
%ground_neg_in_stack_(not(Goal), [chs(NegGoal)|Ss], Intervening, MaxInter, found) :-
ground_neg_in_stack_(not(Goal), [chs(NegGoal)|Ss], Intervening, MaxInter, found) :-
    Intervening =< MaxInter,
    Goal =.. [Name|ArgGoal],
    NegGoal =.. [Name|ArgNegGoal], 
    if_user_option(check_calls, format('\t\tCheck disequality of ~p and ~p\n',[not(Goal),chs(NegGoal)])),
    %               if_user_option(check_calls, format('\t\tCheck disequality of ~p and ~p\n',[not(Goal),NegGoal])),
    \+ \+ Goal = NegGoal,
    loop_list(ArgGoal, ArgNegGoal), !,
    max(MaxInter, Intervening, NewMaxInter),
    NewInter is Intervening + 1,
    ground_neg_in_stack_(not(Goal), Ss, NewInter, NewMaxInter, found).
ground_neg_in_stack_(not(Goal), [NegGoal|Ss], Intervening, MaxInter, found) :-
    Intervening =< MaxInter,
    Goal =.. [Name|ArgGoal],
    NegGoal =.. [Name|ArgNegGoal], 
    if_user_option(check_calls, format('\t\tCheck disequality of ~p and ~p\n',[not(Goal),NegGoal])),
    %               if_user_option(check_calls, format('\t\tCheck disequality of ~p and ~p\n',[not(Goal),NegGoal])),
    \+ \+ Goal = NegGoal,
    loop_list(ArgGoal, ArgNegGoal), !,
    max(MaxInter, Intervening, NewMaxInter),
    NewInter is Intervening + 1,
    ground_neg_in_stack_(not(Goal), Ss, NewInter, NewMaxInter, found).
ground_neg_in_stack_(Goal, [_|Ss], Intervening, MaxInter, Flag) :- !,
    max(MaxInter, Intervening, NewMaxInter),
    NewInter is Intervening + 1,    
    ground_neg_in_stack_(Goal, Ss, NewInter, NewMaxInter, Flag).


% restrict even more the constrained in the stack
constrained_neg_in_stack(_,[]).
constrained_neg_in_stack(not(Goal), [NegGoal|Ss]) :-
    Goal =.. [Name|ArgGoal],
    NegGoal =.. [Name|NegArgGoal],
    if_user_option(check_calls, format('\t\tCheck if not(~p) is consistent with ~p\n',[Goal,NegGoal])), !,
    loop_list(ArgGoal, NegArgGoal), !,
    if_user_option(check_calls, format('\t\tOK\n',[])),
    constrained_neg_in_stack(not(Goal), Ss).
constrained_neg_in_stack(Goal, [not(NegGoal)|Ss]) :-
      Goal =.. [Name|ArgGoal],
      NegGoal =.. [Name|NegArgGoal],
    if_user_option(check_calls, format('\t\tCheck if not(~p) is consistent with ~p\n',[Goal,NegGoal])), !,
    loop_list(ArgGoal, NegArgGoal), !,
    if_user_option(check_calls, format('\t\tOK\n',[])),
    constrained_neg_in_stack(Goal, Ss).
constrained_neg_in_stack(Goal, [_|Ss]) :-
    constrained_neg_in_stack(Goal, Ss).



%% proved_in_stack
proved_in_stack(Goal, S) :-
    proved_in_stack_(Goal, S, 0, -1),
    if_user_option(check_calls, format('\tGoal ~p is already in the stack\n',[Goal])).
proved_in_stack_(Goal, [[]|Ss], Intervening, MaxInter) :- 
    NewInter is Intervening - 1,
    proved_in_stack_(Goal, Ss, NewInter, MaxInter).
proved_in_stack_(Goal, [S|_], Intervening, MaxInter) :-
    S \= [],
    Goal == S, !,
    Intervening =< MaxInter.
proved_in_stack_(Goal, [chs(S)|_], Intervening, MaxInter) :-
    Goal == S, 
    Intervening =< MaxInter.
proved_in_stack_(Goal, [S|Ss], Intervening, MaxInter) :-
    S \= [],
    max(MaxInter, Intervening, NewMaxInter),
    NewInter is Intervening + 1,
    proved_in_stack_(Goal, Ss, NewInter, NewMaxInter).

% %% neg_proved_in_stack
% neg_proved_in_stack(Goal, S) :-
%       neg_proved_in_stack_(Goal, S, 0, -1),
%       if_user_option(check_calls, format('\tThe negation of ~p is already in the stack\n',[Goal])).
% neg_proved_in_stack_(Goal, [[]|Ss], Intervening, MaxInter) :- 
%       NewInter is Intervening - 1,
%       neg_proved_in_stack_(Goal, Ss, NewInter, MaxInter).
% neg_proved_in_stack_(Goal, [not(S)|_], Intervening, MaxInter) :-
%       S \= [],
%       Goal == S, !,
%       Intervening =< MaxInter.
% neg_proved_in_stack_(not(Goal), [S|_], Intervening, MaxInter) :-
%       S \= [],
%       Goal == S, !,
%       Intervening =< MaxInter.
% neg_proved_in_stack_(Goal, [S|Ss], Intervening, MaxInter) :-
%       S \= [],
%       max(MaxInter, Intervening, NewMaxInter),
%       NewInter is Intervening + 1,
%       neg_proved_in_stack_(Goal, Ss, NewInter, NewMaxInter).

max(A,B,A) :-
    A >= B.
max(A,B,B) :-
    A < B.

%% check if it is a even loop -> coinductive success
type_loop(Goal, Stack, Type) :-
    Goal \= not(_),
    Intervening = 0,
    NumberNegation = 0,
    type_loop_(Goal, Intervening, NumberNegation, Stack, Type).
type_loop(not(Goal), Stack, Type) :-
    Intervening = 0,
    NumberNegation = 1,
    type_loop_(not(Goal), Intervening, NumberNegation, Stack, Type).

type_loop_(Goal, Iv, N, [[]|Ss], Type) :- !,
    NewIv is Iv - 1,
    type_loop_(Goal, NewIv, N, Ss, Type).
type_loop_(Goal, Iv, N, [_S|Ss], Type) :-
    Iv < 0,
    NewIv is Iv + 1,
    type_loop_(Goal, NewIv, N, Ss, Type).

type_loop_(Goal, 0, 0, [S|_],fail_pos(S)) :-  \+ \+ Goal == S.
type_loop_(Goal, 0, 0, [S|_],fail_pos(S)) :-  \+ \+ variant(Goal, S), if_user_option(warning,format("\nWARNING: Failing in a positive loop due to a variant call (tabling required).\n\tCurrent call:\t~p\n\tPrevious call:\t~p\n",[Goal,S])).
%type_loop_(Goal, 0, 0, [S|_],fail_pos(S)) :-  \+ \+ Goal = S.
type_loop_(Goal, 0, 0, [S|_],pos(S)) :-  \+ \+ Goal = S.

% avoid loops due to repeated negated goal... this is not the right solution ->
% It should be solved using tabling !!
%% type_loop_(not(Goal), 0, N, [not(S)|_],fail_pos(S)) :- Goal == S, N > 0, 0 is mod(N, 2).

type_loop_(not(Goal), 0, _N, [not(S)|_],even) :- \+ \+ Goal == S. %% redundant, for debugging proposals
type_loop_(not(Goal), 0, _N, [not(S)|_],even) :- variant(Goal,S), Goal = S. 
type_loop_(Goal, 0, N, [S|_],even) :- Goal \= not(_), Goal == S, N > 0, 0 is mod(N, 2).

type_loop_(Goal, 0, N, [S|Ss],Type) :-
    Goal \== S,
    S = not(_),
    NewN is N + 1,
    type_loop_(Goal, 0, NewN, Ss,Type).
type_loop_(Goal, 0, N, [S|Ss], Type) :-
    Goal \== S,
    S \= not(_),
    type_loop_(Goal, 0, N, Ss,Type).

%% ------------------------------------------------------------- %%
:- doc(section, "Auxiliar Predicates").

:- pred predicate(Goal) #"Success if @var{Goal} is a user
predicate".

%% Check if the goal Goal is a user defined predicate
predicate(builtin(_)) :- !, fail.
predicate(not(_ is _)) :- !, fail.
predicate(not(_)) :- !.
predicate(Goal) :-
    Goal =.. [Name|Args],
    length(Args,La),
    pr_user_predicate(Name/La), !.

%% predicate(-_Goal) :- !. %% NOTE that -goal is translated as '-goal' 

:- pred table_predicate(Goal) #"Success if @var{Goal} is defined as
a tabled predicate with the directive @em{table pred/n.}".


table_predicate(Goal) :-
    Goal =.. [Name|Args],
    length(Args,La),
    pr_table_predicate(Name/La).
%% table_predicate(not(Goal)) :-
%%     Goal =.. [Name|Args],
%%     length(Args,La),
%%     pr_table_predicate(Name/La).

shown_predicate(not(Goal)) :-
    predicate(Goal).
shown_predicate(Goal) :-
    Goal \= not(_),
    predicate(Goal).


:- pred prolog_builtin(Goal) #"Success if @var{Goal} is a builtin
    prolog predicate".

prolog_builtin(true).
prolog_builtin(fail).
prolog_builtin(=).
prolog_builtin(\=).
prolog_builtin(<).
prolog_builtin(>).
prolog_builtin(>=).
prolog_builtin(=<).


:- pred clp_builtin(Goal) #"Success if @var{Goal} is a builtin
    constraint predicate".

clp_builtin(.=.).
clp_builtin(.<>.).
clp_builtin(.<.).
clp_builtin(.>.).
clp_builtin(.>=.).
clp_builtin(.=<.).

:- pred clp_builtin_translate(Goal,Goal_T) #"Translate s(CASP)
    constraints into CLP(Q/R) syntax".

clp_builtin_translate('#=',  .=.).
clp_builtin_translate('#<>', .<>.).
clp_builtin_translate('#<',  .<.).
clp_builtin_translate('#>',  .>.).
clp_builtin_translate('#>=', .>=.).
clp_builtin_translate('#=<', .=<.).


:- pred my_copy_term(Var, Term, NewVar, NewTerm) #"Its behaviour is
similar to @pred{copy_term/2}. It returns in @var{NewTerm} a copy of
the term @var{Term} but it only replaces with a fresh variable
@var{NewVar} the occurrences of @var{Var}".

%! my_copy_term(Var, Term, NewVar, NewTerm)
my_copy_term(Var, V, NewVar, NewVar) :- var(V), V == Var, !.
my_copy_term(Var, V, _,V) :- var(V), V \== Var, !.
my_copy_term(_, G, _,G) :- ground(G), !.
my_copy_term(Var, Struct, NewVar, NewStruct) :-
    Struct =.. [Name | Args], !,
    my_copy_list(Var, Args, NewVar, NewArgs),
    NewStruct =.. [Name | NewArgs].

my_copy_list(_,[],_,[]).
my_copy_list(Var,[T|Ts],NewVar,[NewT|NewTs]) :-
    my_copy_term(Var, T, NewVar,NewT),
    my_copy_list(Var, Ts, NewVar,NewTs).


