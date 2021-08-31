:- module(scasp,
          [ scasp_load/1,              % :FileOrFiles
            (?)/1,                     % :Query
            (??)/1,                    % :Query
            clear_flags/0,
            check_calls/0,
            pos_loops/0,
            print_on/0,
            print_tree_on/0,
            run_defined_query/0,

            op(700, fx, not),
            op(700, fx, ??),
            op(700, fx, ?)
          ]).
:- set_prolog_flag(optimise, true).

:- use_module(scasp/solve).
:- use_module(scasp/io).
:- use_module(scasp/compile, [scasp_load/1]). % re-exported
:- use_module(scasp/options).

:- meta_predicate
    ?(:),
    ??(:).
:- module_transparent
    run_defined_query/0.


		 /*******************************
		 *     TOP LEVEL PREDICATES	*
		 *******************************/

%!  check_calls
%
%   Turn on the flag `check_calls`

check_calls :- set(check_calls, on).

%!  pos_loops
%
%   Turn on the flag `pos_loops`

pos_loops :- set(pos_loops, on).

%!  print_on
%
%   Turn on the flag `print`

print_on :- set(print, on).

%!  print_tree_on
%
%   Turn on the flag `print_tree`

print_tree_on :-
    set(print_tree, on).

clear_flags :-
    set(check_calls, off),
    set(pos_loops, off),
    set(print, off),
    set(print_tree, off).

%!  ??(:Query)
%
%   Shorcut predicate to ask queries in the top-level returning also the
%   justification tree. It calls solve_query/1

?? Q :-
    set(print_tree, on),
    solve_query(Q).

%!  ?(:Query)
%
%   Shorcut  predicate  to  ask  queries  in  the  top-level.  It  calls
%   solve_query/1

? Q :-
    set(print_tree, off),
    solve_query(Q).

%!  run_defined_query
%
%   Used from the interactive mode to run the defined query.

run_defined_query :-
    context_module(M),
    defined_query(M:Q),
    solve_query(M:Q),
    print(Q),
    allways_ask_for_more_models, nl, nl.

defined_query(M:_) :-
    M:pr_query([not(o_false)]), !,
    format('\nQuery not defined\n', []),
    fail.
defined_query(M:Q) :-
    M:pr_query(Q).


%!  solve_query(:Q)
%
%   Solve a query from the Prolog toplevel.
%
%   @tbd: Must be integrated into the Prolog toplevel.

solve_query(Q) :-
    process_query(Q, _, Query),
    Query = M:_,

    statistics(runtime, _),
    call_nth(solve(Query, [], StackOut, Model), Counter),
    statistics(runtime, [_|T]),

    format('\nAnswer ~w\t(in ~w ms):', [Counter, T]), nl,

    reverse(StackOut, Reverse_StackOut),
    if_user_option(print_tree,
                   scasp_portray_justification(M:Reverse_StackOut)),
    scasp_portray_model(M:Model, []), nl, nl,

    ask_for_more_models.



