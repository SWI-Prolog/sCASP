:- module(scasp,
          [ scasp_test/2,
            main/1,
            load/1,
            run_defined_query/0,
            (?)/1,
            (??)/1,
            clear_flags/0,
            check_calls/0,
            pos_loops/0,
            print_on/0,
            print_tree_on/0,

            op(700, fx, not),
            op(700, fx, ??),
            op(700, fx, ?)
          ]).
:- set_prolog_flag(optimise, true).

:- use_module(scasp_io).
:- use_module(scasp_ops).
:- use_module(scasp_options).
:- use_module(solve).
:- use_module(sasp/output).           % the pr_* predicates


:- initialization(main, main).

		 /*******************************
		 *        MAIN PREDICATES	*
		 *******************************/

%!  load(+Files) is det.
%
%   Load a list of files.

load(X) :-
    load_program(X).

%!  scasp_test(+Argv, -StackModelPairs) is det.
%
%   Called from test.pl

scasp_test(Args, Stacks-Models) :-
    parse_args(Args, Options, Sources),
    set_options(Options),
    load(Sources),
    defined_query(Q),
    process_query(Q, _, Query),
    findall(
        Stack-Model,
        ( solve(Query, [], StackOut, ModelOut),
          pretty_term([], _, StackOut, Stack),
          pretty_term([], _, ModelOut, Model)
        ),
        Pairs),
    pairs_keys_values(Pairs, Stacks, Models).

%!  main(+Argv)
%
%   Used when calling from command  line   by  passing  the command line
%   options and the input files.

main(Args) :-
    print_on,
    retractall(current_option(_, _)),
    parse_args(Args, Options, Sources),
    set_options(Options),
    load(Sources),
    if_user_option(write_program, (write_program, halt)),
    (   current_option(interactive, on)
    ->  '$toplevel':setup_readline,
        main_loop
    ;   defined_query(Q),
        main_solve(Q)
    ).
main(_).

main_loop :-
    read_term_with_history(R,
                           [ prompt('casp ~! ?- '),
                             variable_names(Bindings)
                           ]),
    maplist(bind_var, Bindings),            % sCASP vars are $(Name), see revar/2.
    conj_to_list(R, RQ),
    capture_classical_neg(RQ, Q),
    (   atom(R),
        end_of_input(R)
    ->  format('~N'),
        halt
    ;   conj_to_list(R, RQ),
        capture_classical_neg(RQ, Q),
        (   main_solve(Q)
        ->  nl, main_loop
        ;   main_loop
        )
    ).

bind_var(Name = Var) :-
    Var = $Name.

end_of_input(end_of_file).
end_of_input(exit).
end_of_input(quit).
end_of_input(halt).

conj_to_list(true, []) :-
    !.
conj_to_list(Conj, List) :-
    comma_list(Conj, List).


capture_classical_neg([], []) :- !.
capture_classical_neg([-S|Ss], [N|NSs]) :- !,
    S =.. [Name|Args],
    atom_concat('-', Name, NegName),
    N =.. [NegName|Args],
    capture_classical_neg(Ss, NSs).
capture_classical_neg([S|Ss], [S|NSs]) :-
    capture_classical_neg(Ss, NSs).

%!  main_solve(+Query)
%
%   Solve a toplevel query. Query is a callable term where variables are
%   represented as $Name.

main_solve(Q0) :-
    current_option(minimal_model, on), !,
    collect_min_models(Q0),
    fail.
main_solve(Q0) :-
    current_option(answers, Number),

    process_query(Q0, Q, Query, D0),
    maplist(arg(2), D0, Vars),

    pretty_term(D0, D1, par(Vars, Q), par(PVars, PQ)),
    print_query(PQ),

    statistics(runtime, _),
    (   call_nth(solve(Query, [], StackOut, Model), Counter)
    *-> nl
    ;   format('\nno models\n\n'),
        fail
    ),
    statistics(runtime, [_,T]),

    format('\tANSWER:\t~w (in ~w ms)\n', [Counter, T]),

    pretty_term(D1, D2, par(Q, Vars, Model), par(PAnswer, Bindings, P_Model)),

    if_user_option(process_stack, (
            reverse(StackOut, Reverse_StackOut),
            pretty_term(D2, _D3, Reverse_StackOut, P_StackOut)
        )),

    if_user_option(html, print_html([PQ, PAnswer, Bindings, PVars], P_Model, P_StackOut)),
    if_user_option(print_tree, print_justification_tree(P_StackOut)),
    print_model(P_Model), nl,

    print_unifier(Bindings, PVars),

    (   Number == -1
    ->  allways_ask_for_more_models, nl, nl
    ;   Number == 0
    ->  nl, nl,
        statistics(runtime, _),
        fail
    ;   Number > 0
    ->  nl, nl,
        statistics(runtime, _),
        Counter = Number
    ),
    !.


%!  run_defined_query
%
%   Used from the interactive mode to run the defined query.

run_defined_query :-
    defined_query(A),
    solve_query(A),
    print(A),
    allways_ask_for_more_models, nl, nl.

defined_query(_) :-
    pr_query([not(o_false)]), !,
    format('\nQuery not defined\n', []),
    fail.
defined_query(Q) :-
    pr_query(Q).


% Predicate aggregated
take_min(Query, MinModel, Model, StackOut, T) :-
    statistics(runtime, _),
    solve(Query, [], StackOut, Model),
    statistics(runtime, [_|[T]]),
    select_printable_literals(Model, [], PrintableModel),
    sort(PrintableModel, MinModel).

collect_min_models(Q0) :-
    process_query(Q0, Q, Query), term_variables(Q, Vars),
    unifiable(Q0, Q, D0),

    pretty_term(D0, D1, par(Vars, Q), par(PVars, PQ)),

    print_query(PQ),
    (   call_nth(take_min(Query, _MinModel, Model, StackOut, T), Counter)
    *-> nl
    ;   format('\nno models\n\n'),
        fail
    ),

    format('\tANSWER:\t~w (in ~w ms)\n', [Counter, T]),

    pretty_term(D1, D2, par(Q, Vars, Model), par(PAnswer, Bindings, P_Model)),

    if_user_option(process_stack, (
            reverse(StackOut, Reverse_StackOut),
            pretty_term(D2, _D3, Reverse_StackOut, P_StackOut)
        )),

    if_user_option(html, print_html([PQ, PAnswer, Bindings, PVars], P_Model, P_StackOut)),
    if_user_option(print_tree, print_justification_tree(P_StackOut)),
    print_model(P_Model), nl,

    print_unifier(Bindings, PVars),

    nl, nl.


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

%!  ??(?Query)
%
%   Shorcut predicate to ask queries in the top-level returning also the
%   justification tree. It calls solve_query/1

?? Q :-
    set(print, on),
    solve_query(Q).

%!  ?(?Query)
%
%   Shorcut  predicate  to  ask  queries  in  the  top-level.  It  calls
%   solve_query/1

? Q :-
    set(print, off),
    solve_query(Q).

%!  solve_query(+Q)
%
%   Solve a query from the interactive toplevel.

solve_query(Q) :-
    process_query(Q, _, Query),

    statistics(runtime, _),
    call_nth(solve(Query, [], StackOut, Model), Counter),
    statistics(runtime, [_|T]),

    format('\nAnswer ~w\t(in ~w ms):', [Counter, T]), nl,

    reverse(StackOut, Reverse_StackOut),
    if_user_option(print_tree, print_justification_tree(Reverse_StackOut)),
    print_model(Model), nl, nl,

    ask_for_more_models.



