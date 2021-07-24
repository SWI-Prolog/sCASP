:- module(scasp_main,
          [ main/1                          % +Argv
          ]).
:- set_prolog_flag(optimise, true).

:- use_module(io).
:- use_module(ops).
:- use_module(options).
:- use_module(solve).
:- use_module(output).

/** <module> sCASP as a stand-alone program

This module allows running scasp as a stand-alone program that loads one
or more scasp source files, answer the (last) query and exit.
*/

:- initialization(main, main).

%!  main(+Argv)
%
%   Used when calling from command  line   by  passing  the command line
%   options and the input files.

main(Args) :-
    set(print, on),
    retractall(current_option(_, _)),
    parse_args(Args, Options, Sources),
    set_options(Options),
    load_program(Sources),
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


defined_query(_) :-
    pr_query([not(o_false)]), !,
    print_message(error, scasp(no_query)),
    halt(1).
defined_query(Q) :-
    pr_query(Q).
