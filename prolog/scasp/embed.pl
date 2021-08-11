:- module(embed,
          [ scasp/1,
            end_scasp/0
          ]).
:- use_module(ops).

:- thread_local
    loading_scasp/4.

scasp(Unit) :-
    scasp_module(Unit, Module),
    prolog_load_context(module, Context),
    source_location(File, Line),
    '$set_source_module'(Old, Module),
    '$declare_module'(Module, scasp, Context, File, Line, false),
    scasp_push_operators,
    asserta(loading_scasp(Unit, Module, File, Old)).

scasp_module(Unit, Module) :-
    atom_concat('_scasp_', Unit, Module).

end_scasp :-
    (   retract(loading_scasp(_Unit, _Module, _File, Old))
    ->  writeln(x),
        '$set_source_module'(_, Old),
        scasp_pop_operators
    ;   throw(error(context_error(scasp_close(-)), _))
    ).

loading_scasp(Unit) :-
    source_location(File, _Line),
    loading_scasp(Unit,_,File,_).

user:term_expansion(end_of_file, _) :-
    loading_scasp(Unit),
    print_message(error, scasp(not_closed_program(Unit))),
    end_scasp,
    fail.
user:term_expansion((:- Constraint), Clause) :-
    loading_scasp(_),
    Constraint \== end_scasp,
    !,
    Clause = ('_false_0' :- Constraint).
user:term_expansion((?- Query), Clause) :-
    loading_scasp(_),
    !,
    Clause = (query(Query)).            % TBD

