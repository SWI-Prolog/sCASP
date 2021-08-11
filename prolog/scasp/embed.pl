:- module(embed,
          [ scasp/1,
            end_scasp/0
          ]).
:- use_module(ops).
:- use_module(compile).
:- use_module(predicates).

:- thread_local
    loading_scasp/4.

scasp(Unit) :-
    scasp_module(Unit, Module),
    prolog_load_context(module, Context),
    source_location(File, Line),
    '$set_source_module'(Old, Module),
    '$declare_module'(Module, scasp, Context, File, Line, false),
    scasp_push_operators,
    style_check(-singleton),
    asserta(loading_scasp(Unit, Module, File, Old)).

scasp_module(Unit, Module) :-
    atom_concat('_scasp_', Unit, Module).

end_scasp :-
    (   retract(loading_scasp(_Unit, _Module, _File, Old))
    ->  '$set_source_module'(_, Old),
        scasp_pop_operators,
        style_check(+singleton)       % TBD: restore old setting
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
    Clause = scasp_query(Query, 1).

%!  scasp_compile_unit(+Unit) is det.
%
%   Compile an sCASP module.

scasp_compile_unit(Unit) :-
    scasp_module(Unit, Module),
    findall(Clause, scasp_clause(Unit, Clause), Clauses),
    scasp_compile(Module:Clauses, []).

scasp_clause(Unit, Clause) :-
    scasp_module(Unit, Module),
    QHead = Module:Head,
    predicate_property(QHead, interpreted),
    \+ scasp_compiled(Head),
    \+ predicate_property(QHead, imported_from(_)),
    @(clause(Head, Body), Module),
    mkclause(Head, Body, Clause).

mkclause(scasp_query(Q,_N), true, Clause) =>
    Clause = (?- Q).
mkclause(Head, true, Clause) =>
    Clause = Head.
mkclause(Head, Body, Clause) =>
    Clause = (Head :- Body).
