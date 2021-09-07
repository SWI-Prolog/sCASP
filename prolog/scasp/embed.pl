/*  Part of sCASP

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(scasp_embed,
          [ begin_scasp/1,              % +Unit
            begin_scasp/2,              % +Unit, +Exports
            end_scasp/0,
            scasp_listing/2,            % +Unit, +Options
            scasp_model/1,              % -Model
            scasp_stack/1,              % -Stack

            op(700, xfx, .\=.)
          ]).
:- use_module(ops).
:- use_module(io).
:- use_module(input).
:- use_module(compile).
:- use_module(predicates).
:- use_module(solve).
:- use_module(model).
:- use_module(stack).
:- use_module(options).

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(prolog_code)).

:- create_prolog_flag(scasp_show_model, true, [keep(true)]).
:- create_prolog_flag(scasp_show_justification, true, [keep(true)]).
:- initialization set_options(['--tree']).

/** <module>  Embed sCASP programs in Prolog sources

This module allows embedding sCASP programs inside a Prolog module.
Currently the syntax is:

```
:- begin_scasp(UnitName).

<sCASP program>

:- end_scasp.
```

The idea is to create wrappers  for   the  sCASP  user predicates in the
target module that evaluate an sCASP  query   as  a normal Prolog query,
providing access to the  model  and   justification.  The  bindings come
available as normal Prolog bindings.

This is an  alternative  interface  to   defining  the  user  accessible
predicates using e.g., `:- scasp p/1,   q/2.`, which will then establish
the reachable predicates and perform  the   sCASP  conversion on them. I
think both have their value and the above one is simpler to start with.

@tbd: incomplete
*/

:- thread_local
    loading_scasp/3.                    % Unit, File, Dict

begin_scasp(Unit) :-
    begin_scasp(Unit, all).

begin_scasp(Unit, Exports) :-
    scasp_module(Unit, Module),
    prolog_load_context(module, Context),
    source_location(File, Line),
    '$set_source_module'(OldModule, Module),
    '$declare_module'(Module, scasp, Context, File, Line, false),
    scasp_push_operators,
    '$style_check'(OldStyle, OldStyle),
    style_check(-singleton),
    discontiguous(Module:(#)/1),
    asserta(loading_scasp(Unit, File,
                          _{ module:Module,
                             old_module:OldModule,
                             old_style:OldStyle,
                             exports:Exports
                           })).

scasp_module(Unit, Module) :-
    atom_concat('_scasp_', Unit, Module).

end_scasp :-
    throw(error(context_error(nodirective, end_scasp), _)).

end_scasp(Clauses) :-
    (   retract(loading_scasp(Unit, _File, Dict))
    ->  _{ old_module:OldModule,
           old_style:OldStyle,
           exports:Exports
         } :< Dict,
        '$set_source_module'(_, OldModule),
        scasp_pop_operators,
        '$style_check'(_, OldStyle),
        scasp_compile_unit(Unit),
        link_clauses(OldModule, Unit, Clauses, Exports)
    ;   throw(error(context_error(scasp_close(-)), _))
    ).

loading_scasp(Unit) :-
    source_location(File, _Line),
    loading_scasp(Unit,File,_).

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
user:term_expansion(#(discontiguous(Preds)), (:- discontiguous(Preds))) :-
    loading_scasp(_).
user:term_expansion((:- end_scasp), Clauses) :-
    \+ current_prolog_flag(xref, true),
    end_scasp(Clauses).

%!  scasp_compile_unit(+Unit) is det.
%
%   Compile an sCASP module.

:- thread_local
    done_unit/1.                  % allow for mutually recursive #include

scasp_compile_unit(Unit) :-
    call_cleanup(scasp_compile_unit_(Unit),
                 retractall(done_unit(_))).

scasp_compile_unit_(Unit) :-
    scasp_module(Unit, Module),
    (   current_module(Module)
    ->  true
    ;   existence_error(scasp_unit, Unit)
    ),
    findall(Clause, scasp_clause(Unit, Clause), Clauses),
    scasp_compile(Module:Clauses, []).

%!  scasp_clause(+Unit, -Clause) is nondet.
%
%   True when Clause is an sCASP clause or directive defined in Unit.

scasp_clause(Unit, _Clause) :-
    done_unit(Unit),
    !,
    fail.
scasp_clause(Unit, Clause) :-
    assertz(done_unit(Unit)),
    scasp_module(Unit, Module),
    QHead = Module:Head,
    predicate_property(QHead, interpreted),
    \+ scasp_compiled(Head),
    \+ predicate_property(QHead, imported_from(_)),
    @(clause(Head, Body), Module),
    mkclause(Head, Body, Clause).

mkclause(scasp_query(Query,_N), true, Clause) =>
    Clause = (?- Query).
mkclause(#(include(Unit)), true, Clause) =>
    scasp_clause(Unit, Clause).
mkclause(#(Directive), true, Clause) => % TBD: #abducible
    Clause = #(Directive).
mkclause(Head, true, Clause) =>
    Clause = Head.
mkclause(Head, Body, Clause) =>
    Clause = (Head :- Body).

%!  link_clauses(+ContextModule, +Unit, -Clauses, +Exports) is det.
%
%   Create  link  clauses  that  make  the  user  predicates  from  Unit
%   available as Prolog predicates from Module.

link_clauses(_ContextModule, Unit, Clauses, Exports) :-
    scasp_module(Unit, Module),
    findall(Head, link_predicate(Module:Head), Heads),
    check_exports(Exports, Heads),
    convlist(link_clause(Module, Exports), Heads, Clauses).

link_predicate(Module:Head) :-
    Module:pr_user_predicate(PI),
    \+ not_really_a_user_predicate(PI),
    pi_head(PI, Head).

% TBD: merge with user_predicate/1.
not_really_a_user_predicate((not)/1).
not_really_a_user_predicate(o_nmr_check/0).
not_really_a_user_predicate(global_constraints/0).

check_exports(all, _) :- !.
check_exports(Exports, Heads) :-
    must_be(list, Exports),
    maplist(check_export(Heads), Exports).

check_export(Heads, Export) :-
    pi_head(Export, EHead),             % raises an exception on malformed PI.
    (   memberchk(EHead, Heads)
    ->  true
    ;   existence_error(predicate, Export)
    ).

link_clause(Module, Exports, Head,
            (Head :- scasp_embed:scasp_call(Module:Head))) :-
    (   Exports == all
    ->  true
    ;   pi_head(PI, Head),
        memberchk(PI, Exports)
    ).


%!  scasp_call(:Query)
%
%   Solve an sCASP goal from the interactive toplevel

:- public scasp_call/1.

scasp_call(Query) :-
    process_query(Query, _, Query1),
    scasp_stack(StackIn),
    solve(Query1, StackIn, StackOut, Model),
    save_model(Model),
    Query1 = M:_,                       % TBD: Properly handle the module
    save_stack(M:StackOut).

%!  save_model(+Model) is det.
%
%   Save the model.
%
%   @tbd We must qualify the model.

save_model(Model) :-
    (   nb_current(scasp_model, Model0)
    ->  append(Model, Model0, FullModel),
        b_setval(scasp_model, FullModel)
    ;   b_setval(scasp_model, Model)
    ).

%!  scasp_model(-Model) is semidet.
%
%   True when Model  represents  the  current   set  of  true  and false
%   literals.

scasp_model(Model) :-
    nb_current(scasp_model, RawModel),
    canonical_model(RawModel, Model).

save_stack(Stack) :-
    b_setval(scasp_stack, Stack).

%!  scasp_stack(-Stack) is det.
%
%   True when Stack represents the justification   of  the current sCASP
%   answer.

scasp_stack(Stack) :-
    (   nb_current(scasp_stack, Stack0)
    ->  Stack = Stack0
    ;   Stack = []
    ).

%!  scasp_listing(+Unit, +Options)
%
%   List the transformed program for Unit

scasp_listing(Unit, Options) :-
    scasp_module(Unit, Module),
    scasp_portray_program(Module:Options).

:- residual_goals(scasp_residuals).

%!  scasp_residuals// is det.

scasp_residuals -->
    { scasp_residual_types(Types) },
    scasp_residuals(Types).

scasp_residuals([]) -->
    [].
scasp_residuals([model|T]) -->
    (   {scasp_model(Model)}
    ->  [ scasp_set_model(Model) ]
    ;   []
    ),
    scasp_residuals(T).
scasp_residuals([justification|T]) -->
    (   {scasp_stack(Stack), Stack \== []}
    ->  [ scasp_set_stack(Stack) ]
    ;   []
    ),
    scasp_residuals(T).

scasp_residual_types(Types) :-
    findall(Type, scasp_residual_type(Type), Types).

scasp_residual_type(model) :-
    current_prolog_flag(scasp_show_model, true).
scasp_residual_type(justification) :-
    current_prolog_flag(scasp_show_justification, true).

user:portray(scasp_set_model(Model)) :-
    format('sCASP model: ~p', [Model]).
:- if(false).
user:portray(scasp_set_stack(M:Stack)) :-
%   format('sCASP justification', []),
%   process_stack(Stack, _).
    reverse(Stack, Reverse_StackOut),
    pretty_term([], _D3, Reverse_StackOut, P_StackOut),
    scasp_portray_justification(M:P_StackOut).
:- else.
user:portray(scasp_set_stack(Stack)) :-
    format('sCASP justification', []),
    justification_tree(Stack, Tree, []),
    print_justification_tree(Tree).
:- endif.

:- multifile
    prolog:alternate_syntax/4,
    prolog:xref_update_syntax/2.

prolog:alternate_syntax(scasp, Module, Setup, Restore) :-
    Setup = scasp_ops:scasp_push_operators(Module),
    Restore = scasp_ops:scasp_pop_operators.

prolog:xref_update_syntax(begin_scasp(_Unit), Module) :-
    scasp_ops:scasp_push_operators(Module).
prolog:xref_update_syntax(end_scasp, _Module) :-
    scasp_ops:scasp_pop_operators.
