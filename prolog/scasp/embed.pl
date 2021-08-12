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
          [ begin_scasp/1,
            end_scasp/0
          ]).
:- use_module(ops).
:- use_module(compile).
:- use_module(predicates).

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
    loading_scasp/4.

begin_scasp(Unit) :-
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
