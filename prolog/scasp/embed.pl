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
            scasp_model/1,              % :Model
            scasp_model/2,              % :Model, +Options
            scasp_stack/1,              % -Stack
            scasp_justification/2,      % -Tree, +Options
            (not)/1,                    % :Query
            (-)/1,                      % :Query
            '\u2209'/2,                 % Inequality

            op(700, xfx, .\=.),
            op(700, xfx, '\u2209'),
            op(900, fy, not)
          ]).
:- use_module(ops).
:- use_module(input).
:- use_module(compile).
:- use_module(predicates).
:- use_module(solve).
:- use_module(model).
:- use_module(stack).
:- use_module(options).
:- use_module(listing).
:- use_module(clp/disequality).

:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(prolog_code)).

:- meta_predicate
    scasp_model(:),
    scasp_justification(:, +),
    not(0),
    -(:).

% one of `false`, `true`, `unicode` or `human`
:- create_prolog_flag(scasp_show_model,
                      false,
                      [keep(true), type(atom)]).
:- create_prolog_flag(scasp_show_justification,
                      false,
                      [keep(true), type(atom)]).

/** <module>  Embed sCASP programs in Prolog sources

This module allows embedding sCASP programs inside a Prolog module.
Currently the syntax is:

```
:- begin_scasp(UnitName[, Exports]).

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

%!  begin_scasp(+Unit).
%!  begin_scasp(+Unit, +Exports).
%
%   Start an embedded sCASP program.  Exports   is  a  list if predicate
%   indicators as use_module/2 that defines   the  sCASP predicates that
%   are made visible from the  enclosing   module  as Prolog predicates.
%   These predicates modify the Prolog syntax by:
%
%     - Defining appropriate operators
%     - Disable singleton checking
%
%   Otherwise the read clauses  are   asserted  verbatim. Directives are
%   terms #(Directive). Prolog directives (:- Directive) are interpreted
%   as sCASP __global constraints__. The   matching end_scasp/0 compiles
%   the sCASP program and creates wrappers  in the enclosing module that
%   call the sCASP solver.
%
%   The  sCASP  code   must   be    closed   using   end_scasp/0.   Both
%   begin_scasp/1,2 and end_scasp/0 must be used as directives.

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

%!  end_scasp
%
%   Close begin_scasp/1,2. See begin_scasp/1,2 for details.

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
        (   Exports == []
        ->  Options = [unknown(fail)]
        ;   Options = []
        ),
        scasp_compile_unit(Unit, Options),
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
user:term_expansion(#(pred(Preds)), #(pred(Preds))) :-
    loading_scasp(_),
    prolog_load_context(variable_names, Vars),
    maplist(bind_var, Vars).
user:term_expansion((:- end_scasp), Clauses) :-
    \+ current_prolog_flag(xref, true),
    end_scasp(Clauses).

bind_var(Name = $(Name)).


%!  scasp_compile_unit(+Unit, +Options) is det.
%
%   Compile an sCASP module.

:- thread_local
    done_unit/1.                  % allow for mutually recursive #include

scasp_compile_unit(Unit, Options) :-
    call_cleanup(scasp_compile_unit_(Unit, Options),
                 retractall(done_unit(_))).

scasp_compile_unit_(Unit, Options) :-
    scasp_module(Unit, Module),
    (   current_module(Module)
    ->  true
    ;   existence_error(scasp_unit, Unit)
    ),
    findall(Clause, scasp_clause(Unit, Clause), Clauses),
    scasp_compile(Module:Clauses, Options).

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
mkclause('_false_0', Body, Clause) =>
    Clause = (:- Body).
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

check_export(Heads, -Name/Arity) :-
    !,
    atom_concat(-, Name, NName),
    check_export(Heads, NName/Arity).
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
    ;   functor(Head, NName, Arity),
        atom_concat(-, Name, NName)
    ->  memberchk(-Name/Arity, Exports)
    ;   pi_head(PI, Head),
        memberchk(PI, Exports)
    ).


%!  scasp_call(:Query)
%
%   Solve an sCASP goal from the interactive toplevel

:- public scasp_call/1.

scasp_call(Query) :-
    scasp_compile_query(Query, Compiled, []),
    scasp_stack(StackIn),
    solve(Compiled, StackIn, StackOut, Model),
    save_model(Model),
    Compiled = M:_,                       % TBD: Properly handle the module
    save_stack(M:StackOut).

%!  not(:Query)
%
%   sCASP NaF negation. Note that  this   conflicts  with the deprecated
%   standard Prolog not/1 predicate which is   a  synonym for \+/1. Make
%   sure to load sCASP into a module   where you want sCASP negation and
%   use \+/1 for Prolog negation in this model.

not(M:Query) :-
    clause(M:Query, scasp_embed:scasp_call(Module:Query)),
    scasp_call(Module:not(Query)).

%!  -(:Query)
%
%   sCASP classical negation.

-(M:Query) :-
    Query =.. [Name|Args],
    atom_concat(-, Name, NName),
    NQuery =.. [NName|Args],
    clause(M:NQuery, scasp_embed:scasp_call(Module:NQuery)),
    scasp_call(Module:NQuery).


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

%!  scasp_model(:Model) is semidet.
%!  scasp_model(:Model, +Options) is semidet.
%
%   True when Model  represents  the  current   set  of  true  and false
%   literals.

scasp_model(M:Model) :-
    scasp_model(M:Model, []).

scasp_model(M:Model, _Options) :-
    nb_current(scasp_model, RawModel),
    canonical_model(RawModel, Model1),
    unqualify_model(Model1, M, Model).

save_stack(Stack) :-
    b_setval(scasp_stack, Stack),
    justification_tree(Stack, Tree, []),
    b_setval(scasp_tree, Tree).

%!  scasp_stack(-Stack) is det.
%
%   True when Stack represents the justification   of  the current sCASP
%   answer.

scasp_stack(Stack) :-
    (   nb_current(scasp_stack, Stack0)
    ->  Stack = Stack0
    ;   Stack = []
    ).

%!  scasp_justification(:Tree, +Options) is semidet.
%
%   Justification for the current sCASP answer.

scasp_justification(M:Tree, Options) :-
    nb_current(scasp_tree, Tree0),
    remove_origins(Tree0, Tree1, Options),
    unqualify_justitication_tree(Tree1, M, Tree).

remove_origins(Tree0, Tree, Options) :-
    option(source(false), Options),
    !,
    remove_origins(Tree0, Tree).
remove_origins(Tree, Tree, _).

remove_origins(M:Tree0, Result) =>
    Result = M:Tree,
    remove_origins(Tree0, Tree).
remove_origins(Term0-Children0, Result) =>
    Result = Term-Children,
    remove_origin(Term0, Term),
    maplist(remove_origins, Children0, Children).
remove_origins(Nodes0, Nodes), is_list(Nodes0) =>
    maplist(remove_origins, Nodes0, Nodes).
remove_origins(Node0, Node) =>
    remove_origin(Node0, Node).

remove_origin(goal_origin(Term, _), Result) =>
    Result = Term.
remove_origin(Term, Result) =>
    Result = Term.


%!  scasp_listing(+Unit, +Options)
%
%   List the transformed program for Unit

scasp_listing(Unit, Options) :-
    scasp_module(Unit, Module),
    scasp_portray_program(Module:Options).

:- residual_goals(scasp_residuals).

%!  scasp_residuals// is det.
%
%   Hook into the SWI-Prolog toplevel  to   add  additional goals to the
%   answer conjunction. Optionally provides the model and justification.

scasp_residuals -->
    { '$current_typein_module'(TypeIn),
      scasp_residual_types(Types)
    },
    scasp_residuals(Types, TypeIn).

scasp_residuals([], _) -->
    [].
scasp_residuals([model(Options)|T], M) -->
    (   {scasp_model(M:Model, Options)}
    ->  [ scasp_show_model(Model, Options) ]
    ;   []
    ),
    scasp_residuals(T, M).
scasp_residuals([justification(Options)|T], M) -->
    (   {scasp_stack(Stack), Stack \== []}
    ->  [ scasp_show_stack(M:Stack, Options) ]
    ;   []
    ),
    scasp_residuals(T, M).

scasp_residual_types(Types) :-
    findall(Type, scasp_residual_type(Type), Types).

scasp_residual_type(model(Options)) :-
    current_prolog_flag(scasp_show_model, Spec),
    Spec \== false,
    res_options(Spec, Options).
scasp_residual_type(justification(Options)) :-
    current_prolog_flag(scasp_show_justification, Spec),
    Spec \== false,
    res_options(Spec, Options).

res_options(List, Options), is_list(List) =>
    Options = List.
res_options(true, Options) =>
    Options = [unicode(true)].
res_options(unicode, Options) =>
    Options = [unicode(true)].
res_options(human, Options) =>
    Options = [human(true)].

user:portray(scasp_show_model(Model, Options)) :-
    ansi_format(comment, '% s(CASP) model~n', []),
    print_model(Model, Options).
user:portray(scasp_show_stack(M:Stack, Options)) :-
    ansi_format(comment, '% s(CASP) justification', []),
    justification_tree(Stack, Tree0, Options),
    unqualify_justitication_tree(Tree0, M, Tree),
    print_justification_tree(Tree, [full_stop(false)|Options]).
user:portray('\u2209'(V,S)) :-          % not element of
    format('~p \u2209 ~p', [V, S]).


		 /*******************************
		 *       HIGHLIGHT SUPPORT	*
		 *******************************/

:- multifile
    prolog:alternate_syntax/4,
    prolog:xref_update_syntax/2.

prolog:alternate_syntax(scasp, Module, Setup, Restore) :-
    Setup = scasp_ops:scasp_push_operators(Module),
    Restore = scasp_ops:scasp_pop_operators.

prolog:xref_update_syntax(begin_scasp(_Unit), Module) :-
    scasp_ops:scasp_push_operators(Module).
prolog:xref_update_syntax(begin_scasp(_Unit, _Exports), Module) :-
    scasp_ops:scasp_push_operators(Module).
prolog:xref_update_syntax(end_scasp, _Module) :-
    scasp_ops:scasp_pop_operators.

:- multifile
    prolog_colour:term_colours/2.

prolog_colour:term_colours(#(Directive),
                           expanded - [DirColours]) :-
    debug(scasp(highlight), 'Got ~p', [Directive]),
    dir_colours(Directive, DirColours).

dir_colours(pred(_Head::_Template),
            expanded -
            [ expanded -
              [ body,
                comment(_)
              ]
            ]).
dir_colours(show(Preds),
            expanded - [Colours]) :-
    decl_show_colours(Preds, Colours).
dir_colours(include(_Unit),
            expanded -
            [ classify
            ]).
dir_colours(discontiguous(_Preds),
            expanded -
            [ declarations(discontiguous)
            ]).

decl_show_colours((A,B), Colours) =>
    Colours = classify-[CA,CB],
    decl_show_colours(A, CA),
    decl_show_colours(B, CB).
decl_show_colours(not(_A), Colours) =>
    Colours = built_in-[declarations(show)].
decl_show_colours(_A, Colours) =>
    Colours = declarations(show).


		 /*******************************
		 *          GXREF SUPPORT	*
		 *******************************/

%!  pce_xref_gui:gxref_called(?Source, ?Callable)
%
%   Hook into gxref/0 that may  extend   the  notion of predicates being
%   called by some infrastructure. Here, do two things:
%
%     - We silence called s(CASP) hooks
%     - If a predicate is defined using scasp_dynamic/1 and either the
%       positive or negative version is called, we consider it called.

:- multifile pce_xref_gui:gxref_called/2.
:- autoload(library(prolog_xref), [xref_called/4]).

pce_xref_gui:gxref_called(Source, Callable) :-
    nonvar(Source),
    callable(Callable),
    !,
    (   xref_called_cond(Source, Callable, _)
    ->  true
    ;   scasp_called(Callable)
    ->  true
    ;   xref_dynamic(Source, Callable),
        scasp_negate(Callable, NegCallable),
        xref_dynamic(Source, NegCallable),
        xref_called_cond(Source, NegCallable, _)
    ).

xref_dynamic(Source, Callable) :-
    xref_defined(Source, Callable, dynamic(_)), !.
xref_dynamic(Source, Callable) :-
    xref_defined(Source, Callable, thread_local(_)).

xref_called_cond(Source, Callable, Cond) :-
    xref_called(Source, Callable, By, Cond),
    By \= Callable.                 % recursive calls

scasp_negate(Callable, NegCallable) :-
    atom(Callable),
    !,
    scasp_neg_atom(Callable, NegCallable).
scasp_negate(Callable, NegCallable) :-
    compound_name_arguments(Callable, Name, Args),
    scasp_neg_atom(Name, NegName),
    compound_name_arguments(NegCallable, NegName, Args).

scasp_neg_atom(Neg, Pos) :-
    atom_concat(-, Pos, Neg),
    !.
scasp_neg_atom(Pos, Neg) :-
    atom_concat(-, Pos, Neg).

scasp_called(pr_pred_predicate(_,_,_,_)).
scasp_called(scasp_expand_program(_,_,_,_)).
scasp_called(-).
