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

:- module(sasp_input,
          [ load_source_files/1,        % +Files
            sasp_read/2                 % +File, -Statements
          ]).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(prolog_code)).

:- use_module(common).
:- use_module(program).

:- dynamic                              % TBD: Get rid of these
    asp_table/1,
    show/1,
    pred/1.

/** <module> Read SASP source code

This module defines reading sCASP input based on the Prolog parser.
*/

% operator that may be used in sCASP   source. They are used through the
% `module` option of read_term/3.
% TBD: Do we need both sets of comparison operators?

:- op(700, xfx, [.=., .\=.,.<>.,.<.,.=<.,.>.,.>=.]).
:- op(700, xfx, [#= , #<>, #< , #> , #=<, #>= ]).
:- op(700, xfx, ::).
:- op(900, fy,  not).
:- op(800, fx,  #).
:- op(300, fx,  [include, table, show, pred, compute, abducible]).

		 /*******************************
		 *            COMPAT		*
		 *******************************/

%!  load_source_files(+Files:list) is det
%
%   Given a list of source files, read, tokenize and parse them, merging
%   their  output  into  a  single  list    of  statements.  Next,  call
%   program:assert_program/1 to process the statements.
%
%   @arg Files The list of files to load.

load_source_files(Fs) :-
    maplist(sasp_read, Fs, StmtsList),
    append(StmtsList, Statements),
    assert_program(Statements).


		 /*******************************
		 *            READER		*
		 *******************************/


%!  sasp_read(+File, -Statements) is det.
%
%   Read File into a list of ASP statements.

sasp_read(File, Statements) :-
    sasp_read(File, Statements, []).

sasp_read(File, Statements, Options) :-
    absolute_file_name(File, Path,
                       [ access(read),
                         extensions([asp,pl,''])
                       ]),
    setup_call_cleanup(
        open(Path, read, In),
        sasp_read_stream_raw(In, Statements, [base(Path)|Options]),
        close(In)).

sasp_read_stream_raw(In, Statements, Options) :-
    setup_call_cleanup(
        prep_read(Undo),
        sasp_read_stream(In, Statements, Options),
        call(Undo)).

%!  prep_read(-Undo)
%
%   Setup the Prolog syntax for reading   sCASP. This currently sets the
%   `allow_variable_name_as_functor` flag, such that  e.g. _female(jane)
%   is valid syntax.

prep_read(Undo) :-
    convlist(update_flag,
             [ allow_variable_name_as_functor-true
             ], UndoList),
    (   UndoList == []
    ->  Undo = true
    ;   comma_list(Undo, UndoList)
    ).

update_flag(Flag-Value, true) :-
    current_prolog_flag(Flag, Value).
update_flag(Flag-Value, set_prolog_flag(Flag, Old)) :-
    current_prolog_flag(Flag, Old),
    set_prolog_flag(Flag, Value).

%!  sasp_read_stream(+In, -Statements, +Options) is det.
%
%   Read the content of the stream In into a list of sCASP statements.

sasp_read_stream(In, Statements, Options) :-
    context_module(M),
    read_term(In, Term,
              [ module(M),
                variable_names(VarNames),
                subterm_positions(Pos)
              ]),
    (   Term == end_of_file
    ->  Statements = []
    ;   sasp_statement(Term, VarNames, New, Pos, Options),
        add_statements(New, Tail, Statements),
        sasp_read_stream(In, Tail, Options)
    ).

add_statements(New, Tail, Statements) :-
    is_list(New),
    !,
    append(New, Tail, Statements).
add_statements(New, Tail, [New|Tail]).

%!  sasp_statement(+Term, +VarNames, -SASP, +Pos, +Options) is det.
%
%   Convert a raw Prolog term into  its sCASP equivalent. Currently does
%   these transformations:
%
%     - Clauses get the shape Head-ListOfBodyAtoms
%     - -Name(Args) is translated into c_Name(Args)
%     - Atoms that need a prefix are prefixed using `d_`
%
%   This also processes directives, terms of the shape #Directive.
%
%   @arg VarNames is  a  list  Name=Var   as  produced  by  read_term/3.
%   Remaining variables are bound to ``_V<N>``.

:- det(sasp_statement/5).

sasp_statement(Term, VarNames, SASP, Pos, Options) :-
    maplist(bind_var,VarNames),
    term_variables(Term, Vars),
    bind_anon(Vars, 0),
    sasp_statement(Term, SASP, Pos, Options).

bind_var(Name=Var) :-
    Var = $Name.

bind_anon([], _).
bind_anon([$Name|T], I) :-
    atom_concat('_V', I, Name),
    I2 is I+1,
    bind_anon(T, I2).

%!  sasp_statement(+Term, -SASPTerm, +TermPos, +Options) is det.
%
%   Translate a single term.

sasp_statement(Head :- Body,  SASP,
               term_position(_,_,_,_,[HP,BP]), Options) =>
    sasp_predicate(Head, SASPHead, HP, Options),
    comma_list(Body, BP, BodyList, BodyPos),
    maplist(sasp_predicate_m(Options), BodyList, SASPBody, BodyPos),
    SASP = (SASPHead-SASPBody).
sasp_statement(?-(Query), SASP,
               term_position(_,_,_,_,[QP]), Options) =>
    comma_list(Query, QP, BodyList, BodyPos),
    maplist(sasp_predicate_m(Options), BodyList, SASPBody, BodyPos),
    SASP = c(1, SASPBody).
sasp_statement(:-(Constraint), SASP,
               term_position(_,_,_,_,[QP]), Options) =>
    comma_list(Constraint, QP, BodyList, BodyPos),
    maplist(sasp_predicate_m(Options), BodyList, SASPBody, BodyPos),
    SASP = '_false_0'-SASPBody.
sasp_statement(#Directive, SASP,
               term_position(_,_,_,_,[DP]), Options) =>
    directive(Directive, SASP, DP, Options).
sasp_statement(Head,  SASP, Pos, Options), callable(Head) =>
    sasp_predicate(Head, SASPHead, Pos, Options),
    SASP = (SASPHead-[]).

%!  sasp_predicate(+Pred, -ASPPred, +Pos, +Options) is det.
%!  sasp_predicate_m(+Options, +Pred, -ASPPred, +Pos) is det.
%
%   Handle an ASP atom. Renames  the   functor  when needed. Knows about
%   -Pred (classical negation) and not(Pred).

sasp_predicate_m(Options, Term, ASPTerm, Pos) :-
    sasp_predicate(Term, ASPTerm, Pos, Options).

sasp_predicate(Pred, ASPPred, PPos, Options) :-
    nonvar(PPos),
    PPos = parentheses_term_position(_,_,Pos),
    !,
    sasp_predicate(Pred, ASPPred, Pos, Options).
sasp_predicate(not(Pred), not(ASPPred),
               term_position(_,_,_,_,[Pos]), Options) :-
    !,
    sasp_predicate(Pred, ASPPred, Pos, Options).
sasp_predicate(-(Pred), ASPPred,
               term_position(_,_,_,_,[_Pos]), _Options) :-
    !,
    Pred =.. [Name|Args0],
    functor(Pred, Name, Arity),
    asp_prefix(Name, Arity, ASPName),
    atom_concat(c_, ASPName, ASPNameNeg),
    maplist(asp_term, Args0, Args),
    ASPPred =.. [ASPNameNeg|Args].
sasp_predicate(Pred, error, Pos, Options) :-
    illegal_pred(Pred),
    !,
    sasp_syntax_error(invalid_predicate(Pred), Pos, Options).
sasp_predicate(Pred, SASPPred, _, _Options) :-
    Pred =.. [Name|Args0],
    functor(Pred, Name, Arity),
    asp_prefix(Name, Arity, ASPName),
    maplist(asp_term, Args0, Args),
    SASPPred =.. [ASPName|Args].

asp_term(Term, Term).

asp_prefix(Name, 2, ASPName) :-
    operator(Name, _, _),
    !,
    ASPName = Name.
asp_prefix(Name, Arity, ASPName) :-
    builtin(Name/Arity),
    !,
    ASPName = Name.
asp_prefix(Name, Arity, ASPName) :-
    handle_prefixes(Name, ASPName0),
    join_functor(ASPName, ASPName0, Arity).

%!  handle_prefixes(+FunctorIn:atom, -FunctorOut:atom)
%
%   If the predicate begins with a reserved prefix, add the dummy prefix
%   to ensure that it won't be treated  the same as predicates where the
%   prefix is added internally. If no prefix, just return the original.

handle_prefixes(Fi, Fo) :-
    needs_dummy_prefix(Fi),
    !,
    atom_concat(d_, Fi, Fo).
handle_prefixes(F, F).

needs_dummy_prefix(F) :-
    has_prefix(F, _),
    !.
needs_dummy_prefix(F) :-
    reserved_prefix(F),
    !.
needs_dummy_prefix(F) :-
    sub_atom(F, 0, _, _, '_').

%!  builtin(+Pred:pi) is semidet.
%
%   Predicates that will be executed by Prolog

builtin(write/1).
builtin(writef/2).
builtin(nl/0).

illegal_pred($_).
illegal_pred(_;_).

%!  directive(+Directive, -Statements, +Pos, +Options) is det.
%
%   Process a directive.

directive(include(File), Statements, _, Options) =>
    option(base(Base), Options),
    absolute_file_name(File, Path,
                       [ access(read),
                         extensions([asp,pl,'']),
                         relative_to(Base)
                       ]),
    sasp_read(Path, Statements).
directive(table(Pred), Statements, _, _) =>
    Statements = (:- table(Pred)).
directive(show(Pred), Statements, _, _) =>
    Statements = (:- show(Pred)).
directive(pred(Pred), Statements, _, _) =>
    Statements = (:- pred(Pred)).
directive(pred(Pred)::Comment, Statements, _, _) =>
    Statements = (:- pred(Pred::Comment)).
directive(abducible(Pred), Rules, Pos, Options) =>
    sasp_predicate(Pred, ASPPred, Pos, Options),
    abducible_rules(ASPPred, Rules).
directive(Directive, Statements, Pos, Options) =>
    sasp_syntax_error(invalid_directive(Directive), Pos, Options),
    Statements = [].

abducible_rules(Head,
                [ Head                 - [ not AHead, abducible_1(Head) ],
                  AHead                - [ not Head ],
                  abducible_1(Head)    - [ not '_abducible_1'(Head) ],
                  '_abducible_1'(Head) - [ not abducible_1(Head) ]
                ]) :-
    Head =.. [F|Args],
    atom_concat('_', F, AF),
    AHead =.. [AF|Args].

%!  sasp_syntax_error(+Error, +Pos, +Options)
%
%   @tbd: properly translate the error location

sasp_syntax_error(Error, Pos, Options) :-
    option(base(Base), Options),
    print_message(error, sasp_error(Error, Pos, Base)).

%!  comma_list(+BodyTerm, +Pos, -BodyList, -PosList) is det.
%
%   Translate a conjunction  into  a   list,  also  translating possible
%   position information.

comma_list(Body, Pos, BodyList, PosList) :-
    comma_list(Body, Pos, BodyList, [], PosList, []).

comma_list(Term, PPos, TL0, TL, PL0, PL) :-
    nonvar(PPos),
    PPos = parentheses_term_position(_,_,Pos),
    !,
    comma_list(Term, Pos, TL0, TL, PL0, PL).
comma_list((A,B), term_position(_,_,_,_,[AP, BP]), TL0, TL, PL0, PL) :-
    !,
    comma_list(A, AP, TL0, TL1, PL0, PL1),
    comma_list(B, BP, TL1, TL,  PL1, PL).
comma_list(One, Pos, [One|TL], TL, [Pos|PL], PL).

