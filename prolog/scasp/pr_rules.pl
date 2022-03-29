/*
* Copyright (c) 2016, University of Texas at Dallas
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the University of Texas at Dallas nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY OF TEXAS AT DALLAS BE LIABLE FOR
* ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

:- module(scasp_pr_rules,
          [ generate_pr_rules/2,        % +Sources, +Options
            process_pr_pred/5,          % +Spec, -Atom, -Children, -Cond, -Human
            clean_pr_program/1          % +Module
          ]).
:- use_module(modules).

/** <module> Output formatting and printing.

Predicates related to formatting and printing output. This includes predicates
that may be used for warning and error output.

@author Kyle Marple
@version 20170510
@license BSD-3
*/

:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(option)).

:- use_module(common).
:- use_module(program).
:- use_module(variables).

:- op(950, xfx, ::).

:- meta_predicate
    generate_pr_rules(:, +).

% fail,warning,error
:- create_prolog_flag(scasp_unknown, warning, [keep(true)]).

%!  format_term(+EntryIn:compound, -EntryOut:compound,
%!              -Constraints:list, +Vars:compound) is det
%
%   Format a term for printing.
%
%   @arg EntryIn The initial entry.
%   @arg EntryOut The formatted entry.
%   @arg Constraints Any constraints on variables in the entry.
%   @arg Vars Variable struct for filling in values.

format_term(X, X) :-
    is_var(X),				% constrained var
    !.
format_term(X, Xo) :-
    callable(X),
    !,
    format_predicate2(X, Xo).
format_term(X, X).		% anything else, just pass along

%!  format_term_list(+ListIn:compound, -ListOut:compound) is det
%
%   Format each term in a list.
%
%   @arg ListIn The initial list.
%   @arg ListOut The formatted list.

format_term_list([], []).
format_term_list([X|T], [X2|T2]) :-
    format_term(X, X2),
    format_term_list(T, T2).

%!  format_predicate2(+EntryIn:compound, -EntryOut:compound) is det
%
%   Given a term, remove the  arity,   strip  any  prefixes, and process
%   arguments.
%
%   @arg EntryIn The initial entry.
%   @arg EntryOut The formatted entry.

format_predicate2(Xi, Xo) :-
    Xi = [_|_], % list
    !,
    format_predicate3(Xi, Xo).
format_predicate2(Xi, Xo) :-
    predicate(Xi, X2, A),
    atom(X2), % compound term, predicate or atom
    !,
    split_functor(X2, X3, _), % strip arity
    strip_prefixes(X3, X4),
    format_predicate3(A, A2),
    (   X4 = not(Xn) % append args
    ->  Xn2 =.. [Xn|A2],
        Xo = not(Xn2)
    ;   Xo =.. [X4|A2]
    ).
format_predicate2(Xi, Xo) :-
    Xi =.. [X2|A], % compound term, but not a predicate or atom head
    !,
    format_predicate3(A, A2),
    Xo =.. [X2|A2].
format_predicate2(X, X).

%!  format_predicate3(+ArgsIn:list, -ArgsOut:list) is det
%
%   Process a list of predicate args or variable constraints.
%
%   @arg ArgsIn Input args.
%   @arg ArgsOut Output args.

format_predicate3([], []) :-
    !.
format_predicate3([X|T], [Y|T2]) :-
    !,
    format_predicate4(X, Y),
    format_predicate3(T, T2).
format_predicate3(X, Y) :-
    format_predicate4(X, Y).

%!  format_predicate4(+ArgsIn:list, -ArgsOut:list) is det
%
%   Process  a  single  predicate  arg  or  variable  constraint.  Where
%   possible, substitute previously used variables   for  variables with
%   the same value ID. This makes the final output more readable.
%
%   @arg ArgIn Input arg.
%   @arg ArgOut Output arg.

format_predicate4(X, X) :-
    is_var(X),
    !.
format_predicate4(Xi, Xo) :-
    format_predicate2(Xi, Xo).

%!  strip_prefixes(+FunctorIn:atom, -FunctorOut:atom) is det
%
%   Strip any reserved prefixes added during processing. If appropriate,
%   modify the output functor to restore   formatting represented by the
%   prefix. To ensure that prefixes  added   by  the user remain intact,
%   this predicate will return after removing a single copy of the dummy
%   prefix, if encountered. Otherwise,  all   reserved  prefixes will be
%   stripped. To prevent errors,  the  dual   rule  prefix,  if present,
%   should always be first.
%
%   @arg FunctorIn Input functor
%   @arg FunctorOut Output functor

strip_prefixes(Fi, Fo) :-
    atom_concat(c_, F1, Fi),		% 'c_': classical negation
    !,
    strip_prefixes(F1, F2),
    atom_concat(-, F2, Fo).
strip_prefixes(Fi, not(Fo)) :-
    atom_concat(n_, F1, Fi),		% 'n_': classical negation
    !,
    strip_prefixes(F1, Fo).
strip_prefixes(Fi, Fo) :-
    has_prefix(Fi, C),
    C \== 'd',				% non-dummy prefix
    !,
    sub_atom(Fi, 2, _, 0, F1),
    strip_prefixes(F1, Fo).
strip_prefixes(Fi, Fo) :-
    atom_concat(d_, Fo, Fi),		% 'd_': dummy prefix, remove and finish
    !.
strip_prefixes(Fi, Fo) :-		% '_c_' prefixes change to 'o_'
    atom_concat('_c_', Fc, Fi),
    !,
    atom_concat('o_-', Fc, Fo).
strip_prefixes(Fi, Fo) :-		% '_' prefixes change to 'o_'
    atom_concat('_', Fc, Fi),
    !,
    atom_concat('o_', Fc, Fo).
strip_prefixes(F, F).

%!  generate_pr_rules(:Sources, +Options)
%
%   Translate the sCASP program from the  defined_* predicates into pr_*
%   predicates  for  sCASP.  It  creates    clauses  for  the  following
%   predicates in the target module:
%
%     - pr_rule/3
%     - pr_query/1
%     - pr_user_predicate/1
%     - pr_table_predicate/1
%     - pr_show_predicate/1
%     - pr_pred_predicate/4.

:- det(generate_pr_rules/2).

generate_pr_rules(M:_Sources, Options) :-
    check_existence(Options),
    findall(O-R, (defined_rule(_, H, B, O), c_rule(R, H, B)), Rs),
    maplist([O-R, O, R]>>true, Rs, Origins, Rs1),
    format_term_list(Rs1,Rs2),
    maplist([O, H-B, c(O, H, B)]>>true, Origins, Rs2, Rs3),
    (   defined_nmr_check(NMR)
    ->  format_term_list(NMR, NMR2)
    ;   NMR2 = []
    ),
    (   defined_query(Q,_),
        format_term_list(Q,Q2),
        assert_pr_query(M:Q2)
    ->  true
    ;   true
    ),
    handle_table_directives(M),
    handle_show_directives(M),
    handle_pred_directives(M),
    assert_pr_rules(Rs3, M),
    assert_pr_rules([c(generated(nmr), 'global_constraints', NMR2)], M).

:- det((handle_table_directives/1,
        handle_show_directives/1,
        handle_pred_directives/1)).

handle_table_directives(M) :-
    findall(T, defined_directive(table(T)), Ts),
    format_term_list(Ts, Ts2),
    assert_pr_table(Ts2, M).

handle_show_directives(M) :-
    findall(S, defined_directive(show(S)), Ss),
    format_term_list(Ss, Ss2),
    assert_pr_show(Ss2, M).

handle_pred_directives(M) :-
    findall(P, defined_directive(pred(P)), Ps),
    format_term_list(Ps, Ps2),
    assert_pr_pred(Ps2, M).

%!  assert_pr_table(+Tabled) is det.

assert_pr_table([], _) => true.
assert_pr_table([H|T], M) =>
    assert_pr_table(H, M),
    assert_pr_table(T, M).
assert_pr_table((H,T), M) =>
    assert_pr_table(H, M),
    assert_pr_table(T, M).
assert_pr_table(Name/Arity, M) =>
    assert(M:pr_table_predicate(Name/Arity)).

%!  assert_pr_show(+Atoms) is det.

assert_pr_show([], _) => true.
assert_pr_show([H|T], M) =>
    assert_pr_show(H, M),
    assert_pr_show(T, M).
assert_pr_show((H,T), M) =>
    assert_pr_show(H, M),
    assert_pr_show(T, M).
assert_pr_show(not(Name/Arity), M) =>
    functor(T, Name, Arity),
    assert(M:pr_show_predicate(not(T))).
assert_pr_show(Name/Arity, M) =>
    functor(T, Name, Arity),
    assert(M:pr_show_predicate(T)).

%!  process_pr_pred(+PredDecl) is det.

assert_pr_pred([], _) => true.
assert_pr_pred([H|T], M) =>
    assert_pr_pred(H, M),
    assert_pr_pred(T, M).
assert_pr_pred((H,T), M) =>
    assert_pr_pred(H, M),
    assert_pr_pred(T, M).
assert_pr_pred(T, M) =>
    process_pr_pred(T, Atom, Children, Cond, Human),
    assert(M:pr_pred_predicate(Atom, Children, Cond, Human)).

%!  process_pr_pred(+Spec, -Atom, -Children, -Condition, -Human) is det.
%
%   Process a ``#pred Spec :: Template.`` directive.
%
%   @arg Spec is a term Head::Template,  where   Head  is  an sCASP atom
%   where the variables are represented  as   $(Name)  and Template is a
%   string that embeds "@(Var)", "@(Var:Type)", "{{Var}}" or
%   "{{Var:Type}}"
%   @arg Pred is a term `Head::format(Fmt, Args)`, where `Fmt` contains
%   ~p and the arguments are of the shape `@($(Var):Type)`, which is
%   printed as ``"<Var>, a <Type>"``

:- det(process_pr_pred/5).

process_pr_pred(Spec::B, A, Children, Cond, format(Fmt,Args)) :-
    atom_codes(B, Chars),
    phrase(pr_pred(FmtChars, Args, Spec, Spec1), Chars),
    atom_codes(Fmt, FmtChars),
    revar(Spec1, Spec2, _),             % need for s(CASP) input with vars
    atom_cond(Spec2, A, Children, Cond).   % not in template

pr_pred([0'~,0'p|Fmt], [@(Var:Type)|Args], A0, A) -->
    temp_var_start(Style), prolog_var_name(VarName),
    { insert_var(A0, A1, VarName, Var) },
    (   ":"
    ->  (   string(TypeChars), temp_var_end(Style)
        ->  {atom_codes(Type, TypeChars)}
        )
    ;   temp_var_end(Style)
    ->  {Type = ''}
    ),
    !,
    pr_pred(Fmt, Args, A1, A).
pr_pred([H|T], Args, A0, A) -->
    [H],
    !,
    pr_pred(T, Args, A0, A).
pr_pred([], [], A, A) -->
    [].

temp_var_start(classic) --> "@(".
temp_var_start(modern)  --> "{{".

temp_var_end(classic) --> ")".
temp_var_end(modern)  --> "}}".

%!  insert_var(+TermIn, -Term, +VarName, +Var) is det.

insert_var($(Name), Repl, Name, Var) =>
    Repl = Var.
insert_var('$VAR'(Name), Repl, Name, Var) =>
    Repl = Var.
insert_var(V0, Repl, Name, Var), var(V0) =>
    (   prolog_load_context(variable_names, Bindings),
        member(Name = V1, Bindings),
        V0 == V1
    ->  Repl = Var
    ;   Repl = V0
    ).
insert_var(Name, Repl, Name, Var) =>
    Repl = Var.
insert_var(In, Out, Name, Var), compound(In) =>
    In =.. [F|Args0],
    maplist(insert_var_r(Name, Var), Args0, Args),
    Out =.. [F|Args].
insert_var(In, Out, _, _) =>
    Out = In.

insert_var_r(Name, Var, In, Out) :-
    insert_var(In, Out, Name, Var).

%!  atom_cond(+Spec, -Atom, -Children, -Condition) is det.

:- det(atom_cond/3).

atom_cond(Atom0-Children0, Atom, Children, Cond) =>
    atom_cond(Atom0, Atom, Cond0),
    atom_cond_list(Children0, Children, Cond0, Cond).
atom_cond(Atom0, Atom, Children, Cond) =>
    Children = '*',
    atom_cond(Atom0, Atom, Cond).

atom_cond((Atom0,Cond0), Atom, Cond) =>
    Atom = Atom0,
    inline_cond(Cond0, Cond).
atom_cond(Atom0, Atom, Cond) =>
    Atom = Atom0,
    Cond = true.

atom_cond_list([], [], Cond, Cond).
atom_cond_list([H0|T0], [H|T], Cond0, Cond) :-
    atom_cond(H0, H, Cond1),
    mkconj(Cond0, Cond1, Cond2),
    atom_cond_list(T0, T, Cond2, Cond).

inline_cond(var(X), Cond) =>
    Cond = (var(X)->true;X = '$VAR'(_)).
inline_cond(nonvar(X), Cond) =>
    Cond = (var(X)->false;X \= '$VAR'(_)).
inline_cond((C0,C1), Cond) =>
    inline_cond(C0, D0),
    inline_cond(C1, D1),
    mkconj(D0,D1, Cond).
inline_cond(C, Cond) =>
    Cond = C.


%!  assert_pr_rules(+Rules:list, +Module) is det.

assert_pr_rules([], _).
assert_pr_rules([c(Origin, Head, Body)|Rs], M) :-
    !,
    revar(Head-Body,H-B, _),
    assert(M:pr_rule(Origin, H, B)),
    assert_pr_user_predicate([H], M),
    assert_pr_rules(Rs, M).


assert_pr_query(M:Q) :-
    assert(M:pr_query(Q)).

assert_pr_user_predicate([], _).
assert_pr_user_predicate([P|Ps], M) :-
    functor(P, Name, La),
    (   M:pr_user_predicate(Name/La)
    ->  true
    ;   assert(M:pr_user_predicate(Name/La))
    ),
    assert_pr_user_predicate(Ps, M).


%!  check_existence(+Options)
%
%   Check that all referenced predicates are defined.

check_existence(Options) :-
    current_prolog_flag(scasp_unknown, DefaultMode),
    option(unknown(Mode), Options, DefaultMode),
    (   Mode == fail
    ->  true
    ;   defined_predicates(Preds),
        exclude(defined, Preds, Undef0),
        maplist(scasp_pred_pi, Undef0, Undef),
        (   Undef == []
        ->  true
        ;   Mode == warning
        ->  maplist(report_undef, Undef)
        ;   throw(error(scasp_undefined(Undef), _))
        )
    ).

defined('_false_0').
defined(true_0).
defined(false_0).
defined(Name) :-
    defined_rule(Name, _, _, _).

scasp_pred_pi(DecoratedName, Name/Arity) :-
    split_functor(DecoratedName, PrefixedName, Arity),
    strip_prefixes(PrefixedName, Name).

report_undef(PI) :-
    scasp_is_defined(PI),
    !.
report_undef(PI) :-
    print_message(warning,
                  error(existence_error(scasp_predicate, PI), _)).

scasp_is_defined(QName/Arity) :-
    encoded_module_term(M:Name, QName),
    !,
    (   raise_negation(Name, -TheName)
    ->  functor(Head, TheName, Arity),
        predicate_property(M:Head, defined)
    ;   functor(Head, Name, Arity),
        predicate_property(M:Head, defined)
    ).

%!  clean_pr_program(+Module) is det.
%
%   Prepare Module to receive a  compiled   sCASP  program. This wipes a
%   possibly existing sCASP program. It also   relies on the side effect
%   of retractall/1 to create a non-existing predicate as _dynamic_.

clean_pr_program(M) :-
    retractall(M:pr_query(_)),
    retractall(M:pr_rule(_,_,_)),
    retractall(M:pr_user_predicate(_)),
    retractall(M:pr_table_predicate(_)),
    retractall(M:pr_show_predicate(_)),
    retractall(M:pr_pred_predicate(_,_,_,_)),
    retractall(M:pr_dcc_predicate(_,_)).
