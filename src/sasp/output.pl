:- module(output, [
                    format_term/4,
                    format_term_list/4,
                    print_chs/3,
                    rb_visit_to_list/3,
                    fill_in_variable_values/5,
                    print_vars/2,
                    print_var_constraints/1,
                    indent/1,
                    print_justification/1,
                    print_html/2,
                    generate_pr_rules/1,
                    pr_rule/2,
                    pr_query/1,
                    pr_user_predicate/1,
                    pr_table_predicate/1,
                    pr_show_predicate/1,
                    pr_pred_predicate/1,
                    revar/2,
%                       refunct/3,
                    print_abducibles/2
                 ]).

/** <module> Output formatting and printing.

Predicates related to formatting and printing output. This includes predicates
that may be used for warning and error output.

@author Kyle Marple
@version 20170510
@license BSD-3
*/

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

:-set_prolog_flag(multi_arity_warnings,off).

:- use_module(library(lists)).
:- use_module(engine(basic_props)).
:- use_module(rbtrees).
%:- use_module(library(writef)).
:- use_module(ciao_auxiliar).
:- use_module(chs).
:- use_module(common).
:- use_module(options).
:- use_module(program).
:- use_module(io).
:- use_module(variables).

%! format_term(+EntryIn:compound, -EntryOut:compound, -Constraints:list, +Vars:compound) is det
% Format a term for printing.
%
% @param EntryIn The initial entry.
% @param EntryOut The formatted entry.
% @param Constraints Any constraints on variables in the entry.
% @param Vars Variable struct for filling in values.
format_term(X, X, Con, V) :-
    is_unbound(X, V, Con, _, _), % constrained var
    !.
format_term(X, Xo, Con, V) :-
    is_var(X),
    var_value(X, V, val(X2)), % bound var
    !,
    format_term(X2, Xo, Con, V).
format_term(X, Xo, Con, V) :-
    X =.. [_ | _], % non-var
    !,
    format_predicate(X, Xo, Con, [], _, V).
format_term(X, X, [], _) :- % anything else, just pass along
    !.

%! format_term_list(+ListIn:compound, -ListOut:compound, -Constraints:list, +Vars:compound) is det
% Format each term in a list.
%
% @param ListIn The initial list.
% @param ListOut The formatted list.
% @param Constraints The list of constraints. MAY CONTAIN DUPLICATES!
% @param Vars Variable struct for filling in values.
format_term_list([X | T], [X2 | T2], Con, V) :-
    format_term(X, X2, C, V),
    !,
    format_term_list(T, T2, Ct, V),
    append(C, Ct, Con).
format_term_list([], [], [], _).

%! print_chs(+CHS:list, +Vars:compound, +Flag:int) is det
% Print the CHS, removing internally added information and formatting.
% Flag indicates the print mode: 0 for normal (positive and negative printing
% literals), 1 for positive literals only, 2 for all (including non-printing)
% literals.
%
% @param CHS The CHS to print.
% @param Vars A variable struct to get bindings for each variable.
% @param Flag An integer 0, 1 or 2 indicating the print mode.
print_chs(CHS, V, Flag) :-
    rb_visit(CHS, CHS2),
    rb_visit_to_list(CHS2, [], CHS3),
    format_chs(CHS3, CHS4, V, Flag),
    !,
    print_chs2(CHS4),
    !.
print_chs(_, _, _) :-
    write_error('could not print CHS'),
    !,
    fail.        

%! rb_visit_to_list(+RBvisit:list, +ListIn:list, -ListOut:list) is det
% Given the results of rb_visit/2, strip the keys from each member.
%
% @param RBvist A Red-Black tree visit.
% @param ListIn Input list.
% @param List Output list.
rb_visit_to_list([-(_, X) | T], Li, Lo) :-
    append(X, Li, L2),
    !,
    rb_visit_to_list(T, L2, Lo).
rb_visit_to_list([], L, L) :-
    !.

%! print_chs2(+CHS:list) is det
% Print the formatted CHS produced by format_chs/4, accounting for negated
% literals.
%
% @param CHS The formatted CHS.
print_chs2([X | T]) :-
    write('{ '),
    X = -(X2, Con),
    (X2 = not(X3) -> % print negation properly
            writef('not ~w', [X3])
    ;
            writef('~w', [X2])
    ),
    (Con \= [] ->
            write(' ( '),
            print_var_constraints(Con),
            write(' )')
    ;
            true
    ),
    !,
    print_chs3(T),
    write(' }').
print_chs2([]) :-
    write('{ }').

%! print_chs3(+CHS:list) is det
% Print the rest of the formatted CHS.
%
% @param CHS The CHS to print.
print_chs3([X | T]) :-
    X = -(X2, Con),
    (X2 = not(X3) -> % print negation properly
            writef(', not ~w', [X3])
    ;
            writef(', ~w', [X2])
    ),
    (Con \= [] ->
            write(' ( '),
            print_var_constraints(Con),
            write(' )')
    ;
            true
    ),
    !,
    print_chs3(T).
print_chs3([]) :-
    !.

% format_chs(+CHS:list, -PrettyCHS:list, +Vars:compound, +Flag:int) is det
% Format CHS entries for printing. Leave negated literals wrapped in not(), to
% be handled later. Flag indicates the print mode: 0 for normal (positive and
% negative printing literals), 1 for positive literals only, 2 for all
% (including non-printing) literals.
%
% @param CHS The CHS.
% @param PrettyCHS The formatted CHS.
% @param Vars The variable struct used to fill in values.
% @param Flag An integer 0, 1 or 2 indicating the print mode.
format_chs(CHSi, CHSo, V, F) :-
    F \= 2,
    once(get_next_printable(CHSi, CHS2)),
    CHS2 \= [],
    !,
    format_chs2(CHS2, CHS3, [], _, V, F), % Reduce number of unique vars in CHS
    sort_chs(CHS3, CHS4), % sort entries by literal and then constraints
    divide_chs(CHS4, CHSp, CHSn),
    (F = 0 ->
            append(CHSp, CHSn, CHSo) % print pos and neg, puttting negative entries after positive ones.
    ;
            CHSo = CHSp % print positive literals only
    ).
format_chs(CHSi, CHSo, V, F) :-
    CHSi \= [],
    F = 2,
    !,
    format_chs2(CHSi, CHS2, [], _, V, F), % Reduce number of unique vars in CHS
    sort_chs(CHS2, CHS3), % sort entries by literal and then constraints
    divide_chs(CHS3, CHSp, CHSn),
    append(CHSp, CHSn, CHSo). % put negative entries after positive ones.
format_chs(_, [], _, _). % CHS is either empty or contains no printable literals

% format_chs2(+CHS:list, -PrettyCHS:list, +UsedVarsIn:list, +UsedVarsOut:list, +Vars:compound, +Flag:int) is det
% Format CHS entries for printing. Leave negated literals wrapped in not(), to
% be handled later. Attach constraints. Flag indicates the print mode: 0 for
% normal (positive and negative printing literals), 1 for positive literals
% only, 2 for all (including non-printing) literals.
%
% @param CHS The CHS.
% @param PrettyCHS The formatted CHS. Members will be of the form
%        -(Literal, Constraints).
% @param UsedVarsIn Input used vars. Format for each is =|-(value_id, var)|=.
% @param UsedVarsOut Output used vars.
% @param Vars The variable struct used to fill in values.
% @param Flag An integer 0, 1 or 2 indicating the print mode.
format_chs2([X | T], [Y | T2], Uvi, Uvo, V, F) :-
    F \= 2,
    format_chs_entry(X, X2, Con, Uvi, Uv1, V),
    Y = -(X2, Con),
    get_next_printable(T, T3),
    !,
    format_chs2(T3, T2, Uv1, Uvo, V, F).
format_chs2([X | T], [Y | T2], Uvi, Uvo, V, F) :-
    F = 2,
    !,
    format_chs_entry(X, X2, Uvi, Uv1, Con, V),
    Y = -(X2, Con),
    !,
    format_chs2(T, T2, Uv1, Uvo, V, F).
format_chs2([], [], Uv, Uv, _, _).

%! format_chs_entry(+EntryIn:compound, -EntryOut:compound, -Constraints:list, +UsedVarsIn:list, +UsedVarsOut:list, +Vars:compound) is det
% Format a CHS entry for printing.
%
% @param EntryIn The initial entry.
% @param EntryOut The formatted entry.
% @param Constraints Any constraints on variables in the entry.
% @param UsedVarsIn Input used vars. Format for each is =|-(value_id, var)|=.
% @param UsedVarsOut Output used vars.
% @param Vars Variable struct for filling in values.
format_chs_entry(X, Xo, Con, Uvi, Uvo, V) :-
    chs_entry(X, X2, A, _, _),
    X3 =.. [X2 | A],
    format_predicate(X3, Xo, Con, Uvi, Uvo, V).

%! format_predicate(+EntryIn:compound, -EntryOut:compound, -Constraints:list, +UsedVarsIn:list, +UsedVarsOut:list, +Vars:compound) is det
% Given a term, call format_predicate2/5, fill in variable values and
% process constraints.
%
% @param EntryIn The initial entry.
% @param EntryOut The formatted entry.
% @param Constraints Any constraints on variables in the entry.
% @param UsedVarsIn Input used vars. Format for each is =|-(value_id, var)|=.
% @param UsedVarsOut Output used vars.
% @param Vars Variable struct for filling in values.
format_predicate(X, Xo, Con, Uvi, Uvo, V) :-
    fill_in_variable_values(X, X2, [], _, V), % fill in bound vars; ignore constraints for now.
    format_predicate2(X2, X3, Uvi, Uv2, V),
    fill_in_variable_values(X3, Xo, [], Con2, V), % get any constraints.
    format_predicate3(Con2, Con3, Uv2, Uvo, V), % format any terms in constraints
    sort(Con3, Con).

%! format_predicate2(+EntryIn:compound, -EntryOut:compound, +UsedVarsIn:list, +UsedVarsOut:list, +Vars:compound) is det
% Given a term, remove the arity, strip any prefixes, and process arguments.
%
% @param EntryIn The initial entry.
% @param EntryOut The formatted entry.
% @param UsedVarsIn Input used vars. Format for each is =|-(value_id, var)|=.
% @param UsedVarsOut Output used vars.
% @param Vars Variable struct for filling in values.
format_predicate2(Xi, Xo, Uvi, Uvo, V) :-
    Xi = [_ | _], % list
    !,
    format_predicate3(Xi, Xo, Uvi, Uvo, V).
format_predicate2(Xi, Xo, Uv, Uv, _) :-
    predicate(Xi, X2, []),
    atom(X2), % compound term, predicate or atom
    atom_chars(X2, ['\'' | X3]), % quoted string; strip outermost quotes and arity
    %% reverse(X3, ['0', '_', '\'' | X4]),
    %% reverse(X4, X5),
    reverse(X3, ['0', '_' | X4]),
    reverse(X4, X5),
    atom_chars(Xo, ['\'' | X5]).
format_predicate2(Xi, Xo, Uvi, Uvo, V) :-
    predicate(Xi, X2, A),
    atom(X2), % compound term, predicate or atom
    !,
    split_functor(X2, Xc, _), % strip arity
    atom_chars(X3, Xc),
    strip_prefixes(X3, X4),
    format_predicate3(A, A2, Uvi, Uvo, V),
    (X4 = not(Xn) -> % append args
            Xn2 =.. [Xn | A2],
            Xo = not(Xn2)
    ;
            Xo =.. [X4 | A2]
    ).
format_predicate2(Xi, Xo, Uvi, Uvo, V) :-
    Xi =.. [X2 | A], % compound term, but not a predicate or atom head
    !,
    format_predicate3(A, A2, Uvi, Uvo, V),
    Xo =.. [X2 | A2].
format_predicate2(X, X, Uv, Uv, _) :-
    !. % not a predicate or atom

%! format_predicate3(+ArgsIn:list, -ArgsOut:list, +UsedVarsIn:list, +UsedVarsOut:list, +Vars:compound) is det
% Process a list of predicate args or variable constraints.
%
% @param ArgsIn Input args.
% @param ArgsOut Output args.
% @param UsedVarsIn Input used vars. Format for each is =|-(value_id, var)|=.
% @param UsedVarsOut Output used vars.
% @param Vars Variable struct for filling in values.
format_predicate3([X | T], [Y | T2], Uvi, Uvo, V) :-
    format_predicate4(X, Y, Uvi, Uv1, V),
    !,
    format_predicate3(T, T2, Uv1, Uvo, V).
format_predicate3(X, Y, Uvi, Uvo, V) :-
    X \= [_ | _],
    X \= [], % can occur if we have a list with an unbound tail
    format_predicate4(X, Y, Uvi, Uvo, V),
    !.
format_predicate3([], [], Uv, Uv, _).

%! format_predicate4(+ArgsIn:list, -ArgsOut:list, +UsedVarsIn:list, +UsedVarsOut:list, +Vars:compound) is det
% Process a single predicate arg or variable constraint. Where possible,
% substitute previously used variables for variables with the same value ID.
% This makes the final output more readable.
%
% @param ArgIn Input arg.
% @param ArgOut Output arg.
% @param UsedVarsIn Input used vars. Format for each is =|-(value_id, var)|=.
% @param UsedVarsOut Output used vars.
% @param Vars Variable struct for filling in values.
format_predicate4(Xi, Xo, Uv, Uv, V) :-
    is_var(Xi),
    atom_chars(Xi, ['?' | Gt]), % get ID without flag
    atom_chars(X2, Gt),
    get_value_id(X2, ID, V),
    member(-(ID, Xo), Uv), % Var with same value already selected.
    !.
format_predicate4(Xi, Xo, Uv, Uv, V) :-
    get_value_id(Xi, ID, V),
    member(-(ID, Xo), Uv), % Var with same value already selected.
    !.
format_predicate4(X, X, Uv, [-(ID, X) | Uv], V) :-
    is_var(X),
    atom_chars(X, ['?' | Gt]), % get ID without flag
    atom_chars(X2, Gt),
    get_value_id(X2, ID, V),
    !.
format_predicate4(X, X, Uv, [-(ID, X) | Uv], V) :-
    get_value_id(X, ID, V),
    !.
format_predicate4(X, X, Uv, Uv, _) :- % variable without ID in V
    is_var(X),
    !.
format_predicate4(Xi, Xo, Uvi, Uvo, V) :-
    !, % non variable
    format_predicate2(Xi, Xo, Uvi, Uvo, V).

%! get_next_printable(+CHSin:compound, -CHSout:compound) is det
% Get the next CHS entry to print, skipping any non-printing literals (starting
% with an underscore). Additionally, skip literals added by the NMR check if the
% appropriate option is set.
%
% @param CHSin Input CHS.
% @param CHSout Output CHS.
get_next_printable([X | T], CHS) :-
    user_option(hide_nmr, true),
    chs_entry(X, _, _, _, 1), % nmr check literal
    !,
    get_next_printable(T, CHS).
get_next_printable([X | T], CHS) :-
    chs_entry(X, 'abducible_1', _, _, _), % Abducible entry; skip it.
    !,
    get_next_printable(T, CHS).
get_next_printable([X | T], CHS) :-
    chs_entry(X, X2, _, _, _),
    strip_prefixes(X2, X3),
    (
            X3 = not(X4)
    ;
            X3 \= not(_),
            X4 = X3
    ),
    (
        atom_chars(X4, ['o', '_' | _])
    ;
        atom_chars(X4, ['_' | _])   % non-printing literal; skip it.
    ),
    !,
    get_next_printable(T, CHS).
get_next_printable([X | T], [X | T2]) :-
    !,
    get_next_printable(T, T2).
get_next_printable([], []) :-
    !.

%! divide_chs(+CHSin:compound, -CHSpos:list, -CHSneg:list) is det
% Split the CHS by negative and positive literals. Input should be sorted by
% literal. Each output will remain sorted by literal.
divide_chs([X | T], P, [X | N]) :-
    X = -(not(_), _), % negated literal
    !,
    divide_chs(T, P, N).
divide_chs([X | T], [X | P], N) :-
    X \= -(not(_), _), % positive literal
    !,
    divide_chs(T, P, N).
divide_chs([], [], []).

%! strip_prefixes(+FunctorIn:ground, -FunctorOut:ground) is det
% Strip any reserved prefixes added during processing. If appropriate, modify
% the output functor to restore formatting represented by the prefix. To ensure
% that prefixes added by the user remain intact, this predicate will return
% after removing a single copy of the dummy prefix, if encountered. Otherwise,
% all reserved prefixes will be stripped. To prevent errors, the dual rule
% prefix, if present, should always be first.
%
% @param FunctorIn Input functor
% @param FunctorOut Output functor
strip_prefixes(Fi, Fo) :-
    has_prefix(Fi, 'c'), % classical negation
    atom_chars(Fi, ['c', '_' | Fc]),
    atom_chars(F1, Fc),
    !,
    strip_prefixes(F1, F2),
    atom_chars(F2, Fc2),
    atom_chars(Fo, ['-' | Fc2]). % restore negation
strip_prefixes(Fi, not(Fo)) :-
    has_prefix(Fi, 'n'), % negation
    atom_chars(Fi, ['n', '_' | Fc]),
    atom_chars(F1, Fc),
    !,
    strip_prefixes(F1, Fo).
strip_prefixes(Fi, Fo) :-
    has_prefix(Fi, C),
    C \= 'd', % non-dummy prefix
    atom_chars(Fi, [C, '_' | Fc]),
    atom_chars(F1, Fc),
    !,
    strip_prefixes(F1, Fo).
strip_prefixes(Fi, Fo) :-
    has_prefix(Fi, 'd'), % dummy prefix, remove and finish
    atom_chars(Fi, ['d', '_' | Fc]),
    atom_chars(Fo, Fc),
    !.
strip_prefixes(Fi, Fo) :- % '_' prefixes change to 'o_'
    atom_chars(Fi, ['_','c','_' | Fc]),
    atom_chars(Fo, ['o', '_','-' | Fc]),
    !.
strip_prefixes(Fi, Fo) :- % '_' prefixes change to 'o_'
    atom_chars(Fi, ['_' | Fc]),
    atom_chars(Fo, ['o', '_' | Fc]),
    !.
strip_prefixes(F, F) :- % no prefixes
    !.

%! sort_chs(+CHSin:compound, -CHSout:compound) is det
% Sort entries in the formatted CHS by functor. If functors match, use =|@<|= to
% compare the entries. Basically a modified merge sort.
%
% @param CHSin The unsorted CHS. Entries of the form -(Predicate, Constraints),
%              where Predicate may be wrapped in a not().
% @param CHSout The sorted CHS.
sort_chs([], []).
sort_chs([X], [X]).
sort_chs(Ci, Co) :-
    length(Ci, L),
    L2 is L // 2,
    split_chs(Ci, Ca, Cb, L2),
    sort_chs(Ca, Ca2),
    sort_chs(Cb, Cb2),
    merge_chs(Ca2, Cb2, Co).

%! split_chs(+CHSi:list, -CHSl:list, -CHSr:list, +Count:int) is det
% Split the CHS (or any other list) into two lists: one with Count elements and
% one with the remainder of the list.
%
% @param CHSi The input CHS.
% @param CHSl The left CHS. Will contain Count elements.
% @param CHSr The right CHS. Will contain the remainder of the list.
% @param Count The number of elements to go in the left list.
split_chs([X | T], [X | T2], R, N) :-
    N > 0,
    !,
    N1 is N - 1,
    split_chs(T, T2, R, N1).
split_chs(R, [], R, 0) :-
    !.
split_chs([], [], [], _) :-
    !.

%! merge_chs(+CHSa:list, +CHSb:list, -CHSc:list) is det
% Merge sorted CHSes A and B into sorted CHS C.
%
% @param CHSa First sorted list.
% @param CHSb Second sorted list.
% @param CHSc Output sorted list.
merge_chs(X, [], X) :-
    !.
merge_chs([], X, X) :-
    !.
merge_chs([X | T], [Y | T2], [X | T3]) :-
    chs_lte(X, Y), % X =< Y
    !,
    merge_chs(T, [Y | T2], T3).
merge_chs(X, [Y | T2], [Y | T3]) :-
    !,
    merge_chs(X, T2, T3).

%! chs_lte(+A:compound, +B:compound) is det
% Compare two CHS entries. First, check functors, disregarding any not()
% wrappers. If functors match, compare the entries with =|@=<|=.
chs_lte(-(not(A), _), -(not(B), _)) :-
    !,
    chs_lte(-(A, _), -(B, _)).
chs_lte(-(not(A), _), -(B, _)) :-
    !,
    chs_lte(-(A, _), -(B, _)).
chs_lte(-(A, _), -(not(B), _)) :-
    !,
    chs_lte(-(A, _), -(B, _)).
chs_lte(-(A, _), -(B, _)) :-
    A \= not(_),
    B \= not(_),
    A =.. [Fa | _],
    B =.. [Fb | _],
    Fa @< Fb, % clear less than, else check args below
    !.
chs_lte(-(A, _), -(B, _)) :-
    A \= not(_),
    B \= not(_),
    A =.. [F | _],
    B =.. [F | _], % functors match
    A @=< B,
    !.

%! fill_in_variable_values(+GoalIn:compound, -GoalOut:compound, +ConstraintsIn:list, -ConstraintsOut:list, +Vars:compound) is det
% Given a goal, replace any bound variables with their values. If a goal or
% value is a compound term, process each arg. For constrained variables, store
% the variable/constraints pair in Constraints. NOTE: Must never return
% constraint entries with empty constraint lists!
%
% @param GoalIn Input goal.
% @param GoalOut Output goal.
% @param ConstraintsIn Input constraints.
% @param ConstraintsOut Output constraints.
% @param VarsOut Output vars.
fill_in_variable_values(G, G, C, C, V) :-
    is_unbound(G, V, [], _, Vl), % unbound variable, no constraints
    Vl =\= 1,
    !.
fill_in_variable_values(Gi, Go, Ci, Co, V) :-
    is_unbound(Gi, V, [], _, 1), % unbound variable, no constraints, flagged
    !,
    atom_chars(Gi, Gc),
    (Gc = ['?' | Gt] -> % don't duplicate flag
            Go = Gi,
            atom_chars(G, Gt),
            ((is_unbound(G, V, Con, _, 1), Con \= []) -> % flag added in last pass, get correct constraints
                    Co = [-(Go, Con) | Ci]
            ;
                    Co = Ci
            )
    ;
            atom_chars(Go, ['?' | Gc]), % add flag
            Co = Ci
    ).
fill_in_variable_values(G, G, C, [-(G, Con) | C], V) :-
    is_unbound(G, V, Con, _, Vl), % unbound variable, constraints
    Vl =\= 1,
    !.
fill_in_variable_values(Gi, Go, C, [-(Go, Con) | C], V) :-
    is_unbound(Gi, V, Con, _, 1), % unbound variable, constraints, flagged
    !,
    atom_chars(Gi, Gc),
    (Gc = ['?' | _] -> % don't duplicate flag
            Go = Gi
    ;
            atom_chars(Go, ['?' | Gc]) % add flag
    ).
fill_in_variable_values(Gi, Go, Ci, Co, V) :-
    var_value(Gi, V, val(G2)),
    G2 =.. [F | A], % compound term, check args
    !,
    fill_in_variable_values2(A, A2, Ci, Co, V),
    Go =.. [F | A2]. % repack term
fill_in_variable_values(Gi, Go, C, C, V) :-
    var_value(Gi, V, val(Go)),
    atom(Go), % atom
    !.
fill_in_variable_values(Gi, Go, Ci, Co, V) :-
    Gi =.. [F | A], % compound term, check args
    !,
    fill_in_variable_values2(A, A2, Ci, Co, V),
    Go =.. [F | A2]. % repack term
fill_in_variable_values(G, G, C, C, _) :-
    \+is_var(G), % other non-var
    !.

%! fill_in_variable_values2(+ArgsIn:list, -ArgsOut:list, +ConstraintsIn:list, -ConstraintsOut:list, +Vars:compound) is det
% Given a list of args, call fill_in_variable_values/5 for each.
%
% @param ArgsIn Input args.
% @param ArgsOut Output args.
% @param ConstraintsIn Input constraints.
% @param ConstraintsOut Output constraints.
% @param VarsOut Output vars.
fill_in_variable_values2([X | T], [X2 | T2], Ci, Co, V) :-
    fill_in_variable_values(X, X2, Ci, C1, V),
    !,
    fill_in_variable_values2(T, T2, C1, Co, V).
fill_in_variable_values2([], [], C, C, _).

%! print_vars(+PrintVars:list, +Vars:compound) is det
% Given a list of variables, group them by value and print them. Don't print
% completely unbound variables that aren't unified with another variable in the
% list.
%
% @param PrintVars The variables to print.
% @param Vars Var struct to get values from.
print_vars(X, V) :-
    X \= [],
    !,
    get_var_vals(X, V, X2),
    group_by_val(X2, X3),
    remove_nonprinting(X3, X4),
    format_vars(X4, X5, V),
    print_vars2(X5).
print_vars([], _) :-
    !.

%! print_vars2(+VarGroups:list) is det
% Given a list of lists of variables with the same value, print each.
%
% @param VarGroups The groups of variables to print.
print_vars2([-(X, V) | T]) :-
    write('\n'), % First set doesn't get a comma before newline.
    print_vars4(X, V),
    !,
    print_vars3(T).
print_vars2([]) :-
    !.

%! print_vars3(+VarGroups:list) is det
% Given a list of lists of variables with the same value, print each.
%
% @param VarGroups The groups of variables to print.
print_vars3([-(X, V) | T]) :-
    write(',\n'), % Sets after the first get a comma before the newline.
    print_vars4(X, V),
    !,
    print_vars3(T).
print_vars3([]) :-
    !.

%! print_vars4(+VarGroup:list, +Value:compound) is det
% Given a list of variables with the same value, print them. List must be
% non-empty.
%
% @param VarGroup The group of variables to print.
% @param Value The value of the variables.
print_vars4([X, Y | T], V) :- % at least two elements
    writef('~w = ~w', [X, Y]), % print first element here
    !,
    print_vars5([Y | T], V). % print remaining elements
print_vars4([X], V) :- % single-element list; constrained variable
    var_con(V, C, _, _),
    !,
    print_var_constraints([-(X, C)]).
print_vars4([X], V) :- % single-element list; bound variable
    V = val(V2),
    !,
    writef('~w = ~w', [X, V2]).

%! print_vars5(+VarGroup:list, +Value:compound) is det
% Given a list of variables with the same value, print them. List must be
% non-empty.
%
% @param VarGroup The group of variables to print.
% @param Value The value of the variables.
print_vars5([X, Y | T], V) :- % at least two elements
    writef(', ~w = ~w', [X, Y]),
    !,
    print_vars5([Y | T], V).
print_vars5([X], V) :- % last element; constrained variable
    var_con(V, C, _, _),
    C \= [],
    !,
    write(', '),
    print_var_constraints([-(X, C)]).
print_vars5([_], V) :- % last element; constrained variable
    var_con(V, [], _, _), % no constraints to print
    !.
print_vars5([X], V) :- % last element; bound variable
    V = val(V2),
    !,
    writef(', ~w = ~w', [X, V2]).

%! get_var_vals(+PrintVars:list, +Vars:compound, -PrintVarsOut:list) is det
% Get the value for each variable in a list, returning a list of
% -(Value, Var) pairs.
%
% @param PrintVars The variables to print.
% @param Vars Var struct to get values from.
% @param PrintVarsOut Output list of -(Value, Var) pairs.
get_var_vals([X | T], V, [-(Val, X) | T2]) :-
    is_var(X),
    var_value(X, V, Val),
    Val \= id(_), % ensure that we don't get IDs
    !,
    get_var_vals(T, V, T2).
get_var_vals([], _, []) :-
    !.

%! group_by_val(+VarsIn:list, -VarsOut:list) is det
% Given a list of -(Val, Var) pairs, return a list of lists of variables with
% the same value. VarsOut will be sorted and each sub-list will be sorted.
%
% @param VarsIn Input list.
% @param VarsOut Output list of lists.
group_by_val(Xi, Xo) :-
    once(sort(Xi, X2)),
    X2 = [-(V, Y) | X3],
    !,
    group_by_val2(X3, V, [Y], Xo).
group_by_val([], []) :-
    !.

%! group_by_val2(+VarsIn:list, +LastVal:int, +CurrSet:list, -VarsOut:list) is det
% Given a list of -(Val, Var) pairs, return a list of pairs of the form
% -(Vars, Val) where vars is a list of variables with the same value. Should
% only be called from group_by_val/2. VarsIn must be sorted. Each list of
% variables will be sorted, but not the outer list.
%
% @param VarsIn Input list.
% @param LastVal The last value encountered.
% @param CurrSet The current set of variables with LastVal as their value.
% @param VarsOut Output list of lists.
group_by_val2([X | T], V, Vs, Vo) :-
    X = -(V, Y), % same value as previous entry
    !,
    group_by_val2(T, V, [Y | Vs], Vo).
group_by_val2([X | T], V, Vs, [-(Vs2, V) | Vo]) :-
    X = -(V2, Y),
    V2 \= V, % different value.
    !,
    reverse(Vs, Vs2), % restore sort order of vars
    group_by_val2(T, V2, [Y], Vo).
group_by_val2([], V, Vs, [-(Vs2, V)]) :- % return final set
    reverse(Vs, Vs2), % restore sort order of vars
    !.

%! remove_nonprinting(+VarGroupsIn:list, -VarGroupsOut:list) is det
% Given a list of lists of variables with the same value, remove lists
% containing only one completely unbound non-loop variable.
%
% @param VarGroupsIn Input var groups.
% @param VarGroupsOut Output var groups.
remove_nonprinting([X | T], T2) :-
    X = -([_], V),
    var_con(V, [], _, Vl), % skip single-element lists where variable is completely unbound
    Vl =\= 1,
    !,
    remove_nonprinting(T, T2).
remove_nonprinting([X | T], [X | T2]) :-
    !,
    remove_nonprinting(T, T2).
remove_nonprinting([], []) :-
    !.

%! format_vars(+VarGroupsIn:list, -VarGroupsOut:list, +Vars:compound) is det
% Call chs:format_term/4 on the value for each variable group.
%
% @param VarGroupsIn Input var groups.
% @param VarGroupsOut Output var groups.
% @param Vars Var struct to get values from.
format_vars([X | T], [X2 | T2], V) :-
    X = -(Vs, val(Val)),
    format_term(Val, Val2, _, V),
    X2 = -(Vs, val(Val2)),
    !,
    format_vars(T, T2, V).
format_vars([X | T], [X2 | T2], V) :-
    X = -(Vs, Val),
    var_con(Val, Con, F, L),
    format_term(Con, Con2, _, V),
    var_con(Val2, Con2, F, L),
    (L =:= 1 ->
            atom_chars(Vs, Vc),
            atom_chars(Vs2, ['?' | Vc])%, % add flag
            % writef('loop var con list = ~w, processed = ~w\n', [Con, Con2])
    ;
            Vs2 = Vs
    ),
    X2 = -(Vs2, Val2),
    !,
    format_vars(T, T2, V).
format_vars([], [], _) :-
    !.

%! print_var_constraints(+Constraints:list) is det
% Given a list of pairs of variables and constraints, format and print them.
% This should only be called with constraints returned by
% variables:fill_in_variable_values/5.
%
% @param Constraints The list of variable/constraint pairs.
print_var_constraints([]) :-
    !.
print_var_constraints([X | T]) :-
    X = -(V, Cs),
    sort(Cs, [C | Cs2]), % order and remove duplicate constraints
    writef('~w \\= ~w', [V, C]), % write first entry here for proper comma placement
    print_var_constraints3(V, Cs2),
    print_var_constraints2(T),
    !.

%! print_var_constraints2(+Constraints:list) is det
% Given a list of constraints on variables in a CHS entry, format and print
% them. Each will be a list of values the constraint can't take, with an empty
% list indicating a completely unbound variable.
%
% @param Constraints The list of variable/constraint pairs.
print_var_constraints2([X | T]) :-
    X = -(V, Cs),
    print_var_constraints3(V, Cs),
    !,
    print_var_constraints2(T).
print_var_constraints2([]) :-
    !.

%! print_var_constraints3(+Var:ground, +Constraints:list) is det
% Given variable and a list of constraints on it, pretty print each constraint.
%
% @param Var The variable name.
% @param Constraints The list of constraints.
print_var_constraints3(V, [C | T]) :-
    writef(', ~w \\= ~w', [V, C]),
    !,
    print_var_constraints3(V, T).
print_var_constraints3(_, []) :-
    !.

%! indent(+Level:int) is det
% Write Level spaces.
%
% @param Level The level to indent to.
indent(N) :-
    N > 0,
    N1 is N - 1,
    write(' '),
    !,
    indent(N1).
indent(0).

%! print_justification(+Justification:list) is det
% Given a justification for a query, prettyprint it.
%
% @param Justification A list of structs of the form -(Goal, Constraints, SubList).
print_justification(X) :-
    writef('\n\nBEGIN JUSTIFICATION'),
    %writef('\ngot justification: ~w\n\n', [X]), % !!!DEBUG: REMOVE LATER
    print_justification(X, 2),
    write('\nEND JUSTIFICATION'), % empty line between solutions.
    !.
% The case below should never trigger, as it indicates a bug.
%print_justification(_) :-
%        write('Justification failed!\n').

%! print_justification(+Justification:list, +Level:int) is det
% Given a justification for a query, prettyprint it.
%
% @param Justification A list of structs of the form -(Goal, Constraints, SubList).
% @param Level The indentation level. Number of spaces printed before each line.
print_justification([X | T], L) :-
    X = -(G, C, J),
    G \= chs__success,
    G \= expand__call(_),
    !,
    write('\n'),
    indent(L),
    write(G),
    (C\= [] ->
            write(', ( '),
            print_var_constraints(C),
            write(' )')
    ;
            true
    ),
    L2 is L + 2,
    !,
    print_justification(J, L2),
    !,
    print_justification(T, L).
print_justification([X], L) :-
    X = -(chs__success, G, C),
    !,
    write('\n'),
    indent(L),
    writef('Coinductive success yields ~w', [G]),
    (C\= [] ->
            write(', ( '),
            print_var_constraints(C),
            write(' )')
    ;
            true
    ),
    !.
print_justification([X], L) :-
    X = -(expand__call(G), C, J),
    !,
    writef(' -> Expand. Unifying with rule head yields ~w', [G]),
    (C\= [] ->
            write(', ( '),
            print_var_constraints(C),
            write(' )')
    ;
            true
    ),
    !,
    print_justification(J, L).
print_justification([], _) :-
    !.

%! print_abducibles(+CHS:list, +Vars:compound) is det
% If any abducibles have succeeded, print them. Otherwise, do nothing.
%
% @param CHS The CHS to print extract abducibles from.
% @param Vars A variable struct to get bindings for each variable.
print_abducibles(CHS, V) :-
    (once(rb_lookup('abducible_1', Es, CHS)) ->
            chs_entry(E, 'abducible_1', _, 1, _), % dummy entry for abducible wrapper
            once(findall(A, (member(E, Es), chs_entry(E, _, [A], _, _)), As)), % get list of abducibles from CHS
            print_abducibles2(As, V)
    ;
            true
    ),
    !.
print_abducibles(_, _, _) :-
    write_error('could not print CHS'),
    !,
    fail.

%! print_abducibles2(+Abducibles:list, +Vars:compound) is det
% If any abducibles have succeeded, print them. Otherwise, do nothing.
%
% @param Abducibles The CHS entries for succeeding abducibles (abducible(X)).
% @param Vars A variable struct to get bindings for each variable.
print_abducibles2([X | T], V) :-
    writef('\n\nAbducibles: { '),
    format_predicate(X, X2, Con, [], Uvi, V),
    (X2 = not(X3) -> % print negation properly
            writef('not ~w', [X3])
    ;
            writef('~w', [X2])
    ),
    (Con \= [] ->
            write(' ( '),
            print_var_constraints(Con),
            write(' )')
    ;
            true
    ),
    !,
    print_abducibles3(T, V, Uvi),
    write(' }').
print_abducibles2([], _) :-
    !.

%! print_abducibles3(+Abducibles:list, +Vars:compound, +UsedVarsIn:list) is det
% If any abducibles have succeeded, print them. Otherwise, do nothing.
%
% @param Abducibles The CHS entries for succeeding abducibles (abducible(X)).
% @param Vars A variable struct to get bindings for each variable.
% @param UsedVarsIn Input used vars for format_predicate/6
print_abducibles3([X | T], V, Uvi) :-
    format_predicate(X, X2, Con, Uvi, Uvo, V),
    (X2 = not(X3) -> % print negation properly
            writef(', not ~w', [X3])
    ;
            writef(', ~w', [X2])
    ),
    (Con \= [] ->
            write(' ( '),
            print_var_constraints(Con),
            write(' )')
    ;
            true
    ),
    !,
    print_abducibles3(T, V, Uvo).
print_abducibles3([], _, _) :-
    !.

%! print_html(+Justification:list,+File:term) is det
% Given a justification for a query, write its html code in File.
%
% @param Justification A list of structs of the form -(Goal, Constraints, SubList).
print_html(X,[Q,CHSo,Qv,Vo]) :-
    writef('\n\nBEGIN HTML JUSTIFICATION'),
    File = 'html/justification.html',
    open_output_file(Stream,File,Current),
    print('<!doctype html>\n <html>\n <head>\n <meta charset="utf-8">\n <title>c(ASP) Justification</title>\n <link href="css/jquery.treemenu.css" rel="stylesheet" type="text/css">\n <link href="css/tree.css" rel="stylesheet" type="text/css">\n \n <link rel="icon" href="logo.ico">\n \n </head>\n \n <body style="font-size:15px;background: #ECECEC; margin:80px; color:#333;">\n \n'),
    print('<h3>Query</h3>'),nl,
    once(print_query(Q)),nl,
    br,br,nl,
    once(print_chs(CHSo, Vo, 0)),nl,
    br,br,nl,
    ( Vo \= [] ->
      get_var_vals(Qv, Vo, X2),
      group_by_val(X2, X3),
      remove_nonprinting(X3, X4),
      (
          X4 \= [] ->
          print('<font color=blue>'),
          once(print_vars(Qv,Vo)),
          print(' ? </font>')
      ;
          true
      )
    ;
        true
    ),
    nl,
    br,br,nl,
    print('<h3> Justification </h3>\n <ul class="tree">'),nl,
    print_list(X,2),
    print('</ul>'),nl,nl,
    print(' <script src="js/jquery-1.11.2.js"></script>\n <script src="js/jquery.treemenu.js"></script>\n \n <script>\n $(function(){\n        $(".tree").treemenu({delay:100}).openActive();\n    });\n </script>\n \n </body>\n </html>'),
    close_output_file(Stream,Current),
    write('\nEND HTML JUSTIFICATION'), 
    !.

print_query(Q) :-
    new_var_struct(V),
    (
        defined_query(Q, _) ->
        format_term_list(Q, Q2, _, V),
        print('<b><font color=blue>?-</font> '),
        print_body(Q2),
        print('</b>')
    ;
        true
    ).
print_body([]) :- print('true.').
print_body([X]):-
    print(X),print('.').
print_body([X,Y|Xs]):-
    print(X),print(', &nbsp; &nbsp;'),
    print_body([Y|Xs]).

open_output_file(Stream,File,Current) :-
    current_output(Current),
    open(File,append,_F),close(_F), %% if File does not exists open it
    open(File,write,Stream),
    set_output(Stream).
close_output_file(Stream,Current) :-
    set_output(Current),
    close(Stream).
    
br :- print('<br>').

print_item([],_) :- !.
print_item([-(expand__call(Head-Body),C,J)],L) :- !,
    print_var(C),
    print(' \t<font color=green size=-1> &emsp;  '), print(Head), print(' :- '),
    print_body(Body), print('</font>'),
    print_item(J,L).
print_item([-(chs__success,_G,C)],_L) :- !,
    print(' <font color=green> &emsp; &#10003; </font> '), % print(chs__success),
 %      print(' : '), print(G),
    print(' '), print_var(C).
print_item(-(A,B,C),L) :- !,
    print(A), print_var(B),
    print_item(C,L).
print_item(X,L) :-
    !,
    nl, indent(L),
    print('  <ul> '),nl,
    L2 is L + 4,
    print_list(X,L2),
    indent(L),
    print('  </ul>'),nl,indent(L).
    
print_list([],_).
print_list([X|Xs],L) :-
    indent(L),
    print('<li> '),
    print_item(X,L),
    print('</li>'),nl,
    print_list(Xs,L).


print_var(B) :- !,
    (
        B\= [] ->
        print(', ( '),
        print(B),
        print(' )')
    ;
        true
    ).




:- dynamic pr_rule/2, pr_query/1, pr_user_predicate/1.
:- dynamic pr_table_predicate/1, pr_show_predicate/1, pr_pred_predicate/1.

%! generate_pr_rules/0
generate_pr_rules(_Sources) :-
    retractall(pr_query(_)), retractall(pr_rule(_,_)), retractall(pr_user_predicate(_)),
    retractall(pr_table_predicate(_)),
    retractall(pr_show_predicate(_)),
    retractall(pr_pred_predicate(_)),
    %% format('\nLoading files: ~w\n',Sources),
    findall(R, (defined_rule(_, H, B), rule(R, H, B)), Rs),
    new_var_struct(V),
    format_term_list(Rs,Rs2,_,V),
    (
        defined_nmr_check(NMR) ->
        format_term_list(NMR, NMR2, _, V)
    ;
        NMR2 = []
    ),
    (
        defined_query(Q,_),
        format_term_list(Q,Q2,_,V),
        assert_pr_query(Q2) ->
        true
    ;
        true
    ),
    (
        findall(T, table(T), Ts),
        format_term_list(Ts, Ts2, _, V),
        assert_pr_table(Ts2) ->
        true
    ;
        true
    ),
    retractall(table(_)),
    (
        findall(S, show(S), Ss),
        format_term_list(Ss, Ss2, _, V),
        assert_pr_show(Ss2) ->
        true
    ;
        true
    ),
    retractall(show(_)),
    (
        findall(P, pred(P), Ps),
        format_term_list(Ps, Ps2, _, V),
        assert_pr_pred(Ps2) ->
        true
    ;
        true
    ),
    retractall(pred(_)),
    assert_pr_rules(Rs2),
    assert_pr_rules([-('global_constraints', NMR2)]),
    % close_output_file(Stream, Current),
    % write('\nEND pr_rules GENERATION\n'),!.
    true.

assert_pr_table([]).
assert_pr_table([[T|Ts]|Tss]) :-
    assert_pr_table([T|Ts]),
    assert_pr_table(Tss).
assert_pr_table([T|Ts]) :-
    assert(pr_table_predicate(T)),
    % print(pr_table_predicate(T)),
    % print('.'),nl,
    assert_pr_table(Ts).

assert_pr_show([]).
assert_pr_show([[T|Ts]|Tss]) :-
    assert_pr_show([T|Ts]),
    assert_pr_show(Tss).
assert_pr_show([not(Name)/Arity|Ts]) :- !,
    length(Args,Arity),
    T =.. [Name|Args],
    assert(pr_show_predicate(not(T))),
    % print(pr_show_predicate(T)),
    % print('.'),nl,
    assert_pr_show(Ts).
assert_pr_show([Name/Arity|Ts]) :-
    length(Args,Arity),
    T =.. [Name|Args],
    assert(pr_show_predicate(T)),
    % print(pr_show_predicate(T)),
    % print('.'),nl,
    assert_pr_show(Ts).

:- op(700, xfx, ['::']).
assert_pr_pred([]).
assert_pr_pred([[T|Ts]|Tss]) :- !,
    assert_pr_pred([T|Ts]),
    assert_pr_pred(Tss).
assert_pr_pred([T|Ts]) :-
    process_pr_pred(T,PT),
    revar(PT,RT),
    assert(pr_pred_predicate(RT)),
    % print(pr_pred_predicate(T)),
    % print('.'),nl,
    assert_pr_pred(Ts).

process_pr_pred(A::B,A::format(PB,List)) :-
    atom_chars(B,Chars),
    process_pr_pred_(Chars,PChars,List),
    atom_chars(PB,PChars).
process_pr_pred_([],[],[]).
process_pr_pred_([@,'('|Cs],[~,p|Ps],[@(V:NV)|Vs]) :- !,
    process_pr_pred_var(Cs,Rs,[],Var,NVar),
    atom_chars(V,Var),
    atom_chars(NV,NVar),
    process_pr_pred_(Rs,Ps,Vs).
process_pr_pred_([C|Cs],[C|Rs],Var) :-
    process_pr_pred_(Cs,Rs,Var).
process_pr_pred_var([':'|R0],Rs,VAc0,VAc1,NAc) :- !,
    reverse(VAc0,VAc1),
    process_pr_pred_name(R0,Rs,[],NAc).
process_pr_pred_var([')'|Rs],Rs,Ac0,Ac1,['\'','\'']) :- !,
    reverse(Ac0,Ac1).
process_pr_pred_var([V0|R0],Rs,Ac0,Ac1,NVar) :-
    process_pr_pred_var(R0,Rs,[V0|Ac0],Ac1,NVar).
process_pr_pred_name([')'|Rs],Rs,NAc0,NAc1) :- !,
    reverse(NAc0,NAc1).
process_pr_pred_name([NV0|R0],Rs,NAc0,NAc1) :-
    process_pr_pred_name(R0,Rs,[NV0|NAc0],NAc1).

    

assert_pr_rules([]).
assert_pr_rules([-(Head, Body)|Rs]) :-
    revar(-(Head,Body),-(H,B)),
    assert(pr_rule(H,B)),
    assert_pr_user_predicate([H]),
    % print(pr_rule(Head,Body)),
    % print('.'),nl,
    assert_pr_rules(Rs).

assert_pr_query(Q) :-
    %%    revar(Query,Q), do revar here, will be done later
    assert(pr_query(Q)),
    % print(pr_query(Q)),
    % print('.'),nl.
    true.

assert_pr_user_predicate([]).
assert_pr_user_predicate([P|Ps]) :-
    P =.. [Name|Args],
    length(Args,La),
    (
        pr_user_predicate(Name/La) ->
        true
    ;
        assert(pr_user_predicate(Name/La)),
        % print(pr_user_predicate(Name/La)),
        % nl,
        true
    ),
    assert_pr_user_predicate(Ps).

:- use_module(library(dict)).

revar(X,Y) :- revar_(X,Y,_Dic).

revar_(X,Y,_Dic) :- var(X), !, Y=X.
revar_(X,Y,Dic) :- varatm(X), !, dic_lookup(Dic,X,Y).
revar_(X,Y,_Dic) :- special_atom(X,Y), !.
revar_(X,Y,Dic) :-
    X=..[F|As],
    revars(As,Bs,Dic),
    Y=..[F|Bs].

varatm(X) :- atom(X), atom_codes(X, [C|_]), varc(C).

special_atom(A/B,'rat'(A,B)) :- num(A), num(B),!.
special_atom(X,'rat'(A,B)) :-
    atom(X),
    atom_codes(X, Codes),
    append(C_A, [0'/ | C_B], Codes),
    number_codes(A,C_A),
    number_codes(B,C_B),!.
special_atom(X,Y) :-
    atom(X),
    atom_chars(X,Codes),
    append(['\''|C_Y],['\''],Codes),
    atom_chars(Y,C_Y).

varc(C) :- C >= 0'A, C =< 0'Z, !.
varc(0'_).
%%varc(0'-) :- display(hi),nl.

revars([],[],_).
revars([X|Xs],[Y|Ys],Dic) :- revar_(X,Y,Dic), revars(Xs, Ys, Dic).
