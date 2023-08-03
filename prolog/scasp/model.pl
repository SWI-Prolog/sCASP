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

:- module(scasp_model,
          [ canonical_model/2,          % +RawModel, -Canonical
            unqualify_model/3,          % +ModelIn, +Module, -ModelOut
            print_model/2               % :Model, +Options
          ]).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).

:- use_module(common).
:- use_module(modules).
:- use_module(output).

:- meta_predicate
    canonical_model(:, -),
    print_model(:, +).

:- multifile
    model_hook/2.

/** <module> sCASP model handling


*/

%!  canonical_model(:RawModel, -Canonical)

canonical_model(M:Model, CanModel) :-
    flatten(Model, FlatModel),
    exclude(nonmodel_term, FlatModel, Model1),
    sort(Model1, Unique),
    maplist(raise_negation, Unique, Raised),
    filter_shown(M, Raised, Filtered),
    sort_model(Filtered, Sorted),
    simplify_model(Sorted, CanModel).

nonmodel_term(abducible(_)).
nonmodel_term(proved(_)).
nonmodel_term(chs(_)).
nonmodel_term(o_nmr_check).
nonmodel_term(not(X)) :-
    nonvar(X),
    !,
    nonmodel_term(X).
nonmodel_term(Term) :-
    functor(Term, Name, _),
    (   sub_atom(Name, 0, _, _, 'o_')
    ->  true
    ;   sub_atom(Name, _, _, 0, '$')
    ).

%!  filter_shown(+Module, +Model, -Shown) is det.
%
%   Handle the show/1 directives.  All  terms   are  shown  for a target
%   module if pr_show_predicate/1  is  not   defined  for  that  module.
%   Otherwise the terms associated with the   module  are filtered using
%   pr_show_predicate/1.

filter_shown(M, Model, Shown) :-
    maplist(tag_module(M), Model, Tagged),
    maplist(arg(1), Tagged, Modules),
    sort(Modules, Unique),
    include(has_shown, Unique, Filter),
    (   Filter == []
    ->  Shown = Model
    ;   convlist(do_show(Filter), Tagged, Shown)
    ).

tag_module(M, Term, t(Q, Unqualified, Term)) :-
    model_term_module(M:Term, Q),
    unqualify_model_term(Q, Term, Unqualified).

has_shown(Module) :-
    clause(Module:pr_show_predicate(_), _),
    !.

do_show(Filter, t(M,Unqualified,Term), Term) :-
    (   memberchk(M, Filter)
    ->  M:pr_show_predicate(Unqualified)
    ;   true
    ).

%!  sort_model(+ModelIn, -Sorted) is det.
%
%   Sort the model by literal, getting   affirming and denying knowledge
%   about a literal together.  If multiple terms about the same literal
%   appear they are ordered:
%
%     1. positive
%     2. not(positive)
%     3. -positive
%     4. not(-positive).

sort_model(ModelIn, Sorted) :-
    map_list_to_pairs(literal_key, ModelIn, Pairs),
    keysort(Pairs, KSorted),
    pairs_values(KSorted, Sorted).

literal_key(Term, Literal-Flags) :-
    literal_key(Term, Literal, 0, Flags).

literal_key(not(X), Key, F0, F) =>
    literal_key(X, Key, F0, F1),
    F is F1+0x1.
literal_key(-(X), Key, F0, F) =>
    literal_key(X, Key, F0, F1),
    F is F1+0x2.
literal_key(X, Key, F0, F) =>
    Key = X,
    F = F0.

%!  simplify_model(+ModelIn, -ModelOut) is det.
%
%   Remove model terms that are entailed by other model terms.

simplify_model([not(X0),-X1|T0], M), X0 == X1 =>
    M = [-X1|M0],
    simplify_model(T0, M0).
simplify_model([X0, not(-(X1))|T0], M), X0 == X1 =>
    M = [X0|M0],
    simplify_model(T0, M0).
simplify_model([H|T0], M) =>
    M = [H|M0],
    simplify_model(T0, M0).
simplify_model([], M) =>
    M = [].

%!  unqualify_model(+ModelIn, +Module, -ModelOut) is det.
%
%   Restore the model relation to modules.

unqualify_model(Model0, Module, Model) :-
    maplist(unqualify_model_term(Module), Model0, Model).

%!  print_model(:Model, +Options) is det.
%
%   Print the model in aligned columns.  Options processed:
%
%     - width(Width)
%       Assumed terminal width.  Default from tty_size/2 or 80.
%
%   Model terms are printed in columns. E.g., for  a 10 atom model and 4
%   columns we get:
%
%   ```
%      1  4  7  10
%      2  5  8
%      3  6  9
%   ```

:- if(\+current_predicate(tty_size/2)).
tty_size(25,80).
:- endif.

print_model(Model, Options) :-
    model_hook(Model, Options),
    !.
print_model(_:Model, Options) :-
    (   option(width(Width), Options)
    ->  true
    ;   catch(tty_size(_, Width), _, Width = 80)
    ),
    layout(Model, Width, Layout),
    compound_name_arguments(Array, v, Model),
    format('{ ', []),
    print_table(0, Array, Layout.put(_{ sep:',',
                                        end:'\n}',
                                        prefix:'  ',
                                        options:Options
                                      })).

print_table(I, Array, Layout) :-
    Cols = Layout.cols,
    Rows = Layout.rows,
    Row is I // Cols,
    Col is I mod Cols,
    Index is Row + Col * Rows + 1,

    % If the next index is outside, we need a newline.  If we are
    % also on the last row we have the very last element
    (   NIndex is Row + (Col+1) * Rows + 1,
        functor(Array, _, LastIndex),
        NIndex =< LastIndex
    ->  NL = false,
        Last = false
    ;   NL = true,
        (   Row+1 =:= Rows
        ->  Last = true
        ;   Last = false
        )
    ),

    % If we are not the last on the line and not the last, print the
    % cell, padding to the column with and followed by a separator (,)
    % Else we print withput padding either a separator or the end.
    (   arg(Index, Array, Atom)
    ->  (   NL == false,
            Last == false
        ->  format('~|~@~w~t~*+',
                   [print_model_term(Atom, Layout.options),
                    Layout.sep, Layout.col_width])
        ;   Last == false
        ->  format('~@~w', [print_model_term(Atom, Layout.options), Layout.sep])
        ;   format('~@~w', [print_model_term(Atom, Layout.options), Layout.end])
        ),
        Print = true
    ;   true
    ),

    % Emit a newline if this is the last one on the line and we printed this
    % cell.
    (   Last == true
    ->  true
    ;   (   Print == true, NL == true
        ->  format('~n~w', [Layout.prefix])
        ;   true
        ),
        I2 is I+1,
        print_table(I2, Array, Layout)
    ).

layout(Atoms, Width, _{cols:Cols, rows:Rows, col_width:ColWidth}) :-
    length(Atoms, L),
    longest(Atoms, Longest),
    Cols0 is max(1, Width // (Longest + 3)),
    Rows is ceiling(L / Cols0),
    Cols is ceiling(L/Rows),
    ColWidth is min(Longest+3, Width // Cols).

longest(List, Longest) :-
    longest(List, 0, Longest).

longest([], M, M) :- !.
longest([H|T], Sofar, M) :-
    write_length(H, Length,
                 [ portray(true),
                   numbervars(true),
                   quoted(true)
                 ]),
    Length >= Sofar,
    !,
    longest(T, Length, M).
longest([_|T], S, M) :-
    longest(T, S, M).

