/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(sscasp_render,
          [ scasp_readable/2,             % @Term, -Readable
            scasp_readable/3,             % @Term, -Readable, +Options
            portray_model/1,               % @Model
            portray_model/2,               % @Model, +Options
            portray_justification/1,       % @Justification
            portray_justification/2,       % @Justification, +Options
            portray_literal/2,             % @Literal, +Options
            portray_rule/2,                % @Clause, +Options
            portray_query/2                % @Query, +Options
          ]).
:- use_module(ops).
:- use_module(mangle, [literal_kind/3, kind_truth/2]).
:- use_module(html,      [html_model//2, html_justification//2]).
:- use_module(html_text, [emit_text/1]).
:- use_module(library(lists), [member/2, nth0/3]).
:- use_module(library(apply), [maplist/3, maplist/4, foldl/4, foldl/5,
                                exclude/3]).
:- use_module(library(listing), [portray_clause/3]).
:- use_module(library(pairs), [pairs_keys_values/3]).
:- use_module(library(option), [option/2, option/3]).
:- use_module(library(ansi_term), [ansi_format/3]).

/** <module> Making an answer readable

An answer is literals carrying attributed variables.  Printed as they stand
that is `_12345` where the interesting part is, because what the answer *says*
about a variable lives in the constraint stores rather than in the term:

    ?- scasp([(p(X) :- X #> 3, X #< 10)], not p(V), M).
    M = [not p(_1234)].

with `V` carrying `{V =< 3}`.  So rendering means three things — read the
stores, give the variables names, and put the two back together:

    not p(A | {A=<3})

## Reading a store without destroying it

`copy_term/3` and nothing else.  `dif:attribute_goals//1` calls `del_attr/2`
on each or-node as it reports it, so that a node shared by several variables
is not reported twice; `copy_term/3` and the toplevel undo that and a direct
call does not.  Copying also means nothing here can disturb the answer it is
printing, which matters because a renderer runs *while* the solver still has
choicepoints.

One `copy_term/3` for the whole term, not one per variable: a constraint
relating two variables is then reported once, on the first of them, instead of
once for each.

## Names

`A`, `B`, ... in order of first appearance, and `_` for a variable that
appears once and carries nothing — a name for a variable nothing is said
about is noise.  A variable that appears once but *is* constrained keeps a
name, because the constraint has to be attached to something.
*/

%!  scasp_readable(@Term, -Readable) is det.
%!  scasp_readable(@Term, -Readable, +Options) is det.
%
%   Readable is Term with its variables replaced by `'$VAR'(Name)`, or by
%   `Name | {Constraints}` where the stores have something to say.  Ready
%   for `print_message/2` or `write_term/2` with `numbervars(true)`.
%
%   Options:
%
%     - constraints(Boolean)
%       Inline the constraint stores.  Default `true`; `false` names the
%       variables and says no more.

:- det(scasp_readable/2).
:- det(scasp_readable/3).

scasp_readable(Term, Readable) :-
    scasp_readable(Term, Readable, []).

scasp_readable(Term, Readable, Options) :-
    copy_term(Term, Readable, Goals0),
    (   option(constraints(false), Options)
    ->  Goals = []
    ;   phrase(separate(Goals0), Goals)
    ),
    term_variables(Readable-Goals, All),
    owners(Goals, All, Owned),
    singletons(Readable, Goals, Singletons),
    name_variables(All, Singletons, Owned).

%   CLP(Q) reports its whole store as **one** goal, `{B > 0, A-B > 0}`,
%   so it would all be printed on whichever variable comes first.  Taken
%   apart, each constraint goes to a variable it is about.

separate([]) ==>
    [].
separate([Goal|Goals]) ==>
    separate_goal(Goal),
    separate(Goals).

separate_goal(_:{Store}) ==>
    separate_store(Store).
separate_goal({Store}) ==>
    separate_store(Store).
separate_goal(Goal) ==>
    [Goal].

separate_store((A, B)) ==>
    separate_store(A),
    separate_store(B).
separate_store(Constraint) ==>
    [{Constraint}].

%!  owners(@Goals, @Variables, -Owned) is det.
%
%   Each variable paired with the residual goals that are *its* — the
%   ones whose first variable, in the term's own order, is that one.  A
%   `dif(A, B)` belongs to A and is printed there; printing it again on B
%   would say the same thing twice.
%
%   @arg Goals are the residual goals `copy_term/3` reported.
%   @arg Owned is a list of `Variable-Goals`, one pair for each of
%       Variables and in the same order, `Goals` possibly `[]`.

owners(Goals, Variables, Owned) :-
    foldl(owner(Variables), Goals, Pairs, []),
    maplist(collect(Pairs), Variables, Owned).

owner(Variables, Goal, Pairs, Tail),
    term_variables(Goal, Mentioned),
    first_of(Variables, Mentioned, Owner) =>
    Pairs = [Owner-Goal|Tail].
owner(_, _, Pairs, Tail) =>             % mentions nothing we are naming
    Pairs = Tail.

first_of([Variable|Variables], Mentioned, Owner) :-
    (   memberchk_eq(Variable, Mentioned)
    ->  Owner = Variable
    ;   first_of(Variables, Mentioned, Owner)
    ).

%   Not `findall/3`: it copies, and a constraint copied away from the
%   variable it is about can no longer be printed on it — the name goes in
%   one term and the constraint keeps the other.

collect(Pairs, Variable, Variable-Mine) :-
    mine(Pairs, Variable, Mine0),
    distinct(Mine0, Mine).

mine([], _, Mine) =>
    Mine = [].
mine([Owner-Goal|Pairs], Variable, Mine) =>
    (   Owner == Variable
    ->  Mine = [Goal|Rest]
    ;   Mine = Rest
    ),
    mine(Pairs, Variable, Rest).

%!  distinct(@Goals, -Distinct) is det.
%
%   One copy of each constraint, in the order they were reported.  The
%   same disequality arrives several times where several proved literals
%   led to it — `consistent/3` posts one per pair — and `A \= polly and A
%   \= polly and A \= polly` says nothing the first one did not.  Compared
%   with `==`, so two constraints about different variables that happen
%   to print alike are still both kept.

distinct([], Distinct) =>
    Distinct = [].
distinct([Goal|Goals], Distinct) =>
    (   memberchk_eq_goal(Goal, Goals)
    ->  Distinct = Rest
    ;   Distinct = [Goal|Rest]
    ),
    distinct(Goals, Rest).

memberchk_eq_goal(Goal, [Head|Tail]) :-
    (   Goal == Head
    ->  true
    ;   memberchk_eq_goal(Goal, Tail)
    ).

%!  singletons(@Term, @Goals, -Singletons) is det.
%
%   The variables worth hiding: they occur once in the term and in no
%   constraint at all.  "In no constraint" is not "carrying none" — a
%   `dif(A, B)` is printed on A, and B still has to be called something
%   for A's line to be readable.

singletons(Term, Goals, Singletons) :-
    term_singletons(Term, Once),
    term_variables(Goals, Mentioned),
    exclude(mentioned(Mentioned), Once, Singletons).

mentioned(Mentioned, Variable) :-
    memberchk_eq(Variable, Mentioned).

%!  name_variables(?Variables, ?Singletons, ?Owned) is det.
%
%   Bind each variable to what should be printed for it: its name, and
%   its constraints if it owns any.
%
%   Three passes, because a constraint may mention a variable other than
%   its owner, and that one may own constraints of its own.  Every
%   variable is given a name first; then each owner's constraints are
%   written with **all** variables replaced by their names; and only
%   then is anything bound.  Binding as they are named would put B's
%   store inside A's wherever A's mentions B, and bind a variable to a
%   term mentioning itself, which is cyclic and prints as the `_G123`
%   the naming was there to remove.
%
%   @arg Owned is the `Variable-Goals` list of owners/3.

name_variables(Variables, Singletons, Owned) :-
    foldl(name_of(Singletons, Owned), Variables, Names, 0, _),
    pairs_keys_values(Named, Variables, Names),
    maplist(printed(Named, Owned), Variables, Names, Printed),
    maplist(=, Variables, Printed).

name_of(Singletons, Owned, Variable, Name, N0, N) :-
    constraints_of(Owned, Variable, Goals),
    (   Goals == [],
        memberchk_eq(Variable, Singletons)
    ->  Name = '$VAR'('_'),
        N = N0
    ;   variable_name(N0, Letter),
        Name = '$VAR'(Letter),
        N is N0+1
    ).

printed(Named, Owned, Variable, Name, Printed) :-
    constraints_of(Owned, Variable, Goals),
    (   Goals == []
    ->  Printed = Name
    ;   maplist(readable_constraint, Goals, Readable0),
        substitute(Named, Readable0, Readable),
        comma_list(Conjunction, Readable),
        Printed = '| '(Name, {Conjunction})
    ).

%!  substitute(+Named, @In, -Out) is det.
%
%   Out is In with every variable of Named replaced by its name.
%
%   @arg Named is a list of `Variable-'$VAR'(Name)`.

substitute(Named, In, Out), var(In) =>
    (   named(Named, In, Name)
    ->  Out = Name
    ;   Out = In
    ).
substitute(Named, In, Out), compound(In) =>
    compound_name_arguments(In, Functor, Arguments0),
    maplist(substitute(Named), Arguments0, Arguments),
    compound_name_arguments(Out, Functor, Arguments).
substitute(_, In, Out) =>
    Out = In.

named([V-N|T], Variable, Name) :-
    (   V == Variable
    ->  Name = N
    ;   named(T, Variable, Name)
    ).

constraints_of(Owned, Variable, Goals) :-
    (   member(Other-Mine, Owned),
        Other == Variable
    ->  Goals = Mine
    ;   Goals = []
    ).

variable_name(N, Name) :-
    Letter is N mod 26 + 0'A,
    Index is N // 26,
    (   Index =:= 0
    ->  char_code(Name, Letter)
    ;   format(atom(Name), '~c~d', [Letter, Index])
    ).

%!  readable_constraint(@Goal, -Constraint) is det.
%
%   A residual goal as the program would have written it.  `dif/2` is one
%   of the two backings of the Herbrand store and `{}/1` wraps what
%   CLP(Q) has to say, so both come off: what is printed is the source
%   language, not the store that happens to hold it.
%
%   A prohibited-value set is written `A ∉ [b]` however many values it
%   holds, as the old implementation writes it: one spelling for one
%   thing, so a reader need not know that `A \= b` and `A ∉ [b, c]` are
%   the same constraint with one value more.  `A \= B` is left for a
%   `dif/2` the set cannot hold — between variables, or compound terms.

readable_constraint(dif(A, B), Constraint) => Constraint = (A \= B).
readable_constraint({Inner}, Constraint)   => Constraint = Inner.
readable_constraint(_:{Inner}, Constraint) => Constraint = Inner.
readable_constraint(Goal, Constraint)      => Constraint = Goal.

comma_list(Conjunction, [Goal]) =>
    Conjunction = Goal.
comma_list(Conjunction, [Goal|Goals]) =>
    Conjunction = (Goal, Rest),
    comma_list(Rest, Goals).

memberchk_eq(Variable, [Head|Tail]) :-
    (   Variable == Head
    ->  true
    ;   memberchk_eq(Variable, Tail)
    ).

%!  already_readable(@Term, -Readable, +Options) is det.
%
%   `readable(true)` says the caller has been through `scasp_readable/3`
%   already.  It matters when a model and its justification are printed
%   together: run separately they name the same variable twice, and the
%   two names then mean different things.

already_readable(Term, Readable, Options) :-
    (   option(readable(true), Options)
    ->  Readable = Term
    ;   scasp_readable(Term, Readable, Options)
    ).

		 /*******************************
		 *           THE MODEL		*
		 *******************************/

%!  portray_model(@Model) is det.
%!  portray_model(@Model, +Options) is det.
%
%   Write the model.  Options are `scasp_readable/3`'s, plus:
%
%     - layout(lines|columns)
%       One literal per line (the default), or across the width of the
%       terminal inside `{ }`.  A model of thirty literals is a page
%       one way and a paragraph the other.
%     - indent(Columns)
%       Left margin of `layout(lines)`.  Default 4.
%     - width(Columns)
%       Assumed terminal width for `layout(columns)`.  Default from
%       `tty_size/2`, or 80.
%     - human(Boolean)
%       Say each literal in words instead — `html.pl` through
%       `html_text.pl`; `html.pl` documents `annotations(Table)` and
%       `show(Format)`.  The other options above do not
%       apply to it.
%
%   @arg Model is a list of literals as scasp/3 returns it: `p(..)`,
%       `not p(..)`, `-p(..)` or `not -p(..)`.  With `readable(true)` it
%       has been through scasp_readable/3 already.

:- det(portray_model/1).
:- det(portray_model/2).

portray_model(Model) :-
    portray_model(Model, []).

portray_model(Model, Options), option(human(true), Options) =>
    already_readable(Model, Readable, Options),
    emit_text(\html_model(Readable, Options)).
portray_model(Model, Options), option(layout(columns), Options) =>
    already_readable(Model, Readable, Options),
    model_columns(Readable, Options).
portray_model(Model, Options) =>
    option(indent(Indent), Options, 4),
    already_readable(Model, Readable, Options),
    foldl(model_line(Options, Indent), Readable, first, _).

%   The newline goes *before* each line but the first, so that nothing
%   here ends with one.  A printer that ends with a newline cannot be
%   used to portray a term — which is how the toplevel shows a model —
%   because whatever the toplevel writes next, a comma or a full stop,
%   lands on a line of its own.

model_line(Options, Indent, Literal, State, next) :-
    (   State == first
    ->  true
    ;   nl
    ),
    format("~t~*|", [Indent]),
    portray_literal(Literal, Options).

		 /*******************************
		 *          IN COLUMNS		*
		 *******************************/

%!  model_columns(+Model, +Options) is det.
%
%   The model across the width of the terminal, reading **down** the
%   columns:
%
%       { 1  4  7
%         2  5  8
%         3  6     }
%
%   Down rather than across because the model is sorted, so a column is
%   then a run of related literals and the eye can find a predicate.
%   This is the old implementation's `model.pl:print_model/2`.
%
%   @arg Model is the model already through scasp_readable/3.

model_columns(Model, Options), length(Model, Count), Count > 0 =>
    width(Options, Width),
    widest(Model, Options, Widest),
    Columns0 is max(1, Width // (Widest + 3)),
    Rows is ceiling(Count / Columns0),
    Columns is ceiling(Count / Rows),
    ColumnWidth is min(Widest + 3, Width // Columns),
    grid(0, Rows, Columns, Model, Grid),
    format("{ ", []),
    print_rows(Grid, ColumnWidth, Options),
    format("~n}", []).
model_columns(_, _) =>
    format("{}", []).

%!  grid(+Row, +Rows, +Columns, +Model, -Grid) is det.
%
%   The model laid out as a list of rows.  Cell (R, C) is the model's
%   `R + C*Rows`-th literal, so a *column* is a run of the sorted model
%   and the eye can find a predicate.  A short last column simply has
%   fewer cells.
%
%   @arg Row is the 0-based index of the first row still to lay out.
%   @arg Grid is a list of rows, each a list of literals.

grid(Row, Rows, _, _, Grid), Row >= Rows =>
    Grid = [].
grid(Row, Rows, Columns, Model, Grid) =>
    Grid = [Cells|Grid1],
    row_cells(0, Columns, Row, Rows, Model, Cells),
    Row1 is Row+1,
    grid(Row1, Rows, Columns, Model, Grid1).

row_cells(Column, Columns, _, _, _, Cells), Column >= Columns =>
    Cells = [].
row_cells(Column, Columns, Row, Rows, Model, Cells) =>
    Index is Row + Column*Rows,
    (   nth0(Index, Model, Literal)
    ->  Cells = [Literal|More]
    ;   Cells = More
    ),
    Column1 is Column+1,
    row_cells(Column1, Columns, Row, Rows, Model, More).

print_rows([Row|Rows], ColumnWidth, Options) :-
    print_row(Row, Rows, ColumnWidth, Options),
    (   Rows == []
    ->  true
    ;   format("~n  "),
        print_rows(Rows, ColumnWidth, Options)
    ).

%   The comma goes after every literal but the last one printed, and the
%   padding after every one but the last of its row — a padded last cell
%   would put the closing brace a column's width away from it.

print_row([Cell|Cells], MoreRows, ColumnWidth, Options) :-
    (   Cells == [],
        MoreRows == []
    ->  Separator = ''
    ;   Separator = ','
    ),
    (   Cells == []
    ->  format("~@~w", [portray_literal(Cell, Options), Separator])
    ;   format("~|~@~w~t~*+",
               [portray_literal(Cell, Options), Separator, ColumnWidth]),
        print_row(Cells, MoreRows, ColumnWidth, Options)
    ).

width(Options, Width) :-
    (   option(width(Width), Options)
    ->  true
    ;   catch(tty_size(_, Width), _, fail)
    ->  true
    ;   Width = 80
    ).

widest(Model, Options, Widest) :-
    foldl(wider(Options), Model, 0, Widest).

wider(Options, Literal, Sofar, Widest) :-
    with_output_to(string(Text), portray_literal(Literal, [colour(false)|Options])),
    string_length(Text, Length),
    Widest is max(Sofar, Length).

		 /*******************************
		 *          A LITERAL		*
		 *******************************/

%!  portray_literal(@Literal, +Options) is det.
%
%   One literal, with the negations spelled out and coloured.  Options:
%
%     - format(unicode|ascii)
%       `¬ p` or `-p`.  Default `unicode`.
%     - colour(Boolean)
%       Default `true`.
%
%   The connector is written rather than left to the operator table for
%   two reasons.  `not -p(X)` is printed `not-p(X)` by the writer, which
%   reads back correctly and reads *wrong* to a person — `-` looks like
%   an infix there.  And the four shapes are four different claims, so
%   they are worth four colours: what holds is bold, what is known not to
%   hold is bold red, and what there is no evidence for is red.
%
%   @arg Literal is a readable literal of the model, or a node of a
%       justification, which may also be wrapped in `chs/1` or
%       `proved/1`.

:- det(portray_literal/2).

portray_literal(Literal, Options) :-
    literal_parts(Literal, Connectors, Term, Style),
    forall(member(Connector, Connectors),
           connector(Connector, Options)),
    term_text(Term, Text),
    styled(Style, Options, '~w', [Text]).

%!  term_text(@Term, -Text) is det.
%
%   The term as this language writes it.  Written to a string first
%   because the colour is put on by `ansi_format/3`, which takes a format
%   rather than write options — and the options matter: `module/1` for
%   the operators, without which `A \= b` is `\=(A, b)`, and `portray/1`
%   so that a constrained variable says what is known about it.

term_text(Term, Text) :-
    with_output_to(string(Text),
                   write_term(Term,
                              [ module(sscasp_ops),
                                portray(true),
                                numbervars(true),
                                quoted(true),
                                spacing(next_argument)
                              ])).

%   `chs/1` and `proved/1` are about the *proof* rather than about the
%   literal, so they are said in front of whatever the literal would
%   have been said as.  They appear in a justification and never in a
%   model.

literal_parts(chs(Inner), Connectors, Term, Style) =>
    Connectors = [chs|Connectors1],
    literal_parts(Inner, Connectors1, Term, Style).
literal_parts(proved(Inner), Connectors, Term, Style) =>
    Connectors = [proved|Connectors1],
    literal_parts(Inner, Connectors1, Term, Style).
literal_parts(Literal, Connectors, Term, Style) =>
    literal_kind(Literal, Kind, Term),
    kind_connectors(Kind, Connectors),
    kind_truth(Kind, Style).

kind_connectors(plain,   []).
kind_connectors(neg,     [negation]).
kind_connectors(dual,    [not]).
kind_connectors(dualneg, [not, negation]).

connector(Connector, Options) :-
    connector_text(Connector, Options, Text),
    styled(connector, Options, '~w', [Text]).

connector_text(not, _, Text) => Text = 'not '.
connector_text(chs, _, Text) => Text = 'chs '.
connector_text(proved, _, Text) => Text = 'proved '.
connector_text(negation, Options, Text), option(format(ascii), Options) =>
    Text = '-'.
connector_text(negation, _, Text) =>
    Text = '\u00ac '.

styled(Style, Options, Format, Args) :-
    (   option(colour(false), Options)
    ->  format(Format, Args)
    ;   style(Style, Ansi),
        ansi_format(Ansi, Format, Args)
    ).

style(true,      [bold]).
style(false,     [bold, fg(red)]).
style(likely,    []).
style(unlikely,  [fg(red)]).
style(connector, [fg(cyan)]).

		 /*******************************
		 *           A RULE		*
		 *******************************/

%!  portray_rule(@Clause, +Options) is det.
%
%   Write a clause of the program.  `portray_clause/3` does all of it:
%   the `;` shape, the line width, the variable names, and — given
%   `module(sscasp_ops)` — this language's operators, without which
%   `not p(X)` is `not(p(X))` and `A \= b` is `\=(A, b)`.
%
%   Two things had to be fixed in SWI-Prolog for that to be true, and
%   both are there rather than here: `listing/2` dropped its options
%   before `portray_clause/3` ever saw them, so `module/1` had no
%   effect; and the writer wrote `not -p(X)` as `not-p(X)`, which reads
%   back correctly — the reader always prefers the prefix operator — and
%   *looks* like an infix `-`.
%
%   `indent(Columns)` is the useful option.  A listing is written in the **source** spelling, `-p(X)` and
%   not `¬ p(X)`, because that is what a reader compares with their
%   program; the model and the justification go through
%   `portray_literal/2` and read the other way.
%
%   @arg Options is read for `variable_names/1` and `indent/1` only;
%       the other `portray_clause/3` options are fixed here.

:- det(portray_rule/2).

portray_rule(Clause, Options) :-
    write_options(Options, WriteOptions),
    portray_clause(current_output, Clause, WriteOptions).

%!  portray_query(@Query, +Options) is det.
%
%   `?- p(A), not q(A).`
%
%   @arg Query is the query as a conjunction of literals, without `?-`.
%   @arg Options are portray_rule/2's; `variable_names(Bindings)` keeps
%       the names the reader gave the query's variables.

portray_query(Query, Options) :-
    portray_rule((?- Query), Options).

%   `variable_names/1` and `indent/1` are passed on where the caller
%   gave them.  The first is how a *query* keeps the names the reader
%   saw: `?- flies(X)` and not `?- flies(_)`, which is the whole point of
%   reading them.

write_options(Options, WriteOptions) :-
    passed_on(variable_names, Options, WriteOptions, Rest),
    passed_on(indent, Options, Rest,
              [ module(sscasp_ops),
                numbervars(true),
                quoted(true)
              ]).

%   Not `findall/3`: it copies, and `variable_names(['X'=V])` copied is
%   a name for some *other* variable — the query would print `?- p(_)`
%   with the binding list carefully naming nothing.

passed_on(Name, Options, List, Tail) :-
    Option =.. [Name, _],
    (   option(Option, Options)
    ->  List = [Option|Tail]
    ;   List = Tail
    ).

		 /*******************************
		 *       THE JUSTIFICATION	*
		 *******************************/

%!  portray_justification(@Justification) is det.
%!  portray_justification(@Justification, +Options) is det.
%
%   Write the justification as an indented tree.  Options are
%   `scasp_readable/3`'s, plus:
%
%     - indent(Columns)
%       Left margin of the roots.  Default 4.
%     - step(Columns)
%       Added per level.  Default 4.
%     - max_depth(Depth)
%       Print no deeper, marking what was cut with `...`.  Default
%       `infinite`.
%     - human(Boolean)
%       Say the tree in words instead; see `portray_model/2`.
%
%   Like `portray_model/2` it does not end with a newline.
%
%   The whole forest is made readable in **one** call, so that a variable
%   shared between a literal and its justification gets one name.  Doing
%   it per node would name the same variable `A` in two places and mean
%   two different things.
%
%   @arg Justification is a list of `Literal-Children` trees, as scasp/4
%       returns it: Children is a list of the same, a Literal may be
%       wrapped in `chs/1` or `proved/1`, and the NMR check is the root
%       `'$nmr'-Children`.

:- det(portray_justification/1).
:- det(portray_justification/2).

portray_justification(Justification) :-
    portray_justification(Justification, []).

portray_justification(Justification, Options), option(human(true), Options) =>
    already_readable(Justification, Readable, Options),
    emit_text(\html_justification(Readable, Options)).
portray_justification(Justification, Options) =>
    option(indent(Indent), Options, 4),
    option(step(Step), Options, 4),
    option(max_depth(Max), Options, infinite),
    already_readable(Justification, Readable, Options),
    foldl(portray_node(Indent, Step, Max, Options), Readable, first, _).

portray_node(Column, Step, Max, Options, Literal-Children, State, next) :-
    (   State == first
    ->  true
    ;   nl
    ),
    format("~t~*|", [Column]),
    portray_root(Literal, Options),
    (   Children == []
    ->  true
    ;   Max == 0
    ->  Deeper is Column+Step,
        format("~n~t~*|...", [Deeper])
    ;   Deeper is Column+Step,
        next_depth(Max, Left),
        foldl(portray_node(Deeper, Step, Left, Options), Children, next, _)
    ).

%   The NMR check's node is not a literal, and is said in the style of
%   the connectors rather than printed as the atom that marks it.

portray_root('$nmr', Options) =>
    styled(connector, Options, 'global constraints', []).
portray_root(Literal, Options) =>
    portray_literal(Literal, Options).

next_depth(infinite, Left) => Left = infinite.
next_depth(N, Left) => Left is N-1.

		 /*******************************
		 *          PORTRAYING		*
		 *******************************/

%!  user:portray(@Term) is semidet.
%
%   `'| '` is an operator so that a reader can parse what is printed, but
%   its name has a space in it and `writeq/1` therefore quotes it —
%   `A'| '{A=<3}` where `A | {A =< 3}` was meant.  So it is portrayed.
%
%   `∉` is portrayed for the other reason an operator can print badly:
%   nothing makes the writer put spaces around it, and `X∉[a,b]` is
%   harder to read than `X ∉ [a, b]` for no gain.
%
%   In `user` because that is where `print/1` and `portray(true)` look —
%   and the toplevel, which is where an answer's remaining constraints
%   are shown.  `scasp/output.pl` has the same two clauses for the same
%   terms and prints the same thing, so a process with both libraries is
%   no worse off than one with either.

:- multifile user:portray/1.

user:portray('| '(Variable, Constraints)) :-
    format("~p | ~p", [Variable, Constraints]).
user:portray('∉'(Variable, Values)) :-
    format("~p ∉ ~p", [Variable, Values]).
