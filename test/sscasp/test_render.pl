:- module(test_render,
          [ test_render/0
          ]).
:- use_module('../scasp_plunit', []).        % sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/render)).
:- use_module(library(plunit)).
:- use_module(library(lists), [member/2, nth1/3]).
:- use_module(library(debug), [assertion/1]).

/** <module> Writing an answer as the program wrote it

`sscasp/render.pl` is the machine rendering: the literal as s(CASP) spells
it, with the connectors said and the shapes told apart by colour.
`test_api.pl` covers `scasp_readable/3` — the naming and the stores; what
is left, and what is here, is what reaches the terminal.

Colour is not asserted.  `ansi_format/3` emits nothing when the stream is
not a terminal, which is what `with_output_to/2` gives it, so a test could
only check that the escapes are *absent* — which they are either way.  What
is checked is the text, and that the four shapes go through four different
styles is a three-line table in `render.pl`.
*/

test_render :-
    run_tests([ render_literals,
                render_columns,
                render_justification
              ]).

%!  printed(:Goal, -Text) is det.

:- meta_predicate
    printed(0, -).

printed(Goal, Text) :-
    with_output_to(string(Text), Goal).

		 /*******************************
		 *          A LITERAL		*
		 *******************************/

/** The connector is written rather than left to the operator table, and
    `not -p(X)` is why: the writer prints it `not-p(X)`, which reads back
    correctly and reads *wrong* to a person, `-` looking like an infix
    there.
*/

:- begin_tests(render_literals).

test(the_four_shapes, [forall(shape(Literal, Expected))]) :-
    printed(portray_literal(Literal, [indent(0)]), Text),
    assertion(Text == Expected).

shape(p(a),            "p(a)").
shape(not p(a),        "not p(a)").
shape(-p(a),           "¬ p(a)").
shape(not -p(a),       "not ¬ p(a)").

test(ascii_says_the_same_in_less, [forall(ascii(Literal, Expected))]) :-
    printed(portray_literal(Literal, [format(ascii)]), Text),
    assertion(Text == Expected).

ascii(-p(a),     "-p(a)").
ascii(not -p(a), "not -p(a)").

%   `chs/1` and `proved/1` are about the proof rather than about the
%   literal, so they are said in front of whatever the literal would have
%   been said as.  They appear in a justification and never in a model.

test(the_proof_wrappers, [forall(wrapped(Literal, Expected))]) :-
    printed(portray_literal(Literal, []), Text),
    assertion(Text == Expected).

wrapped(chs(p(a)),         "chs p(a)").
wrapped(proved(p(a)),      "proved p(a)").
wrapped(chs(not p(a)),     "chs not p(a)").
wrapped(proved(-p(a)),     "proved ¬ p(a)").

test(a_constrained_variable_says_what_is_known) :-
    printed(portray_literal('| '('$VAR'('A'), {'$VAR'('A') =< 3}), []), Text),
    assertion(sub_string(Text, _, _, _, "A=<3")).

:- end_tests(render_literals).

		 /*******************************
		 *         THE COLUMNS		*
		 *******************************/

/** A model of thirty literals is a page one way and a paragraph the other.
*/

:- begin_tests(render_columns).

model([p(1), p(2), p(3), p(4), p(5), p(6)]).

test(one_literal_per_line_by_default) :-
    model(Model),
    printed(portray_model(Model, [readable(true)]), Text),
    split_string(Text, "\n", "", Lines),
    assertion(length(Lines, 6)).

test(and_nothing_after_the_last_one) :-
    model(Model),
    printed(portray_model(Model, [readable(true)]), Text),
    assertion(\+ sub_string(Text, _, _, 0, "\n")).

%   In words too: it goes through `html_text.pl`, whose lines used to end
%   the way print_message_lines/3 ends them, the last one included.

test(and_nothing_after_the_last_one_in_words) :-
    model(Model),
    printed(portray_model(Model, [readable(true), human(true)]), Text),
    assertion(Text \== ""),
    assertion(\+ sub_string(Text, _, _, 0, "\n")).

%   The same rule for the columns, and it is not cosmetic: a printer that
%   ends with a newline cannot portray a term, because whatever the
%   toplevel writes next — a comma, a full stop — lands on a line of its
%   own.  That is how a model reaches the terminal.

test(the_columns_end_where_the_brace_does) :-
    model(Model),
    printed(portray_model(Model, [readable(true), layout(columns)]), Text),
    assertion(sub_string(Text, _, _, 0, "}")).

test(the_model_is_braced) :-
    model(Model),
    printed(portray_model(Model, [readable(true), layout(columns),
                                  width(80)]), Text),
    assertion(sub_string(Text, 0, 2, _, "{ ")).

%   Down the columns rather than across: the model is sorted, so a column
%   is a run of related literals and the eye can find a predicate.

test(the_columns_read_downwards) :-
    model(Model),
    printed(portray_model(Model, [readable(true), layout(columns),
                                  width(30)]), Text),
    split_string(Text, "\n", "", [First|_]),
    assertion(sub_string(First, _, _, _, "p(1)")),
    assertion(\+ sub_string(First, _, _, _, "p(2)")).

test(a_narrow_terminal_gets_one_column) :-
    model(Model),
    printed(portray_model(Model, [readable(true), layout(columns),
                                  width(12)]), Text),
    split_string(Text, "\n", "", Lines),
    assertion(length(Lines, 7)).             % six literals and the brace

%   The comma goes after every literal but the last one *printed*, which
%   is not the last one in the model when the last column is short.

test(every_literal_but_the_last_is_followed_by_a_comma) :-
    model(Model),
    printed(portray_model(Model, [readable(true), layout(columns),
                                  width(30)]), Text),
    findall(Start, sub_string(Text, Start, _, _, ","), Commas),
    assertion(length(Commas, 5)).

test(an_empty_model_is_empty_braces) :-
    printed(portray_model([], [readable(true), layout(columns)]), Text),
    assertion(Text == "{}").

:- end_tests(render_columns).

		 /*******************************
		 *      THE JUSTIFICATION		*
		 *******************************/

:- begin_tests(render_justification).

tree([ p(1)-[ q(1)-[ r(1)-[] ] ],
       '$nmr'-[]
     ]).

%   The NMR check's node is `'$nmr'`, a name no program gives a
%   predicate, so a program's own `nmr` is a literal like any other.

test(the_check_is_labelled_and_a_user_nmr_is_a_literal) :-
    printed(portray_justification([nmr-[], '$nmr'-[]],
                                  [readable(true), colour(false)]), Text),
    split_string(Text, "\n", " ", Lines),
    assertion(Lines == ["nmr", "global constraints"]).

test(a_child_is_indented_under_its_parent) :-
    tree(Tree),
    printed(portray_justification(Tree, [readable(true)]), Text),
    split_string(Text, "\n", "", Lines),
    assertion(indents(Lines, [4, 8, 12, 4])).

test(the_step_can_be_chosen) :-
    tree(Tree),
    printed(portray_justification(Tree, [readable(true), indent(0), step(2)]),
            Text),
    split_string(Text, "\n", "", Lines),
    assertion(indents(Lines, [0, 2, 4, 0])).

%   Cutting the tree off says so, rather than looking like a leaf.

test(a_depth_limit_marks_what_it_cut) :-
    tree(Tree),
    printed(portray_justification(Tree, [readable(true), max_depth(1)]), Text),
    assertion(sub_string(Text, _, _, _, "...")),
    assertion(\+ sub_string(Text, _, _, _, "r(1)")).

test(and_nothing_follows_the_last_line) :-
    tree(Tree),
    printed(portray_justification(Tree, [readable(true)]), Text),
    assertion(\+ sub_string(Text, _, _, 0, "\n")).

test(and_nothing_follows_the_last_line_in_words) :-
    tree(Tree),
    printed(portray_justification(Tree, [readable(true), human(true)]), Text),
    assertion(Text \== ""),
    assertion(\+ sub_string(Text, _, _, 0, "\n")).

indents([], []).
indents([Line|Lines], [Indent|Indents]) :-
    sub_string(Line, Indent, 1, _, Char),
    Char \== " ",
    !,
    indents(Lines, Indents).

:- end_tests(render_justification).
