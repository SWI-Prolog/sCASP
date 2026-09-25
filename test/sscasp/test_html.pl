:- module(test_html,
          [ test_html/0
          ]).
:- use_module('../scasp_plunit', []).        % sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/html)).
:- use_module(library(sscasp/html_text), [emit_as//2, fixup_layout/2]).
:- use_module(library(sscasp/render), [scasp_readable/2]).
:- use_module(library(sscasp/run), [scasp/4]).
:- use_module(library(sscasp/annotations), [program_annotations/2]).
:- use_module(library(plunit)).
:- use_module(library(http/html_write), [print_html/1]).
:- use_module(library(lists), [member/2, append/3]).
:- use_module(library(debug), [assertion/1]).

/** <module> An answer described once

`sscasp/html.pl` describes an answer as HTML and `html_text.pl` reads the
same description as plain text.  Both are checked here, and against each
other: the point of the design is that there is one description, so a test
that only looked at one of them would not be testing the thing.

The text is what most of these read, because it is what a person sees and
because an assertion about it is legible.  Where the HTML carries something
the text deliberately drops — the machine form of a literal, the classes a
stylesheet needs — the HTML is read instead.
*/

test_html :-
    run_tests([ html_text_vocabulary,
                html_text_descriptions,
                html_text_structure,
                html_markup,
                html_templates
              ]).

		 /*******************************
		 *          THE HARNESS		*
		 *******************************/

%!  text(:HTML, -Text) is det.
%
%   What the plain-text reading of an HTML description comes out as.
%   The `\` is not decoration: `emit_as//2` takes `html//1` *input*, and
%   a DCG body is input only when it is written as one.

:- meta_predicate
    text(//, -).

text(Goal, Text) :-
    phrase(emit_as(\Goal, plain), Tokens0),
    fixup_layout(Tokens0, Tokens),
    with_output_to(string(Text),
                   print_message_lines(current_output, '', Tokens)).

%!  markup(:HTML, -String) is det.

:- meta_predicate
    markup(//, -).

markup(Goal, String) :-
    phrase(emit_as(\Goal, html), Tokens),
    with_output_to(string(String), print_html(Tokens)).

%!  answer(+Program, +Query, -Model, -Justification, -Annotations) is nondet.
%
%   An answer, made readable — which is what the renderer takes.  Model
%   and justification are made readable **together**, or a variable they
%   share is named twice and means two different things.

answer(Program, Query, Model, Justification, Annotations) :-
    program_annotations(Program, Annotations),
    scasp(Program, Query, Model0, Justification0),
    scasp_readable(Model0-Justification0, Model-Justification).

%!  said(+Program, +Query, -Text) is nondet.
%
%   The model of the first answer, in words.

said(Program, Query, Text) :-
    answer(Program, Query, Model, _, Annotations),
    text(html_model(Model, [annotations(Annotations)]), Text).

contains(Text, Needle) :-
    sub_string(Text, _, _, _, Needle).

		 /*******************************
		 *        THE VOCABULARY		*
		 *******************************/

/** Without a description a literal is said with `lang.pl`'s words.  These
    are the ones every program has, so they are the ones a program that
    describes nothing gets.
*/

:- begin_tests(html_text_vocabulary).

test(a_literal_holds_for_its_arguments) :-
    once(said([p(a)], p(_), Text)),
    assertion(contains(Text, "p holds for a")).

test(an_atom_simply_holds) :-
    once(said([p], p, Text)),
    assertion(contains(Text, "p holds")).

test(a_negative_literal_says_there_is_no_evidence) :-
    once(said([q(1), (p(X) :- not q(X))], p(2), Text)),
    assertion(contains(Text, "there is no evidence that q holds for 2")).

test(classical_negation_says_it_is_not_the_case) :-
    once(said([penguin(polly), (-flies(X) :- penguin(X))], -flies(polly), Text)),
    assertion(contains(Text, "it is not the case that flies holds for polly")).

%   A coinductive hypothesis is in the *proof* and not in the model:
%   the model says `a` holds, and why it holds is that it was assumed.

test(a_coinductive_assumption_is_said_to_be_one) :-
    once(answer([(a :- not b), (b :- not a)], a, _, Justification, A)),
    text(html_justification(Justification, [annotations(A)]), Text),
    assertion(contains(Text, "it is assumed that a holds")).

%   A constrained variable says what is known about it rather than its
%   name, which is the whole of what an ungrounded answer has to say.

test(a_dense_constraint_is_read_out) :-
    once(said([(p(X) :- X #> 3, X #< 10)], not p(_), Text)),
    assertion(contains(Text, "is less than or equal to 3")).

test(a_disequality_is_read_out) :-
    once(said([q(1), (r(X) :- not q(X))], r(_), Text)),
    assertion(contains(Text, "is not 1")).

%   Several prohibited values are **one** statement, not a conjunction of
%   them: that is what the store keeps and it is the shorter thing to
%   read.

test(several_constraints_are_joined) :-
    once(said([q(1), q(2), (r(X) :- not q(X))], r(_), Text)),
    assertion(contains(Text, "is not 1 or 2")).

%   A constraint appears once however many proved literals led to it.
%   `consistent/3` posts one per pair and several collapse to the same
%   thing, and "A is not polly and A is not polly and A is not polly"
%   says nothing the first one did not.

test(a_constraint_said_twice_is_said_once) :-
    once(said([ bird(tweety), bird(polly), penguin(polly),
                (flies(X) :- bird(X), not penguin(X)),
                (-flies(X) :- penguin(X))
              ], -flies(polly), Text)),
    split_string(Text, "\n", "", Lines),
    forall(member(Line, Lines),
           ( occurrences(Line, "is not polly", N),
             assertion(N =< 1)
           )).

occurrences(Text, Needle, Count) :-
    findall(Start, sub_string(Text, Start, _, _, Needle), Starts),
    length(Starts, Count).

:- end_tests(html_text_vocabulary).

		 /*******************************
		 *       THE DESCRIPTIONS		*
		 *******************************/

/** A `:- describe` is the principled route: a program that says what its
    predicates mean gets output in its own words rather than in the
    renderer's.
*/

:- begin_tests(html_text_descriptions).

program([ bird(tweety), bird(polly), penguin(polly),
          (flies(X) :- bird(X), not penguin(X)),
          (:- describe flies('$VAR'('X'))     :: "@(X) can fly"),
          (:- describe not flies('$VAR'('X')) :: "@(X) cannot fly"),
          (:- describe bird('$VAR'('X'))      :: "@(X) is a bird")
        ]).

test(a_description_is_what_the_literal_is_said_with) :-
    program(P),
    once(said(P, flies(tweety), Text)),
    assertion(contains(Text, "tweety can fly")),
    assertion(contains(Text, "tweety is a bird")).

%   `not flies(X)` and `flies(X)` are two literals and two descriptions.
%   A literal with no description of its own gets the vocabulary's
%   prefix and its positive description underneath, which is why
%   `not penguin` reads as it does and `not flies` does not.

test(a_negative_literal_reads_its_own_description) :-
    program(P),
    once(said(P, not flies(polly), Text)),
    assertion(contains(Text, "polly cannot fly")),
    assertion(\+ contains(Text, "there is no evidence that polly can fly")).

test(and_falls_back_on_the_vocabulary_without_one) :-
    program(P),
    once(said(P, flies(tweety), Text)),
    assertion(contains(Text, "there is no evidence that penguin holds for tweety")).

test(a_description_of_an_individual_wins) :-
    program(P0),
    append(P0, [(:- describe flies(tweety) :: "the famous one flies")], P),
    once(said(P, flies(tweety), Text)),
    assertion(contains(Text, "the famous one flies")).

test(a_placeholder_takes_a_constrained_variable) :-
    once(said([ q(1),
                (r(X) :- not q(X)),
                (:- describe r('$VAR'('X')) :: "@(X) is admissible")
              ], r(_), Text)),
    assertion(contains(Text, "is not 1 is admissible")).

test(a_typed_placeholder_says_the_type) :-
    once(said([ p(a),
                (:- describe p('$VAR'('X')) :: "@(X:thing) is here")
              ], p(_), Text)),
    assertion(contains(Text, "the thing a is here")).

:- end_tests(html_text_descriptions).

		 /*******************************
		 *         THE STRUCTURE		*
		 *******************************/

:- begin_tests(html_text_structure).

%   A justification is a tree and the text is an indented one: a literal
%   with a body says "because" and its body is indented under it.

test(a_body_is_indented_under_its_head) :-
    once(answer([bird(tweety), (flies(X) :- bird(X))], flies(tweety),
                _, Justification, A)),
    text(html_justification(Justification, [annotations(A)]), Text),
    split_string(Text, "\n", "", Lines),
    member_containing(Lines, "flies holds for tweety, because", Head),
    member_containing(Lines, "bird holds for tweety", Body),
    indent(Head, HeadIndent),
    indent(Body, BodyIndent),
    assertion(BodyIndent > HeadIndent).

test(siblings_are_joined_with_and) :-
    once(answer([bird(tweety), tame(tweety),
                 (flies(X) :- bird(X), tame(X))], flies(tweety),
                _, Justification, A)),
    text(html_justification(Justification, [annotations(A)]), Text),
    assertion(contains(Text, ", and")).

%   The NMR check is a question of its own, so it is one node with a
%   name, and `nmr(false)` keeps whatever it needed closed.

test(the_nmr_check_is_one_node) :-
    once(answer([p], p, _, Justification, A)),
    text(html_justification(Justification, [annotations(A)]), Text),
    assertion(contains(Text, "The global constraints hold")).

test(and_can_be_left_closed) :-
    once(answer([ bird(tweety), bird(polly), penguin(polly),
                  (flies(X) :- bird(X), not penguin(X)),
                  (-flies(X) :- penguin(X))
                ], -flies(polly), _, Justification, A)),
    text(html_justification(Justification, [nmr(false), annotations(A)]), Closed),
    text(html_justification(Justification, [annotations(A)]), Open),
    assertion(contains(Closed, "The global constraints hold")),
    string_length(Closed, Short),
    string_length(Open, Long),
    assertion(Short < Long).

test(the_query_is_a_question) :-
    text(html_query((flies(tweety), bird(tweety)), []), Text),
    assertion(contains(Text, "I would like to know if")),
    assertion(contains(Text, "flies holds for tweety")),
    assertion(contains(Text, "bird holds for tweety")).

member_containing(Lines, Needle, Line) :-
    member(Line, Lines),
    sub_string(Line, _, _, _, Needle),
    !.

indent(Line, Indent) :-
    sub_string(Line, Indent, 1, _, Char),
    Char \== " ",
    !.

:- end_tests(html_text_structure).

		 /*******************************
		 *          THE MARKUP		*
		 *******************************/

/** What the text drops on purpose, so that dropping it stays deliberate.
*/

:- begin_tests(html_markup).

test(html_carries_both_readings) :-
    once(answer([bird(tweety)], bird(tweety), Model, _, A)),
    markup(html_model(Model, [annotations(A)]), HTML),
    assertion(contains(HTML, "class=\"human\"")),
    assertion(contains(HTML, "class=\"machine\"")),
    assertion(contains(HTML, " holds for ")).

test(and_text_keeps_only_the_human_one) :-
    once(answer([bird(tweety)], bird(tweety), Model, _, A)),
    text(html_model(Model, [annotations(A)]), Text),
    assertion(contains(Text, "bird holds for tweety")),
    assertion(\+ contains(Text, "bird(tweety)")).

test(show_machine_says_the_literal_itself) :-
    once(answer([bird(tweety)], bird(tweety), Model, _, A)),
    text(html_model(Model, [show(machine), annotations(A)]), Text),
    assertion(contains(Text, "bird(tweety)")),
    assertion(\+ contains(Text, "holds for")).

%   The machine form is written with **our** operators, or `not p` comes
%   out as `not(p)` and a reader cannot tell it from a predicate called
%   `not`.

test(the_machine_form_uses_this_languages_operators) :-
    once(answer([q(1), (p(X) :- not q(X))], p(2), Model, _, A)),
    text(html_model(Model, [show(machine), annotations(A)]), Text),
    assertion(contains(Text, "not q(2)")).

test(the_justification_is_a_nested_list) :-
    once(answer([bird(tweety), (flies(X) :- bird(X))], flies(tweety),
                _, Justification, A)),
    markup(html_justification(Justification, [annotations(A)]), HTML),
    assertion(contains(HTML, "class=\"scasp-justification\"")),
    assertion(contains(HTML, "class=\"collapsable-content\"")).

:- end_tests(html_markup).

		 /*******************************
		 *       HTML TEMPLATES		*
		 *******************************/

/** A description may be written as HTML rather than as text, which is what
    makes this the principled way to generate output: the program says how
    its literals should look, and the renderer places them.
*/

:- begin_tests(html_templates).

program([ bird(tweety),
          (:- describe bird('$VAR'('X'))
               :: html(['the bird ', b('$VAR'('X'))]))
        ]).

test(an_html_template_is_emitted_as_markup) :-
    program(P),
    once(answer(P, bird(tweety), Model, _, A)),
    markup(html_model(Model, [annotations(A)]), HTML),
    assertion(contains(HTML, "the bird <b>")),
    assertion(contains(HTML, "tweety")).

%   The markup is dropped in text and the content is not.  Without that
%   `<b>tweety</b>` comes out as `b(tweety)`, which is not a description.

test(and_its_content_survives_as_text) :-
    program(P),
    once(answer(P, bird(tweety), Model, _, A)),
    text(html_model(Model, [annotations(A)]), Text),
    assertion(contains(Text, "the bird tweety")),
    assertion(\+ contains(Text, "b(tweety)")).

%   A variable in an HTML template is a *value*, not an element.  Handing
%   it to html//1 as it stands produces `<| $VAR="A">`; it has to be
%   rendered as the renderer renders any other value, constraints and
%   all.

test(a_variable_in_a_template_is_rendered_as_a_value) :-
    once(answer([ q(1),
                  (r(X) :- not q(X)),
                  (:- describe r('$VAR'('X')) :: html(['ok ', b('$VAR'('X'))]))
                ], r(_), Model, _, A)),
    text(html_model(Model, [annotations(A)]), Text),
    assertion(contains(Text, "ok A is not 1")),
    assertion(\+ contains(Text, "$VAR")).

test(and_stays_well_formed_in_html) :-
    once(answer([ q(1),
                  (r(X) :- not q(X)),
                  (:- describe r('$VAR'('X')) :: html(['ok ', b('$VAR'('X'))]))
                ], r(_), Model, _, A)),
    markup(html_model(Model, [annotations(A)]), HTML),
    assertion(\+ contains(HTML, "$VAR")),
    assertion(\+ contains(HTML, "<| ")).

:- end_tests(html_templates).
