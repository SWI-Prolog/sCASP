:- module(test_describe,
          [ test_describe/0
          ]).
:- use_module('../scasp_plunit', []).        % sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/describe)).
:- use_module(library(sscasp/annotations),
              [ program_annotations/2, argument_domain/4,
                description/3, described/3
              ]).
:- use_module(library(plunit)).
:- use_module(library(sscasp/run), [scasp/3]).
:- use_module(library(sscasp/source), [read_program/2]).
:- use_module(library(lists), [member/2, reverse/2]).
:- use_module(library(apply), [exclude/3]).
:- use_module(library(debug), [assertion/1]).

/** <module> Saying a literal in words

`:- describe Literal :: Template` says how to put a literal into a sentence.
It changes nothing about what is proved, which is why these tests look only
at the annotation table and the text that comes out of it.

Two spellings reach the same place: `:- describe` and the deprecated
`:- pred Literal :: Template`, which is the old implementation's.  The
deprecation is a *warning*, so the second spelling has to keep working —
these tests check that it does, and that it warns.

A description written as a **term** says its variable's name with
`'$VAR'(Name)`, because a term list has no reader to ask.  A description
read from a file gets the reader's bindings; `test_source.pl` covers that
path and these tests use the term form.
*/

test_describe :-
    run_tests([ describe_template,
                describe_kinds,
                describe_table,
                describe_deprecated,
                describe_errors,
                describe_subtree
              ]).

		 /*******************************
		 *        THE TEMPLATE		*
		 *******************************/

:- begin_tests(describe_template).

%!  text(+Directive, +Literal, -Text) is semidet.
%
%   What a `describe` directive says about a literal, in one step, which
%   is the whole of what a template is for.

text(Directive, Literal, Text) :-
    annotation(Directive, Annotation),
    description_text(Annotation, Literal, Text).

test(a_placeholder_takes_the_value_of_its_variable) :-
    text(describe(flies('$VAR'('X')) :: "@(X) can fly"),
         flies(tweety), Text),
    assertion(Text == 'tweety can fly').

test(a_template_may_have_several_placeholders) :-
    text(describe(likes('$VAR'('A'), '$VAR'('B')) :: "@(A) likes @(B)"),
         likes(tweety, seed), Text),
    assertion(Text == 'tweety likes seed').

test(a_variable_may_be_named_twice) :-
    text(describe(same('$VAR'('X')) :: "@(X) is @(X)"),
         same(one), Text),
    assertion(Text == 'one is one').

test(the_brace_spelling_says_the_same) :-
    text(describe(flies('$VAR'('X')) :: "{{X}} can fly"),
         flies(tweety), Text),
    assertion(Text == 'tweety can fly').

test(a_placeholder_may_carry_a_type) :-
    text(describe(flies('$VAR'('X')) :: "@(X:bird) can fly"),
         flies(tweety), Text),
    assertion(Text == 'tweety can fly').

test(a_template_with_no_placeholder_is_a_constant) :-
    text(describe(raining :: "it is raining"),
         raining, Text),
    assertion(Text == 'it is raining').

test(an_atom_is_a_template_too) :-
    text(describe(flies('$VAR'('X')) :: '@(X) can fly'),
         flies(tweety), Text),
    assertion(Text == 'tweety can fly').

%   The Prolog reader calls a description's variable a singleton, so a
%   program in a Prolog file underscores it.  The placeholder then has to
%   find it under either spelling, or the accommodation is no help.

test(the_leading_underscore_is_ignored, [forall(underscored(Literal, Template))]) :-
    text(describe(Literal :: Template), flies(tweety), Text),
    assertion(Text == 'tweety can fly').

underscored(flies('$VAR'('_X')), "@(X) can fly").
underscored(flies('$VAR'('X')),  "@(_X) can fly").
underscored(flies('$VAR'('_X')), "@(_X) can fly").

%   An unbound argument is printed as a variable, so a description of a
%   literal that is not ground still says something.

test(an_unbound_argument_prints_as_one) :-
    text(describe(flies('$VAR'('X')) :: "@(X) can fly"), flies(_), Text),
    assertion(sub_atom(Text, _, _, 0, ' can fly')).

:- end_tests(describe_template).

		 /*******************************
		 *         THE LITERAL		*
		 *******************************/

:- begin_tests(describe_kinds).

%   The vocabulary is mangle.pl's, so that a renderer looking at a proof
%   node can ask for the description of exactly the literal it has.

test(the_four_kinds_are_told_apart, [forall(kind(Literal, Kind))]) :-
    annotation(describe(Literal :: "whatever"), Annotation),
    assertion(Annotation = description(Kind, flies/1, _, _)).

kind(flies('$VAR'('X')),       plain).
kind(not flies('$VAR'('X')),   dual).
kind(-flies('$VAR'('X')),      neg).
kind(not -flies('$VAR'('X')),  dualneg).

test(a_description_of_a_negated_literal_fits_only_that) :-
    annotation(describe(not flies('$VAR'('X')) :: "@(X) cannot fly"), A),
    assertion(description_text(A, not flies(polly), 'polly cannot fly')),
    assertion(\+ description_text(A, flies(polly), _)).

:- end_tests(describe_kinds).

		 /*******************************
		 *          THE TABLE		*
		 *******************************/

:- begin_tests(describe_table).

program([ (:- describe flies('$VAR'('X')) :: "@(X) can fly"),
          (:- describe flies(tweety) :: "the famous one flies"),
          (:- describe not flies('$VAR'('X')) :: "@(X) cannot fly"),
          bird(tweety),
          bird(polly),
          penguin(polly),
          (flies(X) :- bird(X), not penguin(X))
        ]).

test(a_description_reaches_the_table) :-
    program(Terms),
    program_annotations(Terms, A),
    assertion(described(A, not flies(polly), 'polly cannot fly')).

%   `not flies(X)` and `flies(X)` are two literals and two descriptions,
%   and asking for one must not answer with the other.

test(a_negated_literal_reads_its_own_description) :-
    program(Terms),
    program_annotations(Terms, A),
    assertion(described(A, flies(polly), 'polly can fly')),
    assertion(described(A, not flies(polly), 'polly cannot fly')).

%   Later wins, which is what a reader would expect: a description
%   written after another is the more particular case.

test(the_later_description_is_the_more_specific_one) :-
    program(Terms),
    program_annotations(Terms, A),
    assertion(described(A, flies(tweety), 'the famous one flies')),
    assertion(described(A, flies(polly), 'polly can fly')).

%   Asking must not bind what is asked about: the description of
%   flies(tweety) says nothing about flies(X).

test(a_description_does_not_bind_the_literal) :-
    program(Terms),
    program_annotations(Terms, A),
    described(A, flies(V), Text),
    assertion(var(V)),
    assertion(Text \== 'the famous one flies').

test(a_literal_nothing_describes_has_no_text) :-
    program(Terms),
    program_annotations(Terms, A),
    assertion(\+ described(A, bird(tweety), _)).

test(every_description_that_fits_is_offered) :-
    program(Terms),
    program_annotations(Terms, A),
    findall(Text,
            ( description(A, flies(tweety), D),
              description_text(D, flies(tweety), Text)
            ),
            Texts),
    assertion(Texts == ['the famous one flies', 'tweety can fly']).

%   A description says nothing about what is proved.  That is the whole
%   claim of the feature, and it is cheap to check.

test(a_description_changes_no_answer) :-
    program(Terms),
    findall(X-Model, scasp(Terms, flies(X), Model), With),
    exclude_descriptions(Terms, Bare),
    findall(X-Model, scasp(Bare, flies(X), Model), Without),
    assertion(With == Without).

exclude_descriptions(Terms, Bare) :-
    exclude(description, Terms, Bare).

description((:- describe _)).

:- end_tests(describe_table).

		 /*******************************
		 *        THE OLD SPELLING	*
		 *******************************/

:- begin_tests(describe_deprecated).

%   `:- pred p(X) :: "..."` is the old implementation's spelling.  It
%   still works, and it warns, because in *this* implementation `:- pred`
%   means a signature and a signature changes what is proved.

test(the_old_spelling_still_describes) :-
    annotation(pred(flies('$VAR'('X')) :: "@(X) can fly"), A),
    assertion(description_text(A, flies(tweety), 'tweety can fly')).

%   The warning belongs to *reading* the program and not to compiling
%   it: `annotation/2` is asked once a compile, and a program queried in
%   a loop would repeat the warning once a query.

test(compiling_the_old_spelling_does_not_warn) :-
    warnings(annotation(pred(flies('$VAR'('X')) :: "@(X) can fly"), _),
             Warnings),
    assertion(Warnings == []).

test(reading_the_old_spelling_warns) :-
    warnings(read_temp(":- pred p(X) :: '@(X) is p'.\np(1).\n", _), Warnings),
    assertion(Warnings = [scasp(deprecated_pred_description(_))]).

test(reading_the_new_spelling_does_not) :-
    warnings(read_temp(":- describe p(X) :: '@(X) is p'.\np(1).\n", _), Warnings),
    assertion(Warnings == []).

%   `#pred` is the spelling of the files that already exist, and it is
%   not deprecated -- it is read, and it becomes a description.

test(the_file_spelling_does_not_warn) :-
    warnings(read_temp("#pred p(X) :: '@(X) is p'.\np(1).\n", Source), Warnings),
    assertion(Warnings == []),
    program_annotations(Source.terms, A),
    assertion(described(A, p(1), '1 is p')).

%   The two directives share a name and mean different things, so the
%   table has to keep them apart: a signature is not a description and a
%   description is not a signature.

test(a_signature_is_not_a_description) :-
    program_annotations([ (:- domain day = [mon, tue]),
                          (:- pred wrap(day)),
                          wrap(mon)
                        ], A),
    assertion(argument_domain(A, wrap/1, 1, day)),
    assertion(A.descriptions == []).

test(a_description_is_not_a_signature) :-
    program_annotations([ (:- pred wrap('$VAR'('X')) :: "@(X) is wrapped"),
                          wrap(mon)
                        ], A),
    assertion(\+ argument_domain(A, wrap/1, 1, _)),
    assertion(described(A, wrap(mon), 'mon is wrapped')).

:- end_tests(describe_deprecated).

		 /*******************************
		 *           ERRORS		*
		 *******************************/

:- begin_tests(describe_errors).

test(a_placeholder_naming_no_variable,
     [ throws(error(existence_error(description_variable, 'Y'), _)) ]) :-
    annotation(describe(flies('$VAR'('X')) :: "@(Y) can fly"), _).

test(a_template_that_is_not_text,
     [ throws(error(domain_error(description_template, 42), _)) ]) :-
    annotation(describe(flies('$VAR'('X')) :: 42), _).

test(a_described_literal_that_is_not_callable,
     [ throws(error(domain_error(described_literal, 42), _)) ]) :-
    annotation(describe(42 :: "the answer"), _).

test(a_describe_without_a_template,
     [ throws(error(domain_error(description, flies(_)), _)) ]) :-
    annotation(describe(flies(_)), _).

:- end_tests(describe_errors).

		 /*******************************
		 *          MESSAGES		*
		 *******************************/

%!  warnings(:Goal, -Warnings) is det.
%
%   The warning terms Goal printed, and not printed.  A deprecation is a
%   message rather than an error, so intercepting it is the only way to
%   see it — and the only way to keep `make check` from printing it
%   dozens of times over.
%
%   The hook is installed for the life of the file and collects only
%   while `seen/1` says to, which is what makes it safe under `jobs(N)`:
%   `seen/1` is thread-local, so a unit running in another thread is
%   neither silenced nor read.

:- multifile user:message_hook/3.
:- thread_local seen/1.

user:message_hook(Term, warning, _) :-
    test_describe:note(Term).

note(Term) :-
    retract(seen(Seen)),
    !,
    assertz(seen([Term|Seen])).

:- meta_predicate
    warnings(0, -).

warnings(Goal, Warnings) :-
    setup_call_cleanup(
        assertz(seen([])),
        once(Goal),
        forget(Reversed)),
    reverse(Reversed, Warnings).

forget(Seen) :-
    (   retract(seen(Seen))
    ->  true
    ;   Seen = []
    ).

%!  read_temp(+Text, -Source) is det.
%
%   Read an s(CASP) program from Text.  The deprecation is the *reader's*
%   and the only way to provoke it is to read something.

read_temp(Text, Source) :-
    setup_call_cleanup(
        tmp_file_stream(File, Out, [extension(pl), encoding(utf8)]),
        ( write(Out, Text), close(Out) ),
        true),
    setup_call_cleanup(true,
                       read_program(File, Source),
                       delete_file(File)).


		 /*******************************
		 *           SUBTREES		*
		 *******************************/

:- begin_tests(describe_subtree).

test(a_pattern_is_a_tree_description) :-
    annotation(describe(p('$VAR'('X')) - [q('$VAR'('X'))|-] :: "@(X) is p"),
               Annotation),
    Annotation = tree_description(plain, p/1, p(X), [q(Y)|-],
                                  [@(Z:''), text(' is p')]),
    assertion(X == Y),
    assertion(X == Z).

test(an_unbound_pattern_keeps_nothing) :-
    annotation(describe(p(_) - _ :: "p"), Annotation),
    assertion(Annotation = tree_description(plain, p/1, _, -, _)).

test(classical_negation_is_not_a_pattern) :-
    annotation(describe(-p(_) :: "not p"), Annotation),
    assertion(Annotation = description(neg, p/1, _, _)).

test(the_literal_of_a_pattern) :-
    description_literal(p(a) - [q] :: "p", Literal),
    assertion(Literal == p(a)).

:- end_tests(describe_subtree).
