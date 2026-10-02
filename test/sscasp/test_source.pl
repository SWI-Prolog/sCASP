:- module(test_source,
          [ test_source/0
          ]).
:- use_module('../scasp_plunit', []).        % sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/source)).
:- use_module(library(plunit)).
:- use_module(library(sscasp/annotations), [program_annotations/2, described/3]).
:- use_module(library(lists), [member/2, memberchk/2]).
:- use_module(library(debug), [assertion/1]).

/** <module> Reading an s(CASP) source file

`sscasp/source.pl` reads a program written for the `scasp` executable.  Its
job is to sort what is in the file into the program and the things that are
*about* the program, and to splice in what a file includes.

The corpus tests read twenty-one real programs through it, so what is worth
testing here is the sorting itself — which is where the two meanings of
`pred` meet.
*/

test_source :-
    run_tests([source_terms, source_directives, source_includes]).

%!  read_temp(+Text, -Source) is det.
%
%   Read Source from Text, written to a file because that is what
%   `read_program/2` takes and the alternative is a second entry point
%   that exists only for tests.

read_temp(Text, Source) :-
    setup_call_cleanup(
        tmp_file_stream(File, Out, [extension(pl), encoding(utf8)]),
        ( write(Out, Text), close(Out) ),
        true),
    setup_call_cleanup(true,
                       read_program(File, Source),
                       delete_file(File)).

:- begin_tests(source_terms).

test(clauses_and_facts) :-
    read_temp("p(1).\np(X) :- q(X).\n", S),
    assertion(S.terms = [p(1), (p(_) :- q(_))]).

test(a_constraint_is_a_term_of_the_program) :-
    read_temp(":- p, q.\n", S),
    assertion(S.terms == [(:- p, q)]).

%   The last query wins, which is the one the `scasp` executable answers.

test(the_query) :-
    read_temp("p(1).\n?- p(X).\n", S),
    assertion(S.query = p(_)).

test(the_last_of_several) :-
    read_temp("?- p.\n?- q.\n", S),
    assertion(S.query == q).

test(no_query_at_all) :-
    read_temp("p(1).\n", S),
    assertion(S.query == none).

test(a_query_is_not_a_clause) :-
    read_temp("p(1).\n?- p(X).\n", S),
    assertion(S.terms == [p(1)]).

:- end_tests(source_terms).

		 /*******************************
		 *       THE # DIRECTIVES	*
		 *******************************/

%   `#` is a prefix operator at 1200 with `pred`, `show` and the rest
%   under it, so `#pred p(X) :: 'text'.` reads as `#(pred(::(...)))`.
%   Those operators are declared in `source.pl` rather than in `ops.pl`:
%   they are the syntax of files that already exist.

:- begin_tests(source_directives).

%   A template is a **description**, so it is a directive of the program
%   as well as a report about the file.  The bindings come with it: the
%   placeholder says `X` and a read term does not carry that.

test(a_template_is_a_description) :-
    read_temp("#pred p(X) :: '@(X) is p'.\np(1).\n", S),
    assertion(S.templates = [(p(_) :: '@(X) is p')]),
    assertion(S.terms = [(:- describe((p(V) :: '@(X) is p'), ['X'=V])), p(1)]).

test(a_template_reaches_the_annotation_table) :-
    read_temp("#pred p(X) :: '@(X) is p'.\np(1).\n", S),
    program_annotations(S.terms, A),
    assertion(described(A, p(1), '1 is p')).

test(the_describe_spelling_says_the_same) :-
    read_temp(":- describe p(X) :: '@(X) is p'.\np(1).\n", S),
    program_annotations(S.terms, A),
    assertion(described(A, p(1), '1 is p')),
    assertion(S.templates == []).

test(shown_predicates) :-
    read_temp("#show p/1.\np(1).\n", S),
    assertion(S.shown == [p/1]),
    assertion(S.terms == [(:- show(p/1)), p(1)]).

%   An abducible is a *statement*, not metadata about the program, so it
%   stays in the terms — as `:- abducible q(1)`, which `compile.pl`
%   expands into the four rules of its even loop.  The `abducible` field
%   is the report, for a caller that wants to know what was declared.

test(abducibles) :-
    read_temp("#abducible q(1).\np(1).\n", S),
    get_dict(abducible, S, Abducible),  % S.abducible: a prefix operator here
    assertion(Abducible == [q(1)]),
    assertion(S.terms == [(:- abducible(q(1))), p(1)]).

%   `pred` means two things and the `::` tells them apart: a template
%   has one, a signature does not, whichever prefix it is written with.

test(the_two_spellings_of_pred_do_not_collide) :-
    read_temp("#pred p(X) :: 'text'.\n:- pred p(day).\n:- domain day = [mon].\np(mon).\n", S),
    assertion(S.templates = [(p(_) :: text)]),
    assertion(memberchk((:- pred p(day)), S.terms)),
    assertion(memberchk((:- domain day = [mon]), S.terms)).

test(a_hash_pred_without_text_is_a_signature) :-
    read_temp("#pred p(day).\n:- domain day = [mon].\np(mon).\n", S),
    assertion(S.templates == []),
    assertion(memberchk((:- pred p(day)), S.terms)).

%   `#sld` and `#domain` are the file's spelling of `:- sld` and
%   `:- domain`: in a file `:- Body` is a global constraint.

test(a_hash_sld_is_a_declaration) :-
    read_temp("#sld big/1.\n#sld (p/1, q/1) as opaque.\nbig(1).\n", S),
    assertion(S.terms == [ (:- sld(big/1)),
                           (:- sld((p/1, q/1) as opaque)),
                           big(1)
                         ]).

test(a_hash_describe_keeps_its_names) :-
    read_temp("#describe flies(X) :: \"@(X) can fly\".\n", S),
    assertion(S.terms = [(:- describe(flies(V) :: "@(X) can fly", ['X'=V]))]).

test(a_hash_domain_is_a_declaration) :-
    read_temp("#domain day = [mon, tue].\n#domain hour = 0..23.\n", S),
    assertion(S.terms == [ (:- domain(day = [mon, tue])),
                           (:- domain(hour = 0..23))
                         ]).

%   One that parses but is not implemented.  `#compute` is s(CASP)'s,
%   and saying so beats treating it as a clause.

test(an_unhandled_directive_is_refused,
     [ throws(error(existence_error(scasp_directive, _), _)) ]) :-
    read_temp("#compute foo.\n", _).

%   A variable read as a term is not a query, however well it unifies
%   with one.

test(a_variable_term_is_refused,
     [ throws(error(instantiation_error, _)) ]) :-
    read_temp("p(1).\nX.\n", _).

:- end_tests(source_directives).

		 /*******************************
		 *         INCLUDES		*
		 *******************************/

:- begin_tests(source_includes).

%   A real corpus program, because the point of `#include` is resolving
%   the name relative to the file that wrote it.

included(File) :-
    source_file(included(_), Here),
    file_directory_name(Here, Sscasp),
    file_directory_name(Sscasp, Dir),
    atomic_list_concat([Dir, '/all_programs/bec_light.pl'], File).

test(an_included_file_is_read) :-
    included(File),
    read_program(File, S),
    assertion(S.files = [_, _|_]),
    assertion(( member(F, S.files),
                file_base_name(F, 'bec_theory.incl')
              )).

test(its_clauses_are_in_the_program) :-
    included(File),
    read_program(File, S),
    assertion(( member(Term, S.terms),
                Term = (holdsAt(_, _) :- _)
              )).

test(the_query_survives_the_include) :-
    included(File),
    read_program(File, S),
    assertion(S.query \== none).

%   Two files that include the same third: its clauses once, not once per
%   file that includes it.

test(a_file_included_twice_is_read_once) :-
    tmp_file(includes, Dir),
    make_directory(Dir),
    call_cleanup(
        ( write_file(Dir, 'c.incl', "c(1).\n"),
          write_file(Dir, 'a.incl', "#include('c.incl').\n"),
          write_file(Dir, 'b.incl', "#include('c.incl').\n"),
          write_file(Dir, 'main.pl',
                     "#include('a.incl').\n#include('b.incl').\n?- c(X).\n"),
          directory_file_path(Dir, 'main.pl', Main),
          read_program(Main, S)
        ),
        delete_directory_and_contents(Dir)),
    assertion(S.terms == [c(1)]),
    length(S.files, Files),
    assertion(Files == 4).

write_file(Dir, Name, Text) :-
    directory_file_path(Dir, Name, File),
    setup_call_cleanup(open(File, write, Out),
                       write(Out, Text),
                       close(Out)).

:- end_tests(source_includes).
