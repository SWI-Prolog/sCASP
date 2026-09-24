:- module(sscasp_main,
          [ main/1                      % +Argv
          ]).
:- use_module(ops).
:- use_module(source,      [read_program/2]).
:- use_module(run,         [sscasp/4, sscasp_program/2]).
:- use_module(render,      [sscasp_readable/3, portray_model/2,
                            portray_justification/2, portray_query/2]).
:- use_module(listing,     [portray_listing/3, titled/2]).
:- use_module(json,        [answer_json/3, answers_json/2]).
:- use_module(html,        [html_model//2, html_justification//2,
                            html_query//2]).
:- use_module(html_text,   [emit_text/1]).
:- use_module(annotations, [program_annotations/2]).
:- use_module(library(main), [argv_options/3]).
:- use_module(library(lists), [member/2, append/3, last/2]).
:- use_module(library(apply), [maplist/3, foldl/4]).
:- use_module(library(option), [option/2, option/3, merge_options/3]).
:- use_module(library(ansi_term), [ansi_format/3]).
:- use_module(library(json), [json_write_dict/3]).
:- use_module(library(http/html_write)).

/** <module> The sscasp program

    $ sscasp --tree bird.pl
    % Query
    ?- flies(A).

    % Answer 1 (0.003 sec)
    % Model
    { bird(tweety),  flies(tweety),  not ab(tweety) }
    % Justification
        flies(tweety)
            bird(tweety)
            not ab(tweety)
    A = tweety.

Read one or more s(CASP) source files, answer the query the last of them
ends with, and exit.  This is the old implementation's `scasp`, over the
new solver.

## The shape of a main program

`library(main)` does the work: `main/1` is called by
`:- initialization(main, main)`, which also arranges that an uncaught
error exits non-zero and that `halt/0` inside is not an error.
`argv_options/3` reads the command line against the `opt_type/3` table
below and prints `--help` from `opt_help/2`, so the flags and their
documentation are one thing rather than two.

Two things this file owes the caller:

  - **The exit status says whether the query was answered.**  0 for at
    least one answer, 1 for none, 2 for an error.  A program that cannot
    be told apart from a program with no answers is no use in a script.
  - **An expected error is reported as itself.**  A missing file or a
    program with no query is a message and `halt(2)`, not a stack trace.

## What it does not have

`--html` writes a page, but the interactive toplevel (`-i`), `--dcc` and
the four `forall` algorithms are not here because they are not in the
solver either.  `--plaindual`, `--variant` and `--real` likewise.
*/

:- initialization(main, main).

		 /*******************************
		 *          THE OPTIONS		*
		 *******************************/

opt_type(answers,  answers,       nonneg).
opt_type(s,        answers,       nonneg).
opt_type(n,        answers,       nonneg).
opt_type(query,    query,         term([variable_names(_)])).

opt_type(code,     code,          boolean).
opt_type(duals,    duals,         boolean).
opt_type(constraints, constraints, boolean).
opt_type(source,   source,        boolean).
opt_type(compiled, compiled,      boolean).

opt_type(tree,     tree,          boolean).
opt_type(human,    human,         boolean).
opt_type(json,     json,          file(write)).
opt_type(html,     html,          file(write)).

opt_type(width,    width,         nonneg).
opt_type(colour,   colour,        boolean).
opt_type(color,    colour,        boolean).

opt_type(nmr,      nmr,           boolean).
opt_type(olon,     olon,          boolean).

opt_type(version,  version,       boolean).

opt_help(answers,  "Number of answers to report (0 = all)").
opt_help(query,    "Query to answer, instead of the program's own").
opt_help(code,     "Write the program and exit").
opt_help(duals,    "... with the dual rules").
opt_help(constraints, "... with the integrity constraints").
opt_help(source,   "... with the terms as read").
opt_help(compiled, "... with the clauses the solver runs").
opt_help(tree,     "Show the justification of each answer").
opt_help(human,    "Say the answer in natural language").
opt_help(json,     "Write the answers as JSON to FILE ('-' for stdout)").
opt_help(html,     "Write the answers as HTML to FILE ('-' for stdout)").
opt_help(width,    "Assumed terminal width").
opt_help(colour,   "Use ANSI colour").
opt_help(nmr,      "Run the NMR check (--no-nmr to skip it)").
opt_help(olon,     "Compile rules on odd loops (--no-olon to skip them)").
opt_help(version,  "Print the version and exit").
opt_help(help(usage), " [option ...] file ...").

opt_meta(answers,  'COUNT').
opt_meta(query,    'GOAL').
opt_meta(json,     'FILE').
opt_meta(html,     'FILE').
opt_meta(width,    'COLUMNS').

		 /*******************************
		 *           THE MAIN		*
		 *******************************/

%!  main(+Argv) is det.

main(Argv) :-
    set_prolog_flag(rational_syntax, natural),
    set_prolog_flag(prefer_rationals, true),
    argv_options(Argv, Files, Options),
    catch_with_backtrace(run(Files, Options), Error, failed(Error)).

run(_, Options) :-
    option(version(true), Options),
    !,
    version(Version),
    format("sscasp ~w~n", [Version]).
run([], _) :-
    !,
    print_message(error, sscasp(no_input_files)),
    halt(2).
run(Files, Options) :-
    compile_flags(Options),
    read_files(Files, Source),
    Terms = Source.terms,
    (   option(code(true), Options)
    ->  listing_options(Options, ListingOptions),
        show_source(Terms, ListingOptions),
        portray_listing(Terms, Source.query, ListingOptions),
        show_compiled(Terms, ListingOptions)
    ;   query(Source, Query, Bindings, Options)
    ->  answer(Terms, Query, Bindings, Options)
    ;   print_message(error, sscasp(no_query)),
        halt(2)
    ).

%   `--code` on its own is the query and the user predicates; naming any
%   other section asks for that one and not for the defaults, which is
%   what `scasp --code --long` means without a second flag to say so.

listing_options(Options, ListingOptions) :-
    (   named_section(Options)
    ->  ListingOptions = [query(false), user(false)|Options]
    ;   ListingOptions = Options
    ).

named_section(Options) :-
    member(Section, [duals, constraints, source, compiled]),
    Asked =.. [Section, true],
    option(Asked, Options),
    !.

%   Two sections `listing.pl` knows nothing about, because they are not
%   the compiled program: the terms as the reader gave them, and the
%   clauses `emit.pl` makes for the solver to run.

show_source(Terms, Options) :-
    (   option(source(true), Options)
    ->  titled('Program as read', Options),
        forall(member(Term, Terms), show_term(Term))
    ;   true
    ).

show_compiled(Terms, Options) :-
    (   option(compiled(true), Options)
    ->  titled('Emitted clauses', Options),
        sscasp_program(Terms, Clauses),
        forall(member(Clause, Clauses), show_term(Clause))
    ;   true
    ).

show_term(Term) :-
    portray_clause(current_output, Term, [module(sscasp_ops)]).

%!  read_files(+Files, -Source) is det.
%
%   One program from several files, which is what the old
%   implementation's `scasp a.pl b.pl` means: the terms in order, and
%   the **last** query.

read_files(Files, Source) :-
    maplist(read_program, Files, Sources),
    foldl(join, Sources, source{terms: [], query: none, bindings: []}, Source).

join(Source, Joined0, Joined) :-
    append(Joined0.terms, Source.terms, Terms),
    (   Source.query == none
    ->  Query = Joined0.query,
        Bindings = Joined0.bindings
    ;   Query = Source.query,
        Bindings = Source.bindings
    ),
    Joined = Joined0.put(_{terms: Terms, query: Query, bindings: Bindings}).

%!  query(+Source, -Query, -Bindings, +Options) is semidet.
%
%   `--query` beats the program's own, which is what lets one file be
%   asked several things.

query(_, Query, Bindings, Options) :-
    option(query(Term-Bindings), Options),
    !,
    Query = Term.
query(Source, Query, Bindings, _) :-
    Query = Source.query,
    Query \== none,
    Bindings = Source.bindings.

%!  compile_flags(+Options) is det.
%
%   `--no-nmr` and `--no-olon` are the old implementation's flags,
%   spelled and scoped exactly as `prolog/scasp/nmr_check.pl` does so
%   that a caller setting one gets the same thing from either tree.

compile_flags(Options) :-
    (   option(nmr(Nmr), Options)
    ->  set_prolog_flag(scasp_compile_nmr, Nmr)
    ;   true
    ),
    (   option(olon(Olon), Options)
    ->  set_prolog_flag(scasp_compile_olon, Olon)
    ;   true
    ).

%!  version(-Version) is det.
%
%   The pack's version, read from `pack.pl` at **compile** time.  A
%   saved state has no `pack.pl` beside it, so reading it at run time
%   would report `unknown` from the one place it matters; frozen here it
%   says what the state was built from, and the version is still written
%   in one place.

%   The helpers come first: a term is expanded as it is **read**, so
%   anything the expansion calls has to be defined above it.

pack_version(File, Version) :-
    setup_call_cleanup(
        open(File, read, In),
        version_term(In, Version),
        close(In)).

version_term(In, Version) :-
    repeat,
    read_term(In, Term, []),
    (   Term == end_of_file
    ->  !,
        fail
    ;   Term = version(Version)
    ->  !
    ;   fail
    ).

term_expansion(version_from_pack, version(Version)) :-
    prolog_load_context(source, Here),
    file_directory_name(Here, Dir),             % prolog/sscasp
    file_directory_name(Dir, Prolog),           % prolog
    file_directory_name(Prolog, Root),
    atomic_list_concat([Root, '/pack.pl'], Pack),
    catch(pack_version(Pack, Version), _, fail),
    !.
term_expansion(version_from_pack, version(unknown)).

version_from_pack.

		 /*******************************
		 *          ANSWERING		*
		 *******************************/

%!  answer(+Terms, +Query, +Bindings, +Options) is det.
%
%   Exits 1 where the query has no answer, which is not an error and is
%   the one thing a script has to be able to tell.

answer(Terms, Query, Bindings, Options) :-
    reporter(Terms, Options, Report, Finish),
    (   document(Options)
    ->  true                            % a document is all that is written
    ;   show_query(Query, Bindings, Terms, Options)
    ),
    State = state(0),
    forall(( wanted_answer(Terms, Query, Model, Justification, Time, N,
                           Options),
             counted(State, N)
           ),
           call(Report, answer(Query, Model, Justification), Bindings,
                Time, N)),
    call(Finish),
    arg(1, State, Count),
    (   Count > 0
    ->  true
    ;   print_message(warning, sscasp(no_answers)),
        halt(1)
    ).

counted(State, N) :-
    nb_setarg(1, State, N).

%!  wanted_answer(+Terms, +Query, -Model, -Justification, -Time, -N, +Options)
%
%   The answers asked for, numbered and timed.  `--answers 0` means all,
%   which is the old implementation's spelling.

wanted_answer(Terms, Query, Model, Justification, Time, N, Options) :-
    option(answers(Max), Options, 1),
    call_nth(call_time(sscasp(Terms, Query, Model, Justification), Time), N),
    (   Max =:= 0
    ->  true
    ;   N =< Max,
        (   N =:= Max
        ->  !
        ;   true
        )
    ).

		 /*******************************
		 *         THE REPORTS		*
		 *******************************/

%!  reporter(+Terms, +Options, -Report, -Finish) is det.
%
%   How to say an answer, and what to do once they are all said.  JSON
%   and HTML are documents and have to be finished; the terminal is not
%   and does not.

document(Options) :-
    (   option(json(_), Options)
    ;   option(html(_), Options)
    ),
    !.

reporter(_, Options, json_answer(Out, State, Options),
         finish_json(Out, State, Options)) :-
    option(json(File), Options),
    !,
    State = state([]),
    open_sink(File, Out).
reporter(Terms, Options, html_answer(Out, State, Terms, Options),
         finish_html(Out, State, Options)) :-
    option(html(File), Options),
    !,
    State = state([]),
    open_sink(File, Out).
reporter(Terms, Options, text_answer(Terms, Options), true).

open_sink('-', current_output) :-
    !.
open_sink(File, Out) :-
    open(File, write, Out, [encoding(utf8)]).

close_sink(current_output) :-
    !.
close_sink(Out) :-
    close(Out).

%   Collected rather than written as they come, because a document is a
%   whole thing: a run stopped half way should leave no half a document.

collect(State, Item) :-
    arg(1, State, Sofar),
    nb_setarg(1, State, [Item|Sofar]).

collected(State, Items) :-
    arg(1, State, Reversed),
    reverse(Reversed, Items).

		 /*******************************
		 *          AS TEXT		*
		 *******************************/

text_answer(Terms, Options, Answer, Bindings, Time, N) :-
    Answer = answer(Query, Model, Justification),
    answer_heading(N, Time, Options),
    sscasp_readable(answer(Query, Model, Justification),
                    answer(RQuery, RModel, RJustification),
                    Options),
    how(Terms, Options, How),
    titled('Model', Options),
    portray_model(RModel, [layout(columns)|How]),
    nl,
    (   option(tree(true), Options)
    ->  titled('Justification', Options),
        portray_justification(RJustification, How),
        nl
    ;   true
    ),
    _ = RQuery,
    show_bindings(Bindings, Options).

%   `--human` needs the program's descriptions, and they are the
%   program's: `annotations.pl` builds the table from the terms that
%   were read.

how(Terms, Options, How) :-
    (   option(human(true), Options)
    ->  program_annotations(Terms, Annotations),
        How = [readable(true), human(true), annotations(Annotations)|Options]
    ;   How = [readable(true)|Options]
    ).

answer_heading(N, Time, Options) :-
    width(Options, Width),
    Line is Width-8,
    coloured(Options, comment, '~N% ~`―t~*|~n', [Line]),
    coloured(Options, comment, '~N%', []),
    coloured(Options, bold, '~t Answer ~D (~3f sec) ~t~*|~n', [N, Time.cpu, Line]),
    coloured(Options, comment, '~N% ~`―t~*|~n', [Line]).

%   The bindings are the query's variables as the *reader* named them,
%   which is what `--query` and a program's own `?-` both carry, and the
%   answer's term is what they came out as.

show_bindings([], _) :-
    !.
show_bindings(Bindings, Options) :-
    titled('Bindings', Options),
    forall(member(Name=Value, Bindings),
           ( format("~w = ", [Name]),
             print_value(Value),
             nl
           )).

%   Inside a double negation, because making a value readable binds its
%   variables and the next binding shares them.

print_value(Value) :-
    \+ \+ ( sscasp_readable(Value, Readable, []),
            print(Readable)
          ).

width(Options, Width) :-
    (   option(width(Width), Options)
    ->  true
    ;   catch(tty_size(_, Width), _, fail)
    ->  true
    ;   Width = 80
    ).

coloured(Options, Style, Format, Args) :-
    (   option(colour(false), Options)
    ->  format(Format, Args)
    ;   ansi_format(Style, Format, Args)
    ).

show_query(Query, Bindings, Terms, Options) :-
    (   option(human(true), Options)
    ->  how(Terms, Options, How),
        titled('Query', Options),
        emit_text(\html_query(Query, How))
    ;   titled('Query', Options),
        portray_query(Query, [variable_names(Bindings)|Options])
    ).

		 /*******************************
		 *          AS JSON		*
		 *******************************/

json_answer(_Out, State, Options, Answer, Bindings, _Time, _N) :-
    merge_options([variable_names(Bindings)], Options, JSONOptions),
    answer_json(Answer, Dict, JSONOptions),
    collect(State, Dict).

finish_json(Out, State, Options) :-
    collected(State, Dicts),
    answers_json(Dicts, Document),
    json_options(Options, JSONOptions),
    json_write_dict(Out, Document, JSONOptions),
    format(Out, '~N', []),
    close_sink(Out).

json_options(Options, [width(Width)]) :-
    width(Options, Width).

		 /*******************************
		 *          AS HTML		*
		 *******************************/

html_answer(_Out, State, Terms, Options, Answer, _Bindings, _Time, N) :-
    Answer = answer(Query, Model, Justification),
    sscasp_readable(answer(Query, Model, Justification),
                    answer(_, RModel, RJustification),
                    []),
    how(Terms, Options, How),
    collect(State, answer(N, RModel, RJustification, How)).

finish_html(Out, State, Options) :-
    collected(State, Answers),
    phrase(answers_page(Answers, Options), Tokens),
    print_html(Out, Tokens),
    close_sink(Out).

answers_page(Answers, Options) -->
    { stylesheet(Options, CSS) },
    html(html([ head([ meta(charset('UTF-8')),
                       title('s(CASP) answers'),
                       style(CSS)
                     ]),
                body(div(class('scasp-answers'),
                         \answers(Answers)))
              ])).

answers([]) -->
    [].
answers([answer(N, Model, Justification, How)|Answers]) -->
    html(div(class('scasp-answer'),
             [ h2('Answer ~D'-[N]),
               h3('Model'),
               \html_model(Model, How),
               h3('Justification'),
               \html_justification(Justification, How)
             ])),
    answers(Answers).

%   The stylesheet is written into the page rather than linked, because
%   a file of answers is something to send somebody and a link to a file
%   on this machine is not.  It is the old implementation's, which is
%   what the DOM was written for.

stylesheet(_Options, CSS) :-
    (   absolute_file_name(library('scasp/web/css/scasp.css'), File,
                           [access(read), file_errors(fail)]),
        catch(read_file_to_string(File, CSS, []), _, fail)
    ->  true
    ;   CSS = ""
    ).

		 /*******************************
		 *          THE ERRORS		*
		 *******************************/

%!  failed(+Error) is det.
%
%   An expected error is a message and `halt(2)`; anything else keeps its
%   backtrace, because it is a bug here rather than a mistake out there.

failed(Unwind) :-
    subsumes_term(unwind(_), Unwind),
    !,
    throw(Unwind).                      % halt/0,1 are not errors
failed(error(existence_error(source_sink, File), _)) :-
    !,
    print_message(error, sscasp(no_such_file(File))),
    halt(2).
failed(Error) :-
    print_message(error, Error),
    halt(2).

		 /*******************************
		 *         THE MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(sscasp(no_input_files)) -->
    [ 'sscasp: no input files.  Try `sscasp --help`.'-[] ].
prolog:message(sscasp(no_such_file(File))) -->
    [ 'sscasp: no such file: ~w'-[File] ].
prolog:message(sscasp(no_query)) -->
    [ 'sscasp: the program has no `?- Query.` and none was given.'-[], nl,
      'Use --query=GOAL to say what to prove.'-[]
    ].
prolog:message(sscasp(no_answers)) -->
    [ 'sscasp: no answers.'-[] ].
