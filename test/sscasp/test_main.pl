:- module(test_main,
          [ test_main/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(process), [process_create/3, process_wait/2]).
:- use_module(library(readutil), [read_file_to_string/3]).
:- use_module(library(filesex), [directory_file_path/3,
                                 delete_directory_and_contents/1]).
:- use_module(library(lists), [append/3, member/2]).
:- use_module(library(debug), [assertion/1]).

/** <module> The sscasp command line

The plunit suites drive `library(sscasp)`.  This one covers what only the
executable has: argument parsing, the output formats, and — the thing a
script depends on and nothing else tests — the **exit status**.

Runs `swipl prolog/sscasp/main.pl -- ...` rather than the built
`./sscasp`, so the suite does not depend on `make` having been run.

Every test asserts a status.  A CLI that answers correctly and exits 1,
or finds nothing and exits 0, is broken in the way that matters most and
in the way a test that only reads stdout does not notice.
*/

test_main :-
    run_tests([ main_info,
                main_solving,
                main_listing,
                main_formats,
                main_interactive,
                main_errors,
                main_standalone,
                main_built
            ]).

		 /*******************************
		 *         THE HARNESS		*
		 *******************************/

%!  run_sscasp(+Args, -Status, -Output) is det.

run_sscasp(Args, Status, Output) :-
    run_sscasp(Args, "", Status, Output).

%!  run_sscasp(+Args, +Input, -Status, -Output) is det.
%
%   Input is written to the process's stdin and the stream closed, which
%   is what `-i` reads its queries from.  Writing it before reading the
%   output is safe only because these runs are small: a program that
%   filled the pipe buffer would deadlock, and none here comes close.

run_sscasp(Args, Input, Status, Output) :-
    sscasp_dir(Dir),
    atomic_list_concat([Dir, '/prolog/sscasp/main.pl'], Main),
    append(['-p', 'library=prolog', Main, '--'], Args, ProcArgs),
    process_create(path(swipl), ProcArgs,
                   [ stdin(pipe(In)),
                     stdout(pipe(Out)),
                     stderr(pipe(Out)),   % documented: one stream for both
                     process(PID),
                     cwd(Dir)
                   ]),
    write(In, Input),
    close(In),
    read_string(Out, _, Output),
    close(Out),
    process_wait(PID, Status).

%!  run_program(+Args, +Program, -Status, -Output) is det.

run_program(Args, Program, Status, Output) :-
    sscasp_dir(Dir),
    atomic_list_concat([Dir, '/test/all_programs/', Program], File),
    append(Args, [File], AllArgs),
    run_sscasp(AllArgs, Status, Output).

sscasp_dir(Dir) :-
    source_file(sscasp_dir(_), Here),
    file_directory_name(Here, Sscasp),
    file_directory_name(Sscasp, Test),
    file_directory_name(Test, Dir).

contains(Text, Needle) :-
    sub_string(Text, _, _, _, Needle).

write_file(File, Text) :-
    setup_call_cleanup(open(File, write, Out),
                       write(Out, Text),
                       close(Out)).

		 /*******************************
		 *           HELP		*
		 *******************************/

:- begin_tests(main_info).

test(version_exits_zero) :-
    run_sscasp(['--version'], Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "sscasp ")),
    assertion(\+ contains(Out, "unknown")).

test(help_exits_zero) :-
    run_sscasp(['--help'], Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "Usage")).

%   The flags and their documentation are one thing — `opt_help/2` — so a
%   flag that is not in the help is a flag nobody can find.

test(help_lists_the_flags, [forall(member(Flag, ["--tree", "--human",
                                                "--json", "--code",
                                                "--query", "--no-nmr",
                                                "--dcc", "--domain",
                                                "--version"]))]) :-
    run_sscasp(['--help'], _, Out),
    assertion(contains(Out, Flag)).

:- end_tests(main_info).

		 /*******************************
		 *          SOLVING		*
		 *******************************/

:- begin_tests(main_solving).

test(a_program_is_solved_and_exits_zero) :-
    run_program([], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "Answer 1")),
    assertion(contains(Out, "Model")).

%   The query is shown with the names the *reader* saw, and so are the
%   bindings: `X` and not `A`, which is what the person who wrote the
%   file will recognise.

test(the_query_keeps_its_variable_names) :-
    run_program([], 'birds.pl', _, Out),
    assertion(contains(Out, "?- flies(X)")),
    assertion(contains(Out, "X = tweety")).

test(the_justification_is_asked_for) :-
    run_program(['--tree'], 'birds.pl', _, Out),
    assertion(contains(Out, "Justification")).

test(and_is_not_shown_otherwise) :-
    run_program([], 'birds.pl', _, Out),
    assertion(\+ contains(Out, "Justification")).

test(one_answer_by_default) :-
    run_program([], 'pq.pl', _, Out),
    assertion(contains(Out, "Answer 1")),
    assertion(\+ contains(Out, "Answer 2")).

test(and_all_of_them_on_request) :-
    run_program(['--answers', '0'], 'min_model.pl', _, Out),
    assertion(contains(Out, "Answer 4")).

test(or_a_given_number) :-
    run_program(['--answers', '2'], 'min_model.pl', _, Out),
    assertion(contains(Out, "Answer 2")),
    assertion(\+ contains(Out, "Answer 3")).

test(a_query_may_be_given_instead) :-
    run_program(['--query', 'bird(X)'], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "?- bird(X)")).

%   It is read with the language's operators, or `not`, `-` and the
%   constraints are syntax errors there that they are not in a file.

test(a_given_query_may_use_the_operators,
     [ forall(member(Query-Shown, [ 'not penguin(tweety)'-"?- not penguin(tweety)",
                                    '-flies(sam)'-"?- -flies(sam)"
                                  ]))
     ]) :-
    run_program(['--query', Query], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, Shown)).

%   Several files are one program, and a file two of them include is
%   read once, as it is when two includes of one file name it.

test(a_file_included_by_two_is_read_once,
     [ setup(tmp_file(sscasp_include, Dir)),
       cleanup(delete_directory_and_contents(Dir))
     ]) :-
    make_directory(Dir),
    directory_file_path(Dir, 'shared.incl', Shared),
    directory_file_path(Dir, 'a.pl', A),
    directory_file_path(Dir, 'b.pl', B),
    write_file(Shared, "r(1).\n"),
    write_file(A, "#include 'shared.incl'.\n"),
    write_file(B, "#include 'shared.incl'.\n?- r(X).\n"),
    run_sscasp(['-n0', A, B], Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "Answer 1")),
    assertion(\+ contains(Out, "Answer 2")).

test(said_in_words) :-
    run_program(['--human', '--tree'], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "can fly")).

:- end_tests(main_solving).

		 /*******************************
		 *          THE LISTING		*
		 *******************************/

:- begin_tests(main_listing).

test(code_writes_the_program_and_exits) :-
    run_program(['--code'], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "User predicates")),
    assertion(contains(Out, "flies(A) :-")),
    assertion(\+ contains(Out, "Answer 1")).

%   Naming a section asks for that one and not for the defaults, which is
%   what `--code --duals` has to mean if it is to be useful.

test(and_a_named_section_is_the_only_one) :-
    run_program(['--code', '--duals'], 'birds.pl', _, Out),
    assertion(contains(Out, "Dual rules")),
    assertion(contains(Out, "not flies(A)")),
    assertion(\+ contains(Out, "User predicates")).

test(the_constraints_are_titled_by_what_they_came_from) :-
    run_program(['--code', '--constraints'], 'birds.pl', _, Out),
    assertion(contains(Out, "Integrity constraints")),
    assertion(contains(Out, "check(")).

test(and_the_emitted_clauses_are_there_if_asked) :-
    run_program(['--code', '--compiled'], 'birds.pl', _, Out),
    assertion(contains(Out, "$dual$flies$1")).

%   `--query` is the listing's query too, under the reader's names.  It
%   used to switch the query section off: the option and the section
%   share a name, and the goal is not `true`.

test(code_lists_a_given_query) :-
    run_program(['--code', '--query', 'penguin(X)'], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "?- penguin(X).")).

%   The terms as *read*, which is the only section that shows a
%   directive: the compiler turns those into clauses and they stop being
%   visible as themselves.

test(and_the_terms_as_read) :-
    run_program(['--code', '--source'], 'birds.pl', _, Out),
    assertion(contains(Out, "Program as read")),
    assertion(contains(Out, "bird(tweety)")).

:- end_tests(main_listing).

		 /*******************************
		 *         THE FORMATS		*
		 *******************************/

/** A document is *all* that is written: a JSON file with a `% Query`
    heading in front of it is not JSON.
*/

:- begin_tests(main_formats).

test(json_is_written_alone) :-
    run_program(['--json=-'], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(sub_string(Out, 0, 1, _, "{")),
    assertion(\+ contains(Out, "% Query")),
    assertion(\+ contains(Out, "Answer 1")).

test(and_says_the_bindings_by_name) :-
    run_program(['--json=-'], 'birds.pl', _, Out),
    assertion(contains(Out, "\"X\"")),
    assertion(contains(Out, "tweety")).

test(and_parses_as_json) :-
    run_program(['--json=-'], 'birds.pl', _, Out),
    atom_string(Atom, Out),
    atom_json_dict(Atom, Dict, []),
    Answers = Dict.answers,
    assertion(Answers = [_|_]).

test(html_is_a_page) :-
    run_program(['--html=-'], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "<html>")),
    assertion(contains(Out, "scasp-model")),
    assertion(\+ contains(Out, "% Query")).

test(and_carries_its_own_style) :-
    run_program(['--html=-'], 'birds.pl', _, Out),
    assertion(contains(Out, "<style>")).

:- use_module(library(json), [atom_json_dict/3]).

:- end_tests(main_formats).

		 /*******************************
		 *        INTERACTIVELY		*
		 *******************************/

/** `-i` reads queries until end of input.  Three things are worth a
    test, and the second is the reason this exists: the old
    implementation's loop has no `catch/3`, so **one misspelled goal
    ends the session** and exits 1.  A toplevel that dies on a typo is
    not one.
*/

:- begin_tests(main_interactive).

%!  interactively(+Queries, -Status, -Output) is det.

interactively(Queries, Status, Output) :-
    sscasp_dir(Dir),
    atomic_list_concat([Dir, '/test/all_programs/birds.pl'], File),
    atomic_list_concat(Queries, '\n', Input0),
    atomic_list_concat([Input0, '\n'], Input),
    run_sscasp(['-i', File], Input, Status, Output).

test(a_query_is_answered_and_the_session_exits_zero) :-
    interactively(['flies(X).'], Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "X = tweety")).

test(and_several_are_answered_in_turn) :-
    interactively(['flies(X).', 'bird(Y).'], _, Out),
    assertion(contains(Out, "?- flies(X)")),
    assertion(contains(Out, "?- bird(Y)")).

%   The one that matters.  A goal the program does not have is reported
%   and the loop goes on, where the old implementation exits 1 there and
%   then.

test(a_misspelled_goal_does_not_end_the_session) :-
    interactively(['nosuchgoal.', 'flies(X).'], Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "X = tweety")).

%   And it is reported in the language the reader typed, not the
%   compiler's: `scasp$plain$nosuchgoal$0/3` is true and no use.

test(and_names_the_predicate_the_reader_wrote) :-
    interactively(['nosuchgoal.'], _, Out),
    assertion(contains(Out, "nosuchgoal/0")),
    assertion(\+ contains(Out, "$plain$")).

test(a_syntax_error_does_not_end_it_either) :-
    interactively(['flies(.', 'flies(X).'], Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "X = tweety")).

%   End of input is the way out, and so is saying so.

test(halt_ends_it, [forall(member(Word, ['halt.', 'quit.', 'exit.']))]) :-
    interactively([Word, 'flies(X).'], Status, Out),
    assertion(Status == exit(0)),
    assertion(\+ contains(Out, "X = tweety")).

%   Away from a terminal there is nobody to ask "more?", and asking
%   would read the next query's first character.  So a pipe honours
%   `--answers` instead.

test(answers_are_not_offered_one_at_a_time_to_a_pipe) :-
    interactively(['flies(X).'], _, Out),
    assertion(\+ contains(Out, "more?")).

test(and_a_count_may_be_asked_for) :-
    sscasp_dir(Dir),
    atomic_list_concat([Dir, '/test/all_programs/min_model.pl'], File),
    run_sscasp(['-i', '--answers', '0', File], 'p(X).\n', _, Out),
    assertion(contains(Out, "Answer 4")).

%   Asking for another answer where there is none says so, as `false`
%   does at the Prolog toplevel.  Stopping before the last does not.

test(running_out_of_answers_is_said) :-
    sscasp_dir(Dir),
    atomic_list_concat([Dir, '/test/all_programs/min_model.pl'], File),
    run_sscasp(['-i', '--answers', '0', File], 'p(X).\n', _, Out),
    assertion(contains(Out, "no more models")).

test(stopping_before_the_last_is_not) :-
    interactively(['flies(X).'], _, Out),
    assertion(\+ contains(Out, "no more models")).

%   A query with no answer says so and the session survives it, which is
%   not the same as the non-interactive run exiting 1.

test(no_answer_is_reported_and_survived) :-
    interactively(['flies(sam).', 'flies(X).'], Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "no answers")),
    assertion(contains(Out, "X = tweety")).

:- end_tests(main_interactive).

		 /*******************************
		 *         ON ITS OWN		*
		 *******************************/

/** `prolog/sscasp/` loads nothing out of `prolog/scasp/`.  It used to
    read the other tree's stylesheet and script through
    `library(scasp/web/...)`; they are copied now, under names of their
    own because `js` and `css` are aliases both trees register.

    Checked in a **subprocess**, because it cannot be seen from inside
    `make check`: this process has both libraries loaded, so asking
    `source_file/1` here would find the other tree whatever the new one
    does.
*/

:- begin_tests(main_standalone).

%!  in_subprocess(+Goal, -Status) is det.
%
%   Run Goal in a fresh `swipl` with this repo's `prolog/` as the
%   library root.  Exit 0 iff it succeeded.

in_subprocess(Goal, Status) :-
    sscasp_dir(Dir),
    process_create(path(swipl),
                   ['-q', '-p', 'library=prolog', '-g', Goal, '-t', 'halt'],
                   [ stdin(null), stdout(pipe(Out)), stderr(pipe(Out)),
                     process(PID), cwd(Dir)
                   ]),
    read_string(Out, _, _),
    close(Out),
    process_wait(PID, Status).

test(the_library_loads_nothing_from_the_other_tree) :-
    in_subprocess("use_module(library(sscasp)), \c
                   forall(source_file(F), \c
                          \\+ sub_atom(F, _, _, _, '/prolog/scasp/'))",
                  Status),
    assertion(Status == exit(0)).

%   And the whole thing runs with the other tree **absent**, which is
%   the check the one above cannot make: `main.pl` reads the stylesheet
%   at run time, so nothing about loading it says the file is there.
%
%   A library root holding only this tree, by symlink rather than by
%   copy, so that what runs is the working tree and not a stale picture
%   of it.

test(and_the_program_runs_with_the_other_tree_absent) :-
    setup_call_cleanup(
        only_this_tree(Root),
        run_standalone(Root, ['--tree'], Status, Out),
        delete_directory_and_contents(Root)),
    assertion(Status == exit(0)),
    assertion(contains(Out, "X = tweety")),
    assertion(contains(Out, "Justification")).

%   And the stylesheet with it: `--html` reads it at run time, and
%   `stylesheet/2` falls back to the empty string when the file is
%   missing, so a page that had lost it would still be a page.

test(and_finds_the_stylesheet_there_too) :-
    setup_call_cleanup(
        only_this_tree(Root),
        run_standalone(Root, ['--html=-'], Status, Out),
        delete_directory_and_contents(Root)),
    assertion(Status == exit(0)),
    assertion(contains(Out, "collapse-toggler")).

only_this_tree(Root) :-
    sscasp_dir(Dir),
    tmp_file(sscasp_standalone, Root),
    make_directory(Root),
    forall(member(Rel-As, ['prolog/sscasp.pl'-'sscasp.pl',
                           'prolog/sscasp'-'sscasp']),
           ( atomic_list_concat([Dir, /, Rel], From),
             atomic_list_concat([Root, /, As], To),
             link_file(From, To, symbolic)
           )).

%   Run `main.pl` as a script, the way `run_sscasp/4` does.  Loading it
%   with `-g use_module(...)` instead would run `main/0` a second time —
%   `:- initialization(main, main)` fires on the real argv, which has no
%   input files, so the process exits 2 having already done the work.
%
%   **`--no-packs` is what makes this isolating.**  `-p library=Dir`
%   *prepends*, so without it the installed pack is still on the path and
%   `library(scasp/...)` resolves through it — which is the trap
%   `scasp_plunit.pl` exists to work around, and it made an earlier
%   version of this test pass with the dependency put back.

run_standalone(Root, Args, Status, Out) :-
    sscasp_dir(Dir),
    atomic_list_concat([Dir, '/test/all_programs/birds.pl'], Program),
    atomic_list_concat(['library=', Root], Path),
    atomic_list_concat([Root, '/sscasp/main.pl'], Main),
    append(['-q', '--no-packs', '-p', Path, Main, '--'], Args, Head),
    append(Head, [Program], ProcArgs),
    process_create(path(swipl), ProcArgs,
                   [ stdin(null), stdout(pipe(S)), stderr(pipe(S)),
                     process(PID), cwd(Dir)
                   ]),
    read_string(S, _, Out),
    close(S),
    process_wait(PID, Status).

:- use_module(library(filesex), [delete_directory_and_contents/1,
                                 link_file/3]).

%   The stylesheet is found, and is the copy.  `stylesheet/2` falls back
%   to the empty string when the file is missing, so a page that lost it
%   would still be a page — which is why this looks at the content.

%   `html.pl` declares the resource for a web front end and nothing here
%   calls `html_requires//1`, so its half of the copy has no other test.
%   Both files resolve through the aliases it registers, and the names
%   are ours — which is what keeps `js('sscasp.js')` unambiguous in a
%   process where the other tree has registered `js` too.

test(the_web_resources_resolve_to_our_copies,
     [ forall(member(Spec, ["js('sscasp.js')", "css('sscasp.css')"])) ]) :-
    format(atom(Goal),
           "use_module(library(sscasp)), \c
            absolute_file_name(~w, F, [access(read)]), \c
            sub_atom(F, _, _, _, '/sscasp/web/')",
           [Spec]),
    in_subprocess(Goal, Status),
    assertion(Status == exit(0)).

test(the_stylesheet_is_found_and_is_ours) :-
    run_program(['--html=-'], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "collapse-toggler")).

:- end_tests(main_standalone).

		 /*******************************
		 *      THE BUILT PROGRAM	*
		 *******************************/

/** Everything above runs `swipl prolog/sscasp/main.pl`, so the suite
    does not depend on `make` having been run.  That leaves a gap: a
    **saved state** is not the same process, and two things have already
    differed in it.

    - `read_term_with_history/2` reaches `editline:el_history/2`, which
      raises a domain error when the input is a pipe.  The loop printed
      it, read nothing, and printed it again — forever.
    - SWI's `on_error(print)` makes `halt/0` exit 1 once any error has
      been printed, so a session that reported a typo and carried on
      exited 1 although it did what was asked.

    Neither is visible from source.  So these run the executable when
    there is one, and are skipped when there is not.
*/

:- begin_tests(main_built).

built(Exe) :-
    sscasp_dir(Dir),
    atomic_list_concat([Dir, '/sscasp'], Exe),
    exists_file(Exe).

%!  run_built(+Args, +Input, -Status, -Output) is det.

run_built(Args, Input, Status, Output) :-
    built(Exe),
    sscasp_dir(Dir),
    process_create(Exe, Args,
                   [ stdin(pipe(In)),
                     stdout(pipe(Out)),
                     stderr(pipe(Out)),
                     process(PID),
                     cwd(Dir)
                   ]),
    write(In, Input),
    close(In),
    read_string(Out, _, Output),
    close(Out),
    process_wait(PID, Status).

test(the_executable_answers, [condition(built(_))]) :-
    run_built(['test/all_programs/birds.pl'], "", Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "X = tweety")).

%   The loop terminates, which is the whole of the editline bug: with
%   history on a pipe it printed the same error until it was killed.

test(and_reads_queries_from_a_pipe, [condition(built(_))]) :-
    run_built(['-i', 'test/all_programs/birds.pl'],
              "flies(X).\nhalt.\n", Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "X = tweety")),
    assertion(\+ contains(Out, "el_history")).

%   And a session that reported a typo and carried on exits 0.  Under
%   `on_error(print)` a saved state exits 1 for having printed anything
%   at all, which is right for a script and wrong for a toplevel.

test(and_a_reported_typo_does_not_change_the_status, [condition(built(_))]) :-
    run_built(['-i', 'test/all_programs/birds.pl'],
              "nosuchgoal.\nhalt.\n", Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "nosuchgoal/0")).

%   The statuses a script reads still come through the state.

test(and_no_answers_still_exits_one, [condition(built(_))]) :-
    run_built(['test/all_programs/classic_negation_inconstistent.pl'],
              "", Status, _),
    assertion(Status == exit(1)).

test(and_a_missing_file_still_exits_two, [condition(built(_))]) :-
    run_built(['/no/such/program.pl'], "", Status, _),
    assertion(Status == exit(2)).

%   The stylesheet is in the state, not beside it.  Read at run time it
%   resolved through the *installed pack* — a different checkout of this
%   project on this machine, and nothing at all on a machine without
%   one — so `--html` styled its pages from someone else's working tree
%   and would elsewhere have produced an unstyled page in silence.
%   `stylesheet_text/1` is frozen at compile time like `version/1`.

test(and_the_stylesheet_travels_with_it, [condition(built(_))]) :-
    run_built(['--html=-', 'test/all_programs/birds.pl'], "", Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "collapse-toggler")).

:- end_tests(main_built).

		 /*******************************
		 *          THE ERRORS		*
		 *******************************/

/** Three ways to fail and three statuses, because a script has to tell
    "no" from "I could not".
*/

:- begin_tests(main_errors).

test(no_answers_exits_one) :-
    run_program([], 'classic_negation_inconstistent.pl', Status, Out),
    assertion(Status == exit(1)),
    assertion(contains(Out, "no answers")).

test(no_input_files_exits_two) :-
    run_sscasp([], Status, Out),
    assertion(Status == exit(2)),
    assertion(contains(Out, "no input files")).

test(a_missing_file_exits_two) :-
    run_sscasp(['/no/such/program.pl'], Status, Out),
    assertion(Status == exit(2)),
    assertion(contains(Out, "no such file")).

test(a_program_with_no_query_exits_two) :-
    sscasp_dir(Dir),
    atomic_list_concat([Dir, '/test/all_programs/bec_theory.incl'], File),
    run_sscasp([File], Status, Out),
    assertion(Status == exit(2)),
    assertion(contains(Out, "no `?- Query.`")).

%   A document is written when the answers are in, so a run that ends
%   in an error leaves the file that was there alone.

test(an_error_leaves_the_document_alone,
     [ setup(tmp_file(sscasp_json, File)),
       cleanup(delete_file(File))
     ]) :-
    write_file(File, "before"),
    atom_concat('--json=', File, Flag),
    run_program([Flag, '--query', nosuchgoal], 'birds.pl', Status, _),
    assertion(Status == exit(2)),
    read_file_to_string(File, Text, []),
    assertion(Text == "before").

%   `--domain-error` makes a value outside a signature's domain an error
%   rather than a failure, so the same query goes from "no" to "could not".

test(a_value_outside_the_domain_fails_or_raises,
     [ forall(member(Args-Expected, [[]-exit(1), ['--domain-error']-exit(2)])),
       setup(tmp_file_stream(text, File, Stream)),
       cleanup(delete_file(File))
     ]) :-
    format(Stream, ":- domain d = [1, 2].~n:- pred q(d).~nq(1).~n?- not q(3).~n", []),
    close(Stream),
    append(Args, [File], All),
    run_sscasp(All, Status, Out),
    assertion(Status == Expected),
    (   Expected == exit(2)
    ->  assertion(contains(Out, "Domain error"))
    ;   true
    ).

:- end_tests(main_errors).
