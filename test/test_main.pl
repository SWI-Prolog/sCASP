:- module(test_main,
          [ test_main/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(process), [process_create/3, process_wait/2]).
:- use_module(library(readutil), []).
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
                main_errors
            ]).

		 /*******************************
		 *         THE HARNESS		*
		 *******************************/

%!  sscasp(+Args, -Status, -Output) is det.

sscasp(Args, Status, Output) :-
    sscasp_dir(Dir),
    atomic_list_concat([Dir, '/prolog/sscasp/main.pl'], Main),
    append(['-p', 'library=prolog', Main, '--'], Args, ProcArgs),
    process_create(path(swipl), ProcArgs,
                   [ stdin(null),
                     stdout(pipe(Out)),
                     stderr(pipe(Out)),   % documented: one stream for both
                     process(PID),
                     cwd(Dir)
                   ]),
    read_string(Out, _, Output),
    close(Out),
    process_wait(PID, Status).

%!  sscasp_program(+Args, +Program, -Status, -Output) is det.

sscasp_program(Args, Program, Status, Output) :-
    sscasp_dir(Dir),
    atomic_list_concat([Dir, '/test/all_programs/', Program], File),
    append(Args, [File], AllArgs),
    sscasp(AllArgs, Status, Output).

sscasp_dir(Dir) :-
    source_file(sscasp_dir(_), Here),
    file_directory_name(Here, Test),
    file_directory_name(Test, Dir).

contains(Text, Needle) :-
    sub_string(Text, _, _, _, Needle).

		 /*******************************
		 *           HELP		*
		 *******************************/

:- begin_tests(main_info).

test(version_exits_zero) :-
    sscasp(['--version'], Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "sscasp ")),
    assertion(\+ contains(Out, "unknown")).

test(help_exits_zero) :-
    sscasp(['--help'], Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "Usage")).

%   The flags and their documentation are one thing — `opt_help/2` — so a
%   flag that is not in the help is a flag nobody can find.

test(help_lists_the_flags, [forall(member(Flag, ["--tree", "--human",
                                                "--json", "--code",
                                                "--query", "--no-nmr",
                                                "--version"]))]) :-
    sscasp(['--help'], _, Out),
    assertion(contains(Out, Flag)).

:- end_tests(main_info).

		 /*******************************
		 *          SOLVING		*
		 *******************************/

:- begin_tests(main_solving).

test(a_program_is_solved_and_exits_zero) :-
    sscasp_program([], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "Answer 1")),
    assertion(contains(Out, "Model")).

%   The query is shown with the names the *reader* saw, and so are the
%   bindings: `X` and not `A`, which is what the person who wrote the
%   file will recognise.

test(the_query_keeps_its_variable_names) :-
    sscasp_program([], 'birds.pl', _, Out),
    assertion(contains(Out, "?- flies(X)")),
    assertion(contains(Out, "X = tweety")).

test(the_justification_is_asked_for) :-
    sscasp_program(['--tree'], 'birds.pl', _, Out),
    assertion(contains(Out, "Justification")).

test(and_is_not_shown_otherwise) :-
    sscasp_program([], 'birds.pl', _, Out),
    assertion(\+ contains(Out, "Justification")).

test(one_answer_by_default) :-
    sscasp_program([], 'pq.pl', _, Out),
    assertion(contains(Out, "Answer 1")),
    assertion(\+ contains(Out, "Answer 2")).

test(and_all_of_them_on_request) :-
    sscasp_program(['--answers', '0'], 'min_model.pl', _, Out),
    assertion(contains(Out, "Answer 4")).

test(or_a_given_number) :-
    sscasp_program(['--answers', '2'], 'min_model.pl', _, Out),
    assertion(contains(Out, "Answer 2")),
    assertion(\+ contains(Out, "Answer 3")).

test(a_query_may_be_given_instead) :-
    sscasp_program(['--query', 'bird(X)'], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "?- bird(X)")).

test(said_in_words) :-
    sscasp_program(['--human', '--tree'], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "can fly")).

:- end_tests(main_solving).

		 /*******************************
		 *          THE LISTING		*
		 *******************************/

:- begin_tests(main_listing).

test(code_writes_the_program_and_exits) :-
    sscasp_program(['--code'], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "User predicates")),
    assertion(contains(Out, "flies(A) :-")),
    assertion(\+ contains(Out, "Answer 1")).

%   Naming a section asks for that one and not for the defaults, which is
%   what `--code --duals` has to mean if it is to be useful.

test(and_a_named_section_is_the_only_one) :-
    sscasp_program(['--code', '--duals'], 'birds.pl', _, Out),
    assertion(contains(Out, "Dual rules")),
    assertion(contains(Out, "not flies(A)")),
    assertion(\+ contains(Out, "User predicates")).

test(the_constraints_are_titled_by_what_they_came_from) :-
    sscasp_program(['--code', '--constraints'], 'birds.pl', _, Out),
    assertion(contains(Out, "Integrity constraints")),
    assertion(contains(Out, "check(")).

test(and_the_emitted_clauses_are_there_if_asked) :-
    sscasp_program(['--code', '--compiled'], 'birds.pl', _, Out),
    assertion(contains(Out, "$dual$flies$1")).

%   The terms as *read*, which is the only section that shows a
%   directive: the compiler turns those into clauses and they stop being
%   visible as themselves.

test(and_the_terms_as_read) :-
    sscasp_program(['--code', '--source'], 'birds.pl', _, Out),
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
    sscasp_program(['--json=-'], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(sub_string(Out, 0, 1, _, "{")),
    assertion(\+ contains(Out, "% Query")),
    assertion(\+ contains(Out, "Answer 1")).

test(and_says_the_bindings_by_name) :-
    sscasp_program(['--json=-'], 'birds.pl', _, Out),
    assertion(contains(Out, "\"X\"")),
    assertion(contains(Out, "tweety")).

test(and_parses_as_json) :-
    sscasp_program(['--json=-'], 'birds.pl', _, Out),
    atom_string(Atom, Out),
    atom_json_dict(Atom, Dict, []),
    Answers = Dict.answers,
    assertion(Answers = [_|_]).

test(html_is_a_page) :-
    sscasp_program(['--html=-'], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(contains(Out, "<html>")),
    assertion(contains(Out, "scasp-model")),
    assertion(\+ contains(Out, "% Query")).

test(and_carries_its_own_style) :-
    sscasp_program(['--html=-'], 'birds.pl', _, Out),
    assertion(contains(Out, "<style>")).

:- use_module(library(json), [atom_json_dict/3]).

:- end_tests(main_formats).

		 /*******************************
		 *          THE ERRORS		*
		 *******************************/

/** Three ways to fail and three statuses, because a script has to tell
    "no" from "I could not".
*/

:- begin_tests(main_errors).

test(no_answers_exits_one) :-
    sscasp_program([], 'classic_negation_inconstistent.pl', Status, Out),
    assertion(Status == exit(1)),
    assertion(contains(Out, "no answers")).

test(no_input_files_exits_two) :-
    sscasp([], Status, Out),
    assertion(Status == exit(2)),
    assertion(contains(Out, "no input files")).

test(a_missing_file_exits_two) :-
    sscasp(['/no/such/program.pl'], Status, Out),
    assertion(Status == exit(2)),
    assertion(contains(Out, "no such file")).

test(a_program_with_no_query_exits_two) :-
    sscasp_dir(Dir),
    atomic_list_concat([Dir, '/test/all_programs/bec_theory.incl'], File),
    sscasp([File], Status, Out),
    assertion(Status == exit(2)),
    assertion(contains(Out, "no `?- Query.`")).

:- end_tests(main_errors).
