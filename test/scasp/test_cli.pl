:- module(test_cli,
          [ test_cli/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(process), [process_create/3, process_wait/2]).
:- use_module(library(readutil)).
:- use_module(library(lists), [append/3]).

/** <module> Tests for the scasp command line

The plunit suites drive the *embedded* interfaces.  This one covers what
only the executable has: argument parsing, exit status and the output
formats.  Several of these guard option-handling bugs that were live
until recently - `--plaindual` parsed and was then discarded, `--variant`
could not be passed at all, and `--version` exited 1.

Runs `swipl prolog/scasp/main.pl -- ...` rather than the built ./scasp,
so the suite does not depend on `make` having been run.
*/

test_cli :-
    run_tests([ cli_info,
                cli_solving,
                cli_code,
                cli_formats,
                cli_options,
                cli_errors
              ]).

		 /*******************************
		 *        VERSION / HELP	*
		 *******************************/

:- begin_tests(cli_info).

test(version_exits_zero) :-
    scasp_cli(['--version'], Status, Out),
    assertion(Status == exit(0)),
    assertion(sub_string(Out, _, _, _, "version")).
test(help_exits_zero) :-
    scasp_cli(['--help'], Status, Out),
    assertion(Status == exit(0)),
    assertion(sub_string(Out, _, _, _, "Usage")).
test(help_lists_variant) :-
    scasp_cli(['--help'], _, Out),
    assertion(sub_string(Out, _, _, _, "--variant")).

:- end_tests(cli_info).

		 /*******************************
		 *           SOLVING		*
		 *******************************/

:- begin_tests(cli_solving).

test(solves_and_reports_a_model) :-
    scasp_cli_program(['--tree'], 'pq.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(sub_string(Out, _, _, _, "Answer 1")),
    assertion(sub_string(Out, _, _, _, "p(")).
test(answer_count_is_limited) :-
    scasp_cli_program(['-n', '1'], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(\+ sub_string(Out, _, _, _, "Answer 2")).
%   Asked for another model where there is none, it says so.  Stopping
%   before the last does not.

test(running_out_of_models_is_said) :-
    scasp_cli_input(['min_model.pl'], ";\n;\n;\n;\n", Status, Out),
    assertion(Status == exit(0)),
    assertion(sub_string(Out, _, _, _, "no more models")).

test(stopping_before_the_last_is_not) :-
    scasp_cli_input(['min_model.pl'], "\n", _, Out),
    assertion(\+ sub_string(Out, _, _, _, "no more models")).

test(query_can_be_overruled) :-
    scasp_cli_program(['--query=flies(X)'], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(sub_string(Out, _, _, _, "flies")).

:- end_tests(cli_solving).

		 /*******************************
		 *            --code		*
		 *******************************/

:- begin_tests(cli_code).

test(code_prints_the_dual_program) :-
    scasp_cli_program(['--code'], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(sub_string(Out, _, _, _, "Dual Rules")).

% --plaindual generates duals with single-goal clauses.  It used to parse
% and then be discarded, because options.pl had no set_option/1 clause for
% it, so the output was identical either way.

test(plaindual_changes_the_dual_program) :-
    scasp_cli_program(['--code'], 'birds.pl', _, Plain),
    scasp_cli_program(['--code', '-d'], 'birds.pl', _, Single),
    assertion(Plain \== Single).

:- end_tests(cli_code).

		 /*******************************
		 *          FORMATS		*
		 *******************************/

:- begin_tests(cli_formats).

test(human_output_is_prose) :-
    scasp_cli_program(['--tree', '--human'], 'birds.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(sub_string(Out, _, _, _, "holds")).
test(json_output_is_json) :-
    scasp_cli_program(['--json=-'], 'pq.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(sub_string(Out, _, _, _, "{")).
test(html_output_is_html) :-
    scasp_cli_program(['--html=-'], 'pq.pl', Status, Out),
    assertion(Status == exit(0)),
    assertion(sub_string(Out, _, _, _, "<")).

:- end_tests(cli_formats).

		 /*******************************
		 *          OPTIONS		*
		 *******************************/

:- begin_tests(cli_options).

% --variant had opt_help/2 but no opt_type/3, so it could not be passed.

test(variant_is_accepted) :-
    scasp_cli_program(['--variant'], 'pq.pl', Status, _),
    assertion(Status == exit(0)).

% scasp_real was read by main.pl but never created as a flag.

test(rationals_print_as_rationals_by_default) :-
    scasp_cli_program([], 'rat.pl', _, Out),
    assertion(sub_string(Out, _, _, _, "7/12")).
test(rationals_print_as_decimals_with_r) :-
    scasp_cli_program(['-r', '3'], 'rat.pl', _, Out),
    assertion(sub_string(Out, _, _, _, "0.583")).

test(forall_algorithm_is_accepted) :-
    scasp_cli_program(['--forall=prev'], 'pq.pl', Status, _),
    assertion(Status == exit(0)).
test(unknown_forall_algorithm_is_refused) :-
    scasp_cli_program(['--forall=nonsense'], 'pq.pl', Status, _),
    assertion(Status == exit(1)).

:- end_tests(cli_options).

		 /*******************************
		 *           ERRORS		*
		 *******************************/

:- begin_tests(cli_errors).

test(missing_file_exits_one) :-
    scasp_cli(['/nonexistent/nope.pl'], Status, _),
    assertion(Status == exit(1)).
test(no_arguments_exits_one) :-
    scasp_cli([], Status, _),
    assertion(Status == exit(1)).
test(unknown_option_exits_one) :-
    scasp_cli(['--no-such-option'], Status, _),
    assertion(Status == exit(1)).

:- end_tests(cli_errors).

		 /*******************************
		 *           HELPERS		*
		 *******************************/

%!  scasp_cli(+Args, -Status, -Output) is det.
%!  scasp_cli_program(+Args, +Program, -Status, -Output) is det.
%
%   Run the s(CASP) command line and collect its exit status and combined
%   output.  Program names a file in test/all_programs/.
%
%   The child gets `stdin(null)`.  Without a `-n`, scasp prompts for more
%   models with get_single_char/1 (main.pl:516), so a child that inherits
%   the suite's standard input blocks for as long as that input stays open
%   without delivering.  That makes the test depend on how the suite was
%   invoked: fine from a terminal, hanging when `make check` runs with its
%   input on a pipe.  A null stream reads as end of file, which is what
%   the prompt treats as "no more".

scasp_cli_program(Args, Program, Status, Output) :-
    scasp_dir(Dir),
    atomic_list_concat([Dir, '/test/all_programs/', Program], File),
    append(Args, [File], AllArgs),
    scasp_cli(AllArgs, Status, Output).

scasp_cli(Args, Status, Output) :-
    scasp_cli(Args, null, Status, Output).

%!  scasp_cli_input(+Args, +Input, -Status, -Output) is det.
%
%   As scasp_cli_program/4 with only a program name for Args, but the
%   child reads Input, which is how a test answers the prompt for more
%   models.

scasp_cli_input([Program], Input, Status, Output) :-
    scasp_dir(Dir),
    atomic_list_concat([Dir, '/test/all_programs/', Program], File),
    scasp_cli([File], pipe(In), Status, Output, Input, In).

scasp_cli(Args, Stdin, Status, Output) :-
    scasp_cli(Args, Stdin, Status, Output, "", _).

scasp_cli(Args, Stdin, Status, Output, Input, In) :-
    scasp_dir(Dir),
    atomic_list_concat([Dir, '/prolog/scasp/main.pl'], Main),
    append(['-p', 'library=prolog', Main, '--'], Args, ProcArgs),
    process_create(path(swipl), ProcArgs,
                   [ stdin(Stdin),        % null: see above
                     stdout(pipe(Out)),
                     stderr(pipe(Out)),   % documented: one stream for both
                     process(PID),
                     cwd(Dir)
                   ]),
    (   Stdin = pipe(In)
    ->  write(In, Input),
        close(In)
    ;   true
    ),
    read_string(Out, _, Output),
    close(Out),
    process_wait(PID, Status).

scasp_dir(Dir) :-
    source_file(scasp_dir(_), Here),
    file_directory_name(Here, Scasp),
    file_directory_name(Scasp, TestDir),
    file_directory_name(TestDir, Dir).
