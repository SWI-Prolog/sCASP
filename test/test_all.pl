:- module(test_all,
          [ test_all/0,
            test_all/1                          % +Options
          ]).
:- use_module(scasp_plunit, []).                % sets the library search path
:- use_module(gaps, [gap/3, load_errors/4]).
:- use_module(under_test, [library_under_test/1]).
:- use_module(library(plunit)).
:- use_module(library(lists), [member/2]).
:- use_module(library(filesex), [directory_file_path/3]).
:- use_module(library(apply), [maplist/3, partition/4]).
:- use_module(library(aggregate), [aggregate_all/3]).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(option), [merge_options/3, select_option/4]).

/** <module> Run every plunit suite of one implementation in one process

The tests are in three places:

  - `test/` holds what is shared: the suites that state what s(CASP)
    *means*, written against `scasp/1,2` and loaded through
    `under_test.pl`, the Gelfond-Lifschitz oracle, the harness and the
    program data.
  - `test/scasp/` tests how the old implementation works.
  - `test/sscasp/` tests how the new one works.

The Prolog flag `sscasp` says which implementation a run is
about, and so which of the two directories is loaded beside the shared
suites:

    swipl -g test_all -t halt test/test_all.pl                     % scasp
    swipl -Dsscasp -g test_all -t halt test/test_all.pl

`make check` runs both.  A shared test the new implementation does not
pass yet is recorded in `gaps.pl` with the reason, which keeps the run
green; test_all/1 then runs each recorded gap on its own and fails if it
*passes*, so an entry cannot outlive the problem it records.

The suites can share a process, so they can also share plunit's job pool
and its per-test timeout.

`jobs(N)` runs units concurrently and `timeout(S)` bounds each individual
test.  121 tests take 2.9s sequentially and 1.0s with `jobs(4)`, most of
the difference being test_cli.pl, which spends its time waiting on
subprocesses.

The timeout is per *test*, and the rest of the unit carries on:

    ERROR: test slow:hangs: throw(time_limit_exceeded(2.0))
    % [2/2] slow:quick ................................ passed

which is what a program like test/all_programs/clpq_equality.pl needs - it
does not terminate, and without a bound it stalls the whole run.

Two things have to hold for a unit to share a run, and both do:

  - **A `scasp_*` flag may only be set inside a test.**  A Prolog flag is
    thread-local once set - a thread inherits its creator's value and its
    own changes do not propagate back - so a flag set in the thread plunit
    runs the test in leaves the other jobs alone.  A unit `setup/1` would
    not do: that runs in the main thread, and the value would leak into
    every job thread created after it.  test_scasp.pl sets scasp_dcc this
    way; dyncall_errors avoids the question by passing `unknown(error)` to
    scasp/2 rather than setting the flag at all.
  - **Programs compiled by scasp_load/2 must be in one unit.**  It asserts
    the compiled program into the module of its caller and wipes what was
    there first, so two units compiling into the same module would destroy
    each other's program.  The golden tests are therefore a single unit,
    where plunit runs them one at a time.

That includes the golden-output tests: test_scasp.pl is a plunit suite
too, one test per program.
*/

		 /*******************************
		 *          LOAD ERRORS		*
		 *******************************/

%   A shared suite may not even load against an implementation: a
%   directive it does not know is a syntax error, and a clause that does
%   not load is a test that silently is not there.  So loading counts
%   the errors per file, and a file with a `load_errors/4` entry has its
%   errors counted rather than printed.  Checked by the unit `loading`.

:- dynamic
    loading_suites/0,
    load_error/1.                           % File

:- multifile user:message_hook/3.

user:message_hook(_Term, error, _Lines) :-
    loading_suites,
    source_location(File, _Line),
    assertz(load_error(File)),
    library_under_test(Library),
    file_base_name(File, Base),
    load_errors(Library, Base, _, _).

:- assertz(loading_suites).

		 /*******************************
		 *            SUITES		*
		 *******************************/

% Shared: what s(CASP) means, against either implementation.

:- use_module(test_builtins,       []).
:- use_module(test_clpq,           []).
:- use_module(test_compound,       []).
:- use_module(test_disequality,    []).
:- use_module(test_forall,         []).
:- use_module(test_olon,           []).
:- use_module(test_prolog_calls,   []).
:- use_module(test_differential,   []).
:- use_module(test_stable_models,  []).
:- use_module(test_proof_skeleton, []).
:- use_module(test_dyncall,        []).
:- use_module(test_le,             []).
:- use_module(test_examples,       []).

:- if(\+ current_prolog_flag(sscasp, true)).

% The old implementation.

:- use_module(scasp/test_scasp,       []).
:- use_module(scasp/test_embed,       []).
:- use_module(scasp/test_cli,         []).
:- use_module(scasp/test_just,        []).
:- use_module(scasp/test_proof_model, []).

:- else.

% The new implementation.

:- use_module(sscasp/test_domains,     []).
:- use_module(sscasp/test_annotations, []).
:- use_module(sscasp/test_mangle,      []).
:- use_module(sscasp/test_abstract,    []).
:- use_module(sscasp/test_dual,        []).
:- use_module(sscasp/test_dualise,     []).
:- use_module(sscasp/test_compile,     []).
:- use_module(sscasp/test_graph,       []).
:- use_module(sscasp/test_nmr,         []).
:- use_module(sscasp/test_emit,        []).
:- use_module(sscasp/test_solve,       []).
:- use_module(sscasp/test_corpus,      []).
:- use_module(sscasp/test_crossings,   []).
:- use_module(sscasp/test_api,         []).
:- use_module(sscasp/test_source,      []).
:- use_module(sscasp/test_describe,    []).
:- use_module(sscasp/test_html,        []).
:- use_module(sscasp/test_render,      []).
:- use_module(sscasp/test_herbrand,    []).
:- use_module(sscasp/test_listing,     []).
:- use_module(sscasp/test_abducible,   []).
:- use_module(sscasp/test_json,        []).
:- use_module(sscasp/test_main,        []).

:- endif.

:- retractall(loading_suites).

%!  test_all is semidet.
%!  test_all(+Options) is semidet.
%
%   Run every unit but the recorded gaps, then each gap on its own.
%   Options are passed to run_tests/2; the defaults are `jobs(4)` and
%   `timeout(60)`.

test_all :-
    test_all([]).

test_all(Options) :-
    merge_options(Options, [jobs(4), timeout(60)], Merged),
    library_under_test(Library),
    format(user_error, '% Testing the ~w implementation~n', [Library]),
    findall(Unit:Test, gap(Library, Unit:Test, _), Gaps),
    findall(Unit, current_test_unit(Unit, _), Units0),
    sort(Units0, Units),
    maplist(without(Gaps), Units, Plan),
    run_tests(Plan, Merged),
    gaps_still_fail(Gaps, Merged).

without(Gaps, Unit, Unit:Tests) :-
    findall(Test,
            ( current_test(Unit, Test, _, _, _),
              \+ memberchk(Unit:Test, Gaps)
            ),
            Tests).

%!  gaps_still_fail(+Gaps, +Options) is semidet.
%
%   Each gap is run on its own and quietly, and must still fail; one
%   that does not terminate within 5 seconds counts as failing.  One
%   that passes has been fixed and its entry must go; one that names no
%   test has been renamed or deleted and its entry must go too.

:- dynamic quiet/0.

user:message_hook(_Term, _Kind, _Lines) :-
    quiet.

gaps_still_fail([], _) :-
    !.
gaps_still_fail(Gaps, Options) :-
    select_option(jobs(_), Options, Options0, _),
    select_option(timeout(_), Options0, Options1, _),
    partition(existing_test, Gaps, Existing, Missing),
    setup_call_cleanup(
        assertz(quiet),
        partition(passes([jobs(1), timeout(5)|Options1]),
                  Existing, Passed, Failed),
        retractall(quiet)),
    length(Failed, NFailed),
    format(user_error, '% ~D recorded gaps still fail (gaps.pl)~n',
           [NFailed]),
    forall(member(Gap, Passed),
           print_message(error, test_all(gap_passes(Gap)))),
    forall(member(Gap, Missing),
           print_message(error, test_all(no_such_test(Gap)))),
    Passed == [],
    Missing == [].

existing_test(Unit:Test) :-
    current_test(Unit, Test, _, _, _),
    !.

passes(Options, Gap) :-
    run_tests(Gap, [summary(Summary)|Options]),
    Summary.failed + Summary.timeout =:= 0.

:- multifile prolog:message//1.

prolog:message(test_all(gap_passes(Gap))) -->
    [ '~q passes: remove it from gaps.pl'-[Gap] ].
prolog:message(test_all(no_such_test(Gap))) -->
    [ '~q is in gaps.pl but there is no such test'-[Gap] ].


		 /*******************************
		 *    LOADING THE SUITES	*
		 *******************************/

:- begin_tests(loading).

%   Errors while loading a suite are counted per file.  A file must have
%   exactly as many as gaps.pl records for it, and a file gaps.pl does
%   not mention must have none.

test(load_errors_are_as_recorded, [forall(loaded_with_errors(Base, N))]) :-
    library_under_test(Library),
    (   load_errors(Library, Base, Expected, _)
    ->  assertion(N == Expected)
    ;   assertion(N == 0)
    ).

test(recorded_load_errors_still_happen,
     [forall(( library_under_test(Library),
               load_errors(Library, Base, _, _) ))]) :-
    assertion(loaded_with_errors(Base, _)).

%   A run is about one implementation, so nothing of the other may be
%   loaded.  `test_proof_skeleton.pl` and `sscasp/corpus.pl` once pulled
%   in the old tree's `ops.pl` for operators the new one declares too.

test(only_the_implementation_under_test_is_loaded) :-
    library_under_test(Library),
    other_tree(Library, Dir),
    findall(File, ( source_file(File),
                    sub_atom(File, _, _, _, Dir)
                  ),
            Files),
    assertion(Files == []).

other_tree(scasp,  '/prolog/sscasp').
other_tree(sscasp, '/prolog/scasp/').

loaded_with_errors(Base, N) :-
    setof(File, load_error(File), Files),
    member(File, Files),
    file_base_name(File, Base),
    aggregate_all(count, load_error(File), N).

:- end_tests(loading).


		 /*******************************
		 *     THE MECHANISM ITSELF	*
		 *******************************/

%   `:- if(expect_semantics)` decides at *load* time which of two
%   expectations a suite holds, so a file that forgets to import
%   expect_semantics/0 takes the `:- else` branch in both modes.  SWI does
%   report the unknown procedure, but the file still loads, every test still
%   passes, and `make check_semantics` quietly stops measuring that defect.
%
%   That has already happened once, so it is checked rather than remembered.
%   The check is textual because which branch was taken is not observable
%   after loading.

:- begin_tests(conditional_suites).

:- use_module(library(filesex), [directory_file_path/3]).

%   Importing the module in any form is enough: `use_module(scasp_plunit)`
%   takes everything it exports, and `use_module(scasp_plunit, [...])` has
%   to name expect_semantics/0 to compile at all.

test(every_conditional_suite_imports_scasp_plunit,
     [ forall(conditional_suite(File)) ]) :-
    assertion(( file_has(File, "use_module(scasp_plunit")
              ; file_has(File, "use_module('../scasp_plunit'")
              )).

%!  conditional_suite(-File) is nondet.
%
%   A test source using the conditional, other than the module that
%   defines expect_semantics/0 itself.

conditional_suite(File) :-
    test_directory(Dir),
    directory_file_path(Dir, '*.pl', Pattern),
    expand_file_name(Pattern, Files),
    member(File, Files),
    \+ file_base_name(File, 'scasp_plunit.pl'),
    file_has(File, ":- if(expect_semantics)").

%!  test_directory(-Dir) is multi.
%
%   The shared test directory and the two per-implementation ones.

test_directory(Dir) :-
    source_file(test_directory(_), Here),
    file_directory_name(Here, Top),
    (   Dir = Top
    ;   member(Sub, [scasp, sscasp]),
        directory_file_path(Top, Sub, Dir)
    ).

file_has(File, Needle) :-
    setup_call_cleanup(
        open(File, read, In),
        ( read_string(In, _, String),
          sub_string(String, _, _, _, Needle)
        ),
        close(In)).

%   Both libraries export `scasp/1,2` and the same `scasp_*` API, which
%   is the point — a caller switches by editing one `use_module` line.
%   In one *process* that is fine; in one *file* it is not, because which
%   implementation a bare `scasp(Goal)` reaches would then depend on
%   import order rather than on what the suite is testing.
%
%   `scasp_plunit.pl` imports the old `scasp/2` without re-exporting it,
%   so importing it in full is safe today.  This says so out loud, and
%   fails the day that changes or a suite reaches for both libraries
%   directly.

test(no_suite_imports_both_libraries,
     [ forall(test_source(File)) ]) :-
    (   (   imports(File, "library(sscasp")
        ;   imports(File, "prolog/sscasp'")
        )
    ->  assertion(\+ imports(File, "library(scasp)")),
        assertion(\+ imports(File, "prolog/scasp'"))
    ;   true
    ).

%!  test_source(-File) is nondet.
%
%   Every test source but `scasp_plunit.pl`, which is exempt on purpose:
%   it is the harness both trees share, and it imports the old `scasp/2`
%   for its own `scasp_models/2` while setting the search path the new
%   suites need.  It does not *re-export* it, which is what keeps the
%   suites that import it in full from acquiring one silently.

test_source(File) :-
    test_directory(Dir),
    directory_file_path(Dir, '*.pl', Pattern),
    expand_file_name(Pattern, Files),
    member(File, Files),
    \+ file_base_name(File, 'scasp_plunit.pl').

%!  imports(+File, +Needle) is semidet.
%
%   File has a `:- use_module/1,2` directive mentioning Needle.  Line
%   anchored rather than a plain search of the text, so that a file
%   *writing about* an import — this unit does, three lines up — is not
%   taken for one making it.

imports(File, Needle) :-
    setup_call_cleanup(
        open(File, read, In),
        ( read_string(In, _, String),
          split_string(String, "\n", "", Lines),
          member(Line, Lines),
          split_string(Line, "", " \t", [Trimmed]),
          string_concat(":- use_module(", _, Trimmed),
          sub_string(Trimmed, _, _, _, Needle)
        ),
        close(In)),
    !.

:- end_tests(conditional_suites).
