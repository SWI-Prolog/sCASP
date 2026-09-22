:- module(test_all,
          [ test_all/0,
            test_all/1                          % +Options
          ]).
:- use_module(scasp_plunit, []).                % sets the library search path
:- use_module(library(plunit)).
:- use_module(library(lists), [member/2]).
:- use_module(library(option), [merge_options/3]).

/** <module> Run every plunit suite in one process

The suites can share a process, so they can also share plunit's job pool
and its per-test timeout:

    swipl -g test_all -t halt test/test_all.pl

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

:- use_module(test_scasp,          []).
:- use_module(test_clpq,           []).
:- use_module(test_disequality,    []).
:- use_module(test_prolog_calls,   []).
:- use_module(test_forall,         []).
:- use_module(test_olon,           []).
:- use_module(test_builtins,       []).
:- use_module(test_compound,       []).
:- use_module(test_dyncall,        []).
:- use_module(test_embed,          []).
:- use_module(test_le,             []).
:- use_module(test_cli,            []).
:- use_module(test_just,           []).
:- use_module(test_proof_skeleton, []).
:- use_module(test_stable_models,  []).
:- use_module(test_differential,   []).
:- use_module(test_proof_model,    []).

%!  test_all is semidet.
%!  test_all(+Options) is semidet.
%
%   Run every unit.  Options are passed to run_tests/2; the defaults are
%   `jobs(4)` and `timeout(60)`.

test_all :-
    test_all([]).

test_all(Options) :-
    merge_options(Options, [jobs(4), timeout(60)], Merged),
    run_tests(all, Merged).


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
    assertion(file_has(File, "use_module(scasp_plunit")).

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

test_directory(Dir) :-
    source_file(test_directory(_), Here),
    file_directory_name(Here, Dir).

file_has(File, Needle) :-
    setup_call_cleanup(
        open(File, read, In),
        ( read_string(In, _, String),
          sub_string(String, _, _, _, Needle)
        ),
        close(In)).

:- end_tests(conditional_suites).
