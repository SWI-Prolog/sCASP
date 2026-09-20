:- module(test_all,
          [ test_all/0,
            test_all/1                          % +Options
          ]).
:- use_module(scasp_plunit, []).                % sets the library search path
:- use_module(library(plunit)).
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
:- use_module(test_dyncall,        []).
:- use_module(test_embed,          []).
:- use_module(test_le,             []).
:- use_module(test_cli,            []).
:- use_module(test_just,           []).
:- use_module(test_proof_skeleton, []).

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
