# Known bugs and defects

Found while building the test suite (Stages 0–2 of the rewrite). Everything
here was reproduced; the reproducers are minimal unless stated otherwise.

Open items are first, because those are the ones still to decide on. The
fixed ones are listed at the end for reference — they say what the tests
now guard.

---

## Open

### 1. Two s(CASP) calls in one conjunction fail

```prolog
?- scasp(p(_)), scasp(q(_)).
ERROR: determinism_error(scasp_solve:stack_parents/3, det, fail, property)
```

Six-line reproducer; also fails for two `begin_scasp` wrappers of the *same*
unit, which is exactly what `embed.pl` documents as working:

> successive sCASP calls in one conjunction accumulate one joint model

**Cause, first layer.** `scasp_call/1` (`embed.pl:321`) passes the previous
call's stack to the next `solve/4`:

```prolog
scasp_stack(StackIn),
solve(Compiled, StackIn, StackOut, Model),
save_model(Model),
Compiled = M:_,                       % TBD: Properly handle the module
save_stack(M:StackOut).
```

`save_stack/1` stores the stack *qualified with its module*, and
`scasp_stack/1` returns it that way, so the second call hands `M:Stack` to
`solve/4`, which wants a plain list. No clause of `stack_parents/3` matches
`:/2`, it fails, and the `:- det` declaration turns that into the error above.

**Cause, second layer.** Passing the plain list gets one step further and then
fails in `justification_tree/3`:

```
existence_error(matching_rule,
                scasp_stack:stack_tree([], _, [], [_/[]]))
```

Two concatenated proofs have two roots; `stack_tree/4` builds a single tree.
So the fix needs a decision about what the justification of a conjunction
*is*, not a one-line change. That is why it is not fixed here.

Pinned by `test_dyncall.pl:two_calls_in_one_conjunction_fail`, which will fail
once the behaviour changes.

### 2. `test/all_programs/clpq_equality.pl` does not terminate

```prolog
-mutually_exclusive(S1, S2) :- member(X, S1), member(X, S2).
mutually_exclusive(S1, S2)  :- not -mutually_exclusive(S1, S2).
member(X1, [X2|_]) :- X1 #= X2.
member(X, [_|R])   :- member(X, R).
?- mutually_exclusive([1,2], [3,4]).
```

Runs past any timeout. `test/min_programs/clpq_intercept.pl` is the same shape
with `#=<` instead of `#=` and terminates (with no models), so the two are
worth comparing. Recorded in `test/known_issues.pl`.

### 3. `call/1` and `forall/2` cannot be used as goals

`test/min_programs/solve_call.pl` and `solve_forall.pl`. The solver reports
``scasp_predicate `call/1' does not exist`` and yields no model.

`forall/2` is a control construct that `comp_duals.pl` *generates* for dual
rules; a program cannot write it. Via `scasp/2` it fails differently, with
`permission_error(scasp, procedure, '$apply':forall/2)`, because the name
resolves to the SWI built-in. Both recorded in `test/known_issues.pl`.

### 4. The tabling path is dead code

`predicates.pl:42`:

```prolog
:- if(fail).
table_predicate(M:Goal) => ...
:- else.
table_predicate(_) :- fail.
:- endif.
```

`table_predicate/1` therefore always fails, which makes unreachable:

- `solve.pl:214-222`, the `table_predicate` branch of `solve_goal/8`
- `solve_goal_table_predicate/8` (`solve.pl:424`)
- both `\+ table_predicate(...)` guards (`solve.pl:563`, `:571`)
- **all 63 lines of `clp/call_stack.pl`**, whose only callers are those two
  branches; its exported `dump_rules/3` has no caller at all

`#table`, `handle_table_directives/1` and `pr_table_predicate/1` compile all
the way through and are then ignored. Decide whether tabling is coming back or
whether this should go.

### 5. `scasp_load/2` compiles into its caller's module

`scasp_load(:Sources, +Options)` asserts the `pr_*/N` representation into the
module of its caller, and `clean_pr_program/1` wipes what was there first. Two
threads loading different programs from the same module therefore destroy each
other's program — the second `clean_pr_program/1` removes the clauses the first
is solving against, and the solve hangs.

It is why the golden tests all have to live in one plunit unit: two units
compiling into `test_scasp` on separate jobs hang on their first test.
`scasp/2` does not have the problem, because `dyncall.pl` compiles into an
`in_temporary_module/3`.

### 6. `load_compiled.pl` is broken and unused

- asserts `pr_rule/2` (lines 51, 60) where the rest of the system uses
  `pr_rule/3`
- asserts into the calling module, not the `M` it was given
- `read_compiled_files(M:S)` is called with a qualified term but its clauses
  match `[]` / `[F|Fs]`
- `read_compiled_source/1` has **no caller** anywhere in `prolog/`
- header says `@tbd: Not compatible with module handling.`

So `scasp -c` / `--compiled` cannot work. Either fix or delete.

### 7. `source_ref.pl` is O(n²) and never freed

```prolog
assert_scasp_source_reference(File, Pos, Ref) :-
    scasp_source_reference(Ref, File, Pos), !.
```

The first clause scans every recorded reference for every term read, so
recording is quadratic in program size. Ids are allocated by "first solution
+ 1", which breaks if anything else asserts into the table.

The table was also `dynamic` rather than `thread_local`, so it was shared by
every thread and never freed. `test_scasp.pl` resets it before each program —
otherwise the `goal_origin/2` clause numbers depend on what was loaded earlier
in the process, and golden data compared at `exact` becomes order-dependent —
and that reset threw away the references another thread was using: running the
golden tests beside the dyncall units under `run_tests/2`'s `jobs(4)` failed
eleven of them. It is `thread_local` now, which fixes the sharing and means
the table goes away with the thread. The quadratic scan and the id allocation
remain.

### 8. `opt_rule(detail(print_tree, ...))` is dead

`options.pl:157` reads the option `print_tree`, but `opt_type/3` names it
`tree`, so the rule never fires. It cannot simply be renamed: the rule
rewrites `tree(true)` to `tree(mid)`, and `solve_results/4` (`main.pl:266`)
matches on `tree(true)`, so enabling it would stop the justification being
computed at all.

Nothing is currently broken by it — `--short`/`--mid`/`--long` reach
`filter_tree/4` through the raw option list — except that conflicting detail
options are not diagnosed for trees the way they are for `--code`.

### 9. `exec_neg_findall/5` is a stub

`solve.pl:526`:

```prolog
% TODO: What to do with the negation of findall/3 (if required)
exec_neg_findall(Goal, _, _, _, _) :-
    verbose(format('PENDING: execution of not ~p \n', [Goal])),
    fail.
```

`not findall(...)` therefore always fails.

### 10. `my_copy_term/4`'s fallback branch would not compile

`solve.pl:1116-1140`, the `:- else.` of
`:- if(current_predicate(copy_term_nat/4))`, calls `ord_subtract/3` at line
1136. `library(ordsets)` is not imported by `solve.pl` and `ord_subtract`
appears nowhere else in the file. The branch is for a SWI-Prolog that no
longer exists; if it were ever taken it would raise an existence error.

### 11. Duplicated dead clauses

- `solve.pl:456` and `solve.pl:468` are the same `clp_builtin` clause of
  `solve_goal_builtin/2`; the first cuts, so the second is unreachable.
- `comp_duals.pl:213-218` and `:220-225` are six character-identical
  `dual_goal/2` clauses, under the comments `% constraint` and `% clpq/r`.

### 12. Module dependency cycles

- `common` ↔ `program` — exists only for `has_prefix/2`, four lines of atom
  munging that belong next to `split_functor/3` in `common.pl`
- `clp/clpq` ↔ `clp/disequality`
- `clp/disequality` → `verbose` → `modules` → ... — **a constraint store
  depending on the tracing module**, and `solve.pl`'s hot loop runs through
  `verbose/1` and `scasp_trace_goal/2` on every goal
- `html` ↔ `listing`, plus `listing` → `human` → `html` → `listing`

### 13. `:- pred` trips the singleton check

```prolog
:- pred shown(Thing) :: '@(Thing) is shown'.
Warning: Singleton variables: [Thing]
```

The variable occurs twice in the source but only once after the term
expansion, which is what the singleton check sees. Every file in
`test/le_programs/` carries `:- style_check(-singleton)` for this reason.
The expansion could keep the variable reachable, or `pred/1` could suppress
the check the way `begin_scasp` does.

### 14. Unsupported built-ins in embedded programs

`test/le_programs/list-scasp.pl` fails with
`permission_error(scasp, procedure, system:once/1)`. `once/1` is a compiled
built-in, so `body_calls/3` refuses it. Whether `once/1`, `forall/2` and
friends should be supported is a design question; at present the diagnosis is
at least accurate.

---

## Fixed

Each is covered by a test, so the suite now guards against regression.

| # | Defect | Where |
|---|---|---|
| 1 | `--plaindual`/`-d` parsed and then silently discarded — `opt_type/3` and `opt_help/2` existed, no `set_option/1` clause | `options.pl` |
| 2 | `--variant` had `opt_help/2` but no `opt_type/3`, so it could not be passed at all | `options.pl` |
| 3 | `scasp_real` read by `main.pl` and `json.pl` but never created | `options.pl` |
| 4 | `scasp --version` and `--help` exited 1: `catch_with_backtrace/3` caught the `unwind(halt(0))` | `main.pl` |
| 5 | `#show`/`#pred`/`#prolog` expanded to **static** `pr_*` facts, so `clean_pr_program/1` raised "No permission to modify static procedure" | `dyncall.pl` |
| 6 | Inside `begin_scasp`, every directive except `#discontiguous`/`#pred` leaked to the dyncall expansions and was then filtered away | `embed.pl` |
| 7 | `scasp_compiled/1` omitted `pr_prolog_predicate/2` and `pr_dcc_predicate/2`, which `clean_pr_program/1` retracts | `predicates.pl` |
| 8 | `error_position/3` failed without `base/1`/`stream/1`, turning a syntax error into a silent failure of `directive/4` | `input.pl` |
| 9 | A program defining `forall/2` crashed with `No rule matches qualify_body/3`; now a proper error naming the predicate | `modules.pl`, `dyncall.pl` |
| 10 | `--html` without `--tree` crashed: `solve_results/4` omits the `tree` key, the HTML renderer reads `Answer.tree` unconditionally | `options.pl` |
| 11 | `scasp/1` documented in the pldoc since the module was written but never defined | `dyncall.pl` |
| 12 | A module name containing `:` corrupted every literal in the model *and* the justification: `'family-scasp+http':'//tests.com:is_a_parent_of'(john,mary)`. Split at the first colon instead of the last | `modules.pl` |
| 13 | `sandbox:safe_meta_predicate/1` declared multifile while `safe_meta/2` was defined; `?+++` missing from the safe list | `scasp.pl` |
| 14 | `library(http/json)` moved; warned on every run | `main.pl` |
| 15 | Public-name typos `unqualify_justitication_tree/3` (alias kept) and `contrained_neg/2` | `stack.pl`, `solve.pl` |

Test-infrastructure defects fixed at the same time: `-q` resolved the renamed
`test/programs/`; `test_scasp.pl` declared `prolog_cover:report_hook/2` while
defining `prolog_coverage:report_hook/2`; `test_clp_disequality.pl` imported a
`clp_disequality` that no longer resolves; `.gitlab-ci.yml` was Ciao's and
tested nothing; `doc/Tricks.md` paths were stale; and the plunit suites
resolved `library(scasp)` to the installed pack rather than this tree.
