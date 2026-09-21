# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

The SWI-Prolog port of **s(CASP)** — a goal-directed, *ungrounded* ASP solver with
constraints (fork of the Ciao original at gitlab.software.imdea.org/ciao-lang/sCASP).
It ships as a SWI-Prolog pack (`pack.pl`), a standalone `scasp` executable, and a
library (`library(scasp)`) embeddable in ordinary Prolog programs.

Everything is Prolog; there is no C code and no external build system beyond `make`.

## Commands

```bash
make                    # build ./scasp executable + prolog/scasp/main.qlf
make check_load         # load both entry points, fail on warnings/undefined preds
make check              # full test suite: regression tests, --dcc suite, plunit
make install            # symlink ./scasp into $SWIPL_PREFIX/bin
make distclean          # remove scasp and main.qlf

# Run without building
swipl prolog/scasp/main.pl -- [scasp-options] test/all_programs/family.pl
swipl scasp [options] file...          # as a SWI-Prolog app (slower startup)

# Tests
swipl -g 'run_tests(golden)' -t halt test/test_scasp.pl      # only the golden tests
swipl -g "run_tests(golden:'all_programs/birds')" -t halt test/test_scasp.pl
swipl -g "save_golden('test/all_programs')" -t halt test/test_scasp.pl
swipl -g coverage_by_test -t halt test/coverage_by_test.pl   # which tests add coverage
swipl -g test_all -t halt test/test_all.pl                   # every plunit suite, jobs(4)
swipl -g 'test_all([jobs(1)])' -t halt test/test_all.pl      # ... sequentially
swipl -g 'run_tests(embed_birds)' -t halt test/test_all.pl   # ... one unit
swipl -g test_embed -t halt test/test_embed.pl               # ... or one suite on its own
```

For ordinary coverage there is nothing to write — wrap the run:
`coverage(run_tests(golden), [dir(cov)])`. `test/coverage_by_test.pl` does the other
thing: it runs each golden program alone and reports the smallest set that covers the
same clauses, so a program that only repeats another shows as contributing 0.

`test_scasp.pl` is a plunit suite rather than a command-line tool. Its two entry points
are `run_tests/1,2` and `save_golden/1,2`, which writes `.pass` data where it is absent,
or everywhere with `overwrite(true)`. Programs listed in `known_issues.pl` are skipped
by both — recording their output would record the bug as the expected answer.

### Gotchas

- **`library(scasp)` resolves to the *other* checkout.** The installed pack symlinks
  to `/home/jan/src/prolog/sCASP` (branch `master`), so a test that says
  `use_module(library(scasp))` silently tests that tree instead of this one. Run
  `swipl -p library=prolog ...` from the repo top, or use the idiom in
  `test/test_scasp.pl`, which derives the root from its own source file.
- `make check` runs `test/all_programs/dcc` as a separate invocation, with `--dcc`
  enabled; those `.pass` files are only valid under that flag. `--dcc` also forces
  `--forall=prev` and errors on any other value, which is why it needs its own process.
- Options are process-global Prolog flags (18 of them, created across 9 modules and
  set by `scasp_set_options/1`). `scasp_forall` and `scasp_dcc` change the *semantics*
  of `solve/4` and `generate_nmr_check/1`, so two callers in one process cannot use
  different settings.
- `goal_origin(_, Ref)` clause references come from `scasp_source_reference/3`, a
  process-global counter that is never retracted, so they depend on what was loaded
  earlier in the process. `test_scasp.pl` resets it per test.

## Test conventions

Regression tests are *golden-output* tests. `test/test_scasp.pl` generates one plunit
test per program — solving it with `scasp_load/2` + `scasp_query/3` + `solve/4`, the
front end the executable uses — and compares against `<base>.pass` (a single
`write_term/2` term read back with `module(scasp_ops)` so the s(CASP) operators parse).
Regenerate the data with `save_golden/1,2` after an intended change.

The two halves are compared differently, because the model of an answer is stable while
the *location* of steps in its justification moves whenever the solver changes:

- **Model: exactly.** `answers_model_key/2` normalises variable naming and ignores the
  order of the answers, nothing else. There is no option to weaken it.
- **Justification: at a declared level.** `proof_skeleton/3` (`test/proof_skeleton.pl`)
  maps a justification onto a canonical term at one of `exact` (whole tree),
  `refless` (no clause refs, abduction collapsed), `literals` (bookkeeping nodes such as
  `findall/3` deleted, children reparented), `support` (the set of literal
  parent→child edges) or `reached` (the set of (literal,status) pairs). Variable naming
  is normalised at every level, and inlined constraint stores are sorted.

Every test compares at `exact` unless `test/proof_levels.pl` records a reason to be
looser, so a change to the shape of a proof is a failure by default. A failing test
names the strongest level at which it *would* agree — exactly the entry `proof_levels.pl`
would need — so loosening one is a deliberate act rather than a silent `--pass` rewrite.
`test/test_proof_skeleton.pl` pins both halves of that guarantee against hand-built trees.

A program that cannot be solved gets no `.pass` file: that would record the bug as the
expected answer. It goes in `test/known_issues.pl` with the reason, which shapes its
generated test — `blocked(Reason)` for one that does not terminate (`clpq_equality.pl`
alone was two thirds of `make check`), and for one that fails fast, a test declared
`fail` whose body checks whether the program now yields a model.

### Semantics vs. pinned behaviour

Some tests assert what any correct implementation must do; others pin what *this*
one currently does, bug included. A reimplementation needs to tell them apart, so
where the current answer is known to be wrong and the right one is known, both are
written, selected by conditional compilation:

```prolog
:- if(expect_semantics).
test(the_unbound_answer_admits_every_shape_that_holds, ...) :- ...
:- else.
test(the_unbound_answer_admits_only_the_f_shape, ...) :- ...
:- endif.
```

`expect_semantics/0` (from `scasp_plunit.pl`) is true when the flag `scasp_expect`
is `semantics`. Default is current behaviour, so `make check` stays green;
`make check_semantics` runs the other branch and is **expected to fail** — its
failures are the list of defects still outstanding, and each failure message states
the answer that is wanted. Four are wired up today (`doc/bugs.md` #1, #15, the
`∉` item and negated `findall/3`).

A file using `:- if(expect_semantics)` **must import `scasp_plunit.pl`**. The
condition is evaluated at *load* time, so a missing import makes the directive
error and silently take the `:- else` branch in both modes — every test still
passes and `check_semantics` quietly stops measuring that defect. This has happened
once already, so `test_all.pl:conditional_suites` checks it.

Where the current answer is wrong but the right one has not been *decided*, there is
no second branch: the test carries a `% PINNED, no semantics branch yet.` comment
saying what has to be settled first. Assertions about how many answers a solution
set arrives in are pinned too and marked as such — the set is semantics, the number
of answers it is split into is an implementation choice.

**Do not use `fixme` for that.** plunit does not run the body of a `fixme` test — nor of
a `blocked` one — so it can never report that the problem has gone away. A test declared
`fail` does run, and fails loudly the day the goal starts succeeding. The same applies
to a known-wrong *answer*: assert the wrong value with a comment saying so, rather than
marking the correct assertion `fixme`.

Add a new test by dropping a `.pl` file into `test/all_programs/` and running with
`--save`. Every program in `all_programs`, `sasp`, `dcc` and `min_programs` is pinned
except the three known issues.

`test/le_programs/` is different: those are **embedded** examples — Prolog modules that
load `library(scasp)` and are queried through `scasp/2`. Running them through the file
front end (`scasp_load/2`) reads their `:- module/2` and `:- dynamic` directives as
s(CASP) global constraints and is meaningless. They are driven by `test/test_le.pl`.

### The plunit suites

`test_scasp.pl` covers only the file front end. The suites below cover what
`library(scasp)` actually offers, and load `test/scasp_plunit.pl`, which provides the
model comparison, the `scasp_*` flag save/restore for units that change options, and —
importantly — makes `library(scasp)` resolve to *this* tree.

| Suite | Covers |
|---|---|
| `test/test_dyncall.pl` | `scasp/1,2` over ordinary Prolog clauses: clause collection, global-constraint overlap, directives, `scasp_dynamic` |
| `test/test_embed.pl` | `begin_scasp`/`end_scasp`: `#pred`, `#show`, `#abducible`, `-p(X)` heads, `:- Constraint.`, `?- Query.` |
| `test/test_le.pl` | the Logical English examples, run through `scasp/2` |
| `test/test_cli.pl` | argument parsing, exit status and output formats, by running `main.pl` in a subprocess |
| `test/test_proof_skeleton.pl` | the comparison itself, against hand-built trees |
| `test/test_stable_models.pl` | the brute-force oracle, against worked examples |
| `test/test_differential.pl` | s(CASP) vs. that oracle, on hand-written and random programs |
| `test/test_scasp.pl` | the golden-output tests for the file front end |
| `test/test_clpq.pl` | CLP(Q) constraints and the duals that make `not` constructive |
| `test/test_forall.pl` | the `forall` construct and its four algorithms |
| `test/test_olon.pl` | loops over negation, coinduction and the NMR check |
| `test/test_builtins.pl` | built-ins, `findall/3`, `#abducible`, classical negation |
| `test/test_compound.pl` | compound terms in heads, bodies and negation |
| `test/test_disequality.pl` | constructive disunification and the `∉` operator |
| `test/test_prolog_calls.pl` | `:- prolog p/2`, justified and opaque |

### The independent oracle

`test/stable_models.pl` computes answer sets by brute force, straight from the
Gelfond–Lifschitz definition: ground the program, enumerate subsets of the Herbrand
base, take the reduct, keep the subsets that equal its least model. It is
exponential on purpose — nothing clever can be wrong in a way that agrees with a
clever solver. It covers default negation, classical negation (with the consistency
requirement, so a program deriving `p` and `-p` has no answer set at all), variables
by naive grounding over the program's constants, and CLP(Q) comparisons in bodies,
evaluated while grounding.

**Unsafe rules are refused**, and that is about s(CASP) rather than the oracle:
s(CASP) reads an unsafe rule over an *extended* Herbrand universe, so
`:- not married(X).` means "everyone is married" and a finite grounding would
disagree by design rather than by defect (P4 Example 1, `doc/literature.md`).

It matters because every *other* oracle here descends from the implementation it
judges: `.pass` data records what this solver produces, and the sibling checkout at
`../sCASP` is the same codebase. `test/differential.pl` points the two at the same
program and compares **brave entailment** — `scasp` answers `a` iff `a` is in some
answer set, and answers `not a` iff `a` is absent from some answer set. That is
sharp both ways: it catches an answer no answer set supports, and an answer set the
solver cannot reach. `random_program/3,4` generates programs seeded reproducibly, so a
failure names the seed that repeats it; options `classical/1`, `first_order/1` and
`clp/1` choose the fragment. Seed counts per shape are tuned in `test_differential.pl`
because the *oracle* is exponential in the Herbrand base — three predicates over
three constants is 2¹² interpretations — while s(CASP) is unaffected, never grounding
at all. Widening a sweep is a one-line change to those counts.

`test/test_all.pl` loads every suite — the golden tests included — into one process:
`make check` is a single `swipl` call, 171 tests in 2.5s. The timeout is per *test*,
so a program that does not terminate reports itself and the rest of its unit still runs.

plunit's `jobs(N)` parallelises whole *units*, and two things have to hold for a unit to
share a run:

- **A `scasp_*` flag may only be set inside a test.** Flags are thread-local once set — a
  thread inherits its creator's value and its own changes do not propagate back — so a
  flag set in the thread plunit runs the test in leaves the other jobs alone. A unit
  `setup/1` runs in the *main* thread and would leak into every job created after it.
- **Programs compiled by `scasp_load/2` belong in one unit.** It asserts the compiled
  program into its caller's module and wipes what was there first, so two units
  compiling into the same module destroy each other's program (`doc/bugs.md` §5). The
  golden tests are one unit for that reason; plunit runs its tests one at a time.

**A unit must not change a `scasp_*` flag while `jobs(N) > 1`.** Those flags are
process-global and `scasp_forall`/`scasp_dcc` change solver semantics, so one
unit would silently alter what another is testing. Pass the option to `scasp/2`
instead (`scasp(Goal, [unknown(error)])`); `scasp_plunit.pl` has `scasp_flags/1`
and `set_scasp_flags/1` for a unit that cannot, but it must then run alone.

Note the two spellings of a global constraint: inside `begin_scasp` it is
`:- Constraint.`, but in a plain Prolog module that is an ordinary directive and the
constraint must be written `false :- Constraint.`

## Architecture

### Three entry points, one core

| Entry point | Module | Use |
|---|---|---|
| `prolog/scasp/main.pl` | `scasp_main` | the `scasp` CLI/executable; loads files, answers the last query, exits |
| `prolog/scasp.pl` | `scasp` | the user-facing library: `?-`-style operators (`?--`, `?+-`, `?-+`, `?++`, `??++`, …), `scasp/2` |
| `prolog/scasp/embed.pl` / `dyncall.pl` | `scasp_embed`, `scasp_dyncall` | embed s(CASP) blocks in Prolog (`begin_scasp/1`..`end_scasp/0`) or solve queries against ordinary Prolog clauses |

`dyncall.pl` is the interesting one: `scasp/2` collects the clauses transitively
reachable from the query plus any global constraints whose call tree overlaps, compiles
them into an `in_temporary_module/3`, solves there, and returns model/tree to the caller.
Hooks `scasp_expand/4` and `scasp_expand_program/4` in the source module let programs
rewrite the collected clauses.

### Compilation pipeline (`compile.pl` orchestrates)

1. **`input.pl`** — read source with the Prolog reader plus s(CASP) operators; produce
   *statements*.
2. **`program.pl`** — assert statements into dynamic predicates (`defined_rule/4`,
   `defined_query/2`, `defined_directive/1`, …). This is mutable global state and is
   destroyed (`destroy_program/0`) at the end of every compile.
3. **`comp_duals.pl`** — compute dual rules (rules proving the *negation* of a literal);
   variables introduced in a body become `forall` constructs.
4. **`nmr_check.pl`** — build the call graph (`call_graph.pl`), detect OLONs (odd loops
   over negation), and generate the NMR check / global constraints (`o_chk_N`).
5. **`pr_rules.pl`** — emit the final *pr* representation as clauses asserted into the
   target module: `pr_rule/3`, `pr_query/1`, `pr_user_predicate/1`,
   `pr_table_predicate/1`, `pr_pred_predicate/4`. `predicates.pl` reads them back.
   `clean_pr_program/1` wipes them.

`load_compiled.pl` can read a previously dumped pr program (see `examples/compiled/`).

### Solving (`solve.pl`, ~1200 lines — the hot path)

`solve(:Goals, +StackIn, -StackOut, -Model)`. Key layers: `check_goal/8` (CHS /
coinduction / loop detection) → `solve_goal/8` → `solve_goal_predicate/8`,
`solve_goal_forall/8`, `solve_c_forall/8`, `solve_goal_builtin/2`. Constraint stores live
in `clp/`:

- `clp/disequality.pl` — constructive disunification (`.=.`, `.\=.`, `∉`)
- `clp/clpq.pl` — CLP(Q) extended with `dual_clpq/2` for dualising a constraint store
- `clp/call_stack.pl` — carries StackIn/StackOut/Model as attributes (`~>`/`<~`)

This port's two optimisations over the Ciao original are low-level term copying for
`forall` and an index for loop/proved-literal lookup (~10x). Be careful with determinism
and indexing when editing `solve.pl`; many predicates carry `:- det(...)` declarations.

### Naming conventions inside the compiled program

The solver is **not module aware**. `modules.pl` interns the module into the functor
name instead of carrying `M:Term`, because the solver is the bottleneck. Functors also
carry arity (`split_functor/3`, `join_functor/3`) and reserved single-letter prefixes
(`program.pl:reserved_prefix/1`):

- `n_` — dual (negated) rule, `negate_functor/2`
- `c_` — classical negation (`-p`)
- `o_` — "dummy" escape prefix for names that already start with `_` or a reserved prefix
- `d_` — dummy
- `_false_0` — head of headless rules (constraints), consumed by the NMR check

`raise_negation/2` / `intern_negation/2` in `common.pl` convert between the user-facing
`not`/`-` forms and the interned form.

### Output

One justification tree, many renderings. `stack.pl` turns the solver stack into the
canonical tree (`Node-Children`, nodes wrapped in `not/1`, `-/1`, `chs/1`, `proved/1`,
`assume/1`, `goal_origin/2`); `model.pl` canonicalises the model. Then:

- `output.pl` — term emission, constraint inlining, variable naming (`ovar_*`)
- `html.pl` — the **primary** renderer (HTML justification, model, program listing)
- `html_text.pl` — reinterprets `html//1` tokens as coloured plain text; `human.pl`
  builds on it for `--human` natural-language output. Changing CSS classes or the DOM in
  `html.pl` usually requires a matching change in `html_text.pl`.
- `json.pl` — machine-readable results; `listing.pl` — `--code` program listing
- `lang/en.pl`, `lang/nl.pl` — message translations, selected via the `scasp_lang` flag

### Configuration

Behaviour is controlled by Prolog flags (`scasp_forall`, `scasp_dcc`, `scasp_unknown`,
`scasp_plain_dual`, `scasp_compile_olon`, `scasp_compile_nmr`, `scasp_show_model`,
`scasp_show_justification`, `scasp_lang`, …), all created with `create_prolog_flag/3` in
the module that uses them. `options.pl` maps CLI flags onto them via
`scasp_set_options/1`. Flags that SWISH may set must be listed in the sandbox section at
the bottom of `prolog/scasp.pl`.

### Calling Prolog from s(CASP)

`:- prolog p/2.` (optionally `as opaque`). In `justified` mode `prolog.pl` is a
meta-interpreter that extends the justification tree and handles `!`, `(If->Then;Else)`,
`;`, and `=>` rules; `opaque` calls Prolog natively and shows only the call. The
meta-interpreter cannot handle meta-predicates. Negation of a Prolog goal uses `\+` when
ground, otherwise `findall/3` + disequality constraints.

## Development notes

- `doc/Tricks.md` has the debugging recipes (`gspy/1`, `gtrap/1`, `?- make.`, profiling,
  `gxref`). Its example paths are stale but the techniques are current.
- `doc/test-strategy.md` assesses the suite against the goal of reimplementing the
  reasoner: what is covered, the two gaps that matter (feature *crossings*, and tests
  that pin known bugs as expected), and which tests survive a change of data
  representation. Read it before adding white-box tests.
- `doc/literature.md` summarises the five papers the design comes from and maps each
  idea onto the module that implements it — read it before changing `comp_duals.pl`,
  `nmr_check.pl` or the `forall` code, several of which transcribe a published
  algorithm. It also lists what in this port has no paper behind it.
- `doc/bugs.md` lists the defects found while building the test suite: what is
  fixed (and therefore guarded by a test) and what is still open.
- Run `make check_load` after touching imports/exports — it catches undefined predicates
  in both entry points, which plain loading does not.
- CI is `.github/workflows/ci.yml` (build + `make check`). The old `.gitlab-ci.yml` was
  inherited from Ciao, built with `ciaoc` and tested nothing in this port.
- Requires SWI-Prolog 9 (`pack.pl`); `main/1` additionally asserts `>= 8.5.0-71`.
- `ATTIC/` is dead upstream material; do not use it as a reference for current behaviour.
