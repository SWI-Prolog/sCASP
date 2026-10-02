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
make                    # build ./scasp and ./sscasp + prolog/scasp/main.qlf
make check_load         # load both entry points, fail on warnings/undefined preds
make check              # check_load, then the whole suite against both implementations
make check_scasp        # shared suites + test/scasp/, against the old implementation
make check_sscasp       # shared suites + test/sscasp/, against the new one
make install            # symlink ./scasp into $SWIPL_PREFIX/bin
make distclean          # remove scasp and main.qlf

# Run without building
swipl prolog/scasp/main.pl -- [scasp-options] test/all_programs/family.pl
swipl scasp [options] file...          # as a SWI-Prolog app (slower startup)

# Tests
swipl -g 'run_tests(golden)' -t halt test/scasp/test_scasp.pl      # only the golden tests
swipl -g "run_tests(golden:'all_programs/birds')" -t halt test/scasp/test_scasp.pl
swipl -g "save_golden('test/all_programs')" -t halt test/scasp/test_scasp.pl
swipl -g coverage_by_test -t halt test/scasp/coverage_by_test.pl   # which tests add coverage
swipl -g test_all -t halt test/test_all.pl                   # every suite for scasp, jobs(4)
swipl -Dsscasp -g test_all -t halt test/test_all.pl   # ... for sscasp
swipl -g 'test_all([jobs(1)])' -t halt test/test_all.pl      # ... sequentially
swipl -g 'run_tests(embed_birds)' -t halt test/test_all.pl   # ... one unit
swipl -g test_embed -t halt test/scasp/test_embed.pl               # ... or one suite on its own
```

For ordinary coverage there is nothing to write — wrap the run:
`coverage(run_tests(golden), [dir(cov)])`. `test/scasp/coverage_by_test.pl` does the other
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
  `test/scasp/test_scasp.pl`, which derives the root from its own source file.
- `make check` runs `test/all_programs/dcc` as a separate invocation, with `--dcc`
  enabled; those `.pass` files are only valid under that flag, and `--dcc` forces
  `--forall=prev` and errors on any other value.
- Options are Prolog flags (18 of them, created across 9 modules and set by
  `scasp_set_options/1`). **They are thread-local on write**: a thread's
  `set_prolog_flag/2` does not propagate back to its creator, and a new thread
  inherits the creator's value at creation. So a caller *can* set a flag and restore
  it around a solve — single-threaded that is all it takes. What it cannot do is hold
  two different values at once in one thread, which matters for `scasp_forall` and
  `scasp_dcc` because they change the *semantics* of `solve/4` and
  `generate_nmr_check/1`.
- `goal_origin(_, Ref)` clause references come from `scasp_source_reference/3`, a
  thread-local counter that is never retracted, so they depend on what was loaded
  earlier in *this thread*. `test_scasp.pl` resets it per test.

## Test conventions

### Where the tests are

Three places, because a test says one of three things:

| | | loaded against |
|---|---|---|
| `test/` | what s(CASP) **means**: the suites written against `scasp/1,2` (`test_builtins`, `test_clpq`, `test_compound`, `test_disequality`, `test_forall`, `test_olon`, `test_prolog_calls`, `test_dyncall`, `test_le`, `test_examples`, `test_differential`), the Gelfond–Lifschitz oracle, `proof_skeleton.pl`, the harness `scasp_plunit.pl`, and the program data | either implementation |
| `test/scasp/` | how the old implementation works: the golden front end, the CLI, the obsolete block API and its justification trees | `library(scasp)` |
| `test/sscasp/` | how the new one works | `library(sscasp)` |

A shared suite loads **`under_test.pl`** instead of either library, and the flag
`sscasp` says which one that is: `-Dsscasp` for the new library, the old one otherwise.
The switch is `library(scasp)` itself: under `-Dsscasp`, `prolog/scasp.pl` re-exports
`library(sscasp)` and defines nothing, so a program written for `library(scasp)` — a
Logical English export, an example in `examples/` — runs against either unedited.
`scasp_plunit.pl` takes its `scasp/2` from there too, so `scasp_models/2` and friends
test the same implementation as the suite that calls them. Where one library exports
what the other keeps internal, `under_test.pl` makes them agree: `scasp_query_clauses/2`
is in `library(sscasp)`'s interface and only in `scasp/dyncall.pl` in the old tree.

A suite belongs in `test/` when it tests what **both libraries export** — the question is
the API, not how the answer is found. Cross-running decides it: `test_dyncall` passes 40
of 43 against the new library (the other three are gaps), while `sscasp/test_api`
passes 23 of 88 against the old one, being mostly about what only the new one has
(`scasp_show`, JSON, `:- sld`, `:- describe`), so it stays. `make check` runs
`test_all.pl` twice, once per value.

A shared test the new implementation does not pass yet is a **gap**, recorded in
`test/gaps.pl` with the reason. `test_all/1` leaves the gaps out of the main run, then
runs each on its own, quietly, and **fails if one passes** — so an entry cannot
outlive the problem it records, which is `known_issues.pl`'s discipline again. A clause
that does not even *load* against an implementation (an operator it does not define)
is a test that is silently not there, so load errors are counted per file and must
match `load_errors/4` in the same file; the unit `loading` checks it.

Not plunit's `fixme` for that: it does run the body, but a `fixme` test whose only
failure is an `assertion/1` is counted as *passed*.

Regression tests are *golden-output* tests. `test/scasp/test_scasp.pl` generates one plunit
test per program — solving it with `scasp_load/2` + `scasp_query/3` + `solve/4`, the
front end the executable uses — and compares against `<base>.pass` (a single
`write_term/2` term read back with `module(scasp_ops)` so the s(CASP) operators parse).
Regenerate the data with `save_golden/1,2` after an intended change.

The two halves are compared differently, because the model of an answer is stable while
the *location* of steps in its justification moves whenever the solver changes:

- **Model: exactly.** `answers_model_key/2` normalises variable naming and ignores the
  order of the answers, nothing else. There is no option to weaken it — and it does not
  need one: the model is a *function of the proof* (`test/scasp/test_proof_model.pl`), so two
  answers with the same proof at `support` always carry the same model. Comparing the
  proof therefore subsumes comparing the model rather than competing with it.
- **Justification: at a declared level.** `proof_skeleton/3` (`test/proof_skeleton.pl`)
  maps a justification onto a canonical term at one of `exact` (whole tree),
  `refless` (no clause refs, abduction collapsed), `literals` (bookkeeping nodes such as
  `findall/3` deleted, children reparented), `support` (the set of literal
  parent→child edges) or `reached` (the set of (literal,status) pairs). Variable naming
  is normalised at every level, and inlined constraint stores are sorted.

Every test compares at `exact` unless `test/scasp/proof_levels.pl` records a reason to be
looser, so a change to the shape of a proof is a failure by default. A failing test
names the strongest level at which it *would* agree — exactly the entry `proof_levels.pl`
would need — so loosening one is a deliberate act rather than a silent `--pass` rewrite.
`test/test_proof_skeleton.pl` pins both halves of that guarantee against hand-built trees.

A program that cannot be solved gets no `.pass` file: that would record the bug as the
expected answer. It goes in `test/scasp/known_issues.pl` with the reason, which shapes its
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
the answer that is wanted. Five are wired up today (`doc/bugs.md` #1, #15,
#21, the `∉` item and negated `findall/3`).

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

### No test may rest on an undefined predicate

`scasp/2` collects by **name**: a goal is looked up where the query's module
can see it, so a predicate a test program leaves undefined resolves to
whatever a loaded library happens to export under that name — and the test
then passes or fails depending on what *else* is in the process.

Not hypothetical. `test_api.pl` declared `maybe` abducible and called it;
`maybe/0` is a predicate of `library(random)`, so the moment another suite
loaded that library the collector took its one clause, `random(2) =:= 0`, and
the query answered with a coin toss in the model. The test passed on its own
and failed under `make check`.

So, in any test whose program is a **module's clauses** rather than a term
list:

- **Define it.** `p :- fail.` where the predicate is meant to have no
  solutions. An undefined predicate and one that fails mean the same thing to
  the compiler, which closes the program either way — they do not mean the
  same thing to the *collector*.
- **Or declare it**, where it is an abducible, a domain or an `:- sld`.
  `dyncall.pl:declared/2` reads those and stops the walk, which is what makes
  them safe; it is also why that predicate exists.
- **Or give it a name nothing else could own**, for one that exists only
  between `setup` and `cleanup` — `api_shared_head/1` rather than `shared/1`.

`test_api.pl:api_hygiene` enforces this: it walks what each query actually
collects and checks every predicate is the test module's own or declared. It
was confirmed to fail on the `maybe/0` case before being trusted.

A program written as a **term list** — `scasp/3,4`, and every suite except
`test_api.pl` — is not exposed: there is no module to look a name up in, and
an undefined predicate is closed to false-of-everything by the compiler.

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
s(CASP) global constraints and is meaningless.

`test/le_corpus.pl` is the description — which programs, what each is asked, what it
answers, and what the new library answers where it differs (`le_differs/4`) — and
`test/test_le.pl`, a shared suite, runs it against the library under test. One
description, because written twice they would drift. Each program says
`:- use_module(library(scasp))`, and `-Dsscasp` makes that the new library
(`prolog/scasp.pl`), so the files run as they are. The justification tests
are shared too: they name steps the proof must contain, and `test_le.pl:literals/2`
takes both libraries' trees apart; the one that pins the old tree's *shape* is a gap.

**12 of 13 agree.** `le_differs/4` records the one that does not, with what the new
library answers and why, and the tests compare against that — so the entry fails the
day the difference goes. `list-scasp` defines its own `forall/2`; both libraries refuse
to run it and name a different unsupported thing first. `subset-scasp` does the same and
both name the redefinition. `turingcomplete-scasp` agrees by a different route: it calls
`append/3` without defining it, `library(scasp)` reads that as a predicate the program
never defined and closes it to false, while we follow the name into `library(lists)` and
collect its clauses.

### The plunit suites

`test_scasp.pl` covers only the file front end. The suites below cover what
`library(scasp)` actually offers — those in `test/` itself for either implementation,
through `under_test.pl` — and load `test/scasp_plunit.pl`, which provides the
model comparison, the `scasp_*` flag save/restore for units that change options, and —
importantly — makes `library(scasp)` resolve to *this* tree.

| Suite | Covers |
|---|---|
| `test/test_dyncall.pl` | `scasp/1,2` over ordinary Prolog clauses: clause collection, global-constraint overlap, directives, `scasp_dynamic` |
| `test/scasp/test_embed.pl` | `begin_scasp`/`end_scasp` — an **obsolete** API, kept running so it does not rot silently |
| `test/test_le.pl` | the Logical English examples, run through `scasp/2`, against either library |
| `test/scasp/test_cli.pl` | argument parsing, exit status and output formats, by running `main.pl` in a subprocess |
| `test/test_proof_skeleton.pl` | the comparison itself, against hand-built trees |
| `test/scasp/test_proof_model.pl` | that the model is a function of the proof |
| `test/test_stable_models.pl` | the brute-force oracle, against worked examples |
| `test/test_differential.pl` | s(CASP) vs. that oracle, on hand-written and random programs |
| `test/scasp/test_scasp.pl` | the golden-output tests for the file front end |
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

`test/test_all.pl` loads the shared suites and one implementation's own into one
process — the golden tests included. The timeout is per *test*,
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

Passing the option to `scasp/2` (`scasp(Goal, [unknown(error)])`) is still the
tidier way when it is available; `scasp_plunit.pl` has `with_forall/2`, and
`scasp_flags/1` + `set_scasp_flags/1` for saving and restoring a set of them.

**Prefer `scasp/2` over `scasp_load/2` in tests.** The file front end compiles into
its caller's module and wipes what was there (`doc/bugs.md` #5), so two runs in one
process interfere — which forces whole suites into a single plunit unit and makes
comparing two solver configurations unreliable. `scasp/2` collects the clauses
reachable from the query and has neither problem. `test_scasp.pl` uses the file front
end because that is the entry point it exists to test; everything else uses `scasp/2`.

Note that `scasp/2` is *relevance-scoped* — it collects the clauses reachable from
the query plus the global constraints overlapping them. A contradiction, odd loop or
constraint the query cannot reach therefore does not constrain the answer. Where a
test needs whole-program semantics, name the rest of the program with the `collect`
option: `scasp(q, [collect((p, -p))])`.

### The obsolete block API

`begin_scasp/1,2` .. `end_scasp/0` is obsolete. Use `scasp/2` over ordinary
Prolog clauses. It still works and `test_embed.pl` still runs, so a regression
would be noticed, but it is not part of the design going forward and a
reimplementation need not reproduce it.

Two things this does *not* mean. The rest of `embed.pl` stays: `scasp_call/1` is
how `dyncall.pl` and the toplevel run a query, and `scasp_model/1,2`,
`scasp_stack/1` and `scasp_justification/2` are how a caller reads the result.
And the *language* features `test_embed.pl` happens to exercise through blocks —
`#show`, `#abducible`, `#pred`, `-p(X)` heads, `:- Constraint.` — are covered
independently by the golden corpus (4, 3, 1 and many programs respectively), so
they do not go with it.

Note the two spellings of a global constraint: inside `begin_scasp` it is
`:- Constraint.`, but in a plain Prolog module that is an ordinary directive and the
constraint must be written `false :- Constraint.`

## The fresh implementation

`prolog/sscasp/` is the reimplementation, started fresh rather than grown out of
`prolog/scasp/` — decisions 1–3 of `doc/design-next.md` replace the compiler and the
solver wholesale, so growing it in place would mean a long stretch with no green
suite. It sits under `prolog/`, the pack's library root, so `library(sscasp/ops)`
resolves out of the box beside `library(scasp/compile)`. Modules are named
`sscasp_*`, and load **each other by plain file name** — `:- use_module(ops).` —
reserving `library(...)` for what comes from outside the directory.

**The module names differ deliberately** so both trees load into one process.
That is the point of starting beside rather than inside: the differential harness
can point the same program at both, with the Gelfond–Lifschitz oracle as referee.

**The *predicate* names deliberately do not.** ssCASP is to become sCASP, so its
predicates are the ones a caller will keep: `scasp/1,2,3,4`, `scasp_show/2`,
`scasp_readable/2,3`, `scasp_json/2,3` — the same spellings `library(scasp)`
exports, in a module that is not the same module. A file switches library by
editing one `use_module` line and changes nothing else:

```prolog
:- use_module(library(sscasp)).     % or library(scasp) — the rest is identical
?- scasp(flies(X)).
```

Three consequences worth knowing:

- **A module atom is not a predicate name**, so `module(sscasp_ops)`,
  `imported_from(sscasp_dyncall)` and the attribute name `sscasp_herbrand` keep
  their spelling. The attribute name *must* equal its module's.
- **Anything installed in `user` has to ask which library it is firing for**,
  because both claim `:- pred` and `false :- Body`. `sscasp.pl:ours/1` is that
  question: `prolog_load_context(module, M)` for the file being compiled, then
  `predicate_property(M:scasp(_,_), imported_from(sscasp_dyncall))`. The
  property survives the re-export through `library(sscasp)`.
- **No one file may import both.** In one process two libraries are fine; in
  one file a bare `scasp(Goal)` would resolve by import order.
  `test_all.pl:no_suite_imports_both_libraries` checks it, reading directives
  rather than text so that a file *writing about* an import is not taken for
  one making it. `scasp_plunit.pl` is exempt: it is the harness both trees
  share, and it imports the old `scasp/2` without re-exporting it.

One collision to know about: `scasp_program/2` is ours, and `scasp_program` is
also the name of an old-tree *module* (`prolog/scasp/program.pl`). Different
namespaces, so nothing breaks, and the old one goes when the old tree does.

| Module | |
|---|---|
| `ops.pl` | every operator, in one place |
| `domains.pl` | reading one domain annotation — `:- domain`, `:- pred` |
| `describe.pl` | reading one description — `:- describe flies(X) :: "@(X) can fly"` |
| `annotations.pl` | a program's annotation table, as a value rather than a store |
| `mangle.pl` | functor names — `m$dual.2.inner$flies$1`, always four fields — and the four kinds of literal, with what each claims |
| `clauses.pl` | taking a clause apart — head, body, the one walk over a body's literals, and a literal's polarity and predicate |
| `abstract.pl` | head abstraction — making head unification explicit |
| `dual.pl` | dualising a clause body — one clause per negated literal |
| `dualise.pl` | dualising a predicate — where the pieces meet |
| `graph.pl` | the call graph, and the odd loops over negation in it |
| `nmr.pl` | denials → the NMR check |
| `compile.pl` | the whole pipeline — terms in, `program{clauses, nmr, unbounded, ...}` out |
| `heads.pl` | head guards — a rule literal's head projection checked before its body is proved, and negated in the duals where its arguments are body-only |
| `emit.pl` | the backend — the logical program made runnable |
| `solve.pl` | the runtime the emitted clauses call |
| `runtime.pl` | what the emitted clauses may call, and all of it |
| `run.pl` | `scasp/3,4` — compile, emit, load into a module, answer |
| `source.pl` | reading an s(CASP) source file, `#include` and all |
| `dyncall.pl` | `scasp/1,2` over the clauses of a Prolog module |
| `render.pl` | an answer made readable — names, and the stores inlined |
| `sld.pl` | the goals a `:- sld p/2.` hands to SLD resolution |
| `herbrand.pl` | the Herbrand store — `dif/2` and `∉` behind one interface |
| `listing.pl` | the compiled program, read back into the language it was written in |
| `json.pl` | an answer as data, for a consumer that is a program |
| `main.pl` | the `sscasp` executable |
| `html.pl` | an answer described once, as HTML |
| `html_text.pl` | the same description read as plain text |
| `lang.pl` | the words a justification is made of |
| `compat.pl` | `library(sscasp/compat)`: run a program written for `library(scasp)` on this library |
| `compat/` | what of the old library's interface such a program reaches: `scasp/1,2` with the old tree, the interned `'-p'` negation, `scasp/html`, `scasp/dyncall`, and stubs for the rest |

and `prolog/sscasp.pl` beside them, the user-facing `library(sscasp)`:
operators, the `?`-prefixed query shortcuts, and what the two above export.

Its own tests live in `test/sscasp/`; the shared suites in `test/` run against it
as well (see "Where the tests are"), and `make check` runs both.

### What the new compiler produces

`compile_program(+Module, +Terms, -Program)` returns a **value**, not a store:

```prolog
program{ clauses:   [...],            % every clause, translated and dualised
         nmr:       nmr{goal, clauses, checks},
         unbounded: [...],
         shown:     [...],                % what #show asks for
         dcc:       [...],                % the dynamic consistency rules
         guard:     guard(...) }          % what guarded_query/3 needs
```

Three things are worth knowing about it:

- **The program is closed.** A predicate called but never defined is false of
  everything and its negation true of everything, and both are written out, so
  running the result cannot raise an existence error and the solver need not
  treat "no clauses" as a case of its own.
- **Domains are predicates.** `:- domain day = [mon, tue]` compiles to two
  facts that go through the same path as the user's clauses, duals included.
- **One goal for the NMR check.** Global constraints, rules on odd loops and
  the consistency of classically negated predicates all become *denials*;
  `nmr.checks` says which is which, and `nmr.goal` is what the solver calls.
  Each sub-check has **arity zero**, so its variables are body-only variables
  that the dual generator already quantifies and bounds by declared domains.

**Two flags turn it off**, and they are upstream's — `scasp_compile_nmr` and
`scasp_compile_olon`, spelled and scoped exactly as `prolog/scasp/nmr_check.pl`
does, so that a caller setting one gets the same thing from either tree and
both trees in one process read one flag. `nmr.pl:compiled_sources/2` filters
the sources; `--no-nmr` drops the check altogether, odd loops included, because
upstream empties the list *after* adding the headless rules to it and both a
global constraint and the `:- -p, p` of a classically negated predicate are
headless rules there. `--no-olon` drops only the rules on an odd loop.

Neither is a mode to run a program in: an answer that violates a constraint is
wrong, and with the check off nothing says so. They are for asking what a
*query* costs on its own, which is how the four event-calculus gaps were
traced to the one check they all wait on. `test/scasp_plunit.pl` has
`with_scasp_flag/3` for setting either inside a test.

**Dynamic consistency checking moves part of the check forward**, and is on
by default: the flag `sscasp_dcc`, `--no-dcc` to turn it off. After Marple
and Gupta (TPLP 14(4-5), 2014) and upstream's `create_dcc_rules/2`. For each
global constraint and each user literal in it, `compile.pl:dcc_rules/3` makes a
rule saying what else would complete the constraint; `emit.pl` adds
`solve.pl:dcc/4` to the wrapper of every literal that triggers one, so a
literal in no constraint pays nothing. When a ground literal is established
and the rest holds of the proof or the open ancestors, the goal fails there.
The NMR check still runs, so it only prunes — a branch that completes a
constraint fails anyway or ends in a model the check rejects — except in a
run whose proof is thrown away: `elsewhere/7` and `for_domain/6`'s generator
turn it off there with the backtrackable `sscasp_no_dcc`.

It was measured before it was made the default: the golden corpus answers the
same with it, and so do the oracle sweeps; it takes 12% off the corpus's time,
most of it on `hamiltonian_scasp` and `bec_light`, and costs nothing beyond
noise anywhere. `dcc/hamilton_c`, which upstream answers seven times faster
with it, is unchanged — this solver's CHS check already constrains literals
apart before a bad pair is completed.

**Not upstream's flag**, and deliberately. Upstream's `scasp_dcc` is off by
default and forces `--forall=prev`, and its DCC has no complement exemption;
one flag with a default of `true` here and `false` there would take its
default from whichever library loaded first. So this is the one place the
two trees spell a flag differently.

`unbounded` lists where a quantifier has no declared domain. It is a scan, not
decision 5's undecidability analysis — the solver's `forall` decides many of
them at run time — but it is exactly the list of places where adding a `:- pred`
annotation would replace a search over the universe with an enumeration.

### The solver

`scasp(+Terms, ?Query, -Model)` compiles, emits, loads into a temporary
module and answers. The query is the literals asked for **followed by** the
NMR check, sharing one proof, so a constraint the query's own answer violates
is caught.

**One entry point takes the options**: `run.pl:scasp_run(+Terms, ?Query,
+Options)`. `scasp/3,4` are it with `model/1` and `tree/1`, and `scasp/2` over a
Prolog module (`dyncall.pl`) collects the terms and hands its options through.
`module(?Module)` loads the program into a module that is **kept** — made up if
unbound, emptied of an earlier program's mangled predicates if given — instead
of a temporary one. It is for debugging: the profiler will not keep track of
predicates in a module that may vanish, and a kept program can be listed
afterwards. The executable has it as `--module=Name`.

**`sscasp_commit_nmr`** (default `true`; `--no-commit-nmr` in the executable
turns it off) commits to the first proof of the NMR check
(`run.pl:committed_nmr/6`). The query is proved first, so every
further proof of the check is the same answer with another model:
`bec_loan` proves its query once and its check 14,843 times a minute, 35
models of one answer, and never reaches its other three answers; committed,
all four in 0.1 s. The commitment is made only when the check left the
query's variables as it found them — the projected CLP(Q) store and the
Herbrand constraints among them entailed by what they were before
(`entails_state/2`, which counts `T ∉ [1, 2]` as nothing new under `T > 2`) —
because two proofs narrowing them differently are two answers. What it loses
is alternative models of one answer (`hamcycle_two`'s second cycle), and that
is the right default for what s(CASP) is for: an answer and a justification
of it, not every answer set containing it. It composes with
`sscasp_distinct_answers`, which only hides the repeats the search still
makes. Its cost is reading the query's state twice per answer, a projection
over the proof's whole CLP(Q) class: `yale_shooting_casp_v2`, seven answers,
0.012 s → 0.026 s; nothing else in the corpus moves.

`emit.pl` is a pass of its own because it is the only place that knows how the
solver carries state: three arguments (`Anc` down, `S0`/`S` up) and a wrapper
per literal, so that the coinductive check happens before a predicate's
clauses are tried. The logical form stays what a listing shows and what the
compiler's tests read.

**Two kinds of module qualification, and only one is written.** A goal
`emit.pl` writes *into a clause body* — `check/7`, `forall/5`, `apart/2`,
`{}/1` — is bare, because `run.pl:load_program/2` does

```prolog
add_import_module(Module, sscasp_runtime, start),
```

before asserting, and `in_temporary_module/3` runs its Setup with the context
module set to the new module, so that is the place to do it.
`prolog/sscasp/runtime.pl` re-exports exactly what the emitted clauses call —
from `solve.pl`, `herbrand.pl` and `{}/1` from `library(clpq)` — so it is the
whole interface between a compiled program and the solver, and a goal the
emitter starts writing that it does not export is an existence error rather
than a name that resolves by accident. It also makes those predicates visible
to the cross referencer, which cannot see calls from asserted clauses. Every
asserted head is mangled, so nothing imported can be shadowed, and taking
`{}/1` from `clpq` says once which one is meant where `library(clpr)` defines
another.

A goal a quantifier *holds as a term* keeps its `M:`, because nothing resolves
it until the runtime calls it and `meta_predicate` cannot help: it applies
when a clause is compiled, and these are `assertz/1`-ed. So
`forall(_, Module:Goal, _, _, _)` has exactly one qualifier in it, and
`test_emit.pl` is written to show which.

**A `dif/2` is destroyed by reading it.** `dif:attribute_goals//1` calls
`del_attr/2` on each or-node as it reports it, so that a node shared by
several variables is not reported twice; `copy_term/3` and the toplevel undo
that and a direct call does not. `herbrand.pl:read_store/2` is the one
reader, and must be called inside `\+ \+`; `solve.pl:disequalities/4` does,
and carries positions and ground values out with `nb_setarg/3`. Anything else reaching for a variable's constraints has the
same problem.

**A loop is a variant, or an instance with no negation between.** Merely
*unifying* with an ancestor is not a loop and reading it as one is wrong:
`initiates(E, F, T)` unifies with an ancestor `initiates(overflow, spilling,
15)` and is a more general question, which the ancestor says nothing about.
Taking it for a loop binds E, F and T to that instance and then fails it as a
positive loop — which is why `holdsAt(spilling, T)` had no proof in *any* of
the event calculus programs. So `ancestor/4` walks past an ancestor that only
unifies, and stops at one that is a variant of the goal, or that subsumes it
with no negation on the path — the second being exactly "an instance of
something already being proved from itself", hence unfounded. Upstream's
`prolog/scasp/solve.pl:check_parents/3` reads it the same way.

**And a variant is a variant of the *question*, not of the store.** `=@=`
and `subsumes_term/2` take attributes into account: `f(X) =@= f(_)` is false
once X carries a constraint, and two variables carrying *different* ones are
never variants. So a recursion whose steps constrain their own variable never
looks like a loop — which is every recursion over time in an event-calculus
program. `bec_loan` descends
`not holdsAt(requested_on(the_borrower, 1000, 2), T)` →
`not initiates(ends(1), is_liable_to_litigation(the_lender), T)` → the same
pair one step earlier, and the ancestor chain reaches **6,677** frames of it.

`solve.pl:same_question/2` compares the bare terms and then asks whether
unifying the ancestor into the goal would add anything to either store: no
disequality (`herbrand_instance/2`, by counting) and no dense constraint the
goal's store does not already entail (`dense_instance/2`). The instance rule,
`instance_of/2`, asks the same of `subsumes_term/2` on the bare terms. Simply
ignoring the stores is not the shortcut it looks like — `ancestor/4` unifies
the goal with the ancestor it matched, so a goal that is *less* constrained is
narrowed to the ancestor's region and then failed as a loop, and every answer
outside that region is lost: `p(1). p(X) :- X #> 3, q(X). q(_) :- p(_).`
lost `X > 3` that way until both rules compared the stores. Upstream states
the same rule as `prolog/scasp/clp/clpq.pl:entail_terms/2`.

Order matters for cost, because this is asked once per ancestor per goal and
the chain is long: functors first, then an ordinary `=@=`, and only then the
comparison that has to take the store apart. `within/2` — `inf/2` and `sup/2`,
simplex over the store the two already share — rejects most candidates before
`dense_instance/2` reaches for a projection.

The Herbrand half is the one that decides whether a loop is seen at all: on
`bec_loan`, of 1,378 ancestors that are the goal's question with the stores
set aside, only **2** are still variants with the dense store put back, so
what separates them is the disequalities `consistent/3` posts. It is strict
by design — it holds where they accumulate down one branch and not where two
goals were each constrained against literals the other never met, and a
recursion of the second kind is still not detected. That is the safe
direction: the verdict on a positive zero-transition cycle is to **fail** the
goal, so accepting one that does not hold would lose answers.

**Coinduction is four rules, not six.** Compiling to predicates makes a goal's
polarity static (it is the `Kind` field), so a frame records it and the number
of negations on a path is the number of **polarity transitions** along it —
which is the number of negative edges on the dependency path. Counting `dual`
frames undercounts: dualising `q :- not p` turns `not q`'s body into a
positive call to `p`.

| | |
|---|---|
| the goal's negation unifies with an ancestor or a proved literal | fail |
| an ancestor unifies with the goal, **0** transitions | by polarity — below |
| an ancestor unifies with the goal, **even, non-zero** | succeed coinductively |
| the goal is ground and already proved, **from no assumption** | succeed |

There is no odd case: two frames of the same polarity have an even number of
transitions between them by construction, so an odd loop reaches the goal's
*negation* instead.

A zero-transition loop **fails on `+` and succeeds on `-`**, and the
difference is not a special case. `c :- c` gives `c` no support, so `c` fails;
dualising it gives `not c :- not c`, and `c` being unfounded is exactly why
`not c` holds. Getting this wrong makes `not c` unprovable for every unfounded
`c`, which the Gelfond–Lifschitz oracle catches immediately —
`[a, b, (c :- c), (:- not c, not b)]` has the answer set `{a, b}` and nothing
can reach it. `test_solve.pl` pins both readings and that program.

### Constraints

Two stores, because two stores is what s(CASP) is (decision 4): `herbrand.pl`
for Herbrand disunification and `library(clpq)` for the dense order, both
reached through `emit.pl`'s `builtin/2`. A constraint is not a literal — it is not in
the model and not a step in the justification. It says what the answer *is*,
and it says it on the variable:

```prolog
?- scasp([(p(X) :- X #> 3, X #< 10)], not p(V), M), copy_term(V,_,A).
A = [{V =< 3}] ;
A = [{V >= 10}].          % the complement of an interval is two regions
```

Negation over both is constructive and needs nothing special: `dual.pl` pairs
`#<` with `#>=` and so on, so the dual of a comparison is a comparison. The
model **shares variables with the query** — it is collected by walking, not
with `findall/3`, which would copy the constraints away.

The **CHS check constrains rather than tests**, which is the same move. Asking
`not dom(X)` for an unbound X where `dom(1)` is proved is not a contradiction —
it is one only at `X = 1` — so `consistent/3` posts `dif(Goal, Neg)` between
literals that could be made to contradict, instead of refusing. Refusing lost
every answer that needed the other values; the oracle went from 100
disagreements to none on the first-order fragment when this changed.

#### The Herbrand store has two backings

`herbrand.pl` holds it, and what it holds is in one of two places:

| | holds | |
|---|---|---|
| `dif/2` | a disequality between whole terms | `dif(f(A,B), f(a,b))` is `A \= a` *or* `B \= b`, and commits to neither |
| `∉` | the ground values one variable may not take | `X ∉ [a, b]` |

They are **not** interchangeable and the direction matters: `X ∉ [a,b]` is
always `dif(X,a), dif(X,b)`, but a `dif/2` becomes a prohibited value only
where the two terms disagree in *one* place, that place being a variable
against a ground term. Anything else is a disjunction and no set of values
per variable can hold one. (The old implementation resolves the disjunction
by **backtracking** — `clp/disequality.pl`'s `.\=.` picks one binding to
negate and leaves an alternative — which is a different and more expensive
semantics than `dif`'s single or-node.)

So `apart/2` posts into whichever can say it and everything else goes
through the same interface: `read_store/2` reports both as `apart(A, B)`
terms, `excluded_values/2` gives the ground values from both,
`node_count/2` sizes both. **`solve.pl` names neither**, which is the point
— a reader is asking what the answer says, not which attribute holds it.

Three reasons, and the first is what prompted it:

- **A set is idempotent.** `consistent/3` posts one disequality per proved
  literal a goal might contradict and several collapse to the same thing
  once the answer has bound what they differ in, so a variable carried
  `dif(X, john)` twice and the toplevel said so twice.
- **Reading a `dif/2` destroys it** and a `∉` set is one variable's, so
  there is nothing shared and nothing to delete.
- **`X ∉ [a, b]` is how an answer reads** — the old implementation's
  spelling, and the one its `.pass` data is written in.

Every post must go through `apart/2` or `not_in/2`. A bare `dif(X, ground)`
anywhere else puts a prohibited value where the set already has one, and the
duplication is back by another door.

**One case no hook can see**, and `normalise/1` is the answer to it. A
disequality between two *variables* is `dif/2`'s, correctly — neither side
is ground. Once one of them is bound there is a value to prohibit, and
`library(dif)` simplifies its or-nodes internally without telling anyone.
So when an answer is complete, `run.pl` rebuilds the store over the
variables the answer is about: `dif:attribute_goals//1` deletes each node
as it reports it — the one time that is useful — and what comes back is
posted again through `apart/2`. It costs nothing measurable and it is why

    ?- ? -bird(X).
    X ∉ [john,tweety]

says it once rather than as a set and two or-nodes saying the same.

**`∉` is also a language feature**: `p(X) :- X ∉ [a, b]` compiles to the
store, and `dual.pl` pairs it with `∈`, which is a *generator* — the
negation of "not one of these" is "one of these", and a dual has to be
something to run rather than something to fail. `∈` is ours; the old
implementation has only `∉`.

### The quantifier

`forall/5` is one recursion for all of decision 5's decidable cases: solve the
goal once, see from what the quantified variables are left constrained to
**which part** of the domain that answer covered, then cover the rest the same
way. The cases are not clauses any more — they fall out of what the store says
when `snapshot/4` is asked (`residue/3`):

| the answer left them alone | nothing is left — case 2 |
| it left disequalities | the prohibited values, a finite set — case 3 |
| it left a CLP(Q) region | the negation of each constraint — case 4 |

**All of a clause's body-only variables are covered as one tuple**, not one
nested quantifier each. `forall([B, C], Inner)` says what `forall(B, forall(C,
Inner))` says and costs differently: covering a nest means covering C's domain
once per region of B's, so the work is the *product* of the coverings, where
covering the pair reads one answer's constraints on both at once and the
pieces add up. `bec_tap_02` made 83,593 coverings of a three-variable nest,
and none of the five event calculus programs finished; with the tuple they
answer in milliseconds. `solve_c_forall/8` collects its nest into one
`c_forall(Vars, Goal)` for the same reason. A variable with a *declared*
domain keeps a `for_domain/3` of its own, outside: each value is a search
rather than a region to complement.

The tuple also decides what an answer that **binds** may do. Per variable it
could be rejected outright — a point covers nothing of an infinite domain — and
the goal asked for another; a tuple answer with one component bound is the
ordinary case, and refusing it costs `queens.pl` and `family.pl` every answer
they have. So a ground binding is accepted and its complement is the
disequality, which is `find_duals/4` upstream. It is not free: `queens.pl`
goes from 0.063 s to 0.34 s.

#### One rule, three places

Everything that made the quantifier loop was the same mistake: **negating a
constraint that relates the quantified variable to something the answer
itself invented**. Such a constraint says nothing about the variable — its
true projection is `true` — so its negation narrows nothing, the next answer
invents another, and the recursion adds a level per round without closing.

It has to be got right in four places, because four things can produce one:

| | |
|---|---|
| the linear store | `dump/3`, never `copy_term/3` — the latter reports the store in internal form with **slack variables** |
| CLP(Q) disequalities | `dump/3` cannot eliminate a variable from `A + B =\= c`, so drop constraints whose variables are not exactly the quantified one |
| `dif/2`, per variable | the excluded value must be **ground**; `dif(T, X)` for free X excludes one unknown point |
| `dif/2`, jointly | `escapable/3` — a disequality that differs at a position holding an invented variable can be satisfied by moving *it*, so it says nothing about the quantified ones at all |

The last is the same rule as the second, arrived at later: `consistent/3`
posts a disequality between **whole literals**, so `dif(f(A, C), f(B,
tapOff))` with C a variable of the literal compared against is the ordinary
shape rather than a corner case — 27,767 of the 49,818 disequalities
`bec_tap_01` reads. "Invented" is meant strictly: a variable in the dense
store may be pinned to a point, so those keep the over-approximation.

**A constraint said twice says nothing the second time.** `disequalities/4`
**sorts**, and that is not tidiness. `consistent/3` posts one disequality per
proved literal a goal might contradict; several collapse to the same thing
once the answer has bound what they differ in, and `bec_tap_01` reaches a
quantifier carrying `dif(T, 5)` eight times from eight different pairs of
literals. The disjoint decomposition then turns each duplicate into a piece
of complement that contradicts the piece before it — ten pieces where three
are the answer. The old implementation never had this: `loop_term/2` posts
one *per-argument* exclusion into the `∉` store, which is idempotent per
variable. Together with `escapable/3` this is 7.8 complement pieces per
covering down to 1.43.

Each one on its own took an event-calculus program from looping to
terminating, and each looked like slowness until the narrowing was printed.
`bec_tap_01` carried a region of 155 constraints, every one of the form
`Slot + _Fresh =\= 15`.

The complement is also decomposed **disjointly** — `(¬C₁) ∨ (C₁ ∧ ¬C₂) ∨ …`
rather than a bare `¬Cᵢ` each time, which is what `solve.pl:make_duals/2`
does upstream. Both are correct; this one does not re-cover ground the answer
has already ruled out.

**One covering, and then somewhere else.** A covering is a *proof* that the
goal holds everywhere, and the goal holding everywhere is one fact however
many ways there are to show it — so a second covering of the same ground is a
second copy of the same answer. `bec_light` enumerating its answers gave
**500 answers carrying one model** and never finished. So the covering is
committed to, and the alternatives come from partitioning the goal's *other*
variables against what it fixed them to. That is
`solve.pl:solve_other_forall/11`'s shape and **decision 5c reversed**: the
decision was right that cutting alone loses answers — it leaves `family.pl`'s
NMR check and `bec_tap_01`'s query with none — and wrong that the cut was
therefore not transferable.

**The complement's pieces are proved in one proof** (2026-10-01), each from
the proof the ones before it left, as upstream's `solve_var_forall_/11`
threads its stack. The paragraph below describes what came before: the
pieces were covered in a run `fail` undid, which was fast and **unsound** — a
piece rests on the coinductive hypothesis, and with nothing kept the next
piece could rest on the opposite choice of an even loop, so
`:- dom(Y), not ok(Y)` with `ok(1) :- a`, `ok(2) :- b` passed for a program
with no answer set (`test_differential.pl:the_values_of_a_derived_domain_hold_together`).
Keeping the pieces brings back the store growth it describes:
`bec_tap_01` went from 0.05 s to 0.6 s, `bec_tap_03` from 0.11 s to 1.3 s,
after the loop check stopped walking that store with `term_attvars/2`
(`solve.pl:has_store/1`), which was half of it. Winning the rest back
without losing the proofs is open.

**It used to leave nothing behind but its narrowing.** A branch
contributes no proof, and it must contribute no *constraint store* either:
it invents variables, CLP(Q) keeps every variable a constraint ever reached
in one class, and nothing collects the ones that go out of scope, so the
class grows by everything every branch ever touched and `dump/3` — Fourier–
Motzkin over that class — is asked once per covering. `solve.pl:covered/7`
runs the whole complement inside a run that `fail` undoes and carries **one**
snapshot of the caller's variables back out over fresh plain names. That is
what made the event-calculus family answer; the details, and the two things
that did not work, are under the corpus below.

Three things it needs. The commitment is to the covering of the whole
**nest** — a quantifier's goal is a dual that may open another — and the
outermost's search runs through them all; committing at each level costs
9.2 s of `make check` against 4.8 s for no answer either way. (Re-measured
after `covered/7`: `once/1` at every nested `call_in/5` now keeps the whole
suite green and the corpus's answer counts, and buys 0.395 s → 0.288 s on
`bec_light` and nothing anywhere else. It is still a strengthening of what
the quantifier means, bought for one program, so it is not taken — but the
old claim that it leaves `family.pl`'s NMR check with no answer no longer
holds, and a future change that wants it should re-measure rather than
believe this paragraph.) The snapshot is
**projected** with `dump/3`, for the reason below — an unprojected constraint
would invent a variable again on every round. And nothing attributed leaves
the discarded run: the covering is run once more, `fail` undoes it and
`nb_setarg/3` carries out a description over fresh plain names, so no
constraint store is copied. That second run is only paid when a second answer
is asked for.

An answer that binds a quantified variable to a **non-ground** term is
rejected and the goal asked for another (`solve.pl:coverable/1`): its
complement, "not of this shape", is nothing a store can say. A *ground*
binding is accepted, its complement being the disequality (the tuple
paragraph above). That was not always so: per variable, every binding was
rejected, since one point covers nothing of an infinite domain, and
enumerating points instead had taken 37 s on a five-clause program the old
implementation answers in 9 ms; with the tuple, rejecting them cost
`queens.pl` and `family.pl` every answer.

Where the domain really is finite it has to be declared, and `for_domain/6`
enumerates it — including a range, which compiles to a CLP(Q) region and is
covered rather than listed. That is case 6 against case 1, and the difference
is one annotation.

**A signature is a guard, written where the program would write it by
hand.** `:- pred q(d)` puts `d(X)` into every clause of `q`, *before* the
head unification (`compile.pl:signature_guarded/3`), and in front of every
`not q(T)` a body or query writes (`use_site_guarded/3`, `guarded_query/3`);
the duals are then derived like any other. So outside the domain `q(v)` is
false and so is a written `not q(v)`, and `gap :- not q(W)` asks only about
`d` — with the clause guard alone it held by a `W` outside. A quantifier is
bounded only by a **positive** literal — a signature's, an inferred domain's,
or the domain's own, which is what the use-site guard is. Guarding the dual
alone, as the first version did, made `q(3)` hold while `not q(3)` failed and
proved a program's `outside` and `not outside` together. `doc/design-next.md`
open question 3 records the choice and the two readings it rejected.

**`scasp_domain_error`** (`--domain-error`, off by default) makes a ground
value outside the domain an error rather than a failure: each guard gets a
`'$domain_check'/3` in front (`solve.pl:domain_check/3`), which is `true` or
raises, so its negation is `fail` and `dual.pl:no_dual/1` gives it no dual
clause — but it heads every other branch, so `not q(30)` raises too. The
listing prints it as `must_be(d, X)`.

**Or it is inferred.** A positive body literal of a predicate defined by ground
facts alone (`compile.pl:fact_predicates/2`) bounds a variable to the values
those facts give: for any other the literal is false, so the body is, and its
negation holds. `dualise.pl:var_domain/4` uses that where nothing is declared,
with the literal itself, other arguments fresh (`edge(_, W)`), as the
generator. Only a *positive* literal: outside `q`'s facts `not q(W)` holds. It
took `hamiltonian_scasp` from 0.6 s to 0.03 s, and `bec_light` from 0.15 s to
0.01 s, and it made `not wrap(T)` for `wrap(f(X)) :- item(X)` answer
`T ∉ [f(a)]` where it used to answer `dif(T, f(_))`, which admits `f(a)`.

**A domain's values are proved in one proof.** `for_domain/6` covers each
value from the proof the ones before it left, and keeps the result, as a
`forall` complement now does too. It used to prove every value
from the caller's proof and keep none, which was unsound: `ok(1)` could rest
on `a` and `ok(2)` on `b` of an even loop between them, so
`:- d(X), not ok(X)` passed for a program with no answer set, and
`hamcycle_two` answered with one edge and no cycle. Fact unfolding hid it
wherever it compiled the quantifier away. It also used to **commit** to its
covering when the goal's other variables were ground, on the argument that a
second covering proves the same fact; with the proofs kept a second covering
is a second model, so it no longer does. `test_solve.pl:solve_domains` pins
both.

One thing it needed: duplicate values, which `edge(_, W)` gives, are dropped by `==` rather than
sorted: a restriction carries its constraints, and standard order over those
hung. It also exposed that **dynamic consistency checking must not run in a
`for_domain` generator**, whose proof `findall/3` throws away: it pruned
`d(2)` from a domain because `d(2)` completed a constraint, and the check then
covered too little.

### Using it from Prolog

`library(sscasp)` is the way in: load it, write the program as ordinary
Prolog clauses, and ask.

```prolog
:- use_module(library(sscasp)).

bird(tweety).
flies(X) :- bird(X), not penguin(X).

?- ? flies(X).
Model:
    bird(tweety)
    flies(tweety)
    not penguin(tweety)
X = tweety.
```

**A program written for `library(scasp)`** runs unedited after
`:- use_module(library(sscasp/compat))`, loaded before it. From then on
its imports of `library(scasp)` and `library(scasp/Sub)` are rewritten to
this library and `compat/`, clauses using the old interned negation
`'-p'(X)` become `-p(X)`, a `:- table` of the old Prolog bridge tables
`sscasp_sld:sld_proof/3`, and `::` and `pred` are operators. A bare
import gets this library's `scasp/2` (the new justification); an import
list naming `scasp/2` gets `compat/`'s, which returns the old
`query-Forest` with `o_nmr_check`. The rewriting is `user:term_expansion/2`
and leaves files inside this checkout alone, since the old implementation
and the tests import `library(scasp)` meaning that; `test_compat.pl`
writes its programs to a temporary directory for that reason, and
`test_all.pl` loads it last.

`dyncall.pl` collects the clauses **reachable from the query** plus the
global constraints whose call tree overlaps them — not the whole module,
which is what makes it usable on a program of any size, and why a
contradiction the query cannot reach does not constrain the answer.
`collect(Goal)` widens the collection without widening the query.

Three things that are not obvious and cost an afternoon each:

- **A query is a `:` meta-argument, never a `0`.** A query is a term to
  take apart, not a goal to call, and with `0` the compiler expands the
  argument where it is written. That used also to intern `-p(X)` into a
  predicate named `'-p'` whenever `library(scasp)` was loaded alongside;
  its `goal_expansion/2` is guarded now, but the reason for `:` is the
  first one and stands on its own. `scasp/dyncall.pl` uses `:` too.
- **`p` and `-p` are one predicate.** The compiler writes `:- p(X), -p(X)`
  for every classically negated predicate, so reaching one reaches the
  other and reaching it drags in whatever *it* calls —
  `dyncall.pl:add_counterpart/3`, inside the closure. Left out, this was
  unsound rather than untidy: `q. p :- q. -p :- q.` has no answer set, and
  `?- scasp(p).` said yes, because nothing reachable from `p` mentions
  `-p` and the consistency check was therefore never generated. It is not
  "collect the whole program" creeping back — a global constraint the
  query cannot reach still does not constrain it; what this says is that
  the *rest of a predicate* is not somewhere else.
- **A declaration belongs with the clauses, not with the asker.** A
  global constraint and a `scasp_directive/1` are kept in the module
  they were written in, and the query often runs somewhere else —
  plunit gives every test a module of its own, which inherits the
  predicates and holds none of the declarations. So they are gathered
  from every module that contributes a clause, plus the query module's
  inheritance chain.
- **`false :- Body` is expanded away at load time**, because `:- Body` is
  a Prolog directive and `false/0` cannot be given clauses. Both
  libraries install a `user:term_expansion/2` for it and the first one
  declared wins, so `global_constraint/2` reads what *either* leaves
  behind — ours a clause of `'scasp$constraint'/1`, the old one a clause
  of `-/0`. Both are clauses **in the module**, and that is not a detail:
  ours used to be a fact in a table keyed by the module's *name*, which
  outlived the program. A constraint deleted from the source survived a
  reload, and `in_temporary_module/3` — which names its modules from the
  random state — gave one program the constraints of an earlier one run
  under the same seed. Compiled, it is wiped with the file's other
  clauses on reload and destroyed with the module; asserted at run time,
  it survives a reload like any asserted clause.
  A `scasp_directive/1` — and so `:- sld`, `:- describe`, `:- pred`,
  `:- domain` — is kept the same way, in `'scasp$directive'/1`; being a
  goal rather than a term to expand, it gets there through
  `compile_aux_clauses/1` while its file loads, which the reload wipes
  in the same way. `test_api.pl:api_constraint_lifetime` pins both.

#### The listing

`scasp_show(Query, code)` writes the program the query would run: the
question and the user predicates, with `duals(true)` and
`constraints(true)` for what the compiler made of them, `source(true)`
for the terms as collected (the only section that shows a `:- describe`
or a `:- domain`) and `compiled(true)` for the clauses the solver
actually runs. The defaults are the old implementation's — most of the
time the question is about the program rather than about the compiler.

`listing.pl` reads the compiled program back into the language it was
written in, and the one thing it does beyond demangling is **inline the
helpers as a disjunction**:

    not flies(A) :-
        (   not bird(A)
        ;   bird(A),
            ab(A)
        ).

`dualise.pl` writes that as `not flies` calling a generated helper per
clause, each with one clause per way its body can fail. Printed as it
stands it is a hierarchy of `scasp$dual.1$flies$1` names that the reader
has to put back together; printed as a disjunction it is the dual, which
is the one thing a reader of a dual is asking about. Disjunction and not
separate clauses is the whole trick: a conjunction of two three-clause
helpers is one body with two `;` groups, where splitting it into clauses
would be nine of them. A helper is inlined only where it is called
**once**, which is how they are generated, and that is checked rather
than assumed.

What is left keeps a name. A sub-check prints as `check(1)` under a
comment saying what it came from — a global constraint, the consistency
of a classically negated predicate, an odd loop — which `nmr.checks`
records and nothing else can reconstruct.

**A listing must not fail.** It is what a reader falls back on when
something is wrong, so `wrap/3` has a catch-all: a kind nobody thought of
prints as `generated(Kind, Goal)` rather than taking the listing with it.
`test_listing.pl:listing_corpus` runs every corpus program through it and
checks no mangled name survives — which is what caught the four kinds an
`#abducible` compiles to, and a `not findall(...)` whose inner goal was
being left alone.

It is a listing and not a program: `check(1)` is nothing anybody can call
and a body may hold a `forall/2` only the solver understands. The old
implementation's `--code` *is* loadable (`--compiled`), and gets there by
printing the generated names as they are.

`portray_clause/3` does all of it — the `;` shape, the line width, the
variable names, and, given `module(sscasp_ops)`, this language's
operators, without which `not p(X)` is `not(p(X))`.

**Two things had to be fixed in SWI-Prolog for that to be true**, and
both are in `/home/jan/src/swipl-devel` rather than in this pack:

- `listing/2` dropped its options before `portray_clause/3` saw them, so
  `module/1` had no effect. It now forwards everything that is not one of
  its own (`variable_names/1` means something different to it), which is
  what its `predicate_options` declaration already claimed with
  `pass_to(portray_clause/3, 3)`.
- The writer wrote `not -p(X)` as `not-p(X)`. That reads back correctly —
  SWI's reader always prefers the prefix operator, so `not-x` *is*
  `not(-(x))` — and it looks like an infix `-`. `needSpace()` in
  `pl-write.c` already had the machinery: `C_PREFIX_SIGN` puts a space
  after `-` as a prefix operator when a symbol follows, and the same now
  holds for `C_PREFIX_OP` generally. Nothing changes for a *symbolic*
  prefix operator, where the generic symbol-symbol rule already spaced
  it.

That is why this file has no layout code. `portray/1` is not the tool for
it: `portray_clause/3` produces output that is still **machine**
readable, and `portray/1` is for output that is not.

#### A name the program does not own

A goal the compiler gives a meaning of **its own** — `forall/2`,
`findall/3`, `not/1`, `for_domain/3`, `sld/2`, and everything
`emit.pl:builtin/2` covers — is intercepted before anything looks for
clauses for it. So a module that *defines* one and calls it meaning its
own has written something that can never run, and not loudly:
`test/le_programs/subset-scasp.pl` writes

```prolog
forall(A, B) :- not(A, not(B)).
```

and calls `forall(belongs_to(C, A), belongs_to(C, B))` meaning it. Read as
the quantifier — which is `forall(Vars, Goal)`, with a *literal* where the
variables go — the whole program answered **nothing**, which looks exactly
like a scenario with no solution. Its call tree was not followed either,
so the `not/2` it also calls, which does not exist, went unmentioned.

`dyncall.pl:reinterpreted/2` and `redefined/2` refuse it, with the term
`library(scasp)` refuses it with — `permission_error(define,
scasp_control_construct, PI)` — so a caller catching one catches both. The
check is at the **call**: a definition nothing calls is misread by nobody,
and finding one would mean scanning the module rather than the query's call
tree. `-/1` is deliberately not in the table, since every `-p(X)` in a
program is a clause of it.

#### A program that changes

`scasp_dynamic/1`, `scasp_assert/1,2`, `scasp_retract/1`,
`scasp_retractall/1` and `scasp_abolish/1` mean what they mean in
`library(scasp)`. `scasp/2` reads the module's clauses at query time, so
nothing has to be invalidated — `query_callees/2` recomputes and the next
answer is about what is there now. What they add over `assertz/1` is the
two things an s(CASP) clause can be that a Prolog clause cannot.

**A classically negated head needs no translation here**, and that is the
difference from `library(scasp)` when porting. The old implementation
interns `-p(X)` into a predicate literally named `'-p'`, so its
`scasp_assert/1` renames the functor; this tree leaves the term alone —
`-p(X)` is a clause of `-/1` carrying `p(X)`, which is what the compiler
writes for a file and what `callee_head/2` looks for.

The price is that `-/1` is **one predicate per module**, holding every
negated head in it.  What `scasp_assert/1` adds to a module with dynamic
predicates goes into `'scasp$neg'/1` instead, declared by `scasp_dynamic
p/1` along with `p/1`, and `callee_clause/3` reads both: a `thread_local`
`-/1` would make the file's static `-q :- Body` rules invisible to every
thread but the loading one.  Its scope is settled by the first declaration
to reach it. The old one gives each its own `'-p'/1` and can scope them
separately.

**A global constraint cannot be asserted at all.** `false/0` is a
built-in, so `scasp_assert(M:(false :- Body))` — and the `:- Body`
spelling — asserts it into `'scasp$constraint'/1` through
`scasp_constraint/2`, the predicate the load-time expansion of the same
term compiles it into.

`scasp_assert/2`'s position argument is accepted and ignored: the old
implementation records it so a justification can point back at the text,
and this solver keeps no source references. Taking it means a caller
written against `library(scasp)` ports without losing an argument.

**`:- scasp_dynamic` is one of the directives both libraries claim** — see
"Each library reads only its own files" above. Left to the other library's
expansion, a file of ours got `'-p'/1` declared dynamic: the interned
spelling, which this tree never puts a clause in, while `-/1`, which holds
them, was left undeclared. Found by `test_api.pl:api_dynamic` failing, and
pinned there and in `api_expansion`.

#### `:- sld p/2.`

A predicate proved by **SLD resolution** — the way Prolog proves things,
left to right, first clause first — rather than the way the rest of the
program is. It is not dualised, not closed, and its clauses are not
collected; `compile.pl:sld_bridges/3` writes the two clauses that hand
the goal over instead, in the *translated* language, so nothing above has
to know it is special.

The declaration names the **resolution**, not the language, and that is
deliberate twice over. It leaves room — a goal proved by tabled
resolution, or handed to a constraint solver, would want its own word —
and it avoids the obvious name: a prefix operator called `prolog` makes
`prolog:message//1` a syntax error in every file that can see it, which
is a wart `library(scasp)` has today.

One declaration may list several, as `:- dynamic` does: `:- sld p/2,
q/1.`, and `:- sld (p/2, q/1) as opaque.` — `as` binds tighter than `,`,
so without the parentheses it applies to the last one only
(`compile.pl:sld_declarations/2`).

`:- sld p/2 as opaque.` runs it instead of interpreting it. The
difference is only in the justification: `justified` shows the SLD
derivation beneath the literal, `opaque` shows the call and no more.
Foreign and meta predicates are run whatever was declared, because there
is nothing to interpret.

Negating one **constrains rather than tests**, like everything else here.
`\+ p(X)` for an unbound X asks whether *some* X works and says nothing
about the others, so the solutions are collected and the variable kept
away from each:

```prolog
:- sld small/1.
small(1).  small(2).

?- scasp(not small(V), M).      % V is anything but 1 or 2
```

A ground one is simply run and refused. A solution that leaves a variable
free holds for every value there, so that position excludes nothing, and
one binding nothing at all makes the negation fail; a solution that fixes a
*shape* (`p(f(_))`) would exclude every instance of it, which no
disequality can say, and is refused with
`representation_error(scasp_universal_disequality)`.

An SSU rule reaches the interpreter as `(Guard, !, Body)` — `clause/2`
drops the `=>` — so `sld.pl:ssu_guard/3` splits the guard off at the
first top-level cut. A callee nobody defines raises, under `justified` as
under `opaque`.

**A declaration is also how a program says a name is its own.** `maybe/0`
is a predicate of `library(random)`, so a program declaring `maybe`
abducible and calling it collected *that* — one clause, `random(2) =:= 0`
— the moment anything in the process had loaded the library, and the
query still answered with a coin toss in the model. `declared/2` stops
the walk at anything a directive has already spoken for. The general
hazard remains and `library(scasp)` has it too: a predicate whose name a
loaded library exports gets the library's clauses, because that is what
the name resolves to.

`render.pl` makes an answer readable, which means reading the stores,
naming the variables and putting the two back together: `not p(A |
{A=<3})` where the term alone says `not p(_123)`. One `copy_term/3` for the
whole answer — never a direct `dif:attribute_goals//1`, and never
`findall/3`, which copies the constraint away from the variable it is
about. Model and justification are made readable **together**, or a
variable they share is named twice and the two names mean different
things.

### Saying a literal in words

    :- describe flies(X) :: "@(X) can fly".
    :- describe not flies(X) :: "@(X) cannot fly".
    :- describe -flies(X)   :: "@(X) is known not to fly".

`describe.pl` reads one; `annotations.pl` holds them and answers
`described(+Annotations, +Literal, -Text)`, most specific first — a
description written *later* is the more particular case, so a program may
describe `flies(tweety)` after `flies(X)` and have the individual win. A
description has no effect on what is proved, and `test_describe.pl` pins
that too.

Three things it turns on:

- **The name is the point.** `@(X)` means the variable the directive calls
  `X`, and a name is the one thing a read term does not carry. So the two
  directives that want one are read with `variable_names/1` and handed the
  bindings — `source.pl:named_term/3` for an s(CASP) file,
  `dyncall.pl:describe/1` and `pred/1` for a Prolog one. A program given as
  a **term list** has no reader to ask, and there the variable says its own
  name: `describe(flies('$VAR'('X')) :: "@(X) can fly")`.
- **The Prolog reader calls the variable a singleton**, since it occurs once
  in the directive and the rest of its occurrences are inside a string.
  Underscore it — `:- describe flies(_X) :: "@(X) can fly"` — and note that
  the leading underscore is ignored on either side, so `@(X)` still finds it.
- **The literal is one of four kinds**, in `mangle.pl`'s vocabulary:
  `plain`, `dual`, `neg`, `dualneg`. A renderer looking at a proof node asks
  for the description of exactly the literal it has, so `not flies(X)` reads
  its own description and not `flies(X)`'s.

**Three spellings, one thing.** `#pred p(X) :: 'text'` is what the existing
files say and `source.pl` turns it into a `:- describe`; `:- describe` is
the spelling going forward (decision 6a); and `:- pred p(X) :: "..."` is
deprecated and **warns**, because in this implementation `:- pred p(dom)` is
a *signature* and a signature changes what is proved. The warning is the
reader's — `dyncall.pl:pred/1` and `source.pl:named_term/3` — and not
`annotation/2`'s, which is asked once per compile and would repeat it once
per query.

`--human`, `scasp_human/1,2` and the `??` shortcuts consume it, through
`html.pl` (see "Saying an answer" below).

**Each library reads only its own files.** The two trees read the same
spellings and represent them differently — `-p(X)`, `:- pred`, `#pred`,
`:- scasp_dynamic`, `false :- Body` — and both install their expansions in
`user`, where they see *every* file in the process. Unguarded, what a file
meant by one of them depended on which library had loaded first:

```prolog
:- use_module(library(sscasp)).
alive(tweety).
-dead(X) :- alive(X).        % a clause of -/1 ... or of '-dead'/1
```

With `library(scasp)` also loaded, that head was interned to
`'-dead'/1`, `-/1` was left empty, and `?- scasp(-dead(X))` raised
`existence_error(scasp_predicate, dead/1)` on a program that is perfectly
well formed.

So each asks whether the file being compiled imported **it**:
`sscasp.pl:ours/1` and `scasp/dyncall.pl:ours/0`, both
`prolog_load_context(module, M)` then
`predicate_property(M:scasp(_,_), imported_from(...))`. `scasp/2` is the
marker rather than any one directive, because it is what a file imports the
library *for*, and the property survives the re-export through the facade.

The two never both fire, so ours is a single `user:term_expansion/2`
clause: `sscasp.pl:claimed/3` is a table of the spellings and what
each expands to, and the expansion is that table plus `ours/1`.
`test_api.pl:api_expansion` pins where each spelling lands, and four of its
five tests were confirmed to fail with the guard removed.

**A directive travels with the program it is about.** `scasp/2` is
relevance-scoped and a module's directives are not: collecting
`:- pred shading(shade)` for a query about birds raised
`existence_error(procedure, shading/1)` on a program that is perfectly well
formed, because a signature is *checked* against the definitions
(`annotations.pl:check_signature_arity/4`). So
`dyncall.pl:about_collected/3` keeps a signature or a description only where
its subject is among the callees or is declared by another directive. What
names no predicate — a `domain`, a `#show` — travels with everything; a
`domain` declares a predicate rather than describing one and has no
definition to be checked against.

#### `#abducible` and the consistency check

`#abducible p` is four rules and no special case in the solver:

    p :- abducible(p).            -p :- abducible(-p).
    abducible(p)  :- not abducible(-p).
    abducible(-p) :- not abducible(p).

The last two are an even loop, and an even loop is a free choice.

**A predicate that is only an abducible gets no `:- p, -p` sub-check**,
because the check is *entailed*: those four rules have the answer sets
`{p}` and `{-p}` and never both. Generating it anyway costs — it is a
choice the solver has to make, once per query that drags the declaration
in, and the answers double. `test_api.pl` has one `#abducible` and every
query in the file answered twice.

**Both halves have to be abducible-only**, and testing one was a
soundness bug:

    #abducible p.  q.  p :- q.

has the single answer set `{p, q}` — assuming `-p` gives a model holding
`p` too, which is not one — so `-p` is not entailed. The compiler asked
whether `-p`'s clauses were all abducible, which they were, and left the
check out; `?- -p.` was then answered with the model `[-p]`.
`stable_models.pl` says the program has one answer set and `-p` is not
in it. `test_abducible.pl` pins both halves and hands three shapes to
the oracle.

`abdbirds.pl` is the corpus program for this: `ab/1` has rules of its own
as well as being abducible, so it gets a check and `not -ab(X)` is in
every model; `penguin/1` and `wounded_bird/1` are abducible and nothing
else, so they get none. Three answers, as before and as the old
implementation gives, in 4 ms.

The old implementation reaches the same answers a different way, and
`doc/semantics.md` has the detail: its two front ends disagree about
whether an abducible even *has* a `-p`, and where it does, `scasp/2`
simply does not collect an abducible the query cannot reach — so the
question of exempting it never arises. Ours keeps the declaration (it
declares a predicate rather than describing one, so `about_collected/3`
cannot tell whether the query reaches it), and the exemption does that
work instead.

### The justification

`scasp/4` returns it. It is not a second structure: the proof the solver
threads up **is** the tree. A node is `n(Goal, Status, Children)`, and a
literal's children are exactly what its body added — the body gets the
literal's own `S0` and returns `S2`, a proof is only ever prepended to, so
`S0` is a suffix of `S2` and `children/3` splits it by walking to the shared
tail.

```prolog
?- scasp([(p :- not q), (q :- not p)], p, M, J).
M = [p, not q],
J = [p-[(not q)-[chs(p)-[]]], '$nmr'-[]].
```

`chs/1` marks a literal established by the coinductive hypothesis and
`proved/1` one an earlier branch established; showing them is the point, since
a justification that dropped them would claim a literal was proved from
nothing. Built-ins are not steps — `dif(X, 1)` is a constraint on the answer,
already said on the variable. The query's literals are at the top and
everything the NMR check needed is under one `'$nmr'` node: same proof,
different question.  The `$` is so that a program's own `nmr/0` is not
mistaken for it.

The model is the literals of that forest, sorted, and nothing else — which is
`doc/design-next.md` open question 5's property (the model is a function of
the proof) holding by construction rather than by measurement.

### Saying an answer

`html.pl` describes an answer **once**, as HTML, and `html_text.pl` reads
the same description as coloured plain text. That is the old
implementation's design and this is a port of it. The alternative — two
renderers — is what it exists to avoid, and the reason is drift rather than
economy: the two would disagree about a case neither author remembered, and
the text one is what a test reads.

`emit//1` is `html//1` inside `emit_as(Goal, html)` and message-line tokens
inside `emit_as(Goal, plain)`. The mode is read off the **stack**, by
`emitting_as/1` asking for the enclosing `emit_as/4` frame, which is what
lets `html.pl` be written as ordinary `html//1` rules with nothing about
text in them. Layout is a second pass: `emit//1` leaves `nl(N)`,
`indent(N)`, `bullet`, `ansi(Attrs)` in the token list and
`fixup_layout/2` turns them into spaces, newlines and colour — two passes
because indentation and colour are state and there is no argument to carry
it in, for the same reason the mode has none.

Two things are different here, and both are simplifications:

- **The terms arrive readable.** `render.pl` has already read the
  constraint stores and named the variables, so this layer sees
  `'$VAR'(Name)` and `Name | {Constraints}` — where the old one calls
  `inline_constraints/2` and `ovar_var_name/2` itself. Reading the stores
  once, for the model and the justification together, is also what makes a
  variable they share carry one name.
- **A literal is said three ways**, in order: by the program's own
  `:- describe`, by `lang.pl`'s vocabulary for the things every program has
  (`not`, classical negation, the comparisons, "holds for"), and failing
  both by the term itself. `show(human|machine|both)` chooses, `both` being
  the default: it emits both and lets a stylesheet show one, and in text
  there is no stylesheet so `html_text.pl` drops the machine half.

**A description may be HTML.** `:- describe bird(X) :: html(['the bird ',
b(X)])` is the principled way to generate output — the program says how its
literals look and the renderer places them. A variable in such a template
is a *value*, not an element, so `html_values/3` replaces it by a call that
renders it; handing it to `html//1` as it stands produces `<| $VAR="A">`,
which means the only kind of HTML template worth writing could not be
written at all. In text, `html_text.pl` drops the markup and keeps the
content, or `<b>tweety</b>` comes out as `b(tweety)`.

`scasp_human/1,2` is the whole of what the old `human.pl` did, which is
why there is no such module: it is `--human`, over `library(sscasp)`.

#### The toplevel

The `?`-shortcuts **show nothing themselves**. They leave the answer in a
backtrackable global (`dyncall.pl:remember/4`) and `sscasp.pl` registers a
`residual_goals//0` hook that hands the toplevel a goal to portray. That is
the old implementation's arrangement (`scasp/embed.pl`) and the reason is
naming: the model then comes *after* the bindings, and the toplevel names
the bindings, the model and the constraints still on the answer's variables
in one go, so a variable two of them share is one name. A model printed on
its own could not say that.

    ?- ? flies(X).
    X = tweety,
    % s(CASP) model
    { ab(sam),                   not ¬ wounded_bird(john),
      bird(tweety),              not ab(tweety),
      ...
      not ¬ penguin(sam)
    }.

`??+-`, `??-+` and `??++` are the same in words, which is the old
implementation's spelling too. To print from a *program* — where there is
no toplevel — use `scasp_human/1,2`, or `scasp/2` with `portray_model/2`
and `portray_justification/2`.

Three things `render.pl` does that are not obvious:

- **Down the columns, not across.** The model is sorted, so a column is a
  run of related literals and the eye can find a predicate.
  `layout(columns)` fits them to `tty_size/2`; `layout(lines)` is the
  default and is one per line.
- **Neither printer ends with a newline.** A printer that does cannot
  portray a term, because whatever the toplevel writes next — a comma, a
  full stop — lands on a line of its own.
- **The connector is written, not left to the operator table.** The writer
  prints `not -p(X)` as `not-p(X)`, which reads back correctly and reads
  *wrong* to a person, `-` looking like an infix there. It is `not ¬ p(X)`,
  or `not -p(X)` under `format(ascii)`. And the four shapes are four
  different claims, so they get four colours: what holds is bold, what is
  known not to hold is bold red, what there is no evidence for is red, and
  the connectors are cyan.

    ?- scasp_human(flies(X)).
    Model:
       • tweety is a bird
       • tweety can fly
       • there is no evidence that tweety is a penguin
    Justification:
       tweety can fly, because
          tweety is a bird, and
          there is no evidence that tweety is a penguin
       The global constraints hold

**A description may cover a subtree**, as the old `#pred` may:
`:- describe L - Pattern :: Text` says a node together with the children
Pattern matches — `*`, `-`, `[Spec, ...]`, with `+Literal` to keep a child
and `^Literal` to gather one from anywhere below (`describe.pl`, "Describing
a subtree"). **Not ported**: a spec carrying a condition, which no program
in the corpus or the examples uses; the program listing
(`html_program//1`), which needs the compiler's pr-representation; and the
source references, which need a `goal_origin/2` the new solver does not
produce.

#### As data

`scasp_json(Query, Dict)` gives every answer as a dict
`json_write_dict/3` writes. Where `html.pl` renders an answer for a
person, this reports it for a program: the *shape* of every term is said
rather than shown, so a variable is `{"type":"var","name":"A"}` and not
the letter A.

**The variables are named once and their constraints kept apart.**
`render.pl:scasp_readable/3` names query, model and justification
together, so a variable two of them share is one name — a consumer needs
that as much as a reader does — and the stores it inlines come straight
back out: a term says the variable, and the answer carries one
`constraints` entry per name. Repeating the store at every occurrence
would say the same thing many times and leave the consumer to work out
that they were the same.

Four differences from the old `json.pl`, all forced:

- **`tree` is a list.** The proof is a forest here — the query's literals
  and the NMR check beside them — not one rooted tree.
- **A `constraints` entry is a list.** `∉` and CLP(Q) are two stores and
  a variable may be in both; the old assumes one kind per variable.
- **`truth` is always a string.** The old writes `true` and `false` as
  JSON booleans and `likely`/`unlikely` as strings, and one field with
  two types is a trap for a consumer switching on it.
- `module`, `source_file`, `source_line`, `assume` and `abduced` do not
  appear: the solver is not module aware, keeps no source references,
  assumes nothing but by the coinductive hypothesis (`chs`), and gives an
  abducible no node of its own.

`scasp_json/3` is **deterministic** — a document is a whole thing rather
than something to backtrack into — and `limit(Count)` bounds it. Each
answer's dict is built *inside* the solve: `findall/3` copies, and a
copied answer is no longer the one whose query variables the caller
holds.

#### The executable

`make sscasp` builds it; `prolog/sscasp/main.pl` is the source and runs
without building (`swipl -p library=prolog prolog/sscasp/main.pl -- ...`),
which is how `test_main.pl` drives it.

    $ sscasp --tree birds.pl
    % Query
    ?- flies(X).
    % ――――――――――――――――――――――――――――――――――――――――――――――――――――――
    %                  Answer 1 (0.001 sec)
    % ――――――――――――――――――――――――――――――――――――――――――――――――――――――
    % Model
    { bird(tweety),  flies(tweety),  not penguin(tweety) }
    % Justification
        flies(tweety)
            bird(tweety)
            not penguin(tweety)
        global constraints
    % Bindings
    X = tweety

`library(main)` does the plumbing: `argv_options/3` reads the command
line against `opt_type/3` and prints `--help` from `opt_help/2`, so a
flag and its documentation are one thing rather than two —
`test_main.pl` checks that every flag it tests appears in the help.

Two things the file owes a caller:

- **The exit status says what happened.** 0 for at least one answer, 1
  for none, 2 for an error. A program that cannot be told apart from a
  program with no answers is no use in a script, and every test in
  `test_main.pl` asserts a status for that reason.
- **A document is all that is written.** `--json` and `--html` suppress
  the terminal output; a JSON file with a `% Query` heading in front of
  it is not JSON. They also *collect* and write at the end, so a run cut
  short leaves no half a document.

**The query keeps the reader's variable names.** `source.pl` now returns
`bindings` beside `query` — `?-(Goal, Bindings)` from `named_term/3` —
so the query prints as `?- flies(X)` and the answer as `X = tweety`,
which is what the person who wrote the file will recognise. It reaches
`portray_clause/3` as `variable_names/1` through
`render.pl:write_options/2`, and getting it there needed the
`findall/3` trap avoided for the third time: a copied binding list names
some *other* variable, and the query prints `?- flies(_)` with the names
carefully attached to nothing.

**`:- use_module(library(clpr), []).`** in `solve.pl` is for the saved
state and nothing else. `library(clpqr)` compiles one source into a `_q`
and an `_r` instance and its shared modules name both, so loading only
CLP(Q) leaves the `_r` halves referenced and undefined; nothing calls
them, but `swipl -c --undefined=error` cannot know that and the state
will not build. The old implementation has the same line for the same
reason.

**`--version` and the stylesheet are frozen at compile time.**
`term_expansion/2` reads `pack.pl` and `web/css/sscasp.css` while
`main.pl` is being loaded, because a saved state has neither beside it.
The helpers come *above* the expansions, since a term is expanded as it
is read and anything the expansion calls has to exist by then.

For the stylesheet that is not tidiness. Read at run time through
`library(sscasp/web/...)` it resolved, in the **built** executable, to
whatever the *installed pack* pointed at — on this machine a different
checkout of this project, so `./sscasp --html` styled its pages from
another working tree, and on a machine without one `stylesheet/2` would
have fallen back to `""` and produced an unstyled page in silence. The
path now comes from `prolog_load_context(source, _)`, so it is this
file's neighbour rather than whichever copy the search path reaches
first. `test_main.pl:main_built` pins it.

Not there: `--forall`, `--plaindual`, `--variant` and `--real` —
none of which the solver has either. `scasp_assert/1` and friends **are**
there; see "A program that changes" above.

**`-i` reads queries until end of input**, and differs from the old one
twice on purpose. `run.pl:scasp_loaded/4` holds the compiled program open
for the length of the loop and `scasp_ask/4` asks in it, so the program is
compiled **once** — 92 ms of setup against 1.8 ms per further query on
`bec_tap_01_overflow`. And the solve is caught: the old `main_loop/1` has
no `catch/3`, so one misspelled goal reaches `error/1` and `halt(1)` and
the session dies with a status saying the *program* was wrong.

A typo is reported in the reader's language rather than the compiler's.
`run.pl:query_error/2` turns `existence_error(procedure,
'tmp-1-884':'scasp$plain$nosuchgoal$0'/3)` into
`existence_error(scasp_predicate, nosuchgoal/0)` — the kind
`library(scasp)` raises for the same mistake, so a caller catching one
catches both. Only a *mangled* name is rewritten; anything else the solver
could not find is a bug here rather than a typo in a query.

Answers are offered one at a time only at a terminal. `get_single_char/1`
on a pipe would read the next query's first character, so a non-tty run
honours `--answers` — which is what makes `-i` testable, and what a pipe
wants anyway. History is a terminal's too: `read_term_with_history/2`
reaches `editline:el_history/2`, which **raises** on a pipe, so a non-tty
run reads with `read_term/2`.

**Two of those were found only in the saved state**, and neither is
visible from source:

- `read_term_with_history/2` raising on a pipe, which the loop printed,
  recovered from without consuming anything, and printed again — forever.
  Recovery from an unreadable term now stops when the read made no
  progress, which a syntax error does not trigger because it consumes up
  to the full stop.
- SWI's `on_error(print)` makes `halt/0` exit **1** once any error has
  been printed. Right for a script, wrong for a toplevel: a session that
  reported a typo and carried on did what was asked. `interactive/2` ends
  with an explicit `halt(0)`; a run that could not start still exits 2.

So `test_main.pl:main_built` runs `./sscasp` itself when there is one and
is skipped when there is not — everything else in that suite runs
`swipl prolog/sscasp/main.pl`, deliberately, so the suite does not need
`make`. **And the `Makefile` now gives both executables their sources as
prerequisites**; without them editing a source never rebuilt, and a stale
binary is a bad way to learn that something behaves differently in a
state.

#### It stands on its own

`prolog/sscasp/` loads nothing out of `prolog/scasp/`. The only thing it
ever did was the stylesheet and the script, which are **copies** now —
`web/css/sscasp.css` and `web/js/sscasp.js`, under names of their own
because `js` and `css` are aliases *both* trees register: with two search
paths and one file name, which copy `js('scasp.js')` found would depend on
load order. The resource is `sscasp` for the same reason. The DOM is still
the one the CSS was written for, so the class names say `scasp-`.

`test_main.pl:main_standalone` pins it three ways: nothing from
`prolog/scasp/` is loaded, both web resources resolve under
`/sscasp/web/`, and the program runs from a library root holding only
this tree. **That last one needs `--no-packs`** — `-p library=Dir`
prepends rather than replaces, so without it the installed pack is still
on the path and `library(scasp/...)` resolves through it. An earlier
version of the test passed with the dependency deliberately put back.

### Against the corpus

`test/sscasp/corpus.pl` describes the comparison and `test/sscasp/test_corpus.pl` runs
it: 49 programs across four corpora, compared with the `.pass` data. That is a *different* kind of
evidence from the oracle: `stable_models.pl` shares no design decision with
either implementation and can say who is right, while the corpus shares all
of them with the old one and can only say whether they agree. Both matter,
and the corpus is what says whether the reimplementation is finished.

#### Where it stands

Every golden program, **both implementations in one process**, at the same
moment, all answers enumerated:

| | old | new | |
|---|---|---|---|
| `hanoi` | 1 answer, 0.042 s | 1, **0.005 s** | 7.7× |
| `hanoi_7` | 1, 0.013 s | 1, **0.003 s** | 5.2× |
| `hanoi_scasp_7` | 1, 0.015 s | 1, **0.003 s** | 4.9× |
| `queens` | 2, 0.980 s | 2, **0.35 s** | 2.8× |
| `bec_tap_04` | 2, 0.053 s | 2, **0.031 s** | 1.7× |
| `birds` | 1, 0.004 s | 1, **0.003 s** | 1.5× |
| `bec_light` | 2, 0.062 s | 2, **0.39 s** | 0.2× |
| `classic_negation_inconstistent`, `family`, `forall_arity`, `haplotype`, `min_model`, `olon_unreachable`, `pq`, `rat`, `solve_findall`, `vars` | | same count, 1.3–1.6× | |
| `bec_tap_01` | 1, 0.028 s | 1, **0.087 s** | 0.3× |
| `bec_tap_02` | 1, 0.028 s | 1, **0.086 s** | 0.3× |
| `bec_tap_03` | 2, 0.034 s | 2, **0.171 s** | 0.2× |
| `bec_loan` | 4, ~0.03 s | 4, **0.07 s** (with `--no-commit-nmr` no end in 60 s) | |

**20 of 21 match the old implementation's answer count.** The one that does
not is the gap list below, and the three that used to be beside it are worth
reading about: they answered in milliseconds until the quantifier stopped
**deleting** the disequalities it read (it reads them through
`herbrand.pl:read_store/2` inside `\+ \+` now). `dif:attribute_goals//1` calls `del_attr/2` on each
or-node as it reports it — `copy_term/3` and the toplevel undo that, a direct
call does not — so a quantifier that read a variable's disequalities destroyed
them, covered a domain it had not covered, and lost the narrowing a branch had
just applied. `lion.pl` reached a narrowing **964 deep** and proved
`-live(rat, apartment)` alongside `not -live(rat, apartment)`.

Reading them without destroying them is six lines and is not optional. What it
costs is the measure of what those programs were getting for free: one
sub-check of `bec_tap_01` goes from **8 coverings to 4,859**, because every
value `consistent/3` excludes is a piece of the domain the quantifier has to
cover. Those pieces are not spurious — leaving consistency constraints out of
the region makes `queens.pl` answer 256 times instead of twice, and so does
projecting a compound disequality only where the terms agree elsewhere. The
work is real and the quantifier has to get cheaper at it.

**Two strengths of agreement**, because models legitimately differ where
proofs do (open question 5 again). `answers_agree/1` asks whether the two
implementations bind the **query** the same way — that is what was asked for,
and a difference in it means something about the program. Ours are read off
the query as each answer binds it; the golden data holds only models, so the
old implementation's are read off the literals of the query's shape in them,
and where there are none (a conjunctive query, a query literal `#show`
leaves out) the comparison falls back to models on both sides and says
little — `agrees/1` covers those. `agrees/1`
additionally requires the models to match, which is a real regression test but
not the definition of correct: a model carries what the proof that was found
needed, and two correct proofs may need different things.

**47 of 49 golden programs agree on the answer; 25 also on the model.**
The rest are in `corpus.pl:disposition/2`, and their test asserts
they still *fail* — so an entry cannot outlive the problem it records,
exactly as `known_issues.pl` works.

**Four corpora**, named by the directory each lives in, because a
program's name should say where to find it. They overlap — `hanoi_7` is
in two and `bec_loan` in two others — so a name is `Corpus/Base`:

| `all_programs` | 21 | the regression corpus |
| `sasp` | 14 | from the original s(ASP) distribution |
| `min_programs` | 13 | one feature each, cut down |
| `dcc` | 1 | needs `--dcc` |

**A disposition is one of two things, and they mean opposite things:**

| `gap(Reason)` | the reimplementation does not match, and should |
| `golden(Reason)` | the old implementation is wrong and the `.pass` records it |

Only the first is a to-do list, and it is empty: `bec_loan`, the last
`gap(slow)`, answers since the NMR check is committed to (below). Today:

| `golden(...)` | `min_programs/solve_not_is` and `min_programs/stream_reasoning` |

**Widening the corpus from 21 programs to 49 found two answers the old
implementation should not give**, and nothing wrong with this one:

- `solve_not_is` asks `?- invalid_value(X)` of `invalid_value(X) :- not
  valid_value(X). valid_value(V) :- coord(X), coord(Y), V is X+Y.` over
  `coord(1..3)`. The old answers `invalid_value(2)` as well as the
  constrained answer, and its own justification shows why: under `not
  valid_value(2)` it tries `2 is 2+1`, `2 is 2+2`, `2 is 2+3` and never
  `2 is 1+1`. One clause of the dual is missing.
- `stream_reasoning` answers `valid_stream(1, p(a))`, which is cancelled —
  `higher_prio(2, 1)`, `stream(2, q(a))` and `incompt(p(a), q(a))` all
  hold. Ours is `p(A)` with `A ∉ [a, b]`: the same answer with the
  cancelled individuals taken out.

##### What the event-calculus family was waiting for

**Not the queries.** With `scasp_compile_nmr` set to false, `bec_tap_01`
answers all ten of the queries written in its comments in **6–19 ms**, and
agrees with the old implementation on every one — including
`holdsAt(leak(L), 14)`, where both say no and the comment says `L = 0`.
`test_corpus.pl:corpus_without_nmr` pins that, and it was the measurement
that found the rest: what did not terminate was the **NMR check** that
`scasp/3` appends to every query — here one classical-negation consistency
check, `:- -holdsAt(F, T), holdsAt(F, T)`, quantified over every fluent and
every time. (Comparing against the old implementation without it is not
like-for-like: `solve/4` takes the query alone, and `solve([Q, o_nmr_check],
...)` is the call to make.)

That check reaches `not holdsAt(filling, T)` for `T > 20`, a goal **neither
implementation could prove** — `holdsAt(filling, 25)` fails in 19 ms and its
dual did not terminate in either tree. The old one never reaches it: it
covers the check out of the first dual clause alone, `not -holdsAt(F, T)`,
and never calls `not holdsAt` at all. That covering cannot be right, because
`-holdsAt(filling, 25)` is **true** — `tapOff` terminates `filling` at 20 and
nothing restarts it, and we prove it in 3 ms — so `not -holdsAt(filling, 25)`
is false and the first clause does not cover the domain. Asked directly, the
old implementation answers `-holdsAt(filling, 25)` no. **It prunes too much**,
and matching its timings would have meant reproducing that.

###### What the fix turned out to be

`solve.pl:covered/7`. **The complement leaves its constraint store behind.**
A branch already contributes no proof; what it also must not contribute is a
store. A branch invents variables, CLP(Q) keeps every variable a constraint
ever reached in one class, and nothing collects the ones that go out of
scope — so the class grew by everything every branch had ever touched, and
`dump/3` is Fourier–Motzkin over that class, asked once per covering. The
quantifier got quadratically slower at the thing it does most: a class of 199
variables on `not holdsAt(level(10), T)` for `T > 5`, 67 ms a projection,
no answer in 20 s.

So the complement is covered inside a run that `fail` undoes, and **one**
snapshot of the caller's variables comes back out through `nb_setarg/3` over
fresh plain names — the device `elsewhere/7` already uses. Three things it
needs:

- **The narrowing has to survive.** `not s(B)` for `s(B) :- u(X), X #> B,
  X #< B+3` over `u(5)` holds where 5 is outside the window, and that is said
  by the branch covering X = 5, not by the first answer. Dropping it makes
  `not s(4)` succeed, which is the one test that fails if the store is simply
  discarded.
- **One snapshot, not one per branch.** The branches are a conjunction, so
  what survives them is the projection of the store they leave. Taking it per
  branch was 12.4 of `bec_tap_01`'s 20 seconds and bought nothing.
- **The complement is covered once.** Without `once/1` the run is not a run
  but an enumeration of every way to cover it, and even
  `not holdsAt(level(10), T)`, which answers in 4 ms, does not finish.

`not holdsAt(filling, 25)`: **yes in 42 ms**. `bec_tap_01`, `bec_tap_02` and
`bec_tap_03` answer, agreeing with the golden data, and `queens` is unmoved
at 0.21 s.

###### And what `bec_loan` is still waiting for

**Since 2026-10-01: nothing, with `sscasp_commit_nmr`.** Its first answer
now comes in 0.03 s — which of the day's changes did it is not traced — and
what never ended was enumerating the rest: the query is proved once and the
NMR check is then proved again and again, 14,843 proofs a minute giving 35
models of the one answer `holdsAt(it_is(2), T)`, `T > 1`, and the other
three answers never reached. Committing to the first proof of the check
(above, under "The solver"), the default, gives all four golden answers in
0.07 s, and the program is no longer a gap. The rest of this section is the
history of the search before that.

Not a descent any more — `same_question/2` (above) takes its ancestor chain
from 6,677 frames to 21, and with the NMR check off its query
`?- holdsAt(F, T).` answers in **0.027 s** where it used to answer never.
`test_corpus.pl` pins that.

What is left is the size of the search its NMR check opens. The check is the
same classical-negation consistency check as `bec_tap_01`'s, but `bec_loan`
has seven `initiates` clauses and one of them,

    initiates(ends(_), is_potentially_defaulted(B), A) :-
        B = pays_to(C, D, E), is_due_on_from_to(D, A, C, E),
        holdsAt(it_is(F), F), not holdsAt(paid_to_on(C, D, E, _), F).

dualises to a quantifier over **seven** body-only variables in one tuple.

It does terminate: the first answer comes after **376 s**, against 0.85 s and
880 quantifier entries for the old implementation — so the question is a
factor of 440, not a hang. The search is wide rather than deep: 39,507 goals
in 10 s at depth 20, ~90,000 coverings spread evenly over all seven clauses'
duals, and the complement is *narrow* — 0.6 to 4.2 pieces per covering, with
four of those quantifiers producing **no** pieces at all while being entered
12,000 times each. So it is the caller re-entering them, not the covering
branching, and the next thing to find is what makes `not holdsAt/2`'s own
covering ask for them so often.

Where the recursion starts is the *query*, not the check:
`initiates(ends(2), is_terminated(the_agreement), _) :-
not holdsAt(requested_on(the_borrower, 1000, 2), 2).` The check's own
quantifier is entered **6 times** and yields three pieces each; everything
after that is what proving `not chk(F, T)` at those points costs.

Five things ruled out by measurement, each of which looked likely:

| | |
|---|---|
| the complement being wide | 0.6 to 4.2 pieces per covering |
| `coverable/1` rejecting a tuple member bound to a non-ground term | it never rejects here — 0 of 95,158 answers |
| the **constraining CHS** | making `consistent/3` refuse instead, which is the old implementation's rule and prunes hugely on `bec_tap_01`, leaves `bec_loan` timing out just the same |
| ground goals being re-proved | there are only 24 distinct ones, and the existing reuse already fires 7,542 times against 299 ground expansions |
| committing per nested quantifier | see the note under the quantifier: green, and worth 0.395 s → 0.288 s on `bec_light` alone |

**Measured again on 2026-09-27**, on the smaller query it waits on —
`not holdsAt(paid_to_on(the_borrower, 550, the_lender, _), 4)` with the NMR
check off, 28 s here against 0.031 s for the old implementation: 821,656
proofs resting on no assumption, of **138 distinct goals**. The work is
re-proving the same literals, mostly inside quantifier complements that
`covered/7` throws away. `doc/design-next.md` §7a is the tabling design
that follows from it. Separately, domain inference (`82fcc34`) made the
whole program slower still — no first answer in an hour, against 376 s
before it and 385 s with inference switched off.

What is left is 25,271 expansions of **non-ground** goals in 10 s — the
quantifiers' inner goals, with their own fresh variables — against 8,395
goals for the old implementation's entire answer. That is the number to
attack, and nothing measured so far says which part of it is avoidable.

What the corpus no longer measures, the oracle does. Widening the differential
sweep to 4,410 seeded programs turned up **one** disagreement — a program with
no answer set that both implementations answer anyway. It is fixed, and the
sweep is now clean:

```prolog
p :- not q, r.        % the only answer set is {q}
q :- not r.
r :- p.
```

`p` needs `r` and `r` needs `p`, a loop over no negation and so unfounded. The
route is `p` → `not q` → (`q`'s dual needs `r`) → `r` → `r :- p` → `chs(p)`,
two negations, so the even-loop rule lets it stand; then **`r` is reused for
`p`'s own body as already proved**, closing the positive loop unnoticed. The
old implementation answers `p` too, and then fails `r` — a literal in the
model it just reported.

`proved/4` now records whether a proof rested on the coinductive hypothesis
and `check/7` reuses only one that did not. P4 §3.2 offers the reuse as an
optimisation ("it avoids recomputation"), and qualifying it costs 4% of the
sweep and nothing measurable on the corpus; dropping it altogether costs the
sweep 19.5 s against 217 s, which is why the rule is about the *proof* rather
than about the reuse.

Notable: `clpq_equality.pl`, which the old implementation does not terminate
on (`known_issues.pl`), answers here in half a millisecond.

### Two things that made the quantifier unusable

`queens.pl` did not finish in two minutes; it now answers in 0.26 s, against
0.95 s for the old implementation. Both causes were found by measuring rather
than reading, and both are worth knowing about before touching `solve.pl`:

- **The proof-literal lookup was unindexed.** `consistent/3` walked the whole
  proof forest for every literal, which is quadratic in the proof. The proof
  now carries an index from `Name/Arity` to the goals proved under it, which
  is what the old implementation does (`proved_relatives/3`). It is *derived*
  state, which decision 3 was wary of — but that decision objects to
  **rebuilding** one structure from another on every entry, which is what
  `stack_parents/2` and `stack_proved/2` do; this is maintained as the proof
  grows and shares the goal terms.

- **A quantifier's branches accumulated proofs.** `cover_each/6` threaded each
  branch's proof into the next, so every branch re-solved the same goal and
  added a near-duplicate. The proof doubled per level: **131,135 nodes** on
  `queens.pl` holding 1.3M attributed variables, where the answer has 53
  literals. That is what exhausted a 4 GB stack — inside `term_attvars/2`, at
  frame depth 85, so a small search over enormous terms rather than a deep
  one. Branches then **checked** without contributing, which was later found
  unsound and reversed: they are proved in one proof again, and with ground
  literals reused from the proof `queens.pl` no longer grows (0.10 s either
  way).

**Still missing:** nothing in the renderers. `findall/3` solves its goal
rather than calling it, so what the collection proved is in the model; its
negation is refused rather than failed.

### Testing the new tree

| Suite | Covers |
|---|---|
| `test/sscasp/test_domains.pl`, `test_annotations.pl`, `test_describe.pl` | reading annotations |
| `test/sscasp/test_source.pl` | reading an s(CASP) source file |
| `test/sscasp/test_html.pl` | the renderer, both ways out of one description |
| `test/sscasp/test_render.pl` | the machine rendering — connectors, colour, columns |
| `test/sscasp/test_herbrand.pl` | the Herbrand store, and which of its two backings gets what |
| `test/sscasp/test_listing.pl` | the compiled program read back, over the whole corpus |
| `test/sscasp/test_json.pl` | an answer as data, round-tripped through `json_write_dict/3` |
| `test/sscasp/test_main.pl` | the `sscasp` executable, in a subprocess: flags, formats and **exit status** |
| `test/sscasp/test_mangle.pl` | the naming scheme, both ways |
| `test/sscasp/test_abstract.pl`, `test_dual.pl`, `test_dualise.pl` | head abstraction and duals |
| `test/sscasp/test_graph.pl` | odd loops, against brute-force cycle enumeration |
| `test/sscasp/test_nmr.pl`, `test_compile.pl` | the NMR check and the whole pipeline |
| `test/sscasp/test_emit.pl` | the threading and the wrappers |
| `test/sscasp/test_solve.pl` | the solver, the constraints, the quantifier, and **the oracle** |
| `test/sscasp/test_corpus.pl` | the golden corpus, and what the four slow gaps answer with the NMR check off |
| `test/sscasp/test_crossings.pl` | language features crossed with each other — `doc/test-strategy.md` Gap 1 |
| `test/sscasp/test_api.pl` | `library(sscasp)` over ordinary Prolog clauses: what is collected, what comes back, what it looks like printed, and a program that changes under `scasp_assert/1` |

Two units compare an argument against something that enumerates, because two
steps here are arguments rather than transcriptions: `test_graph.pl` checks
the balance criterion against brute-force cycle enumeration, and
`test_solve.pl` checks brave entailment against `test/stable_models.pl` on 420
seeded random programs — propositional, classically negated, first-order and
CLP(Q). The last two are what bring the *quantifier* into range, since the NMR
sub-check of a rule with a variable is a `forall` over it. Widening either
sweep is a one-line change to a `shape/N` fact.

## Architecture

### Three entry points, one core

| Entry point | Module | Use |
|---|---|---|
| `prolog/scasp/main.pl` | `scasp_main` | the `scasp` CLI/executable; loads files, answers the last query, exits |
| `prolog/scasp.pl` | `scasp` | the user-facing library: `?-`-style operators (`?--`, `?+-`, `?-+`, `?++`, `??++`, …), `scasp/2` |
| `prolog/scasp/embed.pl` / `dyncall.pl` | `scasp_embed`, `scasp_dyncall` | solve queries against ordinary Prolog clauses (`scasp/2`). **`begin_scasp/1`..`end_scasp/0` is obsolete** — see below |

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
- `doc/design.md` describes how this implementation works — the essential steps in
  s(CASP) resolution, from collecting a program through duals, the NMR check and the
  solver's six coinduction cases to the renderers. Start here.
- `doc/design-next.md` is the design round for a fresh implementation: the
  measurements it rests on, the decisions taken, and the questions still open.
- `doc/performance.md` is where the reimplementation stands against the old one
  on the golden corpus — answer counts, times, the two strengths of agreement,
  and the harness to re-measure. Re-measure before quoting any of it.
- `doc/semantics.md` is what the two answer *differently*, split into gaps
  closed, gaps open, choices that are not defects, what is not implemented, and
  the limitations they share. Read it before reporting a difference.
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
- **A `:- meta_predicate` for an exported predicate goes before the first clause**,
  with the imports — not beside the predicate it describes. A meta-predicate mode is
  part of the module's *signature*: a reader, or a tool, taking the interface from the
  head of the file stops at the first clause, and a declaration below that point
  changes how a caller must pass an argument without saying so where the interface is
  stated. A declaration for a *private* helper may stay next to it
  (`scasp/json.pl:query_json/2` does).
- Run `make check_load` after touching imports/exports — it catches undefined predicates
  in both entry points, which plain loading does not.
- CI is `.github/workflows/ci.yml` (build + `make check`). The old `.gitlab-ci.yml` was
  inherited from Ciao, built with `ciaoc` and tested nothing in this port.
- Requires SWI-Prolog 9 (`pack.pl`); `main/1` additionally asserts `>= 8.5.0-71`.
- `ATTIC/` is dead upstream material; do not use it as a reference for current behaviour.
