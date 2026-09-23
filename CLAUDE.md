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

Regression tests are *golden-output* tests. `test/test_scasp.pl` generates one plunit
test per program — solving it with `scasp_load/2` + `scasp_query/3` + `solve/4`, the
front end the executable uses — and compares against `<base>.pass` (a single
`write_term/2` term read back with `module(scasp_ops)` so the s(CASP) operators parse).
Regenerate the data with `save_golden/1,2` after an intended change.

The two halves are compared differently, because the model of an answer is stable while
the *location* of steps in its justification moves whenever the solver changes:

- **Model: exactly.** `answers_model_key/2` normalises variable naming and ignores the
  order of the answers, nothing else. There is no option to weaken it — and it does not
  need one: the model is a *function of the proof* (`test/test_proof_model.pl`), so two
  answers with the same proof at `support` always carry the same model. Comparing the
  proof therefore subsumes comparing the model rather than competing with it.
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
| `test/test_embed.pl` | `begin_scasp`/`end_scasp` — an **obsolete** API, kept running so it does not rot silently |
| `test/test_le.pl` | the Logical English examples, run through `scasp/2` |
| `test/test_cli.pl` | argument parsing, exit status and output formats, by running `main.pl` in a subprocess |
| `test/test_proof_skeleton.pl` | the comparison itself, against hand-built trees |
| `test/test_proof_model.pl` | that the model is a function of the proof |
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

| Module | |
|---|---|
| `ops.pl` | every operator, in one place |
| `domains.pl` | reading one domain annotation — `:- domain`, `:- pred` |
| `annotations.pl` | a program's annotation table, as a value rather than a store |
| `mangle.pl` | functor names — `m$dual.2.inner$flies$1`, always four fields |
| `abstract.pl` | head abstraction — making head unification explicit |
| `dual.pl` | dualising a clause body — one clause per negated literal |
| `dualise.pl` | dualising a predicate — where the pieces meet |
| `graph.pl` | the call graph, and the odd loops over negation in it |
| `nmr.pl` | denials → the NMR check |
| `compile.pl` | the whole pipeline — terms in, `program{clauses, nmr, unbounded}` out |
| `emit.pl` | the backend — the logical program made runnable |
| `solve.pl` | the runtime the emitted clauses call |
| `run.pl` | `sscasp/3,4` — compile, emit, load into a module, answer |
| `source.pl` | reading an s(CASP) source file, `#include` and all |

Its tests live in `test/` like the rest, so there is one `make check`.

### What the new compiler produces

`compile_program(+Module, +Terms, -Program)` returns a **value**, not a store:

```prolog
program{ clauses:   [...],            % every clause, translated and dualised
         nmr:       nmr{goal, clauses, checks},
         unbounded: [...] }
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

`unbounded` lists where a quantifier has no declared domain. It is a scan, not
decision 5's undecidability analysis — the solver's `forall` decides many of
them at run time — but it is exactly the list of places where adding a `:- pred`
annotation would replace a search over the universe with an enumeration.

### The solver

`sscasp(+Terms, ?Query, -Model)` compiles, emits, loads into a temporary
module and answers. The query is the literals asked for **followed by** the
NMR check, sharing one proof, so a constraint the query's own answer violates
is caught.

`emit.pl` is a pass of its own because it is the only place that knows how the
solver carries state: three arguments (`Anc` down, `S0`/`S` up) and a wrapper
per literal, so that the coinductive check happens before a predicate's
clauses are tried. The logical form stays what a listing shows and what the
compiler's tests read.

**A `dif/2` is destroyed by reading it.** `dif:attribute_goals//1` calls
`del_attr/2` on each or-node as it reports it, so that a node shared by
several variables is not reported twice; `copy_term/3` and the toplevel undo
that and a direct call does not. `solve.pl:disequality_goals/2` calls it
inside `\+ \+` and carries the ground excluded values out with `nb_setarg/3`
for that reason. Anything else reaching for a variable's constraints has the
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
something already being proved from itself", hence unfounded.
`solve.pl:check_parents/4` reads it the same way.

**And a variant is a variant of the *question*, not of the store.** `=@=`
and `subsumes_term/2` take attributes into account: `f(X) =@= f(_)` is false
once X carries a constraint, and two variables carrying *different* ones are
never variants. So a recursion whose steps constrain their own variable never
looks like a loop — which is every recursion over time in an event-calculus
program. `bec_loan` descends
`not holdsAt(requested_on(the_borrower, 1000, 2), T)` →
`not initiates(ends(1), is_liable_to_litigation(the_lender), T)` → the same
pair one step earlier, and the ancestor chain reaches **6,677** frames of it.

`solve.pl:same_question/2` compares the bare terms and then asks whether the
goal is an *instance*: the ancestor's disequalities must be among the goal's
(`herbrand_instance/2`) and its dense store must be entailed by the goal's
(`dense_instance/2`). Both halves are needed, and simply ignoring the store
is not the shortcut it looks like — `ancestor/4` unifies the goal with the
ancestor it matched, so a goal that is *less* constrained would be narrowed
by one that is more, and that unification then fails, leaving the loop
unreported anyway. Upstream states the same rule as
`prolog/scasp/clp/clpq.pl:entail_terms/2`.

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

Two stores, because two stores is what s(CASP) is (decision 4): `dif/2` for
Herbrand disunification and `library(clpq)` for the dense order, both reached
through `emit.pl`'s `builtin/2`. A constraint is not a literal — it is not in
the model and not a step in the justification. It says what the answer *is*,
and it says it on the variable:

```prolog
?- sscasp([(p(X) :- X #> 3, X #< 10)], not p(V), M), copy_term(V,_,A).
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

### The quantifier

`forall/5` is one recursion for all of decision 5's decidable cases: solve the
goal once, see from what the quantified variables are left constrained to
**which part** of the domain that answer covered, then cover the rest the same
way. The cases are not clauses any more — they fall out of what the store says
when `snapshot/3` is asked (`residue/3`):

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

**And the complement leaves nothing behind but its narrowing.** A branch
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
**nest**, since `dualise.pl` nests one quantifier per body-only variable and
the outermost's search runs through them all; committing at each level costs
9.2 s of `make check` against 4.8 s for no answer either way. The snapshot is
**projected** with `dump/3`, for the reason below — an unprojected constraint
would invent a variable again on every round. And nothing attributed leaves
the discarded run: the covering is run once more, `fail` undoes it and
`nb_setarg/3` carries out a description over fresh plain names, so no
constraint store is copied. That second run is only paid when a second answer
is asked for.

An answer that **binds** the variable is rejected and the goal asked for
another: it covers one point of an infinite domain, so no finite number of
them could cover it, and any real covering contains an answer that leaves the
variable free or in a region — the points then fall out of *that* answer's
complement. Enumerating them instead is not a small waste: it took 37 s on a
five-clause program the old implementation answers in 9 ms.

Where the domain really is finite it has to be declared, and `for_domain/6`
enumerates it — including a range, which compiles to a CLP(Q) region and is
covered rather than listed. That is case 6 against case 1, and the difference
is one annotation.

### The justification

`sscasp/4` returns it. It is not a second structure: the proof the solver
threads up **is** the tree. A node is `n(Goal, Status, Children)`, and a
literal's children are exactly what its body added — the body gets the
literal's own `S0` and returns `S2`, a proof is only ever prepended to, so
`S0` is a suffix of `S2` and `children/3` splits it by walking to the shared
tail.

```prolog
?- sscasp([(p :- not q), (q :- not p)], p, M, J).
M = [p, not q],
J = [p-[(not q)-[chs(p)-[]]], nmr-[]].
```

`chs/1` marks a literal established by the coinductive hypothesis and
`proved/1` one an earlier branch established; showing them is the point, since
a justification that dropped them would claim a literal was proved from
nothing. Built-ins are not steps — `dif(X, 1)` is a constraint on the answer,
already said on the variable. The query's literals are at the top and
everything the NMR check needed is under one `nmr` node: same proof, different
question.

The model is the literals of that forest, sorted, and nothing else — which is
`doc/design-next.md` open question 5's property (the model is a function of
the proof) holding by construction rather than by measurement.

### Against the corpus

`test/test_corpus.pl` points the new implementation at `test/all_programs/`
and compares the models with the `.pass` data. That is a *different* kind of
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
| `bec_loan` | 4, ~0.03 s | **no answer in 120 s** | |

**20 of 21 match the old implementation's answer count.** The one that does
not is the gap list below, and the three that used to be beside it are worth
reading about: they answered in milliseconds until `disequality_goals/2`
stopped **deleting**
the disequalities it read. `dif:attribute_goals//1` calls `del_attr/2` on each
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
and a difference in it means something about the program. `agrees/1`
additionally requires the models to match, which is a real regression test but
not the definition of correct: `hanoi_scasp_7`'s golden model is a strict
*subset* of ours, missing the `not negmove(...)` that proving each `move(...)`
needs.

**20 of 21 golden programs agree on the answer; 11 of those also on the
model.** The rest are in `gap/2` with a reason, and their test asserts they
still *fail* — so a gap cannot outlive the problem it records, exactly as
`known_issues.pl` works:

| `slow` | `bec_loan` — its query answers in 0.027 s; its NMR check opens a quantifier over a seven-variable tuple, measured below |
| `comparison` | `hanoi_scasp_7` — the golden model has no query literal to read an answer out of |

##### What the event-calculus family was waiting for

**Not the queries.** With `scasp_compile_nmr` set to false, `bec_tap_01`
answers all ten of the queries written in its comments in **6–19 ms**, and
agrees with the old implementation on every one — including
`holdsAt(leak(L), 14)`, where both say no and the comment says `L = 0`.
`test_corpus.pl:corpus_without_nmr` pins that, and it was the measurement
that found the rest: what did not terminate was the **NMR check** that
`sscasp/3` appends to every query — here one classical-negation consistency
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
The search is 39,507 goals in 10 seconds at depth 20 — wide, not deep, and
spread evenly over all seven clauses' duals. That is a different problem from
the ones above, and the first thing to measure is whether it terminates at
all.

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
  one. Branches now **check** without contributing; the quantifier's proof is
  the one its first answer gave, as `solve_other_forall/11` also keeps.

A consequence worth stating: a model no longer contains what only a
complement branch established, which is why the model comparison is the
stricter of the corpus's two checks rather than the definition of agreement.

**Still missing:** `∉`, `:- prolog p/2`, and the renderers — a model still
comes back as literals with attributes rather than with the store inlined,
which is what `output.pl` does for the old one. `findall/3` solves its goal
rather than calling it, so what the collection proved is in the model; its
negation is refused rather than failed. `#abducible` is four rules, two of
them an even loop, and gets **no** `:- p, -p` sub-check — proving that means
proving `not -p(X)` for a quantified X, which decides the abducible for every
individual at once (`abdbirds.pl`: 3 answers without it, 48 with).

### Testing the new tree

| Suite | Covers |
|---|---|
| `test/test_domains.pl`, `test_annotations.pl` | reading annotations |
| `test/test_mangle.pl` | the naming scheme, both ways |
| `test/test_abstract.pl`, `test_dual.pl`, `test_dualise.pl` | head abstraction and duals |
| `test/test_graph.pl` | odd loops, against brute-force cycle enumeration |
| `test/test_nmr.pl`, `test_compile.pl` | the NMR check and the whole pipeline |
| `test/test_emit.pl` | the threading and the wrappers |
| `test/test_solve.pl` | the solver, the constraints, the quantifier, and **the oracle** |
| `test/test_corpus.pl` | the golden corpus, and what the four slow gaps answer with the NMR check off |

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
