# Test strategy for the reimplementation

The suite exists to make a clean reimplementation of the reasoner possible. That
is a different goal from guarding the current code, and it changes what a good
test looks like. This file records where the suite stands against that goal,
what is missing, and — the part that decides most of the design — **which tests
survive a change of data representation and which do not**.

Companion files: `doc/literature.md` (what the design is supposed to be),
`doc/bugs.md` (what is known broken), `CLAUDE.md` (how to run things).

## Where the suite stands

Measured over the whole suite (434 tests, ~2.5 s).

### Line coverage is not the problem

Clauses never reached, per module:

| Module | Unreached | Lines |
|---|---:|---:|
| `comp_duals.pl` | **0** | 338 |
| `variables.pl` | **0** | 173 |
| `call_graph.pl` | 1 | 231 |
| `nmr_check.pl` | 3 | 545 |
| `program.pl` | 4 | 416 |
| `prolog.pl` | 4 | 110 |
| `model.pl` | 8 | 280 |
| `clp/disequality.pl` | 9 | 295 |
| `solve.pl` | 13 | 1172 |
| `predicates.pl` | 13 | 136 |
| `modules.pl` | 13 | 224 |
| `input.pl` | 15 | 458 |
| `stack.pl` | 19 | 338 |
| `clp/clpq.pl` | 23 | 240 |
| `pr_rules.pl` | 24 | 529 |
| `output.pl` | 43 | 408 |
| `dyncall.pl` | 81 | 804 |

The reasoner is well covered. What is not covered is mostly renderers
(`output.pl`) and the embedding layer (`dyncall.pl`).

**Line coverage is the wrong metric for this goal anyway.** It answers "did this
clause run", not "would I notice if the semantics changed". A reimplementation
shares no clauses with this one, so the number transfers to it only as a lower
bound on how thoroughly the *programs* exercise the language.

### Language constructs are well covered

Census of the 37 programs in `all_programs` and `min_programs`, counted on the
compiled form — what the solver actually receives, not what the source says:

| Construct in the compiled program | Programs |
|---|---:|
| `forall` | 30 |
| nested `forall` (two or more body-only variables) | 26 |
| CLP(Q) constraint | 22 |
| non-trivial NMR check (has sub-checks) | 17 |
| disequality against a list cell | 11 |
| disequality against a compound | 9 |
| `findall/3` | 2 |
| abducibles | 2 |

Thin: `findall/3` and abducibles. Dead: `#table` compiles but tabling is
disabled at `predicates.pl` behind `:- if(fail).`, so nothing executes it.

### Gap 1 — combinations are tested only incidentally

The plunit suites are one feature per file, so feature *crossings* fall between
them:

| Crossing | Tests |
|---|---:|
| `forall` × CLP(Q) | 12 |
| OLON × CLP(Q) | **0** |
| CLP(Q) × coinduction | **0** |
| compound terms × CLP(Q) | **0** |
| disequality × CLP(Q) | **0** |

Five of these were probed by hand — an NMR sub-check over a free variable
(the example in P4 §2.4), nested `forall`, `not -p` in a body, OLON × CLP(Q),
and even-loop coinduction × CLP(Q). **All five behave correctly.** So this is
not latent breakage; it is that nothing would report it if a rewrite broke them.

The golden corpus does exercise crossings — a real program uses several features
at once, which is why 26 of 37 programs generate nested `forall`s. But a golden
failure says *"queens.pl changed"*, not *"constraint propagation through
coinduction broke"*. Regression detection without localisation. During a rewrite
that difference is most of the debugging cost.

### Gap 2 — the suite pins bugs as expected behaviour

This is the one that actually blocks a reimplementation.

`test_compound.pl:compound_negation_is_incomplete` deliberately asserts the
*wrong* answer, because that is what the system does today (`doc/bugs.md` §15,
a limitation of the published method — see `doc/literature.md`). The entries in
`known_issues.pl` that generate `fail`-declared tests are the same thing.

**A reimplementation that fixes §15 fails the suite.** Nothing distinguishes

- *this is the semantics; any implementation must satisfy it* from
- *this is what we currently do; a better implementation may differ*

except prose in `doc/bugs.md` and in the test comments. That distinction has to
become machine-readable before the suite can be used as an acceptance test for
new code.

**Addressed** (first pass). Where the current answer is known to be wrong *and*
the right one is known, both expectations are now written, selected by
conditional compilation:

```prolog
:- if(expect_semantics).
test(the_unbound_answer_admits_every_shape_that_holds,
     [ set(Shape == [atom, other_functor, same_functor]) ]) :- ...
:- else.
test(the_unbound_answer_admits_only_the_f_shape,
     [ set(Shape == [same_functor]) ]) :- ...
:- endif.
```

`expect_semantics/0` (`scasp_plunit.pl`) is true when the Prolog flag
`scasp_expect` is `semantics`, set with `swipl -Dscasp_expect=semantics`. The
default is current behaviour, so `make check` stays green; `make check_semantics`
compiles the other branch and is **expected to fail**, its failures being the
outstanding defects. Because the expectation is executable rather than prose, the
failure message states the answer that is wanted:

```
test compound_negation:the_unbound_answer_admits_every_shape_that_holds:
wrong "set" answer:
    Expected: [atom,other_functor,same_functor]
```

Six defects are wired up: `doc/bugs.md` #1 (two `scasp/2` calls in one
conjunction), #9 (negated `findall/3`), #14 (`∉` unusable inside a program),
#15 (negating a structured head) and both halves of #16 (standard order and
arithmetic comparison on an unbound variable). Semantics mode reports exactly
those, as 10 failures against 443 passes.

Two categories deliberately have **no** semantics branch, each marked in place:

- *Undecided* — the current answer is wrong but the right one is a design
  decision not yet taken. Only the four empty Logical English scenarios remain
  here: each needs reading against its own source text before anyone can say
  whether "no answer" is the intended reading. Marked
  `% PINNED, no semantics branch yet.` with what has to be settled.
  (`doc/bugs.md` #16 was in this category and has since been decided.)
- *Artifact* — the assertion is about how many answers a solution set arrives
  in, which is an implementation choice rather than semantics. `X #< 1 ; X #> 1`
  and a single disequality describe the same set; CLP(Q) splits only because it
  has no native disequality. **Removed rather than marked:** `nth_answer/2` is
  gone from all four suites, and the tests now state the solution set with
  `answers_admit/4` (`scasp_plunit.pl`), which takes the union over every
  answer. `test_olon.pl` likewise compares the set of distinct models, with each
  model sorted, instead of counting answers.

That last distinction is worth holding to generally: **the solution set is
semantics; the number of answers it is split into is not.**

## What survives a change of data representation

A reimplementation will change how the compiled program is represented. To see
how much, here is what the compiler emits today for `p :- q(a), not p.`:

```prolog
pr_rule(not(o_chk_1),     [not(o__chk_1_1)], Ref)
pr_rule(not(o__chk_1_1),  [q(a), p],         Ref)
pr_rule(o_nmr_check,      [not(o_chk_1)],    Ref)
```

Heads wrapped in `not/1`; bodies as lists; module and arity interned into the
functor name; `o_` escape prefixes; a clause reference threaded alongside. Every
one of those is a decision that a rewrite is likely to take differently — and
`modules.pl`'s interning is explicitly on the table, since it exists only because
the solver is the bottleneck.

**So: no test may assert on a `pr_rule/3` term, a stack, or a tree node shape.**
This is the same trap `test/proof_skeleton.pl` already avoids for justification
trees, generalised to the rest of the pipeline.

The rule that follows: **white-box tests assert derived properties of a stage,
never the stage's output term.**

| Stage | Representation-independent contract | Verdict |
|---|---|---|
| `input.pl` | round-trip: read → list → re-read → same behaviour | worth it, cheap, untested today |
| `comp_duals.pl` | **complementarity** — for ground `t`, exactly one of `p(t)` and `not p(t)` succeeds; checked by *executing* both | highest value |
| `call_graph.pl` / `nmr_check.pl` | the **set** of predicates in an odd loop, as name/arity pairs | the one genuine white-box assertion that survives anything |
| `clp/clpq.pl`, `clp/disequality.pl` | the negated store's solution set is the complement of the original's, sampled over a domain | worth it |
| `solve.pl` | models exactly; proofs at a declared skeleton level | already black-box; keep |
| renderers | smoke-level, paired `html.pl` / `html_text.pl` assertions | already the policy |

Complementarity is the sharpest instrument on that list. It is the *definition*
of a dual, it runs against any implementation, and it would have found §15
mechanically rather than by accident. Stating the known exceptions as explicit
exclusions also makes them machine-checked: the day compound heads are fixed,
the test says so.

The OLON set is the one place where inspecting the compiler's own output is both
safe and valuable — "which predicates are in an odd loop" is a property of the
program's call graph, not of any data structure chosen to represent it.

### Reading the compiled program from inside Prolog

Do **not** shell out to `scasp --code` for this. The embedded interface exposes
the same listing in-process:

```prolog
?- with_output_to(string(S),
                  scasp_show(Query, code(query(false), user(false),
                                         constraints(true)))).
```

`scasp_show/2` (exported from `library(scasp)`, implemented in `dyncall.pl`)
takes `code(Options)`, and `listing.pl` documents the options:

| Option | Default | Selects |
|---|---|---|
| `query(Bool)` | `true` | the query |
| `user(Bool)` | `true` | the user program |
| `duals(Bool)` | `false` | the dual rules |
| `constraints(Bool)` | `false` | the NMR check / integrity constraints |
| `dcc(Bool)` | `false` | the DCC rules |
| `human(Bool)` | `false` | human-readable form |
| `write_program(Detail)` | — | `short` / `mid` / `long` presets, as the CLI uses |
| `source_module(M)` | — | module to unqualify against |
| `code_file(File)` | — | write to a file instead of current output |

`with_output_to/2,3` captures it; `code_file/1` writes it straight to a file if
that is more convenient.

Two things worth knowing, both of which cost time to rediscover:

- **Plain `--code` is `mid`** — `detail(-, -, -, mid)` in `options.pl` — so it
  shows the query, the user program and the duals, but **not** the NMR check.
  Integrity constraints need `--code --long`, or `constraints(true)` here. It is
  easy to conclude from `--code` alone that a program has no NMR check when it
  has one.
- Selecting *parts* is exactly what a stage-level test wants: ask for
  `constraints(true)` alone and the answer is the NMR check and nothing else.

## What is missing entirely: an independent oracle

Every oracle the suite has is derived from this implementation. The `.pass` data
records what this code does, bugs included. The sibling checkout at
`/home/jan/src/prolog/sCASP` is the same codebase on `master`, so a differential
run against it validates *the port*, not the semantics. No `clingo` is installed.

The fix needs no external dependency: **a brute-force stable-model checker for
the finitely-groundable propositional fragment**, written straight from the
Gelfond–Lifschitz definition —

1. ground the program over its own constants;
2. enumerate subsets of the Herbrand base;
3. for each, take the GL reduct and compute its least model;
4. keep the subsets that equal their own reduct's least model.

That is roughly fifty lines and fast enough to about a dozen atoms. It is an
independent implementation *of the definition*, so it shares no design decision,
no data representation and no bug with either the current system or its
replacement.

Paired with a small random program generator — normal rules, default negation,
classical negation, odd loops, global constraints — it gives systematic
differential testing of exactly the feature crossings the hand-written suites
miss, and it works unchanged against the new implementation.

Limits, stated plainly: it covers only the finitely-groundable fragment. Dense
domains, unbound variables in answers and the constraint stores are out of its
reach — that is what the complement-by-sampling property tests are for. But the
finitely-groundable fragment is precisely where the *compiler's* combinations
interfere (duals × OLON × classical negation), and where the semantics are
unambiguous enough for a definitional oracle to be authoritative.

## Plan, in order

1. ~~**Tag every test `semantics` or `pinned`.**~~ **Done** — see *Gap 2*
   above. Six defects carry an executable statement of the answer they ought to
   give; the answer-count tests are gone in favour of solution sets; only the
   four Logical English scenarios remain undecided, and those need domain
   reading rather than a design decision. Note that #16's two halves are decided
   but *not implemented* — the tests state the target, `make check_semantics`
   measures the distance to it.
2. **Brute-force GL oracle plus random program generator.** The highest-value
   addition by a distance: the first oracle in the project that is independent of
   the implementation.
3. **Complementarity property test for duals**, with the known exceptions
   declared as exclusions.
4. **OLON-set assertion** for `call_graph.pl` / `nmr_check.pl`, read through
   `scasp_show/2` with `constraints(true)`.
5. **Fill the four empty crossings**: OLON × CLP(Q), CLP(Q) × coinduction,
   compound terms × CLP(Q), disequality × CLP(Q).
6. Thicken `findall/3` and abducibles, the two constructs with only two
   programs each.

Items 1 and 2 are the ones that change what the suite is *for*. The rest are
filling in a shape that already works.
