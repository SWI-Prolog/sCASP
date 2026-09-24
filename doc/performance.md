# Performance and the golden corpus

Where the reimplementation in `prolog/sscasp/` stands against the
implementation it is replacing, `prolog/scasp/`.  Every number here was
measured on the machine this was written on, with both implementations
**loaded into one process** and run one after the other, so the comparison is
not across machines or across runs.  Reproduce it with the harness sketched at
the end; re-measure before quoting any of it.

The claim this document supports is narrow: *the new implementation answers
the corpus the way the old one does, and is faster on the programs that were
already fast.* It is not a claim that the new one is faster in general — on
five programs it is slower, for a reason given below.

## The corpus

`test/all_programs/` holds 22 programs, 21 of which carry `.pass` data
recording what the old implementation answers.  That data is the corpus;
`test/corpus.pl` reads it and `test/test_corpus.pl` turns it into tests.

The `.pass` files are **not** an independent oracle — they record what one
implementation produces.  What judges *correctness* is `test/stable_models.pl`,
a brute-force Gelfond–Lifschitz oracle that shares no design decision with
either implementation.  The corpus can only say whether the two agree.  Both
matter, and they answer different questions.

## Answer counts and times

All answers enumerated, 20 s limit, one process:

| program | old | new | ratio |
|---|---:|---:|---:|
| `hanoi` | 1, 0.040 s | 1, **0.006 s** | 7.0× |
| `hanoi_7` | 1, 0.012 s | 1, **0.003 s** | 4.7× |
| `hanoi_scasp_7` | 1, 0.015 s | 1, **0.003 s** | 4.6× |
| `queens` | 2, 0.961 s | 2, **0.209 s** | 4.6× |
| `birds` | 1, 0.004 s | 1, **0.002 s** | 1.8× |
| `vars` | 1, 0.0003 s | 1, **0.0002 s** | 1.7× |
| `bec_tap_04` | 2, 0.050 s | 2, **0.032 s** | 1.6× |
| `solve_findall` | 1, 0.0005 s | 1, **0.0004 s** | 1.4× |
| `family` | 1, 0.003 s | 1, **0.002 s** | 1.4× |
| `haplotype` | 8, 0.0012 s | 8, **0.0009 s** | 1.3× |
| `pq`, `min_model`, `olon_unreachable`, `forall_arity`, `classic_negation_inconstistent`, `rat` | | same count, 1.1–1.3× | |
| `bec_tap_02` | 1, 0.029 s | 1, 0.112 s | 0.26× |
| `bec_tap_01` | 1, 0.021 s | 1, 0.114 s | 0.19× |
| `bec_tap_03` | 2, 0.038 s | 2, 0.224 s | 0.17× |
| `bec_light` | 2, 0.061 s | 2, 0.393 s | 0.15× |
| `bec_loan` | 4, 0.209 s | **no answer in 20 s** | |

**20 of 21 match the old implementation's answer count.**

Two things are worth saying about the five event-calculus programs, since
they are the whole of the slower half.

*They are slower for a reason that is a correctness fix.*  The quantifier used
to read a variable's disequalities with `dif:attribute_goals/3`, a hook that
**deletes** what it reports.  A quantifier that cannot see a disequality covers
a domain it has not covered: `lion.pl` proved `-live(rat, apartment)` and
`not -live(rat, apartment)` at once.  Reading them properly is six lines and is
not optional, and what it costs is the measure of what those programs had been
getting for free — one sub-check of `bec_tap_01` went from 8 coverings to
4,859, because every value the coinductive-hypothesis check excludes is a piece
of domain the quantifier now has to cover separately.

*`bec_loan` terminates.*  Its first answer comes after 376 s.  The gap is a
factor of about 440 in search size, not a hang, and `CLAUDE.md` records the
five hypotheses that have been measured and ruled out.

## Agreement, at two strengths

Models legitimately differ where proofs do — the model is a function of the
proof, so a solver that reaches the same answer by another route carries a
different model with it.  So the corpus asks two questions:

- `answers_agree/1` — do the two bind the **query** the same way?  That is what
  was asked for, and a difference in it means something about the program.
- `agrees/1` — do the models match as well?  A real regression test, but not
  the definition of correct.

| | |
|---|---|
| agree on the answer | **19 of 21** |
| agree on the model as well | 11 of 21 |

The two that do not agree on the answer:

- `bec_loan` — the timeout above.
- `hanoi_scasp_7` — an artefact of the comparison rather than a disagreement.
  Its golden model has no query literal to read an answer out of, so there is
  nothing to compare; the golden model is also a strict *subset* of ours,
  missing the `not negmove(...)` that proving each `move(...)` needs.

## Where the new implementation is faster, and why

`hanoi` (7×) and `queens` (4.6×) are the two that show it, and both come from
the same two changes:

- **Compiling to predicates instead of interpreting `pr_rule/3`.**  That is
  decision 1 of `doc/design-next.md` and it is most of the constant factor.
- **An index from `Name/Arity` to the goals proved under it.**  The
  coinductive-hypothesis check used to walk the whole proof forest per literal,
  which is quadratic in the proof.

`queens` also needed the quantifier to stop accumulating proofs across
complement branches — the proof reached 131,135 nodes holding 1.3M attributed
variables where the answer has 53 literals, which is what exhausted a 4 GB
stack.

## Programs the old implementation cannot do

`test/known_issues.pl` lists three, and the new implementation answers two of
them:

| program | old | new |
|---|---|---|
| `clpq_equality.pl` | does not terminate | **1 answer, 0.008 s** |
| `min_programs/solve_call.pl` | 0 models (`call/1` unsupported) | 1 answer — but see below |
| `min_programs/solve_forall.pl` | 0 models | 0 answers, same |

`solve_call.pl` is not a fix and should not be read as one.  `call/1` is not a
control construct in either implementation; what differs is that the new
compiler **closes** the program, so a predicate called but never defined is
false of everything rather than undefined.  `call(test)` is therefore an
ordinary undefined predicate, `not call(test)` holds, and the program's second
clause proves the query.  The model says so plainly:

```prolog
[exception(test), not call(test), process(test)]
```

That is a difference in what an *unsupported* construct does, not in what a
supported one means.  `doc/semantics.md` treats it with the rest.

## The test suite

`make check` is a single `swipl` call: **900 tests, 1,018 sub-tests, 4.7 s**
(6.7 s wall including load).  `test/test_all.pl` loads every suite — the golden
tests included — into one process and runs whole units concurrently with
`jobs(4)`.

Two sweeps run inside it, and they are what the soundness claim rests on:

| sweep | size | what it compares |
|---|---:|---|
| `test_solve.pl:solve_differential` | 420 seeded programs | the new solver against the Gelfond–Lifschitz oracle |
| `test_differential.pl:diff_random` | 291 seeded programs | the *old* solver against the same oracle |

Both compare **brave entailment**, which is what a goal-directed solver
computes: the solver answers `a` iff `a` is in some answer set, and `not a` iff
`a` is absent from some answer set.  That is sharp in both directions — it
catches an answer no answer set supports *and* an answer set the solver cannot
reach.

The 420 is tuned so `make check` stays quick; the oracle is exponential in the
Herbrand base while the solver is not.  Widening it is a one-line change to the
`shape/5` facts, and the widened sweep — **8,408 programs**, propositional,
classically negated, first-order and CLP(Q) — is clean.  Run that before
landing anything that changes what the solver means; it is 40 s.

## Reproducing this

Both implementations in one process, per program, all answers:

```prolog
:- use_module(library(sscasp/run),    [sscasp/3]).      % new
:- use_module(library(sscasp/source), [read_program/2]).
:- use_module(library(scasp/compile)).                  % old
:- use_module(library(scasp/solve)).

new(File, Count, Time) :-
    read_program(File, S),
    statistics(cputime, T0),
    findall(x, sscasp(S.terms, S.query, _), As),
    statistics(cputime, T1), Time is T1-T0, length(As, Count).

old(File, Count, Time) :-
    statistics(cputime, T0),
    scasp_load(File, [unknown(fail)]),
    scasp_query(Q, _, []),
    findall(x, solve(Q, [], _, _), As),
    statistics(cputime, T1), Time is T1-T0, length(As, Count).
```

Run it with `swipl -p library=prolog` from the repository top, or the pack's
`library(scasp)` resolves to a different checkout — see the gotcha in
`CLAUDE.md`.  Wrap each call in `call_with_time_limit/2`; `bec_loan` needs
several minutes and everything else is under a second.
