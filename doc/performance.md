# Performance and the golden corpus

Where the reimplementation in `prolog/sscasp/` stands against the
implementation it is replacing, `prolog/scasp/`.  Every number here was
measured on the machine this was written on, with both implementations
**loaded into one process** and run one after the other, so the comparison is
not across machines or across runs.  Reproduce it with the harness sketched at
the end; re-measure before quoting any of it.

The claim this document supports is narrow: *the new implementation answers
the corpus the way the old one does, except where the old one is known to be
wrong, and is faster overall* — markedly on long deterministic proofs, and
on the event calculus now at parity with an implementation that checks less
(`doc/design-next.md` §5d, §5e).

## The corpus

Forty-nine programs in four corpora, each named by the directory it lives in,
each carrying `.pass` data recording what the old implementation answers:

| corpus | programs | |
|---|---:|---|
| `all_programs` | 21 | the regression corpus |
| `sasp` | 14 | from the original s(ASP) distribution |
| `min_programs` | 13 | one feature each, cut down |
| `dcc` | 1 | recorded under `--dcc` |

They overlap — `hanoi_7` is in two, `bec_loan` in two others.
`test/sscasp/corpus.pl` describes the comparison and
`test/sscasp/test_corpus.pl` turns it into tests.

The `.pass` files are **not** an independent oracle — they record what one
implementation produces.  What judges *correctness* is `test/stable_models.pl`,
a brute-force Gelfond–Lifschitz oracle that shares no design decision with
either implementation.  The corpus can only say whether the two agree.  Both
matter, and they answer different questions.

## The comparison

All answers enumerated, CPU time, the median of three runs (one where a run
took over 5 s), 60 s limit per run, both implementations in one process.  The
old implementation runs `dcc/` under `--dcc`, which forces `--forall=prev`,
as its golden data was recorded; the new one runs everything with its
defaults, dynamic consistency checking included.

- **speed** is old ÷ new, so **above 1× the new one is faster**; "–" where
  either took under a millisecond or the new one timed out.
- **answer**: the new implementation binds the query as the golden data does
  (`answers_agree/1`); **model**: its models match as well (`agrees/1`).
- **disposition**: `corpus.pl:disposition/2` — `gap(slow)` is a to-do,
  `golden` means the old answer is known to be wrong and the new one right.

| program | old answers | old s | new answers | new s | speed | answer | model | disposition |
|---|---:|---:|---:|---:|---:|:-:|:-:|---|
| all_programs/bec_light | 2 | 0.037 | 2 | 0.010 | 3.9× | ✓ | ✗ |  |
| all_programs/bec_loan | 4 | 0.203 | 4 | 0.072 | 2.8× | ✓ | ✗ |  |
| all_programs/bec_tap_01_overflow | 1 | 0.021 | 1 | 0.024 | 0.9× | ✓ | ✗ |  |
| all_programs/bec_tap_02_no_overflow | 1 | 0.028 | 1 | 0.020 | 1.4× | ✓ | ✗ |  |
| all_programs/bec_tap_03_two_models | 2 | 0.037 | 2 | 0.039 | 1.0× | ✓ | ✗ |  |
| all_programs/bec_tap_04_abducible_infer_event_time | 2 | 0.039 | 2 | 0.022 | 1.8× | ✓ | ✗ |  |
| all_programs/birds | 1 | 0.003 | 1 | 0.002 | 1.6× | ✓ | ✗ |  |
| all_programs/classic_negation_inconstistent | 0 | < 0.001 | 0 | < 0.001 | – | ✓ | ✓ |  |
| all_programs/family | 1 | 0.003 | 1 | 0.003 | 1.0× | ✓ | ✗ |  |
| all_programs/forall_arity | 0 | < 0.001 | 0 | < 0.001 | – | ✓ | ✓ |  |
| all_programs/hanoi | 1 | 0.040 | 1 | 0.008 | 4.9× | ✓ | ✓ |  |
| all_programs/hanoi_7 | 1 | 0.012 | 1 | 0.004 | 3.2× | ✓ | ✓ |  |
| all_programs/hanoi_scasp_7 | 1 | 0.015 | 1 | 0.005 | 3.2× | ✓ | ✓ |  |
| all_programs/haplotype | 8 | 0.001 | 8 | 0.002 | 0.7× | ✓ | ✓ |  |
| all_programs/min_model | 4 | < 0.001 | 4 | < 0.001 | – | ✓ | ✓ |  |
| all_programs/olon_unreachable | 0 | < 0.001 | 0 | < 0.001 | – | ✓ | ✓ |  |
| all_programs/pq | 1 | < 0.001 | 1 | < 0.001 | – | ✓ | ✓ |  |
| all_programs/queens | 2 | 0.996 | 2 | 0.105 | 9.5× | ✓ | ✓ |  |
| all_programs/rat | 1 | < 0.001 | 1 | < 0.001 | – | ✓ | ✓ |  |
| all_programs/solve_findall | 1 | < 0.001 | 1 | 0.003 | – | ✓ | ✓ |  |
| all_programs/vars | 1 | < 0.001 | 1 | < 0.001 | – | ✓ | ✓ |  |
| dcc/hamilton_c | 3 | 0.033 | 3 | 0.039 | 0.8× | ✓ | ✓ |  |
| min_programs/bec_loan | 4 | 0.199 | 4 | 0.072 | 2.8× | ✓ | ✗ |  |
| min_programs/bec_tap_04_abducible_infer_event_time | 2 | 0.039 | 2 | 0.022 | 1.8× | ✓ | ✗ |  |
| min_programs/clpq_gt_gte_lt_lte_equal | 2 | 0.002 | 2 | 0.004 | 0.5× | ✓ | ✗ |  |
| min_programs/clpq_intercept | 9 | 0.056 | 9 | 0.014 | 4.1× | ✓ | ✗ |  |
| min_programs/ex01-dual | 3 | < 0.001 | 3 | < 0.001 | – | ✓ | ✗ |  |
| min_programs/ex02-member | 1 | 0.003 | 1 | 0.003 | 1.0× | ✓ | ✓ |  |
| min_programs/hamiltonian_scasp | 2 | 0.095 | 2 | 0.066 | 1.5× | ✓ | ✓ |  |
| min_programs/nixon-diamond | 3 | 0.003 | 3 | 0.002 | 1.9× | ✓ | ✗ |  |
| min_programs/solve_findall_superset | 16 | 0.002 | 16 | 0.005 | 0.4× | ✓ | ✓ |  |
| min_programs/solve_not_is | 2 | 0.002 | 1 | 0.001 | 2.2× | ✗ | ✗ | `golden` |
| min_programs/solve_not_true | 0 | < 0.001 | 0 | < 0.001 | – | ✓ | ✓ |  |
| min_programs/stream_reasoning | 4 | 0.026 | 3 | 0.032 | 0.8× | ✗ | ✗ | `golden` |
| min_programs/yale_shooting_casp_v2 | 7 | 0.013 | 7 | 0.019 | 0.7× | ✓ | ✓ |  |
| sasp/abdbirds | 3 | 0.002 | 3 | 0.001 | 1.1× | ✓ | ✗ |  |
| sasp/gpa | 1 | 0.001 | 1 | < 0.001 | – | ✓ | ✗ |  |
| sasp/hamcycle | 1 | 0.009 | 1 | 0.004 | 2.4× | ✓ | ✗ |  |
| sasp/hamcycle_two | 1 | 0.025 | 1 | 0.024 | 1.0× | ✓ | ✗ |  |
| sasp/hanoi_7 | 1 | 0.012 | 1 | 0.004 | 3.2× | ✓ | ✓ |  |
| sasp/left-rec-succ_BAD | 1 | < 0.001 | 1 | < 0.001 | – | ✓ | ✓ |  |
| sasp/lion | 1 | 0.006 | 1 | 0.005 | 1.4× | ✓ | ✗ |  |
| sasp/loop_pos | 0 | < 0.001 | 0 | < 0.001 | – | ✓ | ✓ |  |
| sasp/loop_succ_pos | 1 | < 0.001 | 1 | < 0.001 | – | ✓ | ✓ |  |
| sasp/loopvar | 1 | < 0.001 | 1 | < 0.001 | – | ✓ | ✗ |  |
| sasp/loopvar2 | 1 | < 0.001 | 1 | < 0.001 | – | ✓ | ✗ |  |
| sasp/natnum | 1 | < 0.001 | 1 | 0.006 | – | ✓ | ✓ |  |
| sasp/path_loop | 3 | 0.007 | 3 | 0.003 | 2.7× | ✓ | ✗ |  |
| sasp/queens_old | 2 | 0.009 | 2 | 0.012 | 0.8× | ✓ | ✓ |  |

| | |
|---|---|
| agree on the answer | **47 of 49** |
| agree on the model as well | 25 of 49 |
| same number of answers | 47 of 49 |
| speed, geometric mean over the 32 measurable | **1.60×** |
| faster / slower (by more than 10%) | 20 / 8 |
| total | old 1.99 s, new 0.66 s |

Measured on 2026-10-02 — the NMR check committed to, fact
literals first and head guards, all three the defaults — with
`packages/clpqr` at `8209496`, on a lightly loaded machine.

**The two that do not agree on the answer** are the ones where the old
implementation is wrong: its dual of `V is X+Y` misses a case in
`solve_not_is`, and it misses a cancellation in `stream_reasoning`.
`bec_loan`, in both corpora, agrees since the NMR check is committed to: its
first answer always came quickly, and then the search backtracked into the
check for ever — one proof of the query, 14,843 of the check a minute, 35
models of one answer.  Committed, it gives the four golden answers in 0.07 s,
against 0.2 s for the old implementation.

**Answer counts differ** only where the answer does.  `sasp/hamcycle_two`
asks `?- chosen(1,2)` of a graph with two Hamiltonian cycles through that
edge, 0→3→1→2→0 and 0→1→2→3→0, both answer sets; the old implementation
stops at the first, and so does the new one now that the NMR check, which
is where the cycle is chosen, is committed to.  With `--no-commit-nmr` it
gives both.  (Before `for_domain/6` kept the proofs of its values, it gave
one answer here with fact unfolding off, and that answer was no cycle: see
`CLAUDE.md` under inferred domains.)

**Committing to the NMR check has a cost** where an answer's projection is
expensive: the query's state is read before and after the check, a
`dump/3` over the proof's whole CLP(Q) class, and `yale_shooting_casp_v2`
(seven answers) goes from 0.012 s to 0.026 s.  Nothing else moves.

**Models differ** where the proofs do — the model is a function of the proof,
so a solver that reaches the same answer by another route carries a different
model with it.  That is why agreement on the answer is the definition of
agreeing and agreement on the model a regression test.  `hanoi_scasp_7` and
`hamilton_c` say `#show`, and their golden models are the shown part only;
since `#show` is implemented both agree on the model too.  `queens` and
`ex02-member` have agreed since the pieces of a quantifier's complement are
kept in the proof (`8edd644`): the old implementation keeps them, so its
models hold what they proved, and ours left it out.

## Where the new implementation is faster, and why

`hanoi` (5.1×), `queens` (9.3×) and the other `hanoi` programs (3.3×) show
it, and come from the same changes:

- **Compiling to predicates instead of interpreting `pr_rule/3`.**  That is
  decision 1 of `doc/design-next.md` and it is most of the constant factor.
- **An index from `Name/Arity` to the goals proved under it.**  The
  coinductive-hypothesis check used to walk the whole proof forest per literal,
  which is quadratic in the proof.

`queens` also needed the quantifier to stop accumulating proofs across
complement branches — the proof reached 131,135 nodes holding 1.3M attributed
variables where the answer has 53 literals, which is what exhausted a 4 GB
stack.

**Inferred domains** are the rest: `hamiltonian_scasp` (1.5×, from 0.15×),
`bec_light` (4.0×, from 0.48×), `hamcycle` (2.5×) and
`solve_not_is` (from 0.26 s to under a millisecond).  A body-only variable
becomes a quantifier in the dual, and with nothing declared it ranges over
every term: the quantifier covers that universe piece by piece, and each piece
re-proves the body — `hamiltonian_scasp` checked 66,212 ground `node(...)`
literals and opened 2,794 quantifiers where the old implementation opens 232.
But a variable a positive literal of a predicate defined by ground facts
alone ranges over those facts' values: for any other the literal is false, so
the body is.  `dualise.pl:var_domain/4` now reads that off the program, and
the quantifier enumerates the values (`for_domain/6`) where it used to
search — the `:- domain` the program did not write.  Declaring it by hand
took `hamiltonian_scasp` to 0.063 s, which is how this was found.

**Unfolding fact literals** goes one step further, at compile time
(`unfold.pl`).  A program compiled for the data it runs on — a rule base
compiled once per request, the request's data being its facts — knows which
facts a literal can match, so a positive literal of a fact predicate whose
variables are all body-only is bound to its one fact, or the clause split
over its few, before anything is dualised.  The dual then has no body-only
variable and no quantifier.  A literal no fact matches, or whose predicate
has no clauses at all, makes the clause's dual its negation alone, recorded
without a search (`absent/1`): proving `not obs(Kind, V, Unit, current)`
by searching for the values V does not take was the most expensive way to
say that there is no such observation.  On a large rule base of that kind,
without the `scasp_expand/4` rewriting it had used to do this by hand, the
run went from 251 s (59 of 79 tests timing out) to 13 s, against 9.6 s with
its own rewriting; the quantifier calls of one test went from 395k to a few
hundred.  The generator of an inferred domain also keeps the literal's
ground arguments now, so `state(Item, pause, duration, D, day)` enumerates
the durations rather than the value of every state.

**Derived predicates** are unfolded the same way when their answers are
known at compile time.  `unfold.pl:answer_sets/3` computes what an ASP
grounder computes for its domains: the least fixpoint of the program read
bottom-up with every negated literal taken as true, ground built-ins
evaluated, and constructs and `:- sld` predicates taken as true — an
over-approximation of every stable model.  A predicate keeps its ground
answers and the few heads it may prove with variables, so one clause with
an open head does not hide the answers of the others.  In the same rule
base a predicate giving the current dose of a drug has one answer, and
binding it in every rule that compares the dose took a group of tests from
11 s to 1.4 s, one of them from 9.7 s to 0.15 s — faster than the rule
base's own rewriting, which needed a hand-picked list of predicates to do
it.  Its whole suite went from 49 s to 34 s wall, 117 s to 62 s CPU.  The
answers are only used to *bind*: a literal of a derived predicate with no
answer is still proved false, as that proof is the explanation of why a
rule does not apply.

**Committing to the NMR check** (`sscasp_commit_nmr`, the default) is what
makes `bec_loan` answer at all: one proof of its query and then, without
it, thousands of proofs of the check, each the same answer.  Committed, its
four answers take 0.07 s against the old implementation's 0.2 s.

**Head guards** (`heads.pl`) check that a rule literal's clause heads can
match before its body is proved, and in the duals close a branch the same
way.  The event calculus is where it shows: `holdsAt(level(H), 8)` no
longer proves when the tap overflows, in every cycle, before finding that
spilling has no trajectory for a level.  `bec_tap_01` 0.36 → 0.024 s,
`bec_tap_02` 0.31 → 0.020 s and `bec_tap_03` 0.73 → 0.039 s, through the
consistency check's duals — `bec_tap_03` once a head that unfolding had
duplicated no longer counted as two that overlap; a generated tap without
`-holdsAt` stays at 4–6 ms for one to six cycles.
**Fact literals first** (`unfold.pl:facts_first/3`) removed a cliff past
`sscasp_unfold_limit` in the same programs.  `doc/design-next.md` §5e has
both.

## Where it is slower, and why

A handful of small programs, by a few milliseconds: `hamilton_c` (0.8×),
`bec_tap_01` (0.9×, 3 ms) and the ones whose compile-time unfolding is most
of their time.  The paragraphs below are how the event-calculus family got
to parity, and the times in them are those of their day.

*The complement's pieces are proved in one proof.*  They used to be
covered in a run that `fail` undid, keeping only what they said about the
goal's other variables, and that was unsound: a piece rests on the
coinductive hypothesis, so with nothing kept the next piece could rest on
the opposite choice of an even loop and a program with no answer set was
answered (`test_differential.pl:the_values_of_a_derived_domain_hold_together`).
The run existed for speed: the pieces invent variables, CLP(Q) keeps every
variable a constraint reached in one class, and the class is projected
(`dump/3`, Fourier–Motzkin) at every covering.  Kept, the class grows, and
`bec_tap_01`–`03` went from 0.05–0.11 s to 1.2–2.8 s; half of that was the
loop check walking the store with `term_attvars/2` and is back
(`solve.pl:has_store/1`), leaving 0.5–1.3 s.  The projection is the rest.
`doc/design-next.md` §5d has what was measured and the design for keeping
the class small that did not survive it.  `hamilton_c` (0.024 → 0.038 s,
now 0.8×) pays a little of the same.

*CLP(Q) itself was then made faster* (`packages/clpqr` `2f89d7f`), at the
two places where Fourier–Motzkin's cost grew with the class rather than with
the projection: choosing the variable to eliminate walked the whole class
once per candidate, and every elimination step re-checked every bound in
the class for redundancy, where only the inequalities the step adds can be
redundant.  Neither the projections nor the stores held any redundant
constraint to remove.  It takes `bec_tap_01`–`03` to 0.34–0.80 s, about
40% off, and with identical answers; the old implementation gains on
`bec_light` (0.075 → 0.038 s) and `bec_tap_04` (0.048 → 0.037 s).  What is
left is still the projection, over classes the kept pieces make larger.

*The formulation is the larger lever.*  The cost of these programs is the
classical-negation consistency check `:- -holdsAt(F,T), holdsAt(F,T)`, which
the BEC theory's two `-holdsAt` rules bring with them.  Without those rules
`bec_tap_01`–`03` answer in 4–14 ms with the same answers, and a generated
lights scenario stays at 2–3 ms where the theory as written goes from 9 ms to
over a minute; `doc/design-next.md` §5e has the experiments.

Before that, for the event-calculus ones it
was the NMR check, not the query: `bec_tap_01` answers each of its ten
documented queries in 6–19 ms with the check off.  Their quantifiers range
over times, rationals no fact bounds, so inferring a domain does not reach
them: the cost is a CLP(Q) projection (`dump/3`, Fourier–Motzkin) at every
covering — 390 on `bec_tap_01` against the old implementation's 155 — which
is small in absolute terms, 0.07 s against 0.02 s.

*Unfolding is paid at compile time.*  Computing the answers of derived
predicates is a bottom-up fixpoint over the whole program, run on every
compile, and on a program that answers in a millisecond it is most of the
time: `natnum` 0.001 → 0.006 s (its fixpoint runs to
`sscasp_answer_limit`), `solve_findall`, `solve_findall_superset`,
`haplotype` and `clpq_gt_gte_lt_lte_equal` each 1–3 ms.  It is what
programs compiled once per request are made faster by, so the trade is
intended, but a cheaper fixpoint would win these back.
`hamiltonian_scasp` shows the two unfoldings pulling apart — 0.035 s with
neither, **0.161 s** with fact unfolding alone, 0.050 s with both — so
splitting a clause over its facts can cost more than the quantifier it
removes; that is the case to look at if fact unfolding is revisited.

*The quantifier reads what the CHS check posts.*  It used to read a variable's
disequalities with `dif:attribute_goals/3`, a hook that **deletes** what it
reports, and so covered domains it had not covered: `lion.pl` proved
`-live(rat, apartment)` and `not -live(rat, apartment)` at once.  Reading
them properly is not optional, and every value the coinductive-hypothesis check
excludes is a piece of domain the quantifier now covers — one sub-check of
`bec_tap_01` went from 8 coverings to 4,859.

*Backtracking into the check used to re-run coverings for nothing.*  When the
answers are enumerated, each one backtracks into the NMR check's quantifiers,
which ask `elsewhere/7` for an alternative: it re-ran the whole covering and
then partitioned the goal's other variables against what it bound them to.  A
sub-check has arity zero, so there were no other variables and nothing to find.
Skipping that run took `hamiltonian_scasp` from 3.1 s to 0.6 s — its first
answer came at 0.15 s and 2.9 s went on retrying the check after it — and
roughly halved every program with more than one answer.  Before it the
geometric mean was 0.76×.

*Dynamic consistency checking* (`sscasp_dcc`, on by default) prunes a branch
as soon as it completes a global constraint.  It changes no answer and takes
about 12% off the corpus when it was measured, most of it on
`hamiltonian_scasp` and `bec_light`;
the old implementation, where it is off by default, gets 7× from it on
`hamilton_c`, which this one already answers without the pruning.

## An external application

The golden corpus is small programs. One external project, a
decision-support rule base that is not part of this repository, has been
measured with both libraries. Its suite runs against either one unedited, so
it says what the difference is on a real program rather than on a benchmark.
It is reported here in aggregate only.

Its shape is unlike the corpus:

- **One program per request.** The rules are compiled together with the
  request's data, which arrive as facts, so every request pays a compile.
  One request compiles to about 300 clauses (304 and 326 in the two domains
  measured).
- **One wide predicate.** The rules that draw conclusions are clauses of one
  predicate, about 94 of them, and the messages they produce are clauses
  whose heads overlap. Neither qualifies for a head guard (more than 16
  clauses, or heads that unify), so `sscasp_head_guards` does nothing here.
- **Negation over absent data.** Much of the reasoning is "there is no such
  observation", that is, `not` of a fact predicate with variables not
  bound by the head. The quantifiers range over Herbrand values, not over
  CLP(Q) regions.

Measured on 2026-10-01, wall and CPU time for the whole suite with 16 jobs:

| | old library | sscasp | |
|---|---:|---:|---:|
| whole suite, wall | 145.5 s | 35.9 s | 4.1× |
| whole suite, CPU | 777 s | 62.7 s | 12× |
| the largest group of tests (79), wall | | | 34.8× |

Most of the difference is compile-time unfolding, described above. Without
fact unfolding the largest group took 251 s, with 59 of its 79 tests timing
out. Binding derived predicates with a single answer took it from 11 s to
1.4 s. Because the quantifiers range over Herbrand values, keeping the
proofs of a quantifier's complement, which is what the event-calculus
programs pay for (see "Where it is slower"), costs nothing measurable here.

## Programs the old implementation cannot do

`test/scasp/known_issues.pl` lists three, and the new implementation answers two of
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

`make check` is `check_load` and then `test/test_all.pl` twice, once per
implementation: **525 tests and 433 sub-tests** against the old library,
**1,198 and 1,253** against the new one, 35 of whose shared tests are recorded
gaps.  About 7 and 13 s of CPU, 14 s wall for the whole target including the
executables' load checks.  Units run concurrently with `jobs(4)`.

The sweeps against the Gelfond–Lifschitz oracle are what the soundness claim
rests on:

| sweep | size | what it compares |
|---|---:|---|
| `sscasp/test_solve.pl:solve_differential` | ~420 seeded programs | the new solver, through its term-list interface |
| `test_differential.pl:diff_random` | 291 seeded programs | whichever library is under test — a shared suite, run against both |

Both compare **brave entailment**, which is what a goal-directed solver
computes: the solver answers `a` iff `a` is in some answer set, and `not a` iff
`a` is absent from some answer set.  That is sharp in both directions — it
catches an answer no answer set supports *and* an answer set the solver cannot
reach.

The counts are tuned so `make check` stays quick; the oracle is exponential in
the Herbrand base while the solver is not.  Widening one is a one-line change
to the `shape/5` facts, and the widened sweep — **8,408 programs**,
propositional, classically negated, first-order and CLP(Q) — is clean.  Run
that before landing anything that changes what the solver means; it is 40 s.

## Reproducing this

Both implementations in one process, per program, all answers:

```prolog
:- use_module(library(sscasp/run),    [scasp/3]).      % new
:- use_module(library(sscasp/source), [read_program/2]).
:- use_module(library(scasp/compile)).                  % old
:- use_module(library(scasp/solve)).

new(File, Count, Time) :-
    read_program(File, S),
    statistics(cputime, T0),
    findall(x, scasp(S.terms, S.query, _), As),
    statistics(cputime, T1), Time is T1-T0, length(As, Count).

old(File, Count, Time) :-
    statistics(cputime, T0),
    scasp_load(File, [unknown(fail)]),     % for dcc/: scasp_dcc and
                                           % scasp_forall = prev first
    scasp_query(Q, _, []),
    findall(x, solve(Q, [], _, _), As),
    statistics(cputime, T1), Time is T1-T0, length(As, Count).
```

Run it with `swipl -p library=prolog` from the repository top, or the pack's
`library(scasp)` resolves to a different checkout — see the gotcha in
`CLAUDE.md`.  Wrap each call in `call_with_time_limit/2`; `bec_loan` needs
several minutes and everything else is under a second.  The table above took
the median of three runs and read agreement from `corpus.pl`'s
`answers_agree/1` and `agrees/1`.
