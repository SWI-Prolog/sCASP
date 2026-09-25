# What the reimplementation answers differently

`prolog/sscasp/` against `prolog/scasp/`, for a reader who has programs
running on the old one and needs to know what moves.

Every entry below was established by running both implementations on the same
program, not by reading either.  Where one of them is *wrong*, the judge is
`test/stable_models.pl` — a brute-force Gelfond–Lifschitz oracle that shares no
design decision with either — or, for programs outside its fragment, a
hand-checked argument stated with the entry.

Three kinds of difference, kept apart because they matter differently:

1. **Gaps closed** — the old implementation is wrong and the new one is right.
2. **Gaps open** — the other way round, or new.
3. **Choices** — both are right and they differ anyway.

Then what is missing, and what is *shared* — limitations that look like
differences and are not.

## 1. Gaps closed

### A positive loop reached through an even number of negations

```prolog
p :- not q, r.        % the only answer set is {q}
q :- not r.
r :- p.
?- p.
```

| | |
|---|---|
| old | 1 answer, model `[p, not q, r]` |
| new | **no answer** |

`p` needs `r` and `r` needs `p` — a loop over no negation, so unfounded.  The
route is `p` → `not q` → (`q`'s dual needs `r`) → `r` → `r :- p` → `chs(p)`,
which crosses two negations, so the even-loop rule lets it stand; then `r` is
reused for `p`'s own body *as already proved*, closing the positive loop
unnoticed.  The old implementation answers `p` and then fails `r` — a literal
in the model it just reported.

The fix is that `proved/4` records whether a proof rested on the coinductive
hypothesis and `check/7` reuses only one that did not.  P4 §3.2 offers the
reuse as an optimisation ("it avoids recomputation"); qualifying it costs 4% of
the differential sweep and nothing measurable on the corpus.

Found by the oracle, on a 4,410-program sweep.

### `clpq_equality.pl` terminates

`test/scasp/known_issues.pl` records it as a hang for the old implementation — a
ten-line `mutually_exclusive/2` plus `#=` program that on its own was two
thirds of `make check`.  The new implementation answers it in **0.008 s**.

### The consistency check the old implementation does not complete

In `test/all_programs/bec_tap_01_overflow.pl`:

| | |
|---|---|
| `-holdsAt(filling, 25)` old | **no** |
| `-holdsAt(filling, 25)` new | **yes**, 3 ms |

`tapOff` terminates `filling` at 20 and nothing restarts it, so the fluent does
not hold at 25 and its classical negation does.  The old implementation fails
it, and that failure is load-bearing: it is why the old implementation's NMR
check for that program is cheap.  It covers
`forall([F, T], not chk(F, T))` out of the first dual clause alone,
`not -holdsAt(F, T)`, and never calls `not holdsAt` at all — a covering that
cannot be right, since `not -holdsAt(filling, 25)` is false.

This is the one place where the old implementation is faster *because* it is
wrong, and it is worth knowing before reading the timings in
`doc/performance.md`.

### Negating a structured head over a constrained variable

```prolog
p(f(X)) :- X #> 3, X #< 10.
?- not p(f(V)).
```

Both implementations now answer the two regions `V =< 3` and `V >= 10`.  The
new one briefly did not — it reported a single answer constraining nothing,
while `not p(f(5))` correctly failed, so the general answer contradicted the
specific one.  `solve.pl:between_kept/6` is the fix and
`test_crossings.pl:a_dense_region_under_a_compound_head` is the guard.  Listed
here because it is the shape `doc/bugs.md` §15 is about, and because the
crossing suite is what found it.

## 2. Gaps open

### `bec_loan` is 440× slower

It answers — the first answer takes **376 s** against 0.85 s — so this is a
search-size difference and not a hang.  `CLAUDE.md` records the five
hypotheses that have been measured and ruled out.  Nothing about the *answers*
differs; when it finishes it agrees.

### `not findall/3` is refused rather than failed

```prolog
d(1). d(2).
all(L) :- findall(X, d(X), L).
p :- not all([1, 2]).
?- p.
```

| | |
|---|---|
| old | 0 answers, silently |
| new | raises `scasp_error(negated_findall(...))` |

Neither answers it.  The new behaviour is louder, which is better for a
program author and worse for one who was relying on the failure.

### `findall/3` puts the collection's proof in the model

`findall/3` in the new implementation *solves* its goal rather than calling it,
so every literal the collection walked through is in the model.  That matches
the old implementation's `.pass` data for `solve_findall.pl`, which lists every
`member/2` the collection saw — but it is a decision worth knowing about,
because a large collection makes a large model.

## 3. Choices, not defects

### The model carries the negative literals the closure makes true

```prolog
p :- not q.           % q is never defined
?- p.
```

| | |
|---|---|
| old | model `[p]` |
| new | model `[p, not q]` |

The new compiler **closes** the program: a predicate called but never defined
is false of everything and its negation true of everything, and both are
written out.  So `not q` is an ordinary proved literal and belongs in the
model.  Neither reading is wrong; a consumer that compares models literally
will see it.

This is most of why only **11 of 21** corpus programs agree on models where 19
agree on answers.  The other part is that a model is a function of the proof,
so two solvers reaching the same answer by different routes carry different
models — and in the new implementation a model no longer contains what only a
*complement branch* of a quantifier established.

### How many answers one solution set arrives in

```prolog
p(X) :- X #= 3.
q(Y) :- not p(Y).
?- q(Z).
```

| | |
|---|---|
| old | 2 answers: `Z < 3` and `Z > 3` |
| new | 1 answer: `Z =\= 3` |

Same set of solutions.  The old one splits the complement of an equality into
two regions of the dense order; the new one uses the CLP(Q) disequality
directly.  Any test that counts answers sees this.

### Coinduction is four rules, not six

Compiling to predicates makes a goal's polarity static, so the number of
negations on a path is the number of **polarity transitions** along it, and the
six cases the old solver distinguishes collapse to four.  There is no odd case:
two frames of the same polarity have an even number of transitions between them
by construction, so an odd loop reaches the goal's *negation* instead.  No
program in the corpus or the sweeps answers differently because of it.

### A loop is decided on the question, not on the store

`=@=` and `subsumes_term/2` take attributes into account, so a recursion whose
steps constrain their own variable never looks like a loop under a plain
variant test.  `solve.pl:same_question/2` compares the bare terms and then
requires the goal to be an *instance* — the ancestor's disequalities among the
goal's, and its dense store entailed by the goal's.  The old implementation
states the same rule as `clp/clpq.pl:entail_terms/2`.  This is the same
semantics arrived at by a different route, and it is what makes `bec_loan`'s
query terminate.

## 4. Not implemented in the new tree

Language and interface, not semantics — a program using one of these does not
run rather than running differently:

| | |
|---|---|
| the four `forall` algorithms | the new tree has one; `scasp_forall` selects nothing |
| `--dcc` | disjunctive consistency checking |
| `begin_scasp`/`end_scasp` | deliberately, per `doc/design-next.md` decision 2a |

**The new tree stands on its own.** `prolog/sscasp/` loads nothing from
`prolog/scasp/`; the stylesheet and script it used to share are copies,
and `main.pl` embeds the stylesheet at compile time so a saved state
carries it.

**The executable is there.** `make sscasp` builds it and it takes the
old one's flags where the solver has the feature: `--tree`, `--human`,
`--code` (with `--duals`, `--constraints`, `--source`, `--compiled`),
`--json`, `--html`, `--query`, `-s`/`-n`, `--width`, `--colour`,
`--no-nmr`, `--no-olon`, `--version`. It exits 0 for an answer, 1 for
none and 2 for an error, which the old one does not distinguish.

`-i` is there and differs in two ways, both deliberate. The program is
compiled **once** for the session rather than per query — on
`bec_tap_01_overflow` that is 92 ms of setup against 1.8 ms per further
query. And a query that does not compile, or names a predicate the
program does not have, is **reported and survived**: the old loop has no
`catch/3`, so one typo ends the session and exits 1. Away from a
terminal the loop does not offer answers one at a time, because
`get_single_char/1` would read the next query's first character; it
honours `--answers` instead, which is what makes `-i` scriptable.

**The renderers are all there.** `sscasp/html.pl` describes an answer once
and `html_text.pl` reads that description as coloured text, which is
`--human` (`scasp_human/1,2`); `sscasp/render.pl` prints the machine form
in columns and colour, which is what the `?`-shortcuts hand the toplevel;
`sscasp/listing.pl` is `--code`; and `sscasp/json.pl` is `--json`. What
each does differently is below.

`library(sscasp)` covers the rest of the Prolog interface: `scasp/1,2`
over ordinary Prolog clauses, the `?`-prefixed query shortcuts,
`scasp_show/2`, and `scasp_directive/1` for what Prolog has no syntax
for. A global constraint is written `false :- Body.` in both.

**Building a program at run time is there too**, and differs in one way
worth knowing when porting. `scasp_dynamic/1`, `scasp_assert/1,2`,
`scasp_retract/1`, `scasp_retractall/1` and `scasp_abolish/1` mean what
they mean in `library(scasp)`, but the old implementation *interns*
`-p(X)` into a predicate literally named `'-p'` and so has to rename the
functor on the way in; here `-p(X)` is a clause of `-/1` and the term is
asserted as written. Two consequences:

- `scasp_dynamic p/1` declares `p/1` and `-/1` — one predicate holding
  every negated head in the module, so its scope is decided by the first
  declaration to reach it rather than per predicate. The old one gives
  each its own `'-p'/1` and can scope them separately.
- `scasp_assert/2`'s position argument is accepted and ignored. This
  solver keeps no source references for a justification to point at.

A global constraint cannot be asserted in either tree — `false/0` is a
built-in — so `scasp_assert(M:(false :- Body))` **records** it, which is
what the load-time expansion of the same term does.

**The Logical English examples run against both.** `test/le_corpus.pl`
describes the 13 programs in `test/le_programs/`, and the shared suite
`test/test_le.pl` runs it against either library. 12 of 13 agree.
`list-scasp` defines its own `forall/2` and is refused by both — each
naming a different unsupported thing first; `subset-scasp` does the same
and both name the redefinition. `turingcomplete-scasp` agrees
by a different route: it calls `append/3` without defining it, which
`library(scasp)` reads as a predicate the program never defined (so the
query fails) while `library(sscasp)` follows the name into
`library(lists)` and collects its clauses.

**Each library reads only its own files.** Both install their expansions in
`user`, so unguarded each saw every file in the process and which had loaded
first decided what a `-p(X)` head, a `:- pred` or a `#pred` meant. Each now
asks whether the file being compiled imported it
(`predicate_property(M:scasp(_,_), imported_from(...))`), so the two never
both fire. This is a change to `library(scasp)` as well as to
`library(sscasp)`.

**A redefined control construct is refused rather than reinterpreted.**
A module defining `forall/2`, `findall/3`, `not/1` or another goal the
compiler owns has written clauses that can never run, because the goal is
intercepted before anything looks for them. Both libraries refuse, with
the same error term.

**Calling out to Prolog is spelled differently.** The old implementation
declares `:- prolog p/2.`; the new one declares `:- sld p/2.`, naming the
*resolution* rather than the language. Same two modes — `as opaque` runs
it, the default interprets it and shows the derivation — and the same
treatment of negation, which collects the solutions and constrains the
variable away from them rather than testing. The rename is not cosmetic:
a prefix operator called `prolog` makes `prolog:message//1` a syntax
error in every file that can see it, so a program using
`library(scasp)` cannot define a message hook.

**A natural-language template is spelled `:- describe`.** The old
implementation writes `#pred p(X) :: 'text'` and, in a Prolog module,
`:- pred p(X) :: "text"`. The new one reads `#pred` — the existing files
say that, and `source.pl` turns it into a description — but the spelling
going forward is

    :- describe flies(X) :: "@(X) can fly".

and `:- pred p(X) :: "..."` is deprecated and warns. The reason is that
`:- pred` already means something else here and something that changes
what is proved: `:- pred wrap(day)` is a **signature**, saying which
domain an argument ranges over, which is what lets the dual generator
bound a quantified variable. One directive cannot usefully be both.
`doc/design-next.md` decision 6a proposed the rename; this is it.

A description's variable is a singleton as far as the Prolog reader is
concerned — its other occurrences are inside a string — so underscore it
(`flies(_X)`; the leading underscore is ignored when a placeholder is
resolved).

**A description may also be HTML**, `:- describe p(X) :: html(...)`, which
the old implementation allows too. Two differences. A variable in the
template is rendered as a *value* here — the old one hands the template to
`html//1` as it stands, and a template mentioning its literal's variables,
which is the only kind worth writing, comes out as `<| $VAR="A">`. And the
markup is dropped rather than printed when the same description is read as
text.

**What is not read** is the old `#pred`'s subtree matching: `-` collapsing
what is under a literal, `^` gathering one from anywhere below, and a
condition on a spec. Nothing in the corpus or the examples uses it.

### `#abducible` and its negative half

The old implementation's two front ends do not agree with each other, so
there is no single thing to be compatible with.

`prolog/scasp/input.pl` — the **file** front end — makes the negative
half a *fresh* predicate:

    p                 :- not _p, abducible_1(p).
    _p                :- not p.
    abducible_1(p)    :- not '_abducible_1'(p).
    '_abducible_1'(p) :- not abducible_1(p).

so `#abducible p` is a choice between `p` and *not knowing* `p`, and
`?- -p.` reports that `-p/0` does not exist.

`prolog/scasp/dyncall.pl` — the **embedded** front end — makes it `-p`
(`intern_negation/2`), so `#abducible p` is a choice between `p` and `-p`
being *known*. `?- -p.` answers.

The new tree follows the second: `-p(X) :- abducible(-p(X))` is one of
the four rules. It is the stronger reading and the one a program can ask
about.

Having `-p`, the `:- p, -p` check applies. A predicate that is **only**
abducible gets none, because those four rules have the answer sets `{p}`
and `{-p}` and never both — the check is entailed, and generating it is a
free choice the solver must make for every query that can see the
declaration, doubling the answers. The old implementation avoids that a
different way: `scasp/2` does not collect an abducible the query cannot
reach, so the check is never generated in the first place.

Where the predicate has rules of its own the check is needed, and both
implementations refuse `-p` for

    #abducible p.  q.  p :- q.

whose single answer set is `{p, q}`.

### The model of an answer is smaller

Both implementations report a *partial* model — the literals the proof
needed — and the proofs differ, so the models do. On
`examples/dyncall/birds.pl` the old reports 24 literals and the new 13.
Two things account for it, and neither is a wrong answer:

- **A different covering of the same check.** `:- p(X), -p(X)` dualises to
  a quantifier whose body may hold by `not p(X)` or by `not -p(X)`, and the
  two implementations take different branches. The old reports `ab(john)`
  and `¬ flies(sam)`; the new reports `not ¬ ab(sam)` and
  `not ¬ flies(tweety)`. Both discharge the check.
- **A complement branch contributes no proof.** The quantifier's branches
  *check* without adding to the proof, so a literal established only while
  covering the rest of a domain is not in the model — which is where the
  old implementation's `¬ ab(_A)` with `_A ∉ [john,sam]` comes from. That
  is a measured decision rather than an oversight: threading each branch's
  proof into the next doubled the proof per level, 131,135 nodes on
  `queens.pl` where the answer has 53 literals, and exhausted a 4 GB stack.

What is *not* a difference any more is relevance scoping. `scasp/2` used
to answer a query about `p` without collecting `-p`, so the consistency
check was never generated — `q. p :- q. -p :- q.` has no answer set and it
said yes. `p` and `-p` are now one predicate as far as collection goes.

### `∉` is a store, not only a rendering

The old implementation's Herbrand store *is* `∉` — a sorted set of
prohibited ground values per variable — and it resolves a disequality
between compound terms by **backtracking**: `.\=.` picks one of the
positions where they differ and leaves the others as alternatives.

The new one keeps `dif/2` for that case (decision 4) and uses `∉` where
`∉` can say it: one variable against a ground term. `herbrand.pl` puts
both behind one interface, so which of the two holds a given disequality
is not visible to the solver or to a reader.

Two consequences for an answer:

- **No choicepoint for a compound disequality.** `dif(f(A,B), f(a,b))` is
  one constraint here and two answers there. Ours is the stronger
  statement and the cheaper search.
- **`X ∉ [a, b]` where the old also says `X ∉ [a, b]`.** The spellings
  agree where both can say it, which is most of what appears in an answer.

`X ∉ [a, b]` in a *program* works in both. Its dual is `X ∈ [a, b]` here,
which the old implementation does not have.

### The listing shows the dual, not the generated predicates

`--code` in the old implementation prints the compiled program with its
generated names — `not'o_-ab_1'(A) :- ab(A).` — and what it prints is
**loadable** (`--compiled`).

`scasp_show/2` inlines those helpers as a disjunction instead, so a dual
reads as what it is:

    not flies(A) :-
        (   not bird(A)
        ;   bird(A),
            ab(A)
        ).

That is a listing rather than a program — a sub-check prints as
`check(1)`, which is nothing anybody can call — and there is no
`--compiled` to read it back. `code(compiled(true))` shows the clauses
the solver runs, mangled names and all, for when the question is about
the emitter.

The layout is `portray_clause/3`'s in both implementations. Getting the
*operators* and `not -p(X)` through it needed two fixes in SWI-Prolog —
`listing/2` forwarding its options, and the writer putting a space
between a prefix operator and a following symbol — which are in
`swipl-devel` rather than here.

### Two answers the old implementation should not give

Found by widening the corpus comparison from 21 programs to 49
(`test/sscasp/corpus.pl`). Both are recorded as `golden(...)` dispositions
rather than gaps: the `.pass` data is wrong, and the test asserts the
disagreement is still there so that it cannot outlive a fix upstream.

`min_programs/solve_not_is`:

    invalid_value(X) :- not valid_value(X).
    valid_value(V)   :- coord(X), coord(Y), V is X+Y.
    coord(1). coord(2). coord(3).
    ?- invalid_value(X).

The old implementation answers `invalid_value(2)` as well as the
constrained answer. `valid_value(2)` holds — 1+1 — so it should not, and
its own justification shows why it does: under `not valid_value(2)` it
tries `2 is 2+1`, `2 is 2+2` and `2 is 2+3`, and never `2 is 1+1`. One
clause of the dual of `V is X+Y` is missing.

`min_programs/stream_reasoning` answers `valid_stream(1, p(a))`. That is
cancelled: `higher_prio(2, 1)`, `stream(2, q(a))` and `incompt(p(a),
q(a))` all hold. The new implementation's first answer is `p(A)` with
`A ∉ [a, b]` — the same answer with the cancelled individuals taken out —
and it gives three answers where the old gives four.

## 5. Shared limitations

Things that look like differences and are not, so that nobody reports them
twice:

- **`∉` in a program body** does not work in either.  The old
  implementation's `predicates.pl` does not list it in `scasp_builtin/1`, so
  `dyncall.pl` treats it as a user predicate; the new one has the operator
  declared in `ops.pl` and no implementation behind it.  `test_disequality.pl`
  pins the old behaviour.  Called directly as a Prolog goal it works in the
  old tree.
- **`call/1` is not a control construct** in either.  What differs is the
  consequence: `test/min_programs/solve_call.pl` gets 0 models from the old
  implementation and one answer from the new, because the closure makes
  `call(test)` an ordinary undefined predicate and `not call(test)` therefore
  true.  The model says so — `[exception(test), not call(test), process(test)]`
  — and that is a difference in what an *unsupported* construct does, not in
  what a supported one means.
- **`forall/2` written by the author** is solved by both and holds for no list
  pair in `test/min_programs/solve_forall.pl`; both answer 0.
- **Negating a structured head is incomplete for an unbound query**
  (`doc/bugs.md` §15) in both.  It is a limitation of the published method —
  Marple et al. work the same case through for `p([X|T]) :- q(X), p(T).` and
  state that calling the dual with a structure containing unbound variables
  encounters illegal cases.  A declared domain fixes it in both, which is why
  `doc/design-next.md` §6a proposes declaring such domains.

## How this was established

- **The oracle.** `test/stable_models.pl`, from the Gelfond–Lifschitz
  definition.  `test_solve.pl:solve_differential` runs the new solver against
  it on 420 seeded programs in `make check` and 8,408 when widened;
  `test_differential.pl` runs the old one against it on 291.  Both compare
  brave entailment, which is sharp in both directions.
- **The corpus.** `test/sscasp/test_corpus.pl` points the new implementation at the
  `.pass` data the old one produced.  That can only say whether they agree —
  it shares every design decision with the old one — which is why it is not
  the soundness argument.  `doc/performance.md` has the table.
- **The crossings.** `test/sscasp/test_crossings.pl`, 18 programs judged by the oracle
  and 9 by hand, for the feature combinations `doc/test-strategy.md` counted at
  zero tests.
- **By hand, both implementations in one process**, for everything in this
  document that is a single program.  The module names differ deliberately so
  that this is possible; `doc/performance.md` has the harness.
