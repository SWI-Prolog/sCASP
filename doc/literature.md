# The s(CASP) literature

s(CASP) is a port, twice over: from Marple's s(ASP) (Prolog, UT Dallas) to Arias'
s(CASP) (Ciao), and from that to this SWI-Prolog tree. Most of the design was
decided in papers rather than in this repository, and several predicates here are
direct transcriptions of an algorithm printed in one of them.

This file summarises those papers and says, for each idea, **which module
implements it**. It is not a replacement for reading them; it is the map that
tells you which one to open, and it records the places where this port knowingly
does something else.

Nothing here is a design document for the port — that is `doc/design.md`'s job
(not yet written). Bugs and open questions are in `doc/bugs.md`.

## The five papers

| # | Paper | Defines | Implemented in |
|---|---|---|---|
| **P1** | Marple, Salazar, Gupta, *Computing Stable Models of Normal Logic Programs Without Grounding*, 2017 — [arXiv:1709.00501](https://arxiv.org/abs/1709.00501) | dual rules for non-ground programs, head abstraction, `forall`, constructive disunification | `comp_duals.pl`, `clp/disequality.pl` |
| **P2** | Marple, *Goal-Directed Answer Set Programming* (PhD thesis, UT Dallas, 2014) — [pdf](https://personal.utdallas.edu/~gupta/kylethesis.pdf) | the NMR check, coinduction restricted to be faithful to Gelfond–Lifschitz | `nmr_check.pl`, `solve.pl:check_CHS/6` |
| **P3** | Marple, Gupta, *Galliwasp: A Goal-Directed Answer Set Solver*, LOPSTR 2012 — [pdf](http://galliwasp.sourceforge.net/publications/galliwasp.pdf) | the NMR check as a compiled `chk_i` predicate, and its optimisation | `nmr_check.pl` (cited in its header) |
| **P4** | Arias, Carro, Salazar, Marple, Gupta, *Constraint Answer Set Programming without Grounding*, TPLP/ICLP 2018 — [arXiv:1804.11162](https://arxiv.org/abs/1804.11162) | s(CASP) proper: CLP integration, `forall` (Alg. 1) and C-forall (Alg. 2), loop handling | `solve.pl`, `clp/clpq.pl` |
| **P5** | Arias, Carro, Chen, Gupta, *Justifications for Goal-Directed Constraint Answer Set Programming*, ICLP 2020 — [arXiv:2009.10238](https://arxiv.org/abs/2009.10238) | the justification tree and its detail levels | `stack.pl`, `html.pl`, `human.pl` |

A sixth document, the [s(CASP) manual](https://personal.utdallas.edu/~gupta/nfm-ex/scasp-manual.pdf),
is the user-facing reference and is the closest thing to a specification of the
CLI and the directives.

Pereira & Pinto's *Layered Models Top-Down Querying of Normal Logic Programs*
(PADL'09) is sometimes cited for OLONs; it is background for why goal-directed
querying of programs with odd loops is hard, not a description of what this code
does, and no module references it.

## P1 — Dual rules without grounding

The central problem: `not p(X)` must *return the X for which `p(X)` fails*, not
merely fail. The answer is to compile a **dual program** — for each predicate, a
predicate proving its negation — by Clark's completion plus De Morgan. P4 §2.1
gives the cleanest statement of the construction in five steps; `comp_duals.pl`
follows it.

Two details in P1 explain most of what `comp_duals.pl` actually does.

### Head abstraction

A dual must succeed wherever the original fails, *including where head
unification fails*. So the unification implicit in the head is made explicit
before the dual is computed. P1 (§ on dual rules, around the `t(A, A)` example)
specifies three rewrites:

- a variable occurring more than once in the head — each occurrence after the
  first is replaced by a new variable, with a unification goal prepended to the
  body;
- a non-variable argument — replaced by a new variable, with a unification goal
  prepended;
- **a compound argument — the same, applied *recursively* to the arguments of
  the term.**

So `t(A, A).` becomes `t(A, B) :- A = B.` and dualises to `not t(A, B) :- A \= B.`

This port has *two* mechanisms for that rewrite, and the recursive one is gone.
`abstract_structures/4` replaces a compound head argument with a fresh variable
and one `Var = Struct` goal — one level, not recursive. `prep_args2/7` did the
recursive descent P1 specifies, and was dead: `abstract_structures/4` runs first
and leaves it nothing to do. It was removed (`doc/bugs.md` Fixed #22) after
checking that the recursive form changes no answers — see below.

### The limitation P1 admits

P1 works its head abstraction through a list example:

```prolog
p([X | T]) :- q(X), p(T).

not p(Z) :- forall(X, forall(T, np1(Z, X, T))).
np1(Z, X, T) :- Z \= [X | T].
np1(Z, X, T) :- Z = [X | T], not q(X).
np1(Z, X, T) :- Z = [X | T], q(X), not p(T).
```

and then says plainly that calling this dual with a structure containing unbound
variables makes the disunification in the first clause "encounter illegal cases",
in their implementation a fatal error.

**This is `doc/bugs.md` §15.** This port does not error — it silently returns an
incomplete answer: `not wrap(T)` for `wrap(f(X)) :- item(X).` offers only the
`f(_)` shape and never the terms of other shapes that do hold. Same root cause,
different symptom: the `Z \= [X|T]` branch is unreachable for an unbound `Z`
under a universally quantified `X`, `T`.

It is worth knowing that this is a **limitation of the approach as published**,
not damage done in porting. The experiment in `test_compound.pl:`
`compound_head_rewriting_is_neutral` confirms it from the other end: writing the
head abstraction out by hand, in the shallow form and in P1's recursive form,
gives byte-identical answers in the first case and the same single answer in the
second. No rewriting of the head recovers the missing one.

### Ordering goals in the dual

The dual of a body `l1 ∧ l2 ∧ ...` is the disjunction of the negated literals, so
each becomes its own clause. But a goal can bind the variables a later goal sees,
so the clause negating `l_i` must first call the goals before it. P1 settles for
including *every* prior goal rather than computing real dependencies; P4 §2.1
notes the same thing and gives the other reason for it — without the prior
positive literals, the clauses produce **redundant answers**, re-exploring
solutions an earlier dual clause already gave.

### Constructive disunification

`X \= a` is not negation as failure; it means *X is any term not unifiable with
a*. P4 §2.2 describes the s(ASP) restriction — a variable can only be
disequality-constrained against ground terms, and disunifying two compound terms
needs backtracking over the cases (`p(1,Y) \= p(X,2)` succeeds with `X \= 1`,
then on backtracking with `Y \= 2`). s(CASP) replaces this with a real solver,
CLP(≠), over attributed variables. That is `clp/disequality.pl`, whose header
cites P1 for `.=./2` and `.\=./2`.

## P2, P3 — OLONs and the NMR check

An **odd loop over negation** is a cycle in the call graph crossing an odd number
of negations: `p :- q(a), not p.` A goal-directed procedure cannot just run these
— they are not rules for deriving `p` so much as constraints on which models are
admissible. P2 §3.3.4 gives the construction, and it is exactly what
`nmr_check.pl` emits.

For each OLON rule `q_i :- B_i`, a candidate answer set must satisfy
`q_i ∨ ¬B_i`. So the compiler builds

```prolog
chk_qi :- qi.
chk_qi :- not Bi.        % expanded: one clause per literal of Bi
```

and `¬B_i` is expanded into one `chk_qi` clause per literal, negated. For
`B_i = s, not r, t` that is four clauses: `chk_qi :- qi.`, `chk_qi :- not s.`,
`chk_qi :- r.`, `chk_qi :- not t.` The compiler then adds
`nmr_check :- chk_1, ..., chk_n.` and calls it after the query. In this tree the
generated predicates are named `o_chk_N` and the machinery is `nmr_check.pl`
driven by `call_graph.pl`.

P4 §2.4 restates this for the constraint case and adds the detail that matters
for variables: a free variable in an OLON rule has to be universally quantified,
so the sub-checks are wrapped in `forall`. Its example:

```prolog
:- not s(1, X).
p(X) :- q(X), not p(X).

nmr_check :- chk1, forall(A, chk2(A)).
chk1  :- forall(X, s(1, X)).
chk2(X) :- not q(X).
chk2(X) :- q(X), p(X).
```

P3 (Galliwasp) is where the check was first compiled rather than interpreted, and
where the optimisation lives: rather than running the whole NMR check after every
candidate, invoke only the sub-checks corresponding to OLON rules that the
literals actually in the candidate could violate. `nmr_check.pl`'s header cites
it for this.

## P2 — Coinduction, restricted

Top-down execution of a program with loops needs coinduction: a call that unifies
with an ancestor succeeds, on the hypothesis that it holds. The coinductive
hypothesis set (CHS) doubles as the candidate answer set.

Plain co-SLD computes the *greatest* fixpoint, which is wrong for ASP —
`p :- q. q :- p.` would give `{p, q}` where Gelfond–Lifschitz gives neither. P2
§3.3.3 states the fix in one sentence: **a coinductive recursive call may succeed
only if the path back to its ancestor passes through at least one negation.**
That single restriction is what makes the goal-directed method agree with the GL
method, and it is why `solve.pl` must count intervening negations rather than
just test for a repeated call.

P4 Appendix B splits the consequences into the three cases `check_CHS/6`
implements:

| Loop | Rule | Why |
|---|---|---|
| **Odd** (odd number of intervening negations) | fail and backtrack | succeeding would put a contradiction in the model; the NMR check would discard it anyway |
| **Even** (even, non-zero) | succeed | the recursive call partially supports the negations between; safe because the NMR check and the odd-loop test discard any model that tries to make it true |
| **Positive** (no intervening negation) | fail | otherwise the gfp creeps in |

with two refinements from P4 §3.2 that this port keeps:

- for a positive loop, fail **only if the goal and its ancestor are equal**, not
  merely unifiable — failing on unification costs completeness, and through
  negation, soundness (P4's `nat(X)` example);
- if the current call is equal to an **already-proven** ancestor, succeed
  immediately: it avoids recomputation and keeps the justification tree smaller.
  This is the `proved/1` wrapper in `stack.pl`'s trees.

## P4 — s(CASP): constraints, and the two forall algorithms

P4's contribution over s(ASP) is to keep constraints — and therefore variables —
alive through execution and into the answer sets, parametric in the constraint
solver. Concretely: a rule may carry a constraint `c_a` in its body, the
interpreter checks consistency against the store as it goes, and an answer is a
**constrained** partial model. Holzbaur's CLP(Q) is the solver used; `clp/clpq.pl`
is that integration, plus `dual_clpq/2`, which negates a whole constraint store
one constraint at a time keeping the earlier ones.

Two consequences P4 §3.3 calls out, both visible here:

- the compiler must know the CLP(Q) relations when building duals and NMR rules;
- `=`'s dual `\=` cannot be resolved at compile time, because it is not known
  whether it will be called on CLP(Q) or Herbrand variables. It decides at run
  time which solver to call.

### forall

This is the part of the design with the most divergence, so it is worth being
precise about what the papers say.

**Algorithm 1 (P4 §2.3), the s(ASP) `forall`.** `forall(V, Goal)` with `V`
unbound: run `Goal`. If it fails, `forall` fails. If it succeeds with `V` still
unbound, succeed — `Goal` did not depend on `V`. If `V` came back *bound*,
backtrack for other clauses. If `V` came back *constrained* to differ from a list
of values, re-execute `Goal` with `V` substituted by each of those values in turn;
succeed if all of them succeed. Multiple quantified variables nest:
`∀v1,v2.Goal` is `forall(V1, forall(V2, Goal))`.

**Algorithm 2 (P4 §3.3), C-forall.** The generalisation to arbitrary constraint
domains, and the one P4 says "generalizes and clarifies the design of the
original". Run `Goal` under store `C_i`; compare `A_{V,i}`, the answer domain of
`V`, against `C_{V,i}`, its domain going in. If unchanged, succeed — `V` is
irrelevant to the success. Otherwise the domain was *narrowed*, so set
`C_{i+1} = C_i ∧ ¬A_{V,i}` — remove the values that just worked — and re-run.
Fail when `Goal` fails on a non-empty remaining domain.

**What this port does.** Four algorithms, selected by the `scasp_forall` flag:

| Value | Route in `solve.pl` | Relation to the papers |
|---|---|---|
| `all` *(default)* | `solve_c_forall/8` → `solve_other_forall/11`, one variable at a time | C-forall, variable by variable |
| `all_c` | `solve_c_forall/8` → `solve_var_forall_/11` over the whole tuple at once | Ciao's `--all_c_forall` |
| `prev` | the pre-C-forall route, with a cut | Ciao's `--prev_forall`, i.e. Algorithm 1 |
| `sasp` | same, with a second cut | Ciao's `--sasp_forall`, the original s(ASP) behaviour |

`solve_var_forall_/11` is the workhorse, and its own header says it: it
implements P4 §2.3 and "It works different though". It proceeds by copying the
goal, solving it, reading off the instantiations, computing their duals, and
continuing until all duals are proved — recognisably Algorithm 2's
"remove what succeeded, re-run" shape rather than Algorithm 1's substitute-each-
value shape.

The two cuts in `solve_goal_forall/8` under `prev` and `sasp` are undocumented
outside a pair of code comments naming test cases (`prev` — "remove answers in
max.lp"; `sasp` — "remove answers in hamcycle_two.lp / Without cuts the
evaluation may loop - e.g. queens.lp"). They are load-bearing and have no paper
behind them. `test/test_forall.pl` pins all four.

## P5 — Justification trees

P5 defines the justification tree and, usefully for this repository, says what it
is *for* and how much of it is essential.

> **Definition 1 (Minimal s(CASP) Justification Tree).** The ordered list of
> literals in the path of the successful goal-driven proof of the query,
> conjoined with that of the proof of the global constraint.

Two things follow that the test suite depends on:

- It is **minimal** — it contains only the literals needed to support the query
  and the consistency of the global constraints, i.e. the subset of a stable model
  relevant to the query. It is a by-product of backward chaining, not a separate
  search.
- Which literals *appear* is explicitly a **presentation decision**, not part of
  the semantics. P5 §3.4 says controlling it "is more challenging" than
  controlling the model, notes that one may want to hide a literal but keep its
  children, and defines four levels: `short` (only `#show` literals), `mid` (plus
  the other user predicates), `neg` (also the default-negated ones) and `long`
  (everything, including auxiliary predicates, `forall` and built-ins).

That is the published warrant for how `test/proof_skeleton.pl` compares proofs.
The model of an answer is semantics and is compared exactly; the tree is a proof
*presentation* whose level of detail the system itself treats as adjustable, so
comparing it at a declared abstraction level is faithful to the design rather
than a concession to flaky tests. `stack.pl`'s `filter_tree/4` is the same idea
inside the system.

P5 also introduces the natural-language rendering driven by `#pred` patterns —
`human.pl` and `lang/*.pl` here.

## Where this port has no paper behind it

Worth knowing before treating any of it as settled design:

- **The four `forall` algorithms and their cuts.** See above.
- **`--dcc` (dynamic consistency checking).** No paper; forces `--forall=prev`
  and errors on any other value, which is why it needs its own process.
- **Calling Prolog from s(CASP)** (`:- prolog p/2`, `as opaque`, the justified
  meta-interpreter in `prolog.pl`).
- **Module interning** (`modules.pl`): the solver is not module aware and the
  module is interned into the functor name for speed.
- **Source references and coverage annotation** (`source_ref.pl`).
- **The renderers**: `html.pl` is this port's primary output and is much more
  than P5 describes.
- **The performance work**: low-level term copying for `forall` and the
  loop/proved-literal index (~10x), both specific to this tree.
- **`abstract_structures/4`** in place of P1's recursive head abstraction.

## Reading order

If you are picking this up cold: **P4** first (it is self-contained and restates
the dual construction and the loop handling more clearly than P1), then **P2
§3.3** for the NMR check and the coinduction restriction, then **P1** only when
you need the fine print on head abstraction and disunification. **P5** is short
and worth reading before touching `stack.pl` or the test oracle. **P3** only for
the NMR optimisation.
