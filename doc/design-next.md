# Design for a fresh s(CASP) implementation

A design round, not a specification. It records what the current system does,
what measurement says about it, and the decisions taken so far — including the
ones still open. `ssCASP.md` is the origin of most of the positions here;
this file adds the evidence and the consequences.

Companions: `doc/design.md` (how the *current* system works),
`doc/literature.md` (what the papers specify), `doc/bugs.md` (what is broken),
`doc/test-strategy.md` (what the tests guarantee, and what they do not).

## What s(CASP) is

**s(ASP) is ungrounded ASP.** It keeps *variables* alive by refusing to ground,
and the only thing it can say about an unbound one is a constructive
disequality over Herbrand terms: `X \= a`.

**The C in s(CASP) is a constraint solver.** That is the whole of what the
second system added to the language: a variable can carry a store over a dense
domain, and the answer to a query is a *constrained* partial model. The two are
independent — `X #> 1, X #< 2` has no solution in the integers and infinitely
many in the rationals, and no disequality expresses either.

Everything else — duals, the NMR check, coinduction, `forall` — is s(ASP)'s,
which is why P4 restates all of it (`doc/literature.md`).

## Measurements the design rests on

All from this tree, September 2026.

### Size

| Area | Lines | Fate |
|---|---:|---|
| Collect (`dyncall`, `embed`) | 1,405 | keep, minus the obsolete block API |
| Compile (`input` → duals → NMR → `pr_rules`) | 3,284 | rewrite |
| Solve (`solve`, `clp/`, `prolog`) | 1,876 | rewrite — the hard part |
| Output | 3,560 | restructure, HTML-first |
| Driver (`main`, `options`, …) | 1,897 | keep |
| | **12,290** | |

### `forall` is the bottleneck *and* the correctness question

Four algorithms, selected by the `scasp_forall` flag. They differ by more than
constant factors, and they differ in **what they answer**:

| | queens | bec_light | nixon-diamond | ex02-member |
|---|---|---|---|---|
| `all` *(default)* | 0.53 s | 0.018 s | 3 models | 1 model |
| `all_c` (P4 Alg. 2) | **>10 s** | 0.043 s | 10000+ | — |
| `prev` | **0.022 s** | 0.005 s | **5 models** | **8 models** |
| `sasp` | 0.19 s | 0.005 s | 3 models | 2 models |

Those are distinct *partial* models. **They are not distinct answers**, and an
earlier draft of this file was wrong to imply they were. Measured properly:

| | bindings | partial models | consistent? |
|---|---|---|---|
| `nixon-diamond`, all three | **3** | 3 / 5 / 3 | yes |
| `ex02-member`, all three | **1** | 1 / 8 / 2 | yes |

The algorithms agree on what the query *answers* are. They differ in how many
partial models they enumerate on the way, and in how much of the supporting
answer set each one carries — on `birds`, `all` returns `not bird(sam)` and
`-flies(sam)` where `prev` returns `-bird(sam)`, two subsets of the same answer
set reached by different NMR-check proofs. All four also agree with the
Gelfond–Lifschitz oracle on brave entailment.

So this is **redundant enumeration, not disagreement** — the same principle as
`doc/test-strategy.md`'s: the solution set is semantics, the number of answers
it arrives in is an implementation choice. It still costs: `all_c` not
finishing on queens, and `prev` being 24× faster than the default, are real.

The genuine correctness problem is elsewhere, and decision 5 is about that.

### Coverage

The reasoner is well covered: `comp_duals.pl` and `variables.pl` have no
unreached clauses, `call_graph.pl` one, `nmr_check.pl` three, `solve.pl`
thirteen of 1,179 lines. The gaps are `dyncall.pl` (81), `output.pl` (43),
`pr_rules.pl` (24), `clp/clpq.pl` (23).

## Decisions

### 1. Compile to predicates, do not interpret

The current system asserts `pr_rule(Head, Body, Ref)` facts and walks them with
`solve/4`. Generating real Prolog predicates instead buys, in rough order of
value: `spy`/`trace` and per-clause coverage on the actual program, first-
argument indexing, and the removal of a meta-interpretation layer from the hot
path.

The cost is that solver state has to be threaded. Decision 3 addresses that.

### 2. Name mangling stays, but the scheme is replaced

Mangling is not the problem. The collected program lands in **one** temporary
module, and it may draw clauses from several source modules, so the functor has
to say which one a predicate came from. The solver works on functor names
because carrying `M:Term` was measured as the bottleneck, and compiling to
predicates (decision 1) does not change that — a single module still needs
`m1:p/1` and `m2:p/1` to be different predicates.

What is wrong is the *scheme*. `modules.pl` glues module, arity and a
one-letter prefix together with `_`:

| | |
|---|---|
| `n_` | dual rule |
| `c_` | classical negation |
| `d_` | dummy |
| `o_` | **escape**, for a name already starting with `_` or a reserved prefix |

That fourth row is the tell. `o_` exists because the scheme cannot distinguish
a prefix from a name that happens to begin with one, and `_` cannot separate
the arity from a name that happens to end in `_1`. The result is neither
reliably invertible nor readable — `not o_p_2_vh2` says very little about what
it is.

#### The scheme

Four fields, one reserved separator:

```
Module $ Kind $ Name $ Arity
```

```prolog
mangle(Module, Kind, Name, Arity, Mangled) :-
    atomic_list_concat([Module, Kind, Name, Arity], $, Mangled).

demangle(Mangled, Module, Kind, Name, Arity) :-
    atomic_list_concat([Module, Kind, Name, Arity0], $, Mangled),
    atom_number(Arity0, Arity).
```

That is the whole implementation, both directions, and it is unambiguous
because **`$` is reserved**: a module or predicate name containing one is
rejected at compile time with a clear message. `atomic_list_concat/3` splits on
every occurrence, so such a name makes `demangle/5` *fail* rather than silently
decode to something else — the failure mode is detectable, which the current
scheme's is not. No program in the test corpus uses `$` in a name.

`Kind` is a word rather than a letter, so the name says what it is and greps
cleanly:

| Kind | | Example |
|---|---|---|
| `plain` | a user predicate | `user$plain$flies$1` |
| `neg` | classical negation, `-p` | `user$neg$cold$1` |
| `dual` | the dual, `not p` | `user$dual$flies$1` |
| `dualneg` | the dual of `-p` | `user$dualneg$cold$1` |

**`o_` and `d_` disappear**: with the fields separated there is nothing to
escape.

#### Where the generated predicates go

Computing a dual generates three levels, and the current names for
`p(0). p(X) :- q(X), not t(X,Y).` are:

| head | is |
|---|---|
| `p/1` | the user predicate |
| `not p/1` | the dual of the predicate — the conjunction of the clause duals |
| `not o_p_1/1`, `not o_p_2/1` | the dual of clause 1, of clause 2 |
| `not o_p_2_vh2/2` | clause 2's **innermost** dual, with the body-only variables appended |

The helper is named in `comp_duals.pl:comp_dual2/3` as
`<clause-dual> '_vh' <Arity>`, so the number is the **arity** — head arguments
plus body-only variables — and not a counter. There is exactly one per clause
however many body variables it has: two nested `forall`s share one helper of
arity 3, not two helpers.

Placing these needs no new mechanism, because **the `Kind` field is entirely
generated** — it never holds user data. So it can carry sub-structure with a
separator of its own, and that separator needs no reserving:

| Kind | is |
|---|---|
| `dual` | `not p`, the conjunction over clauses |
| `dual.2` | the dual of clause 2 |
| `dual.2.inner` | clause 2's innermost dual, body variables appended |
| `dualneg`, `dualneg.2`, … | the same for `-p` |

`Name` and `Arity` keep naming **the user predicate this derives from**, which
is what makes the name useful in a message: `user$dual.2.inner$p$1` says
*innermost dual of clause 2 of `p/1`*. Note that this is deliberately *not*
the generated predicate's own arity — that one is 2, and anything wanting it
should ask `functor/3` rather than read it out of a name.

The program-level predicates have no user predicate to name, so they leave the
`Name` field **empty** — which is unambiguous, since a predicate must have a
name, and keeps every mangled name to four fields:

| | |
|---|---|
| `user$nmr$$0` | the NMR check |
| `user$chk.1$$1` | its first sub-check, of arity 1 |
| `user$false$$0` | the head of a headless rule |

For these, `Arity` is the generated predicate's own — a sub-check may have
arguments (P4 §2.4's `chk2(X)`). Demangling is then one clause: split on `$`,
take four fields, and read an empty `Name` as "derives from no user
predicate". An earlier draft used a two-field form for these and made the
reader dispatch on field count for no gain.

#### Demangle on the way out

Every user-facing message must demangle, and today one does not:

```
scasp_predicate `'tmp-1-4183594005075597591:p'/0' does not exist
```

That is `doc/bugs.md` #17's second half — an error naming a temporary module
that existed only for that query. With `demangle/5` the reporting path has no
excuse; the message should read `p/0`, and say which source module if it
matters.

The temporary module name is worth a second thought for the same reason: it is
long, it appears in every functor of the compiled program, and it means nothing
to anyone. An alias — `m1`, `m2` — mapped back for reporting would keep the
mangled names short and readable without losing the distinction.

### 3. State: the native stack, plus one dict

The same information lives three ways today — a flat `Stack`, a `Parents` list,
a `Proved` assoc — with two rebuilt from the first on every entry
(`stack_parents/2`, `stack_proved/2`) and all three threaded by hand through
some twenty predicates of 8–11 arguments. `solve.pl:777` concedes the
duplication in a comment.

What is wrong is that the *same* information lives three ways, not that it is
threaded. Ancestors and proved literals are different things with different
modes — one goes down, the other comes up — and the fix is to say so, not to
hide one of them.

**Ancestors go down, the proof comes up.** Three extra arguments on every
compiled predicate: `Anc` in, `S0`/`S` threaded. Nothing is reconstructed from
anything else, and there are no markers to walk.

#### Not the native stack, after all

The first version of this decision proposed finding ancestors with
`prolog_frame_attribute(F, parent_goal, M:Goal)`, as `library(coinduction)`
does. That was a paper decision and it does not survive contact:

- `parent_goal` *unifies* with an ancestor; it does not say **which** one, and
  the negation count on the path back is the whole point. Counting means
  walking frames explicitly and demangling each one's functor.
- Every frame on the path has to be protected from last-call optimisation —
  `library(coinduction)` needs its `no_lco` marker for one wrapper; a solver
  needs it on every predicate, giving up LCO across the whole program.
- It is barely testable. An ancestor list is a value a test can construct; a
  frame chain is not.

**Borrow the semantics, not the mechanism.** What that library is right about
is the restriction, P2 §3.3.3: a coinductive call may succeed only if the path
back to its ancestor crosses a negation. That stays.

#### The six cases collapse to three

Compiling to predicates makes the polarity of a goal **static** — it is in the
functor's `Kind` field — so a frame can record it, and the number of negations
on a path is the number of *polarity transitions* along it. That measure is the
number of negative edges on the corresponding dependency path, which is what
the theory is about; counting `dual` frames instead undercounts, because
dualising `q :- not p` turns `not q`'s body into a positive call to `p` and
consumes one negation.

Written that way, `check_CHS_/6`'s six clauses become three, and one of the six
turns out to be impossible:

| | |
|---|---|
| the goal's **negation** unifies with an ancestor or a proved literal | fail |
| an ancestor unifies with the goal, **0** transitions | fail — positive loop |
| an ancestor unifies with the goal, **even, non-zero** | succeed coinductively |
| the goal is ground and already proved | succeed |

There is no odd case. Two frames of the same polarity have an even number of
transitions between them by construction, so an ancestor matching the goal is
never reached across an odd number of negations — an odd loop reaches the
goal's *negation* instead, which is the first row. That is a property to check
against the oracle rather than to take on faith, and `test_solve.pl` asserts
it directly.

### 2a. Drop the block API

`begin_scasp/1,2` … `end_scasp/0` is obsolete and the new implementation need
not carry it. `scasp/2` over ordinary Prolog clauses does the same work without
a second way to read a program.

It is not the whole of `embed.pl`, which is why the size table says "minus".
`scasp_call/1` is how `dyncall.pl` and the toplevel run a query — and is where
`doc/bugs.md` #1 lives, in a `% TBD: Properly handle the module` — while
`scasp_model/1,2`, `scasp_stack/1` and `scasp_justification/2` are the result
accessors. Those carry over; the block machinery (`begin_scasp/2`,
`end_scasp/1`, `scasp_compile_unit/2`, `link_clauses/4`, `scasp_listing/2` and
the `xref_update_syntax` hooks) does not, which is roughly 250 of 609 lines.

Nothing about the *language* goes with it. `#show`, `#abducible`, `#pred`,
`-p(X)` heads and `:- Constraint.` are program syntax, not API, and the golden
corpus covers each of them independently of blocks.

### 3a. Options can stay flags

Worth stating because it removes a candidate decision. The eighteen option
flags look like process-global state, and threading an options record instead
looks like an obvious cleanup. It is not needed: **Prolog flags are
thread-local on write** — a thread's `set_prolog_flag/2` does not propagate back
to its creator, and a new thread inherits the creator's value at creation.

So a caller that wants a different setting sets the flag and restores it around
the call, and single-threaded that is the whole story. The only thing ruled out
is one thread holding two values at once, which no caller needs.

That leaves `scasp_forall` and `scasp_dcc` as the flags to watch, not because
of scope but because they change the *semantics* of `solve/4` and
`generate_nmr_check/1` — and decision 5 is about removing that choice
altogether rather than carrying it more tidily.

### 4. Constraints: keep both stores, replace the disequality solver

Two stores stay, because two stores *is* s(CASP): CLP(Q) over a dense domain,
and Herbrand disequality.

`clp/disequality.pl` is an attributed variable holding `neg(ProhibitedValues)`
— a flat list per variable. P4 §2.2 admits the limitation: it can only
constrain a variable against **ground** terms, and disunifying two compounds
needs backtracking over cases.

`dif/2` is strictly more general, is implemented in C, and — the property that
matters — is **inspectable**:

```prolog
?- dif(X, f(Y)), dif(Y, a), copy_term(X-Y, C, Attrs).
Attrs = [dif(_A, f(_B)), dif(_B, a)].
```

Introspection is why the original was hand-rolled: s(CASP) has to print the
store in an answer, dualise it, and read instantiations out of it to compute
`forall` duals. `copy_term/3` supplies all three. This looks like a real
replacement rather than a simplification.

It does **not** fix `doc/bugs.md` #15. `dif(A, f(_))` still allows `A = f(b)`,
because "A is not of the form `f(_)`" is a *universally quantified*
disequality, which neither solver can express. Decision 6 is where that goes.

### 5. `forall`: the criterion

`forall(V, G)` has to show G holds for **every** value of V. The answers to G
give regions of V's domain where it holds; the quantifier succeeds iff those
regions cover the domain. So the question is never "which algorithm" but:

> **can we decide that the answers cover the domain?**

That is decidable in exactly four cases, and undecidable outside them.

**Decidable — proceed.**

1. **V's domain is declared finite.** Expand to a finite conjunction, one
   conjunct per value. Sound, complete, no search. This is what annotations
   (decision 6) buy, and it is the state to aim for.

   **Implemented** in `prolog/sscasp/dualise.pl`, though not by expanding: a body-only
   variable with a domain gets `for_domain(Var, Generator, Goal)` instead of
   `forall/2`, naming the domain and leaving the solver to enumerate it or post
   it as a region and label. Expanding at compile time would be right for
   `[a, b]` and wrong for `0..23`. The upshot is that **`forall/2` surviving
   into a compiled program means exactly one thing: a quantifier nothing
   bounds**, which makes decision 5a's report a grep. The domain is read off
   *where the variable is used* — argument 2 of `t/2` — rather than from the signature of
   the predicate being dualised, which says nothing about it. Where a variable
   is used in several places any one of the domains will do, and taking the
   first is not a shortcut: the inner dual negates a conjunction, so for a
   value outside the domain of any single literal that literal fails, the
   conjunction fails and its negation holds — the instance is true for free.
   Bounding the quantifier by one usage leaves nothing unchecked that could
   have failed.
2. **Some answer leaves V wholly unconstrained.** Then G's success does not
   depend on V and that one answer covers the domain. Sound *provided every
   store is consulted* — `doc/bugs.md` #14 was precisely this check failing to
   look at the CLP(Q) store and calling a constrained variable free.
3. **The uncovered set is finite.** The answers constrain V by *ground*
   disequalities, so what they miss is the finitely many prohibited values.
   Test each. This is P4's Algorithm 1.
4. **The uncovered set is a CLP(Q) region that CLP(Q) proves empty.** This is
   C-forall (Algorithm 2): subtract what succeeded, re-run on the remainder,
   stop when the remainder is unsatisfiable.

**Undecidable — diagnose.**

5. **The complement is not representable.** The signature is precise: a
   disequality `A \= T` where `T` is a compound *containing the quantified
   variable*. Covering the rest of the domain would mean asserting "A is not
   of the form `f(_)`" — a universally quantified disequality, which neither
   the current store nor `dif/2` can express. This is `doc/bugs.md` #15, and
   P1 documents it as a fatal error in the original implementation.
6. **V comes back bound to a ground term** over the infinite Herbrand universe.
   One answer covers one point; no finite number of them covers the domain.

### 5a. How to apply it

Case 5 has a **static over-approximation** that costs nothing: at dual
generation, a `forall(V, Helper)` whose helper has a clause with `A \= T`,
`T` a compound containing V's argument position. Running that over the test
corpus flags **20 of 52 programs** — including `member/2` in every guise,
`queens.pl`'s `attack/3`, and `clpq_equality.pl`, the program that does not
terminate (`doc/bugs.md` #2).

That is far too many to refuse, and the reason is that **the static shape is
necessary but not sufficient**. `queens.pl` has it and answers correctly,
because it never calls the negation with the relevant argument unbound. The
incompleteness is only *realised* at run time.

So the policy is split:

| When | What |
|---|---|
| compile time | report the predicates whose negation is shape-incomplete — a diagnostic, never an error |
| run time, case 5 or 6 reached | warn by default, naming the predicate and the variable; refuse under a strict flag |
| annotation present | case 1 applies instead, and neither fires |

The compile-time report is the useful half: it tells an author *before they
run anything* that negating `member/2` is incomplete if the list is unbound,
and that a type declaration on the argument would fix it. That is the link
between decisions 5 and 6, and the reason annotations are worth their cost.

### 5c. There is no algorithm to choose

The four values of `scasp_forall` look like four algorithms. They are not.
`solve_goal_forall/8` already dispatches on `check_unbound/2`, and the four
branches are decision 5's four cases:

| `check_unbound/2` | branch | decision 5 |
|---|---|---|
| `ground` | fail, backtrack for another clause | V got bound — case 6 |
| `[]` | succeed | case 2, V unconstrained |
| `clpq(V, C)` | `dual_clpq/2` + `exec_with_clpq_constraints/11` | case 4, CLP region |
| a disequality list | `exec_with_neg_list/10` | case 3, finite uncovered set |

The dispatch is not optional and not flag-dependent. **Both flag-dependent cuts
sit in the disequality branch alone**, and both are unsound:

```prolog
(   current_prolog_flag(scasp_forall, prev) ->  !  ;   true ),
exec_with_neg_list(...),
(   current_prolog_flag(scasp_forall, sasp) ->  !  ;   true ),
```

`prev` cuts *before* the completion check, committing to the first solution of
the goal before knowing whether its prohibited set can be completed; if it
cannot, the `forall` fails although another solution might have worked. `sasp`
cuts after, discarding alternative covers.

They lose answers on real programs — `bec_loan.pl` goes from four to **none**
under either (`doc/bugs.md` #18), and `--dcc` inherits it, because it forces
`forall(prev)` and errors on anything else.

**So: remove the flag.** Keep the dispatch, drop both cuts, and the question
"which algorithm" does not arise — the case determines the method, which is
what decision 5 says it should.

The remaining axis is real but small: `all` nests the quantifiers one variable
at a time (`solve_other_forall/11`), `all_c` handles the whole tuple at once
(`solve_var_forall_/11` directly). `all_c` fails to terminate on `queens.pl`
and `bec_loan.pl`, so per-variable nesting wins on the evidence available.

And the performance worry does not go away — `prev`'s 24× on `queens.pl` was
real. But it was bought with incompleteness, so it is not available. The
legitimate route to the same speed is decision 6a: give the quantified variable
a domain, and case 1 or case 4 applies instead of case 3, with no search to cut
short.

### 6. Annotations, optional

Optional annotations avoid most `forall` problems and improve performance
dramatically. They are additions to the language, never requirements.

The starting point is the type system of **Schrijvers, Santos Costa,
Wielemaker & Demoen, *Towards Typed Prolog*, ICLP 2008** (LNCS 5366,
pp. 693–697; implemented as the SWI pack `library(type_check)`). Hindley-Milner,
optional, typed and untyped code mixing freely, with dynamic checks inserted at
the boundary. Its syntax:

```prolog
:- type boolean ---> true ; false.
:- type list(T) ---> [] ; [T|list(T)].
:- pred append(list(E), list(E), list(E)).
```

**Two extensions are needed**, and they are the whole reason this matters for
s(CASP):

- **Enumerations of atoms.** `:- type day ---> mon ; tue ; … ; sun.` already
  parses as Hindley-Milner — it is a sum of nullary constructors. What is
  missing is the *consequence*: the checker must know this type is **finite and
  enumerable**, so that `forall(D, …)` over a `day` becomes a seven-way
  conjunction. `list(T)` is the same shape of declaration and is infinite, so
  finiteness has to be derived, not assumed.
- **Integer domains.** `:- type hour ---> 0..23.` is not Hindley-Milner at all:
  `integer` is a primitive and infinite. A subrange type is a genuine addition.
  It is also what makes the constraint side tractable — a `forall` over a
  bounded integer is a finite conjunction, whereas over `integer` it is not.

A third annotation from `ssCASP.md`, worth keeping separate from types:
**closed-world facts.** `age(peter, 32)` meaning peter is 32 and *not* 33 —
i.e. negation as failure coincides with classical negation for that predicate.
That licenses grounding its negation from its extension alone.

See §6a: measurement changed the shape of this considerably.

### 6a. Domains are predicates

Before adopting a type system, note what already works.

**ASP's answer is that there is no type system.** `#domain` was lparse and
gringo-3 syntax and has been removed; a modern ASP program binds a variable
with a **domain predicate in the body** — `p(X) :- dom(X), q(X).` — and
safety plus restrictedness is what guarantees a finite grounding. Intervals
(`dom(1..8).`) are sugar for a finite extension.

**That mechanism already works in s(CASP), unchanged.** The §15 incompleteness
— `not wrap(T)` for `wrap(f(X)) :- item(X).` offering only the `f` shape —
disappears the moment the variable is guarded by a finite domain:

```prolog
item(a).
wrap(f(X)) :- item(X).
?- not wrap(T).                       % T = f(A | {A ∉ [a]})   — incomplete

dom(f(a)). dom(f(b)). dom(g(a)). dom(b).
nw(T) :- dom(T), not wrap(T).
?- nw(T).                             % T = f(b) ; T = g(a) ; T = b   — complete
```

And integer domains work two ways, the second better:

```prolog
num(1). num(2). num(3). num(4).       % enumerated: X = 1 ; X = 2
num(X) :- X #>= 1, X #=< 4.           % CLP region: X | {X #>= 1, X #=< 2}
```

The CLP form returns *one* constrained answer rather than one per value, and
scales to a range no one would enumerate. In the terms of decision 5, the first
is case 1 and the second is case 4 — both already decidable.

So the gap is **declarative, not mechanical**. The user must currently write
the guard into every rule by hand; the compiler cannot know that `wrap/1`'s
argument ranges over `dom/1`, so it cannot insert the guard into the dual.

#### The proposal

A **domain is a unary predicate**. Nothing else. Finite ones are those with a
finite ground extension; CLP-constrained ones are regions. This is Ciao's
`regtype` idea and ASP's domain predicate, and it needs no type language.

```prolog
:- pred wrap(shape).            % wrap/1's argument ranges over shape/1
shape(f(a)). shape(f(b)). shape(g(a)). shape(b).

:- pred big(num).
num(X) :- X #>= 1, X #=< 4.     % a domain may be a CLP region
```

with sugar for the two cases `ssCASP.md` calls out, since writing them out is
the tedious part:

```prolog
:- domain day  = mon ; tue ; wed ; thu ; fri ; sat ; sun.   % → seven facts
:- domain hour = 0..23.                                     % → a CLP region
```

What the compiler does with it: when generating the dual of a predicate whose
argument has a declared domain, insert the domain goal. `not wrap(T)` becomes
`shape(T), not wrap(T)`, which moves it from decision 5's undecidable case 5
into case 1 or 4.

That is a real restriction of the universe of discourse for that argument, and
should be understood as such rather than as a hint — which is the honest
reading of a type anyway.

#### Why not Hindley-Milner

`library(type_check)` gives structural, polymorphic types — `list(int)`,
`pair(K,V)` — with inference and static checking. Worth having eventually, for
the errors it catches. But it does not solve this problem: what `forall` needs
is **extensional finiteness**, and `list(T)` is a perfectly well-typed infinite
domain. The two extensions `ssCASP.md` asks for are exactly the places HM has
nothing to say — an enumeration must be known finite, not merely a sum of
nullary constructors, and an integer subrange is not HM at all.

Under "a domain is a predicate" neither is an extension: an enumeration is a
set of facts and a subrange is a CLP region. That is the argument for doing
this first and types, if ever, later.

#### Naming

`#pred` is the wrong name for what it does — it declares a natural-language
rendering for a goal, not a predicate:

```prolog
#pred shown(Thing) :: '@(Thing) is shown'.
```

Proposed: **rename it `#describe`**, keeping `#pred` as a deprecated alias,
which reads as what it is — *describe `shown(Thing)` as '…'*. `#template` and
`#phrase` are the alternatives; the choice is taste, the rename is not.

That frees `pred` for the signature above, where it matches both
`library(type_check)` and Ciao. The collision is shallow in any case:
`input.pl:directive/4` matches `pred(Pred::Template)`, so a signature without
`::` is *rejected* today rather than silently misread — the error message is
`invalid_directive`.

Adding these is small: `input.pl` declares `:- op(1150, fx, [table, show,
pred])` and dispatches in `directive/4`; `domain` needs one more operator and
one more clause each.

### 7. Tabling: probably not, and the reason is constraints

`ssCASP.md` lists tabling or hand-rolled memoisation as an option, and the
current tabling path is dead code (`doc/bugs.md` #4, `:- if(fail).`).

The blocker is that **tabling and constraints are hard to combine**.
SWI-Prolog's tables handle attributed variables, and that is all: there is no
notion of answer subsumption over a constraint store, no way to say that a
tabled answer with `X #> 5` subsumes a call with `X #> 7`. Since carrying
constraints through execution is the whole of what s(CASP) adds to s(ASP),
tabling cannot simply be switched on.

Where it *could* apply is the annotated, finite-domain fragment of decision 6,
after constraints have been resolved into ground instances. That is worth
revisiting once annotations exist, and not before.

### 8. Output: HTML first, text derived

`html.pl` is already the primary renderer, and `html_text.pl` reinterprets its
token stream as coloured plain text — so the two must change together whenever
a CSS class or DOM node moves. Generating HTML and deriving text through
`library(lynx)` removes that coupling.

## Open questions

1. ~~**The `forall` criterion.**~~ **Settled** — decision 5. Four decidable
   cases, two undecidable ones, a static over-approximation for the common
   undecidable one, and a compile-time diagnostic rather than a refusal.
2. ~~**`#pred` versus `:- pred`.**~~ **Settled** — §6a. The natural-language
   template is renamed (`#describe` proposed, `#pred` kept as a deprecated
   alias), which frees `pred` for an argument-domain signature. Domains are
   unary predicates, not types.
3. **How much of the language do annotations change?** Optional, certainly —
   but a program that *needs* an annotation to be accepted (decision 5) has, in
   practice, a required one. §6a softens this: since a domain is just a
   predicate, a program can always write the guard by hand instead, and the
   declaration only saves repeating it.
4. ~~**Which `forall` algorithm, in the decidable region?**~~ **Settled** —
   §5c. There is no algorithm to choose: the flag is one case-dispatch plus two
   cuts, and both cuts lose answers (`doc/bugs.md` #18). Remove the flag, keep
   the dispatch, keep per-variable nesting.
   The oracle cannot help, since it agrees with all four.
5. ~~**Which partial model should a query return?**~~ **Largely settled.**
   The model is a **function of the proof**: two answers whose proofs are equal
   as sets of literal parent-child edges always carry the same model. Checked
   by `test/scasp/test_proof_model.pl` over ten programs spanning `forall`,
   constraints, even loops and classical negation — and on those, the number of
   distinct models equals the number of distinct proofs exactly.

   So "which partial model?" is not an independent question. It reduces to
   **which proofs the reasoner enumerates**, and a more roundabout proof simply
   carries a larger model. Two consequences:

   - A reimplementation should return **one answer per distinct proof**, not
     one per derivation path. The converse of the property does not hold today:
     `s :- a, b.` and `s :- b, a.` give two answers for one proof and one model,
     which is redundancy, not meaning.
   - Comparing the proof at `support` **subsumes** comparing the model rather
     than being independent of it. `CLAUDE.md`'s "Model: exactly … no option to
     weaken it" is therefore not over-strict after all — but it is not carrying
     its own weight either.

   What remains open is narrow: the property is checked on ten programs, not
   proved, and the exact model-count/proof-count correspondence is the part
   most likely to have exceptions.

## Sequencing

1. `doc/design.md` — describe the current system. `ssCASP.md`'s first step, and
   still the gap: the `forall` disagreement above surfaced twice by accident,
   which is what happens when nobody has had to write the behaviour down.
2. Settle the open questions above.
3. Annotations first, solver second. The annotation machinery decides what the
   solver can assume, and decision 5 makes the solver's correctness depend on
   it.
4. Compile-to-predicates, with the differential harness (`test/differential.pl`)
   running against the old implementation throughout.
