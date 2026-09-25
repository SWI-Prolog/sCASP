# How this s(CASP) works

The essential steps in s(CASP) resolution, as this tree implements them.

Scope: what the code does, not what it should do. Where the two differ,
`doc/bugs.md` has it; where the implementation departs from the papers,
`doc/literature.md` says so; where a rewrite would change something,
`doc/design-next.md` argues it.

## The shape of an answer

A query does not return a substitution. It returns three things:

- **bindings** for the query variables, which may be *constrained* rather than
  bound — `X | {X #> 10}`, or `X | {X ∉ [a,b]}`;
- a **partial stable model**: the literals needed to support the query, a
  subset of a full answer set, each possibly constrained;
- a **justification**: the proof tree, from which every rendering is derived.

"Partial" is the key word. s(CASP) never computes a whole answer set; it
computes the part a query needs, which is why it can work on programs whose
grounding would be infinite.

## Three entry points, one core

| Entry point | Module | Use |
|---|---|---|
| `prolog/scasp/main.pl` | `scasp_main` | the `scasp` executable: load files, answer the last query, exit |
| `prolog/scasp.pl` | `scasp` | the library: `scasp/2` and the `?-`-style operators |
| `embed.pl` / `dyncall.pl` | `scasp_embed`, `scasp_dyncall` | solving against ordinary Prolog clauses, `scasp/2` |

They meet at `scasp_load/2` → `scasp_query/3` → `solve/4`.

`embed.pl` also carries `begin_scasp/1,2` … `end_scasp/0`, which embeds an
s(CASP) block in a Prolog source file. **That API is obsolete** — use `scasp/2`
— though the rest of the module is not: `scasp_call/1` is how `dyncall.pl` and
the toplevel run a query, and `scasp_model/1,2`, `scasp_stack/1` and
`scasp_justification/2` are how a caller reads the result back.

`dyncall.pl` is the one with something to say. `scasp/2` does not compile a
file: it collects the clauses *reachable from the query*, adds any global
constraint whose call tree overlaps them, compiles that into an
`in_temporary_module/3`, solves there, and hands back model and tree. Hooks
`scasp_expand/4` and `scasp_expand_program/4` let the source module rewrite the
collected clauses on the way.

## Step 1 — Collect and read

`input.pl` reads the source with the Prolog reader plus the s(CASP) operators
(`ops.pl`: `not` at 900 fy, `#=`/`#<`/… at 700 xfx, `::`, `∉`). The result is a
list of *statements*, which `program.pl` asserts into thread-local dynamic
predicates: `defined_rule/4`, `defined_query/2`, `defined_directive/1`,
`defined_predicates/1`, `defined_nmr_check/1`.

That store is mutable global state with no save/restore. `destroy_program/0`
retracts it unconditionally at the end of every compile, so a nested compile
destroys the outer one's program — a live hazard, since `dyncall.pl` compiles
once per query.

## Step 2 — Dual rules

The heart of it. `not p(X)` must *return the X for which `p(X)` fails*, not
merely fail, so the compiler synthesises a **dual program**: for every
predicate, a predicate proving its negation. Clark's completion, then De
Morgan (P4 §2.1). `comp_duals.pl`.

Given

```prolog
p(0).
p(X) :- q(X), not t(X, Y).
```

the dual of the *predicate* is the conjunction of the duals of its clauses, and
each clause contributes one dual clause per body literal —
`not o_p_N` for the *N*-th clause of `p`:

```prolog
not p(A) :- not o_p_1(A), not o_p_2(A).

not o_p_1(A) :- A \= 0.

not o_p_2(A) :- forall(B, not o_p_2_vh2(A, B)).
not o_p_2_vh2(A, _) :- not q(A).
not o_p_2_vh2(A, B) :- q(A), t(A, B).
```

(verbatim from `--code`, modulo layout — the `_vh2` suffix marks the helper
the `forall` quantifies over)

Three things are going on there, and each matters:

- **Head unification becomes explicit.** A dual must succeed wherever the
  original fails, *including where head unification fails*. So before
  dualising, a repeated head variable, a non-variable argument and a compound
  argument are each replaced by a fresh variable plus a unification goal at the
  front of the body. `abstract_structures/4` does the compounds;
  `prep_args/6` the rest.
- **Earlier positive literals are kept.** The clause negating the *i*-th body
  literal first calls the literals before it (`q(A)` in `not o_p_2_vh2` above). Two
  reasons: a goal can bind what a later goal sees, and without them the clauses
  produce redundant answers, re-exploring what an earlier dual clause gave.
- **Body-only variables become `forall`.** `Y` occurs in the body of `p/1`'s
  second clause but not its head. Negating "∃Y such that …" gives "∀Y such
  that not …", so the dual wraps the quantified variable in `forall/2`.
  Nested variables nest: `forall(X, forall(Y, …))`.

At the end of the chain, unification itself is negated, which is where
constructive disequality comes from (step 5).

## Step 3 — OLONs and the NMR check

An **odd loop over negation** is a cycle in the call graph crossing an odd
number of negations: `p :- q(a), not p.` Such a rule is not a way to derive
`p` so much as a constraint on which models are admissible — with `p` absent
the rule fires and derives it; with `p` present the rule is unusable and
nothing supports it.

`call_graph.pl` builds the graph and finds the loops. `nmr_check.pl` compiles
each into a check: a candidate answer set must satisfy `q_i ∨ ¬B_i` for every
OLON rule `q_i :- B_i`, so the compiler emits

```prolog
not o_chk_1 :- not o__chk_1_1.
not o__chk_1_1 :- q(a), p.
not o__chk_1_1 :- not q(a).
o_nmr_check :- not o_chk_1.
```

one `chk` clause per literal of the body, negated. Global constraints
(`:- Body.`, headless rules, internally `_false_0`) are compiled the same way.
A free variable in an OLON rule has to be universally quantified, so its
sub-check is wrapped in `forall`.

`compile.pl` then appends `o_nmr_check` to every query, which is how the
check runs: after the query proper, against the model it produced.

## Step 4 — The `pr` program

`pr_rules.pl` emits the result as clauses asserted into the target module:

```prolog
pr_rule(not(o_chk_1), [not(o__chk_1_1)], Ref)
pr_rule(o_nmr_check,  [not(o_chk_1)],    Ref)
```

Heads may be wrapped in `not/1`; bodies are lists; `Ref` points back at the
source through `source_ref.pl`, for the justification. Alongside:
`pr_query/1`, `pr_user_predicate/1`, `pr_pred_predicate/4`,
`pr_table_predicate/1`. `predicates.pl` reads them back; `clean_pr_program/1`
wipes them.

### Names

The solver is **not module aware**. `modules.pl` interns the module into the
functor name rather than carrying `M:Term`, because the solver is the
bottleneck. Functors also carry arity, and reserved single-letter prefixes
(`program.pl:reserved_prefix/1`) mark what a name is:

| | |
|---|---|
| `n_` | dual rule (`negate_functor/2`) |
| `c_` | classical negation, `-p` |
| `o_` | escape for a name that already starts with `_` or a reserved prefix |
| `d_` | dummy |
| `_false_0` | head of a headless rule, consumed by the NMR check |

`raise_negation/2` and `intern_negation/2` in `common.pl` convert between the
user-facing `not`/`-` forms and the interned ones.

## Step 5 — Solving

`solve(:Goals, +StackIn, -StackOut, -Model)`, in `solve.pl`. Goals are solved
left to right; each one passes through:

```
solve/4  →  check_goal/8  →  solve_goal/8  →  solve_goal_predicate/8
                                           →  solve_goal_forall/8
                                           →  solve_goal_builtin/2
```

State is threaded by hand: a flat `Stack`, a `Parents` list and a `Proved`
assoc, the last two rebuilt from the first on entry (`stack_parents/2`,
`stack_proved/2`).

### Coinduction: the six cases

`check_goal/8` consults the coinductive hypothesis set — the candidate answer
set — before expanding anything. `check_CHS_/6`, in clause order:

1. **proved** — the goal is ground and already proved in the stack. Succeed
   immediately; this is also what keeps the justification small.
2. **co_success** — an ancestor unifies with the goal across an **even,
   non-zero** number of negations. Succeed: the recursive call partially
   supports the negations between.
3. **co_failure** — the *negation* of the goal is in the stack. Fail;
   succeeding would put a contradiction in the model.
4. **co_failure** — a **positive** loop (no intervening negation). Fail, with
   a warning. This is the restriction that makes goal-directed evaluation agree
   with Gelfond–Lifschitz rather than computing the greatest fixpoint.
5. a positive loop that is reported and then falls through to failure.
6. **cont** — none of the above: continue inductively, constraining the goal
   against the negations already visited.

### `forall`

Where the dual introduced a universally quantified variable, `solve_goal_forall/8`
has to show the goal holds for *every* value. Four algorithms, chosen by the
`scasp_forall` flag:

| Value | Route | Origin |
|---|---|---|
| `all` *(default)* | `solve_c_forall/8` → `solve_other_forall/11`, one variable at a time | this port |
| `all_c` | `solve_c_forall/8` → `solve_var_forall_/11` over the whole tuple | Ciao `--all_c_forall` |
| `prev` | the pre-C-forall route, with a cut | Ciao `--prev_forall` |
| `sasp` | the same, with a second cut | the original s(ASP) |

`solve_var_forall_/11` copies the goal, solves it, reads off the instantiations,
computes their duals, and repeats until all duals are proved — recognisably
P4's Algorithm 2 (remove what succeeded, re-run) rather than §2.3's
substitute-each-value, which its own header concedes: *"It works different
though."*

**The four agree on answers and differ on partial models.** On `nixon-diamond`
all of them give the same 3 bindings, but enumerate 3, 10000+, 5 and 3 *partial
models* on the way; on `ex02-member`, 1 binding and 1, —, 8 and 2 models. Each
model is consistent, and all four agree with the Gelfond–Lifschitz oracle on
brave entailment — so this is redundant enumeration rather than disagreement.
It is not free: `all_c` does not finish on `queens.pl`, where `prev` is 24×
faster than the default.

**And `prev` and `sasp` lose answers.** Both cuts sit in the disequality
branch; `prev` commits to a solution before checking that its prohibited set
can be completed. `bec_loan.pl` gives four answers by default and **none**
under either, and `--dcc` inherits it by forcing `forall(prev)`
(`doc/bugs.md` #18). The default is the only complete setting.

`doc/design-next.md` §5c argues the flag should go entirely: the four values
are one case-dispatch — which `check_unbound/2` already performs — plus two
unsound cuts.

### Constraint stores

Two, and both are attributed-variable solvers:

- **`clp/clpq.pl`** — Holzbaur's CLP(Q) over the rationals, plus `dual_clpq/2`,
  which negates a whole store one constraint at a time keeping the earlier
  ones. This is what makes `not` constructive over a dense domain.
- **`clp/disequality.pl`** — constructive disunification: `.=.`, `.\=.`, `∉`.
  An attributed variable holds `neg(ProhibitedValues)`.

They interact. `=`'s dual `\=` cannot be resolved at compile time, since it is
not known whether it will be called on CLP(Q) or Herbrand variables, so it
decides at run time.

`clp/call_stack.pl` carries stack and model as attributes (`~>`, `<~`) — but
only for the tabling path, which is dead (`doc/bugs.md` #4), so all 63 lines of
it are unreachable.

### Calling Prolog

`:- prolog p/2.`, optionally `as opaque`. In justified mode `prolog.pl` is a
meta-interpreter that extends the justification tree and handles `!`,
`(If->Then;Else)`, `;` and `=>`; `opaque` calls Prolog natively and shows only
the call. The meta-interpreter cannot handle meta-predicates. Negating a Prolog
goal uses `\+` when ground and `findall/3` plus disequality otherwise.

Built-ins that are *not* compiled reach `solve_goal_builtin/2` and run through
`exec_goal/1`, which calls them directly. Errors propagate (`doc/bugs.md` #17,
fixed): turning an error into failure made a goal and its negation both fail.

## Step 6 — Output

One tree, many renderings. `stack.pl` turns the solver stack into the canonical
justification tree — `Node-Children`, nodes wrapped in `not/1`, `-/1`, `chs/1`,
`proved/1`, `assume/1`, `goal_origin/2` — and `filter_tree/4` prunes it to a
detail level. `model.pl` canonicalises the model. Then:

- `output.pl` — term emission, constraint inlining, variable naming (`ovar_*`)
- `html.pl` — the primary renderer
- `html_text.pl` — reinterprets `html//1` tokens as coloured plain text;
  `human.pl` builds `--human` on top. The two must change together.
- `json.pl`, `listing.pl` (`--code`), `lang/en.pl`, `lang/nl.pl`

P5 defines the tree as *minimal*: the literals needed to support the query and
the consistency of the global constraints, and nothing else. Which of them are
*shown* is explicitly a presentation decision — `short`, `mid`, `neg`, `long` —
which is the published warrant for comparing proofs at an abstraction level
(`doc/test-strategy.md`).

## Configuration

Behaviour is controlled by eighteen Prolog flags, created with
`create_prolog_flag/3` in whichever module uses them and set by
`scasp_set_options/1` from the CLI.

**Flags are thread-local on write.** A thread's `set_prolog_flag/2` does not
propagate back to its creator, and a new thread inherits the creator's value at
the moment it is created:

```prolog
?- create_prolog_flag(f, initial, []),
   thread_create(set_prolog_flag(f, changed), T, []), thread_join(T, _),
   current_prolog_flag(f, V).
V = initial.
```

So the scope is narrower than "global", and usefully so: a caller can set a flag
and restore it around a solve, and single-threaded that is the whole story. What
cannot happen is one thread holding two values at once — which matters, because
`scasp_forall` and `scasp_dcc` change the *semantics* of `solve/4` and
`generate_nmr_check/1` rather than merely its output.

Two consequences the test suite relies on: a `scasp_*` flag set *inside a test*
is confined to the thread plunit runs that test in, so parallel jobs do not
disturb each other; one set in a unit `setup/1` runs in the main thread and
leaks into every job created afterwards.

## Where the behaviour is unspecified

Worth knowing before relying on any of it:

- **The four `forall` algorithms**, above: they disagree, and the cuts in
  `prev`/`sasp` are documented only by two code comments naming test cases.
- **`--dcc`** (dynamic consistency checking): Marple and Gupta, "Dynamic
  Consistency Checking in Goal-Directed Answer Set Programming", TPLP 14(4-5),
  2014, from s(ASP); forces `--forall=prev`.
- **Unsafe rules.** s(CASP) accepts them and reads them over an *extended*
  Herbrand universe, so `:- not married(X).` means "everyone is married". Which
  quantifier a body-only variable gets, and where, is not written down
  anywhere — and it is the source of the `forall` disagreement.
- **Module interning, source references, the HTML renderer, and the ~10×
  performance work** are all this port's, with no paper behind them.
