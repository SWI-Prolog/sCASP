# Two implementations in one pack: switching and migrating

This release of the `scasp` pack contains **two** implementations of
s(CASP):

| | library | executable | source |
|---|---|---|---|
| the SWI-Prolog port of s(CASP) | `library(scasp)` | `scasp` | `prolog/scasp/` |
| **ssCASP**, the new implementation | `library(sscasp)` | `sscasp` | `prolog/sscasp/` |

It is an intermediate release. ssCASP is meant to replace the port, and a
later release will make `library(scasp)` the new implementation. This
release lets applications try ssCASP, compare the two, and migrate at their
own pace. Nothing changes for an application that does not ask for it.

This document says what the release changes, how to choose an
implementation, and how to move a program to ssCASP. The language and the
interface of ssCASP are described in the [user manual](manual.md); the
answers that differ between the two, with an example each, are in
[semantics.md](semantics.md).

---

## 1. What changes for an existing application

**By default, nothing that changes an answer.** `use_module(library(scasp))`
loads the old implementation, and `scasp` is the old executable. Both
implementations can be loaded into the same process: their modules have
different names, and each installs its term expansions only for files that
imported it (see §5).

The old library changed in a few places since version 1.1.4, mostly to
let the two coexist:

- **`:- prolog p/2` is now `:- sld p/2`.** The operator `prolog` is gone,
  because a prefix operator with that name makes `prolog:message//1` a
  syntax error in every file that can see it. `as opaque` is unchanged.
  The `:- prolog` declaration was only ever on the `master` branch and in
  no tagged release, so most applications are not affected.
- **`:- describe p(X) :: Text`** is accepted as a synonym of
  `:- pred p(X) :: Text`. It is the spelling both libraries share; see §4.
- **Each library reads only the files that imported it.** Both libraries
  give a meaning to `-p(X)` heads, `:- pred`, `#pred`, `:- scasp_dynamic`
  and `false :- Body` through `user:term_expansion/2`. Before, whichever
  library loaded first decided what such a term meant in *every* file.
- **Redefining a control construct is an error.** A module that defines
  its own `forall/2`, `findall/3`, `not/1`, … and calls it can never run
  that definition, because the solver intercepts the call. Both libraries
  now raise `permission_error(define, scasp_control_construct, PI)` rather
  than silently giving the construct its built-in meaning.

---

## 2. Choosing an implementation

There are four ways, from least to most effort.

### 2.1 The executable

`scasp` is the old implementation and `sscasp` the new one. Both take a
file and answer its query; `make` builds both and `make install` links
both into SWI-Prolog's `bin` directory. To run the new one without
building:

```sh
swipl -p library=prolog prolog/sscasp/main.pl -- [option ...] file ...
```

As a pack, s(CASP) is also a SWI-Prolog *app*: `swipl scasp [option ...]
file ...` runs the old implementation, `swipl -Dsscasp scasp ...` the new
one, and `swipl sscasp ...` the new one always. That needs no `make`, at the
price of loading the solver at start-up. (Run it from a directory that does
not hold a file named `scasp`, such as the checkout after `make`, or
`swipl` takes that file for the script.)

The options mostly agree. The ones that differ are listed in
[manual §11](manual.md#11-changes-relative-to-the-swi-prolog-port); the
important ones for scripts are:

- `sscasp` stops after **one** answer unless given `-s N` (`-s 0` for all),
  and asks interactively only with `-i` at a terminal.
- The exit status is 0 for an answer, 1 for no answer and 2 for an error.
  `scasp` exits 1 on an error and 0 otherwise.
- `--forall`, `--short`/`--mid`/`--long`, `--pos`, `--assume`, `-r`, `-u`,
  `-v`, `-w`, `--unknown` and some others do not exist in `sscasp`.

### 2.2 A Prolog flag: `sscasp`

When the Prolog flag `sscasp` is `true` at the moment `library(scasp)` is
first loaded, `library(scasp)` re-exports `library(sscasp)` and defines
nothing of its own. A program written for `library(scasp)` then runs on
the new implementation **without being edited**. Set it on the command
line:

```sh
swipl -Dsscasp myprogram.pl
```

or in the program (or in `init.pl`), before anything loads `library(scasp)`:

```prolog
:- set_prolog_flag(sscasp, true).
:- use_module(library(scasp)).
```

Setting the flag after `library(scasp)` has been loaded has no effect.

The sub-libraries switch with it. Under the flag, `library(scasp/dyncall)`,
`scasp/html`, `scasp/human`, `scasp/listing`, `scasp/model`,
`scasp/options` and `scasp/output` are the modules of the compatibility
layer (§2.3), so a program that loads them keeps working, whether it
loads them before or after `library(scasp)`.
`library(scasp/json)` gives `scasp_results_json/2`, which takes the old
results dict and writes ssCASP's JSON document (§4, *The output*).
`library(scasp/main)` is the `sscasp` command line. `library(scasp/embed)`,
the obsolete `begin_scasp/end_scasp` blocks, has no counterpart and
**refuses to load** with an error saying so.

Two predicates exist both in `library(sscasp)` and in an old sub-library,
with different meanings. A module may import both `library(scasp)` and
the sub-library, so under the flag:

- `html_model//2` is the one from `library(scasp/html)`. It accepts the
  model straight from `scasp/2`, as the old renderer did, and a model
  already passed through `scasp_readable/2`, as the new one does. Given
  the same input, it produces the same HTML as the one in
  `library(sscasp)`.
- `scasp_query_clauses/2` is not exported by `library(scasp)`, which it
  never was. `library(scasp/dyncall)` gives it, returning
  `source(Ref, Module:Clause)` terms as the old library did. A program
  that wants the new form, the clauses as terms, imports it from
  `library(sscasp)`.

This is the quickest way to try an application on ssCASP. It is not
enough when the program

- calls something only the old library has: `begin_scasp/1,2`,
  `scasp_model/1`, `scasp_justification/2`, `scasp_stack/1`,
  `scasp_listing/2`, `scasp_trace/2`;
- depends on the old justification shape (`query-Forest` with
  `o_nmr_check`, `goal_origin/2`) or uses the interned negation
  `'-p'(X)`.

For those, use §2.3, which also rewrites the interned negation and gives
an import list naming `scasp/2` the old justification shape.

### 2.3 The compatibility layer: `library(sscasp/compat)`

Loading `library(sscasp/compat)` before the program makes the program's
own imports point at the new implementation, and gives the parts that
differ their old behaviour:

```prolog
:- use_module(library(sscasp/compat)).
:- use_module(my_application).        % says use_module(library(scasp))
```

From then on, as files are loaded:

- `use_module(library(scasp))` loads `library(sscasp)`.
- An import list that names `scasp/2` gets a `scasp/2` that returns the old
  justification shape: `query-Forest`, with the check under `o_nmr_check`.
- `library(scasp/Sub)` loads `library(sscasp/compat/Sub)`. `html`,
  `dyncall`, `json`, `model` and `output` provide what programs use of them;
  `human`, `listing` and `options` exist so that loading them succeeds.
  Other sub-libraries are not provided.
- Clauses using the old interned negation `'-p'(X)` are rewritten to
  `-p(X)`.
- `:- table scasp_prolog:_` (tabling the old bridge to Prolog) tables the
  new one, `sscasp_sld:sld_proof/3`.
- `::` and `pred` are operators; `scasp_trace/2` runs its goal without
  tracing.

Files inside the pack itself are left alone, because the old implementation
loads `library(scasp)` meaning the old one. The compatibility layer is a
bridge for running existing code; it is not where new code should be
written.

### 2.4 Edit one line

The predicates of `library(sscasp)` have the same names as those of
`library(scasp)`, so a program that only uses the shared interface changes
implementation by editing its import:

```prolog
:- use_module(library(sscasp)).     % was: library(scasp)
```

This is the end state of a migration (§3). One rule: **no file may import
both libraries.** Two libraries in one process are fine; in one file a bare
`scasp(Goal)` would resolve by import order.

---

## 3. Migrating an application

A suggested order:

1. **Run it with `-Dsscasp`** (§2.2). If it loads and answers as before,
   go to step 4.
2. If it does not load, **run it with `library(sscasp/compat)`** (§2.3).
   This tells you whether the problem is the interface or the answers.
3. **Port the code** using §4, removing the need for the compatibility
   layer one item at a time.
4. **Compare the answers** (§3.1), then change the import to
   `library(sscasp)` (§2.4).

### 3.1 Comparing answers

Expect the **bindings** of the query to agree and the **models** to differ.
Over the 49 programs of the regression corpus the two agree on the answer
for 47; the other two are wrong answers of the old implementation. They
agree on the model for 25. The differences are documented in
[semantics.md](semantics.md); the ones you are most likely to meet:

- **The model includes the closure.** `p :- not q.` with no clauses for `q`
  has the model `[p, not q]`, not `[p]`. An undefined predicate is false of
  everything, and its negation is a proved literal.
- **Models are often smaller.** A model is the literals the proof needed,
  and the two solvers find different proofs.
- **One model per answer.** By default ssCASP reports one model for each
  answer, where the old implementation reports every proof of the NMR check
  as another answer. The flags `sscasp_commit_nmr` and
  `sscasp_distinct_answers` (both default `true`) bring back more of the
  old behaviour when set to `false`.
- **A solution set may come in fewer answers.** `not p(Y)` with
  `p(X) :- X #= 3` is one answer, `Y =\= 3`, rather than `Y < 3` and
  `Y > 3`. A disequality between compound terms is one constraint, not a
  choice point.
- **Some old answers are wrong.** semantics.md §1 lists the programs where
  the old implementation answers what no stable model supports, or fails
  to answer what one does. The judge is a brute-force Gelfond–Lifschitz
  oracle (`test/stable_models.pl`) that shares no code with either.

A test that compares answers should therefore compare the query's bindings
and constraints, and compare models only where it is the model that
matters.

### 3.2 Comparing speed

Long deterministic proofs (Towers of Hanoi, N queens) run 3–10 times
faster; the event-calculus programs run at about the same speed. Over the
corpus, 20 programs are faster by more than 10% and 8 slower, and the
total is 0.66 s against 1.99 s.
[performance.md](performance.md) has the figures per program and the harness
to measure your own.

---

## 4. Porting, item by item

### Interface

| `library(scasp)` | `library(sscasp)` |
|---|---|
| `scasp/2`, then `scasp_model/1`, `scasp_justification/2` | `scasp(Query, [model(M), tree(J)])` |
| `scasp_stack/1` | not available; use `tree(J)` |
| `begin_scasp/1,2` … `end_scasp` | write ordinary Prolog clauses in a module |
| `scasp_listing/2` | `scasp_show(Query, code(Options))` |
| `scasp_trace/2` | not available |
| `?+++` | not available; there is one justification form |
| `scasp(Q, [unknown(error)])` | not available: an undefined predicate is false |
| `library(scasp/html)`: `html_model//2`, … | exported by `library(sscasp)` itself |
| `library(scasp/human)`, `--human` | `scasp_human/1,2`, `??` shortcuts |
| `library(scasp/json)`: `scasp_results_json/2` | `scasp_json/2,3`; the old predicate still works through `compat` |

New in `library(sscasp)`: `scasp_readable/2,3`, `portray_model/1,2`,
`portray_justification/1,2`, `scasp_query_clauses/2,3`,
`scasp_directive/1`, and `scasp/3,4` in `library(sscasp/run)` for a
program given as a list of terms.

### The program

- **Classical negation.** Write `-p(X)` as before. The old library stored
  it as a predicate named `'-p'/1`; ssCASP stores it as a clause of `-/1`.
  Code that names `'-p'` directly — in a clause, `:- dynamic` or
  `assertz/1` — must be changed to `-p(X)` or go through `scasp_assert/1`.
  `scasp_dynamic p/1` declares what is needed for both `p/1` and `-p/1`.
- **Descriptions.** Write `:- describe p(X) :: "text"`. `#pred` is still
  read. `:- pred p(X) :: "text"` works but warns, because in ssCASP
  `:- pred p(d)` (without `::`) is a **signature**: it says that the
  argument ranges over the domain `d`, which changes what is proved.
  Mention a variable in the text as `@(X)`, and write it as `_X` in the
  head so that Prolog does not report a singleton.
- **Calling Prolog.** `:- sld p/2` (and `as opaque`), as in the old library
  in this release. One declaration may list several:
  `:- sld (p/2, q/1) as opaque.`
- **Global constraints** are written `false :- Body.` in a Prolog module,
  as before.
- **`∉` in a body** works: `p(X) :- X ∉ [a, b].` Its negation is `∈`. In the
  old implementation it does not run in a program.
- **Domains** are new and optional: `:- domain day = [mon, tue, wed].`,
  `:- domain slot = 1..10.`, and `:- pred meets(person, day)`. They make
  negation and `forall` enumerate a finite domain instead of covering an
  infinite one, which can be much faster. See manual §3.5.
- **Standard order** (`@<`, `@>`, …) and `inf/2`, `sup/2` are not built-ins
  of ssCASP yet. A program that uses them does not run.
- **`findall/3` under negation** is answered in ssCASP (`not findall(...)`
  holds where the list differs from the solutions); the old implementation
  always fails it.

### The output

- **Justification.** A forest of `Literal-Children`: the query's literals,
  then `'$nmr'-Children` for the NMR check. The old tree is one
  `query-Children` root with `o_nmr_check` among them. The wrappers are
  `chs/1` and `proved/1`; there is no `goal_origin/2`, `assume/1` or
  `abduced/1`, because ssCASP keeps no source references.
- **JSON** (`--json`, `scasp_json/2,3`). `tree` is a list; a `constraints`
  entry is a list, because a variable can have both a disequality and a
  CLP(Q) constraint; `truth` is always a string. `module`, `source_file`,
  `source_line`, `assume` and `abduced` are not produced.
- **Listing** (`--code`, `scasp_show/2`). Duals are printed as disjunctions
  and the output cannot be loaded back. There is no `--compiled` input.
- **Language.** Justifications in words are English only.

### Flags

`scasp_forall`, `scasp_plain_dual`, `scasp_unknown`, `scasp_lang`,
`scasp_real`, `scasp_assume`, `scasp_verbose` and the other old flags are
not read by ssCASP. `scasp_compile_nmr` and `scasp_compile_olon` are shared,
with the same meaning. Dynamic consistency checking is `sscasp_dcc`, on by
default, and not the old `scasp_dcc`, which is off by default and forces
`--forall=prev`. The new flags are in manual §7.

For debugging, the flag `scasp_module` keeps the compiled program in a
module after the query: `true` for a generated name, or an atom naming the
module. It does the same as the option `module(M)` of `scasp/2` and the
`--module` option of `sscasp`, for every query at once.

### SWISH

Both libraries declare what SWISH may run. ssCASP allows the query
shortcuts, `scasp/1,2`, `scasp_show/2`, `scasp_json/2,3` and
`scasp_human/1,2`, and refuses the option `module(M)`.

---

## 5. Using both in one process

The two implementations have different module names (`scasp_*` and
`sscasp_*`), so both can be loaded together, for example to compare them
on the same program. Three things to know:

- **One file, one library.** A file imports `library(scasp)` or
  `library(sscasp)`, not both.
- **Directives belong to the library the file imported.** A `-p(X)` head,
  `:- pred`, `#pred`, `:- scasp_dynamic` and `false :- Body` are expanded by
  the library the file imported, and by no other. A file that imports
  neither gets ordinary Prolog.
- **`library(scasp)` decides once.** With `-Dsscasp`, `library(scasp)`
  and its sub-libraries are the new implementation for the whole process,
  so the old one cannot then be loaded under those names. To compare the
  two, run without the flag and import `library(sscasp)` in the files
  meant for the new one.

---

## 6. What comes next

In a later release `library(scasp)` and `scasp` become the new
implementation and the old tree is removed. What this means for an
application:

- A program that runs under `-Dsscasp` today will run unchanged.
- A program that needs `library(sscasp/compat)` should be ported before
  then, using §4.
- A program that imports `library(sscasp)` will keep working.

Please report a program that answers differently and is not explained by
[semantics.md](semantics.md) at <https://github.com/SWI-Prolog/sCASP/issues>,
with the program and the query.
