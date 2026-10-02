# ssCASP user manual

ssCASP is an implementation of **s(CASP)** for SWI-Prolog. s(CASP) is
answer set programming that answers a query *goal-directed*, the way
Prolog does, rather than by grounding the whole program first. Because
nothing is grounded, variables may range over infinite domains. They may
also be constrained: by disequality (`X ∉ [a, b]`) and by linear
arithmetic over the rationals (`T #> 3`). Every answer comes with the
part of a stable model that supports it, and with a justification that
says why.

s(CASP) comes from two groups. Gopal Gupta's group at the University of
Texas at Dallas developed goal-directed answer set programming: coinductive
logic programming, the s(ASP) solver, the NMR check and dual rules
[Simon et al. 2006; Marple 2014; Marple et al. 2017]. Joaquín Arias, with
Manuel Carro in the Ciao group at IMDEA Software and the Universidad
Politécnica de Madrid, extended it with constraints into s(CASP), built
the system in Ciao Prolog, and added justifications and the event-calculus
encoding [Arias et al. 2018, 2020, 2022]. The SWI-Prolog port followed
[Wielemaker et al. 2021]. §12 lists the papers.

ssCASP is a new implementation of the same language. It was written beside
the SWI-Prolog port of s(CASP) (`library(scasp)`) and is meant to replace
it. It ships in the same pack as `library(sscasp)` and as the `sscasp`
executable. The last two sections of this manual list what is different
from the Ciao Prolog original and from the SWI-Prolog port.

Contents:

1. [Installing](#1-installing)
2. [A first program](#2-a-first-program)
3. [The language](#3-the-language)
4. [What an answer is](#4-what-an-answer-is)
5. [Using it from Prolog](#5-using-it-from-prolog)
6. [The `sscasp` executable](#6-the-sscasp-executable)
7. [Flags](#7-flags)
8. [Running a program written for `library(scasp)`](#8-running-a-program-written-for-libraryscasp)
9. [Limitations](#9-limitations)
10. [Changes relative to s(CASP) for Ciao Prolog](#10-changes-relative-to-scasp-for-ciao-prolog)
11. [Changes relative to the SWI-Prolog port](#11-changes-relative-to-the-swi-prolog-port)
12. [References](#12-references)

---

## 1. Installing

ssCASP is written entirely in Prolog. It needs SWI-Prolog 9.3.33 or later
for `library(json)`; `pack.pl` still says 9.

**As a library.** Install the pack from the top directory of a checkout:

```prolog
?- pack_install(.).
```

After that, `:- use_module(library(sscasp)).` loads it. Alternatively, run
from the checkout without installing by putting its `prolog` directory on
the library path:

```sh
swipl -p library=prolog myprogram.pl
```

**As an executable.** `make sscasp` builds `./sscasp`, a saved state with
the whole solver in it. `make install` symlinks it, and the old `scasp`,
into SWI-Prolog's `bin` directory. To run it without building:

```sh
swipl -p library=prolog prolog/sscasp/main.pl -- [option ...] file ...
```

On Windows, put SWI-Prolog's `bin` directory on `%PATH%` and run the same
command that `make` does:

```sh
swipl -f none --no-pce --undefined=error -O -o sscasp -c prolog/sscasp/main.pl
```

`prolog/sscasp/` loads nothing from `prolog/scasp/`, so the new
implementation runs from a library root that holds only that tree.

---

## 2. A first program

Here is a program as an s(CASP) file, `birds.pl`:

```prolog
bird(tweety).
bird(sam).
penguin(sam).

flies(X) :- bird(X), not penguin(X).

?- flies(X).
```

```
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
```

The same clauses work as an ordinary Prolog module. You load the library
and ask a question:

```prolog
:- use_module(library(sscasp)).

bird(tweety).
bird(sam).
penguin(sam).

flies(X) :- bird(X), not penguin(X).
```

```prolog
?- ? flies(X).
X = tweety,
% s(CASP) model
{ bird(tweety),  flies(tweety),  not penguin(tweety) }.

?- scasp(flies(X), [model(M)]).
X = tweety,
M = [bird(tweety), flies(tweety), not penguin(tweety)].
```

A program can also say its literals in words (see `:- describe` in §3.6):

```prolog
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
```

---

## 3. The language

A program is a set of Prolog clauses. Their bodies may use two kinds of
negation, constraints and a few constructs of their own. The same language
can be written in two places:

- **An s(CASP) file** is read by the executable and by `scasp/3,4`. It holds
  clauses, `#` directives, `:- ...` terms and a `?- Query.`
- **A Prolog module** is queried with `scasp/1,2` and the `?` shortcuts. It
  holds ordinary clauses and directives.

The two differ in exactly one place: how a global constraint is written
(§3.3).

### 3.1 Negation

| | | |
|---|---|---|
| `not p(X)` | default negation | there is no evidence for `p(X)` |
| `-p(X)` | classical negation | `p(X)` is known to be false |

`not` is *constructive*. Asking `not p(X)` for an unbound `X` does not just
test whether the query fails. It answers with the values of `X` for which
`p(X)` cannot be proved, and it states them as constraints:

```prolog
p(X) :- X #> 3, X #< 10.
?- not p(X).
%  X = A | {A =< 3} ;
%  X = A | {A >= 10}
```

A classically negated head is written as is: `-flies(X) :- penguin(X).`
Every predicate that has a classically negated form also gets the
constraint `:- p(X), -p(X)`, so a model may not hold both.

**The program is closed.** If a predicate is called but never defined, it
is false of everything, and its negation is true of everything. There is no
warning and no error. The one exception is the *query*: asking for a
predicate the program does not define is reported, as
`existence_error(scasp_predicate, PI)`, because that is most likely a typo.

### 3.2 Constraints on variables

**Disequality.** `X \= Y` posts a disequality in the Herbrand store rather
than failing. `X ∉ [a, b]` says that `X` is neither `a` nor `b`. Its
negation, `X ∈ [a, b]`, is a *generator*: it enumerates the values.

**Linear arithmetic over the rationals** (CLP(Q)):

| | |
|---|---|
| `#=`, `#<>`, `#<`, `#>`, `#=<`, `#>=` | constraints over Q |
| `is`, `=:=`, `=\=`, `<`, `>`, `=<`, `>=` | evaluated when ground, otherwise posted as the constraint above |

In a file, `1/2` is read as a rational. Each comparison has a comparison as
its dual (`#<` against `#>=`), so negating a rule whose body has
constraints gives the complement region. A complement may be more than one
region, and then it comes back as more than one answer.

A constraint is not a literal. It does not appear in the model and is not a
step in the justification. It is attached to the variable it constrains,
and it is printed that way: `X = A | {A >= 10}`.

### 3.3 Global constraints

A global constraint (an *integrity constraint*) rules out every model in
which its body holds:

| in an s(CASP) file | in a Prolog module |
|---|---|
| `:- open(D), weekend(D).` | `false :- open(D), weekend(D).` |

In a Prolog module, `:- Body` is a directive that runs when the file is
loaded, so a constraint there has to be written `false :- Body`.

Together with the odd loops over negation (`p :- not p`) and the
consistency of classical negation, global constraints make up the
**NMR check**. The NMR check is proved as part of every query, in the same
proof as the query itself. An answer whose model would violate a constraint
is therefore never reported.

### 3.4 Quantifiers and collections

`forall(V, Goal)` holds when Goal holds for every value of `V`. You rarely
write it yourself. The compiler produces it whenever it negates a clause
whose body introduces variables that are not in the head.
`not forall(V, G)` is existential.

`findall(T, Goal, L)` collects the solutions of Goal. What proves them goes
into the model. `not findall(T, G, L)` holds where `L` differs from the
list of solutions.

### 3.5 Domains and signatures

A variable that the head does not bind is quantified over the whole
universe. When a predicate is only meant for a finite set of values,
declare that set. The program then means what was intended, and the solver
enumerates the values instead of searching the universe.

```prolog
:- domain day = [mon, tue, wed].     % facts day(mon), day(tue), day(wed)
:- domain hour = 0..23.              % a CLP(Q) region, not a list
:- pred open(day).                   % open/1 only holds of a day
```

In an s(CASP) file, write `#domain day = [mon, tue, wed].`: there,
`:- Body` is a global constraint (§3.3).

A **signature** `:- pred open(day)` puts a `day(D)` guard in front of every
clause of `open/1`, and in front of every `not open(T)` in a body or a
query. Outside the domain, both `open(sat)` and `not open(sat)` are false,
and a negation asks only about the domain. Any unary predicate the program
defines can serve as a domain.

`--domain-error` (the flag `scasp_domain_error`) turns a ground value
outside the domain into an error instead of a failure.

You do not always have to declare a domain. When a variable is bound by a
positive literal of a predicate defined only by ground facts, the solver
takes those facts as the variable's domain without being told.

### 3.6 Directives

| Prolog module | s(CASP) file | |
|---|---|---|
| `:- describe L :: Text.` | `#describe L :: Text.`, or `#pred L :: 'Text'.` | how to say literal `L` in words |
| `:- pred p(d1, ...).` | `#pred p(d1, ...).` | a signature (§3.5) |
| `:- domain d = [...]` / `= Lo..Hi` | `#domain d = [...]` / `= Lo..Hi` | a domain |
| `:- abducible p(X).` | `#abducible p(X).` | `p(X)` may be assumed, or not |
| `:- show p/1.` | `#show p/1.` | restrict the printed model |
| `:- sld p/2.` | `#sld p/2.` | prove `p/2` by Prolog's SLD resolution |
| `:- scasp_dynamic p/1.` | — | `p/1` is changed at run time (§5.6) |
| — | `#include(File).` | read another file here |
| — | `?- Query.` | the query; the last one counts |

**Either spelling works in either place.** A Prolog module also reads
`#describe`, `#pred`, `#domain`, `#abducible`, `#show` and `#sld`, and a
file also reads the `:-` forms, so a program can be moved between the two
unchanged. The table gives the spelling that reads naturally in each. In a
file the `#` form is the safer one: there, a `:-` term that is not one of
these declarations is a global constraint (§3.3), so a misspelt declaration
would silently become a constraint. Three entries belong to one place only:
`:- scasp_dynamic` to a module, `#include` and `?-` to a file.

**`:- describe`.** `Text` is a string or an atom. In it, `@(X)` (or
`{{X}}`) stands for the variable the directive calls `X`. `@(X:Type)`
adds a type, which the text uses as a noun. `Text` may also be `html(Spec)`,
for HTML output. Each of the four forms `p(..)`, `not p(..)`, `-p(..)` and
`not -p(..)` can have its own description. When several apply, the one
written *later* wins, so describe `flies(tweety)` after `flies(X)` to
override it for one individual. The Prolog reader reports the variable as a
singleton; write it `_X` and keep `@(X)` in the text, because the leading
underscore is ignored on both sides. `:- describe L - Pattern :: Text`
describes a literal together with part of the proof beneath it.

A description has no effect on what is proved. `:- pred p(X) :: Text` is
still accepted but gives a warning, because `:- pred` is now the signature.

**`#abducible p(X)`** lets the solver assume `p(X)` or `-p(X)`, whichever an
answer needs. It is an even loop between the two, and so a free choice.

**`:- sld p/2`** hands `p/2` to Prolog's SLD resolution: first clause first,
left to right, with cut, if-then-else and `=>` available. Its clauses are
not dualised or collected. `:- sld p/2 as opaque` runs the goal and shows
only the call in the justification. The default, `justified`, shows the
derivation. Several predicates go in one declaration as
`:- sld (p/2, q/1) as opaque`; without the parentheses, `as` applies to the
last predicate only. In an s(CASP) file, the declaration is `#sld p/2`, and
the clauses of `p/2` come from the file itself. They are run as Prolog and
may call any other predicate the file defines. A negated SLD goal is still constructive: the solutions
are collected and the variable is kept away from each of them.

A program may not *redefine* something the compiler gives a meaning of its
own and then call it: `not/1`, `forall/2`, `findall/3`, `,/2` or a built-in.
That raises `permission_error(define, scasp_control_construct, PI)`, rather
than quietly giving the program a meaning it was not written with.

**A warning to heed.** `not p(X, Y)`, where `Y` occurs nowhere else in the
clause, holds when `p(X, Y)` is false for *some* `Y`, and that is rarely
what was meant. The library warns about it when the file is loaded. To say
"there is no such `Y`", define an auxiliary predicate `has_p(X) :- p(X, _)`
and negate that instead.

---

## 4. What an answer is

An answer is three things: **bindings** for the query's variables (with
their constraints), a **model**, and a **justification**.

The **model** is the set of literals the proof used, sorted: what holds, what
classically does not hold (`-p`), and what has no support (`not p`). It is
*partial*. It contains what this answer rests on, not a whole stable model.
The model is a function of the proof, so two proofs of the same bindings may
carry different models.

The **justification** is that proof as a forest. The query's literals come
first, and everything the NMR check needed comes under a node of its own,
printed as "global constraints". Two markers matter:

| | |
|---|---|
| `chs(p)` | `p` holds by the coinductive hypothesis: it is on an even loop over negation and was assumed |
| `proved(p)` | `p` was already established earlier in this proof |

**One answer is one model.** Once the query is proved, ssCASP takes the
first proof of the NMR check that leaves the answer as it is, and does not
go on to enumerate further models of the same answer. That is what s(CASP)
is mostly used for: an answer, and a justification for it. To get every
model, set `--no-commit-nmr` (the flag `sscasp_commit_nmr`). An answer the
search reaches twice, with the same bindings, model and constraints, is
reported once; the flag `sscasp_distinct_answers` controls this.

**Relevance.** `scasp/2` over a Prolog module collects only the clauses the
query can reach. It adds the global constraints that overlap those clauses,
and for every `p` it reaches, also `-p`. As a consequence, a contradiction
in a part of the program the query cannot reach does not affect the answer.
If you want whole-program semantics, name the rest of the program:
`scasp(q, [collect((p, -p))])`.

---

## 5. Using it from Prolog

```prolog
:- use_module(library(sscasp)).
```

### 5.1 Asking

`scasp(:Query)` and `scasp(:Query, +Options)` are true when Query holds in a
model of the clauses it reaches. They are nondeterministic, with one
solution per answer.

| option | |
|---|---|
| `model(-M)` | the model: a list of literals |
| `tree(-J)` | the justification, a forest of `Literal-Children` |
| `collect(:Goal)` | also collect what Goal reaches, without asking it |
| `program(-Terms)` | the program as collected |
| `module(?M)` | load the compiled program into M and keep it there, for debugging or profiling |

The model and the justification **share variables with the query**, so a
constraint on an answer variable is visible in all three. To print them
from a program, use:

| | |
|---|---|
| `portray_model(+M, +Options)` | `layout(lines\|columns)`, `indent(N)`, `width(N)`, `format(unicode\|ascii)`, `colour(Bool)`, `human(Bool)` |
| `portray_justification(+J, +Options)` | `indent(N)`, `step(N)`, `max_depth(N)`, `human(Bool)` |
| `scasp_readable(+Term, -Readable[, +Options])` | name the variables and put their constraints inline, as in `A \| {A >= 10}` |

Neither printer ends with a newline. Pass the model and justification
through `scasp_readable/2` *together* if you print both, so that a variable
they share gets the same name in each.

### 5.2 At the toplevel

| | after the bindings, shows |
|---|---|
| `?-- Q` | nothing more |
| `? Q`, `?+- Q` | the model |
| `?-+ Q` | the justification |
| `?? Q`, `?++ Q` | both |
| `??+- Q`, `??-+ Q`, `??++ Q` | the same, in words |

These print through the toplevel, after the bindings, so a variable that
the bindings, the model and the constraints share has one name. Called from
a program they print nothing. There, use `scasp_human/1,2` or the printers
above.

### 5.3 In words

`scasp_human(:Query[, +Options])` prints `Model:` and `Justification:` in
words. Each literal is said by the program's own `:- describe` if there is
one, otherwise by a built-in vocabulary ("there is no evidence that …", "it
is not the case that …", "… holds for …"). The options
`show_model(Bool)` and `show_justification(Bool)` choose what to print.

The HTML renderer behind this is exported as `html_model//2`,
`html_justification//2`, `html_query//2` and `html_atom//2`, for use in a
web page. The option `show(human|machine|both)` selects which rendering is
produced.

### 5.4 As data

`scasp_json(:Query, -Dict[, +Options])` collects every answer into a dict
that `json_write_dict/3` writes. The options are those of `scasp/2`, plus
`limit(Count)` and `variable_names(Bindings)`. A term describes its own
shape: `{"type":"var","name":"A"}`, `{"type":"compound","functor":...}` and
so on. Each answer has `bindings`, `model`, `tree` and `constraints`.
A model literal has a `truth`, which is always a string: `"true"`,
`"false"`, `"likely"` or `"unlikely"`.

### 5.5 The program the query runs

`scasp_show(:Query, code(Options))` prints the program the query would run.
Its options are `query`, `user` (both on by default), and `duals`,
`constraints`, `source` and `compiled`. The duals are printed as
disjunctions, the way you would write the negation of a predicate by hand:

```prolog
not flies(A) :-
    (   not bird(A)
    ;   bird(A),
        penguin(A)
    ).
```

`scasp_query_clauses(:Query, -Terms)` returns the collected program as a
term list.

### 5.6 A program that changes

```prolog
:- scasp_dynamic p/1.              % thread_local; `p/1 as shared` for dynamic
?- scasp_assert(-p(a)).            % a classically negated fact
?- scasp_assert((false :- p(X), q(X))).
```

`scasp_assert/1,2`, `scasp_retract/1`, `scasp_retractall/1` and
`scasp_abolish/1` handle the two kinds of clause Prolog's own `assertz/1`
cannot: a `-p` head and a global constraint. The next query sees the
change; there is nothing to recompile. The position argument of
`scasp_assert/2` is accepted and ignored.

### 5.7 Programs as terms

`library(sscasp/run)` takes a program as a list of terms. Load
`library(sscasp)` as well, for the operators:

```prolog
?- scasp([(p :- not q), (q :- not p)], p, Model, Justification).
Model = [p, not q],
Justification = [p-[(not q)-[chs(p)-[]]], '$nmr'-[]].
```

`scasp/3` returns the model alone.

---

## 6. The `sscasp` executable

```
sscasp [option ...] file ...
```

All the files are read as one program. The query is the last `?- Q.` in
them, unless `--query` gives one.

| option | |
|---|---|
| `-s N`, `-n N`, `--answers=N` | report N answers, 0 for all; the default is 1 |
| `--query=GOAL` | answer GOAL instead of the program's query |
| `--tree` | print the justification of each answer |
| `--human` | say the model and justification in words |
| `--json=FILE` | write the answers as JSON (`-` for standard output) |
| `--html=FILE` | write a self-contained HTML page (`-` for standard output) |
| `--code` | print the program and exit |
| `--duals`, `--constraints`, `--source`, `--compiled` | … with these sections instead |
| `--width=N`, `--colour` | page width; ANSI colour |
| `-i`, `--interactive` | read queries until end of input |
| `--no-nmr`, `--no-olon` | leave out the NMR check, or only its odd loops (diagnostic) |
| `--no-dcc` | check global constraints only at the end |
| `--no-commit-nmr` | report every model of an answer |
| `--domain-error` | a value outside a signature's domain is an error |
| `--module=M` | keep the compiled program in module M |
| `--version`, `-h` | |

`--no-nmr` and `--no-olon` are for finding out what a query costs on its
own. With the check turned off, an answer that violates a constraint is
still reported.

**Exit status.** 0 if there is at least one answer, 1 if there are none,
2 on an error (a missing file, a syntax error, no query). Scripts can
therefore tell "no" apart from "broken".

**`--json` and `--html`** write only the document; there is no heading in
front of it. They write when the run ends, so a run that is interrupted
leaves no half-written document.

**`-i`** compiles the program once, then answers each query it reads.
An error in one query is reported and the session goes on. At a terminal,
answers are offered one at a time (`;` asks for the next) and there is line
history. Reading from a pipe, `--answers` is honoured. `exit`, `quit`,
`halt` and end of input end the session.

The query keeps the variable names it was written with, in both
`?- flies(X)` and `X = tweety`.

---

## 7. Flags

Every option is also a Prolog flag, so a program using the library can set
it. Flags are thread-local once set.

| flag | default | |
|---|---|---|
| `sscasp_commit_nmr` | `true` | one model per answer (§4) |
| `sscasp_distinct_answers` | `true` | an answer found twice is reported once |
| `sscasp_dcc` | `true` | dynamic consistency checking: a global constraint is checked as soon as the literals it names are proved, as well as at the end [Marple and Gupta 2014] |
| `scasp_compile_nmr` | `true` | compile the NMR check; diagnostic only |
| `scasp_compile_olon` | `true` | compile the odd-loop part of it; diagnostic only |
| `scasp_domain_error` | `false` | §3.5 |
| `sscasp_head_guards` | `true` | test a rule's head patterns before proving its body |
| `sscasp_facts_first` | `true` | move literals of fact predicates to the front of a body (the answers stay the same; the order in the justification changes) |
| `sscasp_unfold_facts` | `true` | unfold body literals against the facts of their predicate … |
| `sscasp_unfold_limit` | `8` | … when there are at most this many |
| `sscasp_unfold_derived` | `true` | … and against the finite answer set of a derived predicate … |
| `sscasp_answer_limit` | `256` | … of at most this many answers |
| `sscasp_table` | `false` | experimental tabling of self-contained subproofs |

The flags from `sscasp_head_guards` down are optimisations. Turning one off
should change the time an answer takes and the shape of its justification,
not the answers themselves.

Two names are shared with `library(scasp)`. `scasp_compile_nmr` and
`scasp_compile_olon` are spelled and scoped the same way, so one setting
affects both libraries in one process. Dynamic consistency checking
deliberately uses a name of its own (`sscasp_dcc`). The old `scasp_dcc` is
off by default and its meaning differs.

---

## 8. Running a program written for `library(scasp)`

Load the compatibility layer first:

```prolog
:- use_module(library(sscasp/compat)).
:- use_module(old_program).          % says use_module(library(scasp))
```

From then on, files loaded from outside the ssCASP checkout are rewritten
as they load:

- `use_module(library(scasp))` loads `library(sscasp)`.
- An import list that names `scasp/2` gets a version that returns the old
  justification shape (`query-Forest`, with the check under `o_nmr_check`).
- `library(scasp/Sub)` loads `library(sscasp/compat/Sub)`. The parts a
  program typically reaches (`html`, `dyncall`, `model`, `output`) are
  provided; the rest are stubs.
- Clauses using the old interned negation `'-p'(X)` become `-p(X)`.
- `:- table scasp_prolog:_` tables `:- sld` goals instead.
- `::` and `pred` are made operators.

`scasp_trace/2` runs its goal without tracing.

---

## 9. Limitations

- **Speed varies.** Long deterministic proofs, such as the Towers of Hanoi
  and N queens, run 3–10 times faster than with the SWI-Prolog port. The
  event-calculus programs run at about the same speed, a little faster or
  slower depending on the program. Over the regression corpus the total is
  0.66 s against 1.99 s. The constraint store grows with the proof, and a
  quantifier that has to cover a large CLP(Q) region repeatedly is where the
  time goes. `doc/performance.md` has the figures.
- **`call/1`** is not a control construct. `call(G)` is an ordinary literal.
- **A negated structured head** asked about with an unbound argument is
  incomplete. Declaring a domain for the argument fixes this.
- **A loop through `findall/3`** is not detected as a loop.
- **No source references.** A justification step does not say which clause
  it came from.

---

## 10. Changes relative to s(CASP) for Ciao Prolog

The SWI-Prolog port was forked from the Ciao Prolog original at commit
`4d33909` (28 May 2021, version 0.21.05.28). Ciao s(CASP) has been
developed further since. This section compares ssCASP with its current
version, 0.25.08.14 (the last commit is from 24 February 2026), and says
which Ciao features arrived after the fork.

### What Ciao s(CASP) gained after the fork

| | |
|---|---|
| 2021 | the s(ASP) `forall` (`--sasp_forall`); `#domain`; dynamic consistency checking (`--dcc`, which forces `--prev_forall`) |
| 2022 | `--co`, coinductive success of positive loops; loop checks extended to CLP(Q) entailment; positive loops detected by unification with occurs check; cut, `var/1`, `display/1`, `nl/0`; `--machine`; `--timeout`; integration with the Ciao toplevel and the s(CASP) playground |
| 2023 | `--bind`, `--txt`, `--quiet`; `#table_once` for ground subgoals; debugging predicates (`!display/1`, `!spy`, …) and `-v0` … `-v6`; `random/1,3` |
| 2025 | forgetting (`--forget`); event-calculus support (`--ec`, `--incremental`, Zeno detection); stratification (`--strata`); warnings for missing predicates; `--json` |
| 2026 | `#abduce_Ntimes`, which bounds how often an abducible may be assumed (like a choice rule in clingo) |

### Distribution and front end

| Ciao s(CASP) | ssCASP |
|---|---|
| a Ciao bundle, `ciao get`, `scasp` executable, and a web playground | a SWI-Prolog pack, `library(sscasp)`, `sscasp` executable |
| programs only from files; `load/1`, `solve/4` and the `?`/`??` shortcuts in a Ciao module | programs from files *and* as ordinary Prolog clauses in any module, queried with `scasp/1,2` or the `?` shortcuts, relevance-scoped (§4) |
| its own tokenizer | the SWI-Prolog reader: `;`, `->`, quoted atoms and strings, `1/2` as a rational, and Unicode operators all work |
| text, `--html`, `--json`, `--bind`, `--txt` | text, `--html`, `--json`, `--human`, and the same three from Prolog |

### Command line

| Ciao | ssCASP |
|---|---|
| `-sN`, `-nN` | `-s N`, `-n N`, `--answers=N` |
| `-i` (leave with `exit.`) | `-i`; any error is reported and the session continues |
| `--code`, `--tree`, `--human`, `--html[=name]`, `--json[=name]` | the same; the file is named in full, and `-` writes to standard output |
| `--plain` | the default |
| `--long`, `--mid`, `--short`, `--pos`, `--neg` | dropped; the justification has one form |
| `-d`, `--plaindual` | dropped; duals are always as compiled |
| `-r[=d]` | dropped; rationals print exactly |
| `--prev_forall` (default), `--sasp_forall`, `--c_forall`, `--all_c_forall` | dropped; there is one quantifier (below) |
| `--dcc`, off by default | `--dcc`, on by default, with the one quantifier; `--no-dcc` |
| `--co` | dropped (below) |
| `--no_nmr`, `--no_olon` | `--no-nmr`, `--no-olon` |
| `--timeout[=MS]` | dropped |
| `--bind`, `--txt`, `--quiet`, `--machine` | dropped |
| `--forget`, `--strata`, `--ec`, `--incremental`, `--zeno_*` | not available |
| `-v`, `-v0` … `-v6`, `-w`, `--update`, `-c`/`--compiled` | dropped |
| — | `--query`, `--duals`, `--constraints`, `--source`, `--compiled`, `--no-commit-nmr`, `--domain-error`, `--module` |
| exit status not significant | 0 answer, 1 none, 2 error |

### Language

| Ciao | ssCASP |
|---|---|
| `#compute N {Goals}` | not supported; use `?- Goals.` and `-s N` |
| `#table`, `#table_once` | not supported |
| `#abduce_Ntimes` | not supported |
| `#pred p(X) :: 'text @(X:type)'` | still accepted, and read as `:- describe`; `:- describe` is the spelling going forward and also takes `not p`, `-p`, `not -p` and `html(...)` |
| `#domain`: a ground call to a domain predicate that has no matching clause is reported as an error | `#domain d = [..]` / `Lo..Hi` (`:- domain` in a module) and `#pred p(dom)` signatures, which bound the quantifiers and the negations as well (§3.5) |
| — | `X ∉ [..]` and `X ∈ [..]` in bodies |
| cut, `var/1`, `display/1`, `random/1,3`, `!display/1` and other debugging predicates | not built in; use `#sld p/2` to call Prolog, where cut, if-then-else and all of Prolog are available |
| CLP(Q) via `#=` etc. or `.=.` | `#=` etc., and `is`/`<`/… posted as constraints when not ground |
| `\=` as constructive disequality | the same, in a store that also holds `∉` sets |
| `not findall/3` fails | `not findall/3` is answered |
| undefined predicates fail; since 2025 `--code` and `-v` report them | undefined predicates are false, and `not p` for them appears in the model (§3.1) |

### Semantics

- **The quantifier.** Ciao has four `forall` algorithms. At the fork the
  default was `c_forall`, which is not exhaustive; today it is
  `prev_forall`, which `--dcc` also requires. ssCASP has one. It covers the
  quantified variables as a tuple, reads which part of the domain each
  answer covered from the constraints it left, and covers the rest. A
  declared or inferred domain is enumerated instead. It always covers the
  whole domain.
- **Loops.** Since 2022 Ciao fails a goal as a positive loop when it merely
  *unifies* with an ancestor (with occurs check), except in
  event-calculus mode. ssCASP fails it only when it is a *variant* of the
  ancestor, or an instance of it with no negation in between, with the
  constraint stores taken into account. A goal that is more general than
  its ancestor is a different question, and failing it loses answers.
  While ssCASP was being developed, reading it the Ciao way left
  `holdsAt(spilling, T)` without a proof in every event-calculus program.
- **Coinduction** is four rules rather than six, counting polarity
  transitions along the path. Ciao's `--co`, which lets a positive loop
  succeed coinductively with a warning, has no counterpart: in ssCASP a
  positive loop always fails, because that is what the stable model
  semantics says.
- **The CHS check states its constraint between whole literals.** Both
  constrain a goal against the opposite literals already proved rather than
  failing: `not d(X)` with `d(1)` proved answers `X ≠ 1`. Ciao posts the
  constraint argument by argument; ssCASP posts a disequality between the
  literals, which says "they differ somewhere" without a choice point.
- **One answer, one model** by default (§4).
- **Dynamic consistency checking** is on by default. Ciao has had it since
  2021, off by default and only with `--prev_forall`.

ssCASP's answers are checked against a brute-force Gelfond–Lifschitz oracle
on thousands of randomly generated programs. The cases where the
SWI-Prolog port, which shares Ciao's design up to the fork, differs from
that oracle are listed in §11.

---

## 11. Changes relative to the SWI-Prolog port

The SWI-Prolog port is `library(scasp)` and its `scasp` executable, at
version 1.1.4 (commit `2dfbdb9` on the `master` branch). It is the code
ssCASP was started beside. Both trees are in this pack, with different
module names, so both can be loaded into one process. The exported
predicate names are the same, so a program changes library by editing one
`use_module/1` line.

### Interface

| `library(scasp)` | `library(sscasp)` |
|---|---|
| `scasp/2` | `scasp/1,2`; also `scasp/3,4` over a term list |
| `scasp_model/1`, `scasp_justification/2`, `scasp_stack/1` | the `model(M)` and `tree(J)` options |
| `?`, `??`, `?--` … `??++`, `?+++` | the same, except `?+++` (there is no long tree) |
| `begin_scasp/1,2` … `end_scasp` | removed; write ordinary clauses |
| `scasp_listing/2` | `scasp_show/2` |
| `scasp_trace/2` | removed (a no-op in `compat`) |
| — | `scasp_json/2,3`, `scasp_human/1,2`, `scasp_readable/2,3`, `portray_model/1,2`, `portray_justification/1,2`, `scasp_query_clauses/2,3`, `scasp_directive/1` |
| `:- prolog p/2 [as opaque]` | `:- sld p/2 [as opaque]`. A prefix operator named `prolog` makes `prolog:message//1` a syntax error, so the declaration names the resolution instead |
| `:- pred p(X) :: Text`, `#pred` | `:- describe p(X) :: Text`; `#pred` is still read; `:- pred` is now a signature |
| `-p(X)` interned as a predicate `'-p'/1` | `-p(X)` is a clause of `-/1`; `library(sscasp/compat)` converts the old spelling |
| — | `:- domain`, `:- pred p(dom)`, `∈`, `scasp_domain_error` |

### Executable

| `scasp` | `sscasp` |
|---|---|
| `--forall=all\|all_c\|prev\|sasp` | dropped; one quantifier |
| `--dcc` (off, forces `--forall=prev`) | `--dcc` on by default; `--no-dcc` |
| — | `--no-commit-nmr`, `--domain-error`, `--module` |
| `--short`/`--mid`/`--long`, `--pos`, `--assume` | dropped |
| `-r=N`, `-u`, `-v`, `-f`, `-w`, `--trace_dcc`, `--unknown`, `--raw`, `-m`, `-c`, `-d`, `--css`, `--script`, `--collapse` | dropped |
| `--code` with `--short/--mid/--long` | `--code` with `--duals`, `--constraints`, `--source`, `--compiled` |
| asks after each answer by default | reports one answer by default (`-s 0` for all); asks only with `-i` at a terminal |
| exit status 1 on error, 0 otherwise | 0 answer, 1 none, 2 error |
| `-i`: an error in a query ends the session | `-i`: the error is reported and the session goes on |
| `--code` output is loadable (`--compiled`) | `--code` is a listing with duals inlined as disjunctions; it is not loadable |

### Flags

`scasp_forall`, `scasp_plain_dual`, `scasp_unknown`, `scasp_lang`,
`scasp_unicode`, `scasp_real`, `scasp_assume`, `scasp_verbose`,
`scasp_warnings`, `scasp_trace_failures`, `scasp_list_raw`,
`scasp_no_fail_loop` and the old `scasp_dcc` have no counterpart. The words
of a justification are English only; the Dutch translation was not carried
over. The new flags are listed in §7.

### Answers

The same program can answer differently. Each kind of difference below is
documented, with an example, in `doc/semantics.md`.

**Answers the SWI-Prolog port gives that are wrong**, and that ssCASP does
not give:

- `p :- not q, r.  q :- not r.  r :- p.` has the single answer set `{q}`.
  The port answers `p`. It does this by reusing `r` as already proved
  inside `p`'s own proof, which closes a positive loop. ssCASP reuses only
  a proof that does not rest on the coinductive hypothesis.
- **Over-pruned quantifiers.** In the event-calculus `bec_tap_01`,
  `-holdsAt(filling, 25)` is true. The port says no, because it covers the
  NMR check from the first dual clause alone.
- **A missing dual clause.** `solve_not_is` answers `invalid_value(2)`,
  although `2 is 1+1`.
- **A cancelled answer.** `stream_reasoning` answers
  `valid_stream(1, p(a))`, which is cancelled by a higher-priority stream.
- **Non-termination.** `clpq_equality.pl` does not terminate in the port.
  ssCASP answers it in under a millisecond.

**Differences that are choices, not defects:**

- **The model contains closure.** The model includes `not q` for a `q` that
  has no clauses.
- **One model per answer.** The port reports every proof. Use
  `--no-commit-nmr` and `sscasp_distinct_answers=false` to get closer to
  that behaviour.
- **Smaller models.** A quantifier's complement contributes its proof but
  not every alternative covering, so models are often smaller.
- **Disequality is a store, not a choice point.** The port resolves
  `f(A,B) \= f(a,b)` by backtracking over which argument differs. ssCASP
  keeps it as one constraint.
- **Complements may come back as one answer.** `not p(Y)` with
  `p(X) :- X #= 3` gives one answer, `Y =\= 3`, rather than two regions.

**The justification** is `Literal-Children` as before. It is a forest of
the query's literals plus `'$nmr'`, rather than one `query` root with
`o_nmr_check`. The wrappers are `chs/1` and `proved/1`. There is no
`goal_origin/2` (no source references), and no `assume/1` or `abduced/1`.
`library(sscasp/compat)` returns the old shape to programs that ask for it.

**JSON**:

- `tree` is a list (a forest).
- A `constraints` entry is a list, because a variable may be in both
  stores.
- `truth` is always a string.
- `module`, `source_file`, `source_line`, `assume` and `abduced` are not
  produced.

### Performance

Over the 49-program regression corpus (`doc/performance.md`):

- ssCASP agrees with the port on the answer for 47 programs. The other two
  are the port's wrong answers listed above.
- It agrees on the model as well for 25.
- The total time is 0.66 s, against 1.99 s for the port.
- Every program in the corpus answers. The last one that did not was
  `bec_loan`, which now answers in 0.07 s.

---

## 12. References

**The language and its semantics**

- M. Gelfond and V. Lifschitz. The stable model semantics for logic
  programming. In *Proceedings of the 5th International Conference and
  Symposium on Logic Programming (ICLP/SLP)*, MIT Press, 1988. The
  semantics s(CASP) computes, and the one ssCASP's test oracle implements.
- J. Arias, M. Carro, E. Salazar, K. Marple and G. Gupta. Constraint
  answer set programming without grounding. *Theory and Practice of Logic
  Programming* 18(3–4), 2018 (ICLP 2018).
  [arXiv:1804.11162](https://arxiv.org/abs/1804.11162). s(CASP) itself:
  constraints, the `forall` algorithms, loop handling.
- J. Arias, M. Carro, Z. Chen and G. Gupta. Justifications for
  goal-directed constraint answer set programming. In *ICLP 2020
  Technical Communications*, EPTCS 325, 2020.
  [arXiv:2009.10238](https://arxiv.org/abs/2009.10238). Justification
  trees and their rendering in natural language (`:- describe`, `--human`).
- J. Arias, M. Carro, Z. Chen and G. Gupta. Modeling and reasoning in
  event calculus using goal-directed constraint answer set programming.
  *Theory and Practice of Logic Programming* 22(1), 2022 (LOPSTR 2019).
  [arXiv:2106.14566](https://arxiv.org/abs/2106.14566). The source of the
  `bec_*` event-calculus programs in the test corpus.

**Goal-directed answer set programming (UT Dallas)**

- L. Simon, A. Mallya, A. Bansal and G. Gupta. Coinductive logic
  programming. In *ICLP 2006*, LNCS 4079, Springer, 2006. The coinduction
  behind the coinductive hypothesis set (CHS).
- R. Min and G. Gupta. Coinductive logic programming with negation. In
  *LOPSTR 2009*, LNCS 6037, Springer, 2010.
- K. Marple and G. Gupta. Galliwasp: a goal-directed answer set solver. In
  *LOPSTR 2012*, LNCS 7844, Springer, 2013. The NMR check as a compiled
  predicate.
- K. Marple. *Goal-Directed Answer Set Programming.* PhD thesis, The
  University of Texas at Dallas, 2014. The NMR check, and coinduction
  restricted to agree with the stable model semantics.
- K. Marple and G. Gupta. Dynamic consistency checking in goal-directed
  answer set programming. *Theory and Practice of Logic Programming*
  14(4–5), 2014. Dynamic consistency checking (`sscasp_dcc`).
- K. Marple, E. Salazar and G. Gupta. Computing stable models of normal
  logic programs without grounding. 2017.
  [arXiv:1709.00501](https://arxiv.org/abs/1709.00501). s(ASP): dual rules
  for programs with variables, head abstraction, `forall`, constructive
  disequality.

**The implementations**

- s(CASP) for Ciao Prolog:
  <https://gitlab.software.imdea.org/ciao-lang/sCASP>, and its manual,
  <https://personal.utdallas.edu/~gupta/nfm-ex/scasp-manual.pdf>.
- J. Wielemaker, J. Arias and G. Gupta. s(CASP) for SWI-Prolog. In
  *Proceedings of the ICLP 2021 Workshops* (GDE'21), CEUR Workshop
  Proceedings, 2021. The SWI-Prolog port, as a library that embeds
  s(CASP) in Prolog programs.
- The SWI-Prolog port and ssCASP: <https://github.com/SWI-Prolog/sCASP>.

`doc/literature.md` says which of these ideas each part of the
implementation follows, and where it departs from them.
