# s(CASP)

The `s(CASP)` system is a top-down interpreter for ASP programs with
constraints.

This work was presented at ICLP'18 ([Arias et al. 2018](https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming/article/constraint-answer-set-programming-without-grounding/55A678C618EF54487777F021D89B3FE7)), also available [here](https://arxiv.org/abs/1804.11162).


## Introduction

`s(CASP)` by [Joaquin Arias](mailto:joaquin.arias@imdea.org), is based on
[`s(ASP)`](https://sourceforge.net/projects/sasp-system/) by
[Kyle Marple](mailto:kmarple1@hotmail.com).

`s(CASP)` is an implementation of the stable model semantics of
constraint logic programming. Unlike similar systems, it does not
employ any form of grounding. This allows `s(CASP)` to execute programs
that are not finitely groundable, including those which make use of
lists and terms. 

## Installation of s(CASP) 


### CIAO

`Ciao` is a programming language that builds up from a logic-based simple kernel, and is designed to be extensible and modular. It is available at [http://ciao-lang.org](http://ciao-lang.org). Its supports:

* constraint logic programming (and, in particular, Prolog)
* different levels of modularity (from small to large scale):
  * modules as (analysis-friendly) compilation units
  * bundles as collections of modules
* packages as modules implementing language extensions (syntactic definitions, compilation options, compiler plugins)
* assertions (as an homogeneous framework that allows static and dynamic verification to work cooperatively in a unified way)
* multiparadigm constructs (meta-programming, higher-order, mutables, concurrency, functions, etc.) and interfacing with foreign code

The system implements some advanced features such as separate and incremental compilation, global program analysis and static debugging and optimization (via source to source program transformation, `CiaoPP preprocessor`), a build automation system, documentation generator, debugger, and (Emacs-based) development environment.

To begin the interactive installation type the following one-liner in
a sh-compatible terminal (For Windows you need [WSL](https://docs.microsoft.com/en-us/windows/wsl/install-win10)):

```
curl https://ciao-lang.org/boot -sSfL | sh
```

Do not forget to rerun the bashrc file `source ~/.bashrc`. If you have any problem go [here](http://ciao-lang.org/install.html) for details.

### s(CASP)

Once you have Ciao working in your computer to install `s(CASP)` type the following one-liner in a sh-compatible terminal:

```
ciao get gitlab.software.imdea.org/ciao-lang/sCASP
```

If you have any problem contact
[this mail](mailto:joaquin.arias@imdea.org?subject=[s(CASP)%20Help]).

## Usage of s(CASP)

Usage:
```
scasp [options] InputFile(s)
```

* General Options:

```
  -h, -?, --help        Print this help message and terminate.
  -i, --interactive     Run in user / interactive mode.
  -a, --auto            Run in automatic mode (no user interaction).
  -sN, -nN              Compute N answer sets, where N >= 0. 0 for all.
  -v, --verbose         Enable verbose progress messages.
  -w, --warning         Enable warning messages (failing in variant loops).
  -j, --justification   Print proof tree for each solution.
  --html                Generate the proof tree in a file named InputFiles(s).html.
  --server              Generate the proof tree in the file named justification.html.
  -d0                   Print the program translated (with duals and nmr_check).
```

### Using the principal options

Let us consider the program `test.pl`:
```
p(A) :- not q(A).
q(A) :- not p(A).
?- p(A).
```

* To obtain the models one by one:

```
$ scasp test.pl
Answer 1	(in 0.09 ms):
p(A) ,  not q(A)

 ? ;
```
for this example there is only one model so when we ask for more models (introducing `;` after the `?`) the evaluation finishes.

* To obtain all the models automatically use the option `-sn` with `n=0`:

```
$ scasp -s0 test.pl
```

* To obtain a specific number of models, e.g., 5, invoke:

```
$ scasp -s5 test.pl
```

* To use scasp with its iterative mode invoke s(CASP) with `-i`, and introduce the query after `?-`:

```
$ scasp -i test.pl
?- q(A).
Answer 1	(in 0.228 ms):
q(A) ,  not p(A)
 ? 
```

### Debuging options

* To print the "translation" of the code (with duals predicates and
check-rules) use `-d0`:

```
$ scasp -d0 test.pl
```

* To obtain the justification tree for each model use `-j`.
```
$ scasp -j test.pl
```


## Examples & Benchmarks & Event Calculus

### Examples

There are some examples, most of them available in the distribution of
s(ASP).  Check them [here](examples/) and in your local installation
(the default folder is `~/.ciao/sCASP`).

### Towers of Hanoi

`s(CASP)` vs `Clingo` _standard_ vs `Clingo` _incremental_.

See more details [here](examples/benchmark_iclp18/towers_hanoi/README.md).

### Stream data reasoning

Let us assume that we deal with series of data items, some of which
may be contradictory. Moreover, different sources may give data a
different degree of trustworthiness which can make some pieces of
inconsistent data to be preferred. Lets us assume that `p(A)` and `q(A)`
are contradictory and we receive, from source _S1_, `p(A)` and, from
source _S2_, `q(a)`. We may decide that: (i) `p(A)` is __true__ because _S1_ is
more realiable; (ii) or if _S2_ is more realiable, `q(a)` is __true__, and any
value `not a` (i.e., _X \= a_) `p(A)` is also __true__; (iii) or, if both
sources are equally reliable, them we have (at least) two different
models: one where `q(a)` is __true__ and another where `p(A)` is __true__ (also
for _X=a_).

See more details [here](examples/benchmark_iclp18/stream_data_reasoning/README.md).

### Traveling salesman

A variant of the traveling salesman problem (visiting every city in a
country only once, starting and ending in the same city, and moving
between cities using the existing connections) where, in addition, we
want to find out the length of the Hamiltonian cycle.

Solutions for this problem using `CLP(FD)` and `ASP` appear in
([Dovier et al. 2005](https://users.dimi.uniud.it/~agostino.dovier/PAPERS/DFP05-cilc.pdf)),
with comparable performance. However, they show that the `ASP`
encoding is more compact, even if the `CLP(FD)` version uses the
library predicate `circuit/1`, which does the bulk of the work and
whose code is non-trivial.

We will show that also in this problem, where the `ASP` solution is more
compact than that of `CLP(FD)`, `s(CASP) `is more expressive.

See more details [here](examples/benchmark_iclp18/traveling_salesman/README.md)


### Yale shooting scenario

Let us compare the expressiveness of `s(CASP)` vs `ASP` + constraints
using the spoiling Yale shooting scenario
([Janhunen et al. 2017](https://arxiv.org/pdf/1707.04053.pdf)).

In this scenario we have an unloaded gun and three possible actions
load, shoot, and wait. If we load the gun, it becomes loaded. If we
shoot the gun and it was loaded for no more than 35 minutes, the
turkey is killed. Otherwise, the gun powder is spoiled. We are looking
for an executable plan such that:
* the turkey is killed within 100 minutes,
* considering that we are not allowed to shoot in the first 35
minutes.

See more details [here](examples/benchmark_iclp18/yale_shooting_scenario/README.md)

## Event Calculus

Let us use s(CASP) to implement Event Calculus, a more complex
application, with several scenarios.

In this [folder](examples/benchmark_EventCalculus/) you will
find the benchmark and instruction to
reproduce the evaluation and example presented in the paper
__"Modelling and Reasoning in Event Calculus using Goal-Directed Constraint Answer Set Programming"__.

See more details [here](examples/benchmark_EventCalculus/README.md)
