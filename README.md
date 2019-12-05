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


## CIAO

`Ciao` is a programming language that builds up from a logic-based simple kernel, and is designed to be extensible and modular. It is available at [http://ciao-lang.org](http://ciao-lang.org). Its supports:

* constraint logic programming (and, in particular, Prolog)
* different levels of modularity (from small to large scale):
  * modules as (analysis-friendly) compilation units
  * bundles as collections of modules
* packages as modules implementing language extensions (syntactic definitions, compilation options, compiler plugins)
* assertions (as an homogeneous framework that allows static and dynamic verification to work cooperatively in a unified way)
* multiparadigm constructs (meta-programming, higher-order, mutables, concurrency, functions, etc.) and interfacing with foreign code

The system implements some advanced features such as separate and incremental compilation, global program analysis and static debugging and optimization (via source to source program transformation, `CiaoPP preprocessor`), a build automation system, documentation generator, debugger, and (Emacs-based) development environment.

To begin the interactive installation type the following one-liner in a sh-compatible terminal:

```
curl https://ciao-lang.org/boot -sSfL | sh
```

Do not forget to rerun the bashrc file `source ~/.bashrc`. If you have any problem go [here](http://ciao-lang.org/install.html) for details.

## s(CASP)

`s(CASP)` is an implementation of the stable model semantics of
constraint logic programming. Unlike similar systems, it does not
employ any form of grounding. This allows `s(CASP)` to execute programs
that are not finitely groundable, including those which make use of
lists and terms. It is available  [here](https://gitlab.software.imdea.org/joaquin.arias/sCASP).

Once you have Ciao working in your computer to install `s(CASP)` type the following one-liner in a sh-compatible terminal:

```
ciao get gitlab.software.imdea.org/joaquin.arias/sCASP
```

If you have any problem contact
[this mail](mailto:joaquin.arias@imdea.org?subject=[s(CASP) help]).

## Usage of s(CASP)

Usage:
```
./scasp [options] InputFile(s)
```

Example:
```
./scasp -j src/test.pl
```

* General Options:

```
  -h, -?, --help         Print this help message and terminate.  
  -i, --interactive      Run in user / interactive mode. 
  -a, --auto             Run in automatic mode (no user interaction). 
  -sN, -nN               Compute N answer sets, where N >= 0. 0 for all. 
  -v, --verbose          Enable verbose progress messages. 
  -j, --justification    Print proof tree for each solution. 
  -d0                    Print the program translated (with duals and nmr_check). 
```

### Examples of use

* To obtain one model of the program (i.e. test.pl)

```
$ ./scasp test.pl
Answer: 1
{ q(X), not p(X) }
$
```
   
* To obtain all the models (answers) of test.pl

```
$ ./scasp -s0 test.pl
```

* To obtain 5 answers of test.pl

```
$ ./scasp -s5 test.pl
```

* To print the "translation" of the code (with duals predicates and
check-rules)

```
$ ./scasp -d0 test.pl
```


* To use scasp with its iterative mode:

```
$ ./scasp -i test.pl
?- p(X).
{ p(X), not q(X) } ? 
false.
?- q(X).
{ q(X), not p(X) } ? ;
false.
?- halt.
```

* The example program test.pl is:

```
p(X) :- not q(X).
q(X) :- not p(X).
?- q(X).
```
   
__NOTE:__ that the program can include the query in order to be use without
iterative mode...

## Examples & Benchmarks & Event Calculus

### Examples

There are some examples, most of them from the distribution of s(ASP),
availables [here](examples/).

### Towers of Hanoi

`s(CASP)` vs `Clingo` _standard_ vs `Clingo` _incremental_.

See files [here](examples/benchmark_iclp18/towers_hanoi/README.md).

### Stream data reasoning

Let us assume that we deal with series of data items, some of which
may be contradictory. Moreover, different sources may give data a
different degree of trustworthiness which can make some pieces of
inconsistent data to be preferred. Lets us assume that `p(X`) and `q(X)`
are contradictory and we receive, from source _S1_, `p(X)` and, from
source _S2_, `q(a)`. We may decide that: (i) `p(X)` is __true__ because _S1_ is
more realiable; (ii) or if _S2_ is more realiable, `q(a)` is __true__, and any
value `not a` (i.e., _X \= a_) `p(X)` is also __true__; (iii) or, if both
sources are equally reliable, them we have (at least) two different
models: one where `q(a)` is __true__ and another where `p(X)` is __true__ (also
for _X=a_).

See files [here](examples/benchmark_iclp18/stream_data_reasoning/README.md).

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

See file [here](examples/benchmark_iclp18/traveling_salesman/README.md)


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

See file [here](examples/benchmark_iclp18/yale_shooting_scenario/README.md)

## Event Calculus

Let us use s(CASP) to implement Event Calculus, a more complex
application, with several scenarios.

In this [folder](examples/benchmark_EventCalculus/) you will
find the benchmark and instruction to
reproduce the evaluation and example presented in the paper
__"Modelling and Reasoning in Event Calculus using Goal-Directed Constraint Answer Set Programming"__.

