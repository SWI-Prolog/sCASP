# README

In this repository you will find the benchmark and instruction to
reproduce the evaluation and example presented in the paper
__"Modelling and Reasoning in Event Calculus using Goal-Directed Constraint Answer Set Programming"__.

### The BEC theory

The file [bec_theory.pl](event-calculus/bec_theory.pl) contains the translation of the Basic Event Calculus theory.

### The light.pl example

This example is based on the example 14 in [Mueller, 2014b] and reason about turning on and off a light.

There are four files implementing different features:

* [bec\_light\_01\_....pl](event-calculus/example-light/bec_light_01.pl) is the simplest version of the problem.
* [bec\_light\_02\_....pl](event-calculus/example-light/bec_light_02.pl) add the global constraint to ensure that light\_red and light\_green.
* [bec\_light\_03\_...pl](event-calculus/example-light/bec_light_03_inconsistent.pl) change the trajectory of light\_red generating an overlap w.r.t. light\_green.
* [bec\_light\_04\_...pl](event-calculus/example-light/bec_light_04_theinconsistencedonothappens.pl) similar to the previous but the overlap does not occur.

To check the behaviour of the first file with several queries execute the script:

```
go_bec_light_01
```

to check the behaviour of the rest of files execute the script:

```
go_bec_light_02_03_04
```

The scripts are located [here](event-calculus/scripts/). Note that there are the queries that success returning a valid partial model while others do not. For each of the queries there is a comment which explain the expected result.

Additionally you can execute the example invoking s(CASP) directly from the terminal. As an example let us execute s(CASP) asking for the justification tree with (note that it will execute the last uncommented query that appear in the file):

```
scasp -j bec_light_01.pl
```

### The tap.pl example

This example is based on an example from [Shanahan, 1999] where a vessel is filled with water.

There are four files implementing different features:

* [bec\_tap\_01\_...pl](event-calculus/example-tap/bec_tap_01_overflow.pl) is the simplest version of the problem.
* [bec\_tap\_02\_...pl](event-calculus/example-tap/bec_tap_02_no_overflow.pl) similar to the previous but in this example the water is not spilled.
* [bec\_tap\_03\_...pl](event-calculus/example-tap/bec_tap_03_two_models.pl) in this example we have two possible worlds/models.
* [bec\_tap\_04\_...pl](event-calculus/example-tap/bec_tap_04_abducible_infer_event_time.pl) the most complex example (the one described in the paper).

To check the behaviour of each file with several queries execute the scripts (located [here](event-calculus/scripts/)):

```
go_bec_tap_01_overflow
go_bec_tap_02_no_overflow
go_bec_tap_03_two_models
go_bec_tap_04_abducible_infer_event_time

```

Note that there are the queries that success returning a valid partial model while others do not. For each of the queries there is a comment which explain the expected result.


# Installation

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
lists and terms. It is available  [here](https://gitlab.software.imdea.org/ciao-lang/sCASP).

Once you have Ciao working in your computer to install `s(CASP)` type the following one-liner in a sh-compatible terminal:

```
ciao get gitlab.software.imdea.org/ciao-lang/sCASP
```

If you have any problem go [here](https://gitlab.software.imdea.org/ciao-lang/sCASP/blob/master/README.md) for details.

# Evaluation of s(CASP) against s(ASP)

In order to compare the run time of s(CASP) versus s(ASP) in the folder `aux-sasp` there are four files:

* [bec\_theory\_discrete.pl](aux-sasp/bec_theory_discrete.pl) with the Basic Event Calculus theory implemented without constraint.
* [bec\_light\_sasp\_discrete\_0.5.pl](aux-sasp/bec_light_sasp_discrete_0.5.pl) the light simpler example adapted to be use using s(ASP) with a step-wise of 0.5 to discretize the time domain from 0 to 5.
* [bec\_light\_sasp\_discrete\_0.25.pl](aux-sasp/bec_light_sasp_discrete_0.25.pl) the light simpler example adapted to be use using s(ASP) with a step-wise of 0.25 to discretize the time domain from 0 to 5.
* [go\_time\_bec\_light\_sasp\_discrete](aux-sasp/go_time_bec_light_sasp_discrete) to run the benchmark using s(ASP).

s(ASP) is available [here](https://sourceforge.net/projects/sasp-system/). We used the version 1.0.7.

In the scripts folder  for s(CASP) (located [here](event-calculus/scripts/)) there is the script to run the benchmarks in the paper computing the time for s(CAPS):

* [go\_time\_bec\_light\_01](event-calculus/scripts/go_time_bec_light_01) to run the benchmark using s(CASP).


# Evaluation of s(CASP) against clingo

In order to compare the run time of s(CASP) versus clingo in the
folder `sCASP-clingo` there are several programs and scripts. We use
extension `.pl` for s(CASP), the extension `.clingo` for clingo 5.2.0
and `.e` for the encoding using F2LP.

In the folder [scripts](sCASP-clingo/scripts/) you can find the files
used to run the benchmarks:

* [go\_light](sCASP-clingo/scripts/go_light) run the evaluation using the light scenario in folder [light](sCASP-clingo/light).
* [go\_vessel](sCASP-clingo/scripts/go_vessel) run the evaluation using the vessel scenario in folder [vessel](sCASP-clingo/vessel).

