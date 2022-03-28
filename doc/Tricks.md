# SWI-Prolog development tricks

## Running the system without building

    swipl prolog/scasp/main.pl -- [scasp-options] test/programs/familty.pl
    swipl test/test_scasp.pl -- [test options] [dir ...] [file ...]

## Loading for debugging

    swipl -l prolog/scasp/main.pl -- [scasp options and program]
    <prepare debugging>
    ?- scasp_main:main.

## Trace something

gspy/1 starts the graphical debugger. The   argument  is flexible. If no
module is specified a spy point is set  on any matching predicate in any
module.  Likewise if the arity is omitted all existing arities are used.

    swipl -l prolog/scasp/main.pl -- test/programs/family.pl
    ?- gspy(solve/4).
    ?- scasp_main:main.

### Complicated conditional debugging

Just edit the source code  and  add   e.g.  This  traps  the (graphical)
debugger at this point, whether running  in   debug  mode  or not. Often
better to start the program in debug mode (after `?- debug.`).

    (   my_condition
    ->  gtrace
    ;   true
    )

### Finding exceptions

#### The graphical way

  - Start the tracer (`?- gtrace, goal.`)
  - Use Edit/Exceptions menu
  - Flag error(_,_) as "Trace always".
  - leap.

The run collects errors  that  happen   during  the  executions. You can
reorder the patterns and tell which ones   you want to trap (first match
counts).

#### Command line

    ?- gtrap(_).
    ?- scasp_main:main.

The   argument   to   gtrap/1   is   the     _formal_    part   of   the
error(Formal,ImplDep) term. Thus, to trap only instantiation errors use:

    ?- gtrap(instantiation_error).

## Reloading code and check for undefined predicates

After edit, run make/0. This reloads  modified   files  and does a quick
search for undefined predicates and their call sites.

    ?- make.

## Coverage report

Create annotated coverage file in _dir_ (default `cov`). Requires 8.3.27
or later.

    swipl test/test_scasp.pl --cov=dir test/programs/family.pl

## Profiling

    swipl -l prolog/scasp/main.pl -- test/programs/family.pl
    ?- profile(scasp_main:main).

## Cross referencing

    swipl -l prolog/scasp/main.pl
    ?- gxref.

## Documentation

    swipl --pldoc -l prolog/scasp/main.pl

(better from the `prolog/scasp` dir  as  you   get  the  files  from the
current dir rather than the main README.md as entry point).
