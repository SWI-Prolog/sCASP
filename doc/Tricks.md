# SWI-Prolog development tricks

## Running the system without building

    swipl src/scasp.pl -- [scasp-options] test/familty.pl
    swipl src/test_scasp.pl [test options] [dir ...] [file ...]

## Loading for debugging

    swipl -l src/scasp.pl
    <prepare debugging>
    ?- main(['--tree', 'test/familty.pl']).

## Trace something

gspy/1 starts the graphical debugger. The   argument  is flexible. If no
module is specified a spy point is set  on any matching predicate in any
module.  Likewise if the arity is omitted all existing arities are used.

    swipl -l src/scasp.pl
    ?- gspy(solve/4).
    ?- main(['--tree', 'test/familty.pl']).

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

Get  https://github.com/JanWielemaker/my-prolog-lib  and    install   as
described in the README.  After that you can do e.g.

    ?- gtrap(error(_,_)).
    ?- main(...).

(will be added to the core system later).


## Reloading code and check for undefined predicates

After edit, run make/0. This reloads  modified   files  and does a quick
search for undefined predicates and their call sites.

    ?- make.

## Coverage report

Create annotated coverage file in _dir_ (default `cov`). Requires 8.3.27
or later.

    swipl src/test_scasp.pl --cov[=dir] test/familty.pl

## Profiling

    swipl -l src/scasp.pl
    ?- profile(main(['test/familty.pl']).

## Cross referencing

    swipl -l src/scasp.pl
    ?- gxref.

