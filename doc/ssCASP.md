# Next sCASP

The sCASP code has gone through a lot of history. The task is to arrive
at a clean fresh implementation. The aim is to improve maintainability and
make the code clean enough to aim at further refinements.  To arrive at this,
I identified the following steps:

  - Understand the current design.  Read the articles about sCASP on the
    web as well as the code.  Create a brief document that describes the
    essential steps in sCASP resolution.
  - Translate all tests to the SWI-Prolog plunit test framework, using the
    embedded sCASP interface (run scasp/1,2 on a set of Prolog clauses)
  - Add new tests.  Note that the MCP server allows running coverage
    analysis.  Aim at a complete coverage of the sCASP reasoner.  If you
    find failing tests, just keep them in as failing tests.
  - Analyse the status based on the tests, code and literature.
  - Plan a new implementation
  - Implement it, using the created tests.

## Design remarks

  - Code for collecting the sCASP program relative to a query may be copied.
    We may want some cleanup, but I think it is generally ok.
  - Generating the complementary (negated program) needs careful rethinking.
    We do not want the current flaky name mangling.  Ideally we go without
    name mangling.  Where we need it, it should be readable and should use
    $ to separate name components.
  - The solver may wish to use SWI-Prolog's native stack examination.  See
    library(coinduction).  It shall generally use dicts to achieve a readable
    data structure.  We may want to use tabling or hand-rolled memoization.
  - Rather then interpreting, we should consider generating predicates where
    each user predicate is converted into an sCASP version.  It should be easy
    to link the two.  This simplifies debugging, coverage etc.
  - _forall_ reasoning is the bottleneck.  We must keep an eye on improving
    performance here.   Among these are grounding the collected program before
    the rest of the processing.  This should be enhanced with annotations:
    - Nothing is true besides the given fact,.  I.e., age(peter, 32) means
      peter is 32 years old and not 33.  This allows improving the grounding.
      We can see that as negation by failure is the same as true negation for
      this predicate.
    - Introduce variable domains.  We can use type declarations for predicates
      and define the domain of certain types.  This can greatly simplify the
      negation.  Types should be optional.
    - Anything we can think of.  Keep the design open for further improments.
  - Rendering the proof as HTML is propably the way to go, deriving the textual
    version from the HTML version.  Possibly we can use SWI-Prolog lynx library
    for the textual version.
  - There is no need to stay close to the current implementation.  Probably it
    is not even a good idea.   The main mechanisms of the sCASP solver stand,
    possibly deviating in handling of constructive negation if we see options
    to make the reasoner much faster at the cost of annotations or justifiable
    changes in semantics.
  - clp/disequality.pl is basically diff/2, no?  If I recall well, the disequality
    solver is too simple.
