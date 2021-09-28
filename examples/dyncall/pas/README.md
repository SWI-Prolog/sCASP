# Using s(CASP) as embedded language

This     demo     is     created      from       PAS      files     from
http://www.cliplab.org/papers/sCASP-ICLP2020/,  the  data  accompagnying
the  paper  "Justifications  for  Goal-Directed  Constraint  Answer  Set
Programming" by Joaquín Arias at all, ICLP 2020.

It provides a  _modular_  implementation  of   the  case.  For  both the
PAS_rules.pl and PAS_guide.pl  we  merged   the  ``.pred``  version  and
declared them to  be  modules  connected   using  normal  Prolog  module
imported and export declarations. The   PAS_patient.pl  file defines the
patient data as a set of dynamic  predicates. The file PAS_case.pl shows
how  the  files  can  be  used  to   reason  about  s(CASP)  cases  data
dynamically. The system allows for multiple   threads  to query the same
set of rules using different patient data.

## Running the demo

### Interactive

    swipl PAS_case.pl
    ?- solve(chose(ace_inhibitors), 1).

### HTTP

    swipl PAS_server.pl
    ?- server(8080).

And then connect to http://localhost:8080/

## Issues

 - The s(CASP) dynamic module translates -Name(Args) into '-Name'(Args).
   The alternative would be to have all classical negation clauses to
   be clauses of the same -/1 predicate.  This causes normal Prolog
   program analysis to fail and makes it impossible to import/export
   classical negation.

 - As a result, we need scasp_assert/1, etc. that translate e.g.
   -p(X) :- -q(X) into `'-p'(X) :- '-q'(X)`.

 - Unfortunately, this does not work for exporting.  Well, we _can_
   use term_expansion/2 rules to expand the :- module(Name, Exports).
   directive, but we can only do that if library(scasp) is loaded
   __before__ loading any scasp modules.   We could provide
   scasp_export/1.

