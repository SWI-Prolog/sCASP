:- module(scasp_known_issues,
          [ known_issue/3                       % ?Test, ?Expectation, ?Reason
          ]).

/** <module> Tests that are expected to fail

A program that cannot be solved must not be given a `.pass` file: saving
its current output would record the bug as the expected answer, and the
test would then "pass" forever.  It is listed here instead.

test_scasp.pl checks that each of these still fails in the way described.
If one starts working it is reported as an *unexpected pass* and the
suite fails, so an entry cannot outlive the problem it records.

Expectations:

  | `hang`            | does not terminate within the timeout     |
  | `no_models`       | yields no answer where one is expected    |
*/

%!  known_issue(?Test, ?Expectation, ?Reason) is nondet.
%
%   @arg Test is the base name of the test program.

known_issue('clpq_equality.pl', hang,
            "mutually_exclusive([1,2],[3,4]) over member/2 with #= does \c
             not terminate.  Compare clpq_intercept.pl, which is the same \c
             shape and does terminate (with no models).").
known_issue('solve_call.pl', no_models,
            "call/1 is not an s(CASP) predicate, so process(test) cannot \c
             be proved.  The solver warns `scasp_predicate call/1 does \c
             not exist' and yields no model.").
known_issue('solve_forall.pl', no_models,
            "A program cannot write forall/2 itself.  The forall construct \c
             exists only as something comp_duals.pl generates for dual \c
             rules; as a goal it is unsupported in both front ends.  The \c
             file front end warns `scasp_predicate forall/2 does not \c
             exist' and yields no model; via scasp/2 it raises \c
             permission_error(scasp, procedure, '$apply':forall/2) because \c
             it resolves to the SWI built-in.").
