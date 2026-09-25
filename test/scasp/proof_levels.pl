:- module(scasp_proof_levels,
          [ proof_level/3                       % ?Test, ?Level, ?Reason
          ]).

/** <module> Per-test justification comparison level

Every regression test compares its model exactly.  Its justification is
compared through proof_skeleton/3 at level `exact` unless it appears
below, so a change to the shape of a proof is a failure by default.

Weakening a test is a deliberate, reviewable act: it needs an entry here
with the reason.  A test whose justification moved fails as

    solve_findall.pl ....... FAILED: proof (exact, agrees at literals)

naming the strongest level that would still agree, i.e. exactly the entry
this file would need.  A proof that changed for real reports
``no agreement`` instead, and no entry here can hide it.

@see test/proof_skeleton.pl for what each level absorbs.
*/

%!  proof_level(?Test, ?Level, ?Reason) is nondet.
%
%   @arg Test is the base name of the test program.

:- dynamic proof_level/3.               % may legitimately be empty

%  No test is weakened at present: the whole corpus compares at `exact`.
%
%  The two that used to need an exception - solve_findall.pl and
%  abdbirds.pl - did so only because their .pass files predated the
%  deliberate changes in the solver (an explicit findall/3 node in the
%  tree, and the abduction subtrees no longer collapsed by filter_tree/4).
%  Their golden data has been regenerated, so they are strict again and
%  the record of the change lives in the history rather than in a
%  permanent weakening of the test.
