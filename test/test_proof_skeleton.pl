:- module(test_proof_skeleton,
          [ test_proof_skeleton/0
          ]).
:- use_module(under_test, [op(_,_,_)]).   % the operators, from either tree
%   `'| '` is written canonically below: it inlines a constraint store
%   when an answer is printed, and library(scasp) does not export it.
:- use_module(proof_skeleton).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).

/** <module> Tests for the justification abstraction

The abstraction is only useful if it is  loose in exactly one direction:
it must absorb the ways a justification is *presented* while still
failing when the proof itself changes.  Both halves are tested here,
against hand-built trees, so the guarantee does not depend on any
particular program remaining in the test suite.
*/

test_proof_skeleton :-
    run_tests([ skeleton_tolerates,
                skeleton_discriminates,
                skeleton_models,
                skeleton_relax
              ]).

		 /*******************************
		 *      WHAT IT ABSORBS		*
		 *******************************/

:- begin_tests(skeleton_tolerates).

% The drift in test/all_programs/solve_findall.pl: a findall/3 bookkeeping
% node was inserted between validate/1 and its children.

findall_drift(Old, New) :-
    Old = query-[ validate([1,2,3])-
                  [ member(1,[1,2,3])-[],
                    member(2,[1,2,3])-[]
                  ]],
    New = query-[ validate([1,2,3])-
                  [ findall(V, member(V,[1,2,3]), [1,2,3])-
                    [ member(1,[1,2,3])-[],
                      member(2,[1,2,3])-[]
                    ]]].

test(findall_node_visible_at_refless) :-
    findall_drift(Old, New),
    \+ same_at(Old, New, refless).
test(findall_node_absorbed_at_literals) :-
    findall_drift(Old, New),
    same_at(Old, New, literals).
test(findall_node_absorbed_at_support) :-
    findall_drift(Old, New),
    same_at(Old, New, support).

% The drift in test/all_programs/sasp/abdbirds.pl: the abduction subtree
% was expanded from abduced(X) into the raw abducible/chs machinery.

abduction_drift(Old, New) :-
    Old = query-[ (not flies(tweety))-
                  [ goal_origin(bird(tweety),5)-[],
                    goal_origin(abduced(ab(tweety)),3)-[]
                  ]],
    New = query-[ (not flies(tweety))-
                  [ goal_origin(bird(tweety),5)-[],
                    goal_origin(ab(tweety),3)-
                    [ chs(ab(tweety))-[],
                      goal_origin(abducible(ab(tweety)),3)-
                      [ goal_origin(abducible(ab(tweety)),3)-
                        [ chs(not o_abducible(ab(tweety)))-[] ]
                      ]
                    ]]].

test(abduction_expansion_visible_at_exact) :-
    abduction_drift(Old, New),
    \+ same_at(Old, New, exact).
test(abduction_expansion_absorbed_at_refless) :-
    abduction_drift(Old, New),
    same_at(Old, New, refless).

test(clause_renumbering_absorbed) :-
    same_at(query-[goal_origin(p(1),1)-[]],
            query-[goal_origin(p(1),9)-[]],
            refless).

test(sibling_order_absorbed_at_support) :-
    same_at(query-[p-[q-[],r-[]]],
            query-[p-[r-[],q-[]]],
            support).

test(sibling_order_visible_at_refless) :-
    \+ same_at(query-[p-[q-[],r-[]]],
               query-[p-[r-[],q-[]]],
               refless).

test(variable_names_absorbed) :-
    same_at(query-[p('$VAR'('X'))-[]],
            query-[p('$VAR'('Y'))-[]],
            refless).

test(constraint_order_absorbed) :-
    same_at(query-[p('| '('$VAR'('X'), {'$VAR'('X') #> 1, '$VAR'('X') #< 9}))-[]],
            query-[p('| '('$VAR'('X'), {'$VAR'('X') #< 9, '$VAR'('X') #> 1}))-[]],
            refless).

test(answer_order_absorbed) :-
    A = [query-[p-[]], query-[q-[]]],
    B = [query-[q-[]], query-[p-[]]],
    answers_skeleton(A, support, SA),
    answers_skeleton(B, support, SB),
    assertion(SA == SB).

:- end_tests(skeleton_tolerates).

		 /*******************************
		 *    WHAT IT MUST NOT ABSORB	*
		 *******************************/

:- begin_tests(skeleton_discriminates).

%  p-[q-[r-[]]]  is not  p-[q-[],r-[]] : in the first r justifies q, in
%  the second r justifies p.  Re-parenting must stay visible.

test(reparenting_visible_at_support) :-
    \+ same_at(query-[p-[q-[r-[]]]],
               query-[p-[q-[],r-[]]],
               support).

test(missing_step_visible_at_support) :-
    \+ same_at(query-[p-[q-[]]],
               query-[p-[]],
               support).

test(missing_step_visible_at_reached) :-
    \+ same_at(query-[p-[q-[]]],
               query-[p-[]],
               reached).

test(different_literal_visible_at_reached) :-
    \+ same_at(query-[p-[q-[]]],
               query-[p-[r-[]]],
               reached).

test(variable_sharing_visible) :-
    \+ same_at(query-[p('$VAR'('X'), '$VAR'('X'))-[]],
               query-[p('$VAR'('X'), '$VAR'('Y'))-[]],
               refless).

test(negation_visible_at_reached) :-
    \+ same_at(query-[p-[q-[]]],
               query-[p-[(not q)-[]]],
               reached).

test(chs_status_visible_at_reached) :-
    \+ same_at(query-[p-[q-[]]],
               query-[p-[chs(q)-[]]],
               reached).

test(deleting_a_real_literal_is_not_bookkeeping) :-
    \+ same_at(query-[p-[findall(_,q,_)-[r-[]]]],
               query-[p-[s-[r-[]]]],
               support).

:- end_tests(skeleton_discriminates).

		 /*******************************
		 *           MODELS		*
		 *******************************/

:- begin_tests(skeleton_models).

test(model_variable_names_absorbed) :-
    model_key([p('$VAR'('X'))], K1),
    model_key([p('$VAR'('Y'))], K2),
    assertion(K1 == K2).

test(model_answer_order_absorbed) :-
    answers_model_key([[p],[q]], K1),
    answers_model_key([[q],[p]], K2),
    assertion(K1 == K2).

test(model_difference_visible) :-
    model_key([p,q], K1),
    model_key([p,r], K2),
    assertion(K1 \== K2).

test(model_extra_literal_visible) :-
    answers_model_key([[p]], K1),
    answers_model_key([[p,q]], K2),
    assertion(K1 \== K2).

:- end_tests(skeleton_models).

		 /*******************************
		 *           RELAX		*
		 *******************************/

:- begin_tests(skeleton_relax).

test(reports_strongest_level, Level == literals) :-
    Old = query-[p-[q-[]]],
    New = query-[p-[findall(_,q,_)-[q-[]]]],
    weakest_agreement(Old, New, Level).

test(reports_exact_when_identical, Level == exact) :-
    weakest_agreement(query-[p-[]], query-[p-[]], Level).

test(fails_when_proof_really_differs) :-
    \+ weakest_agreement(query-[p-[q-[]]], query-[p-[r-[]]], _).

:- end_tests(skeleton_relax).

		 /*******************************
		 *           HELPERS		*
		 *******************************/

same_at(A, B, Level) :-
    proof_skeleton(A, Level, SA),
    proof_skeleton(B, Level, SB),
    SA == SB.
