:- module(test_le,
          [ test_le/0
          ]).
:- use_module('../prolog/scasp').
:- use_module(scasp_plunit).
:- use_module(library(plunit)).
:- use_module(library(lists), [memberchk/2, member/2]).

/** <module> The Logical English examples, run the way they are written

test/le_programs/ holds 13 s(CASP) programs exported from Logical
English.  They are *embedded* programs: ordinary Prolog modules that load
library(scasp), carry `#pred` templates, and are queried with scasp/2.
Running them through the file front end instead reads their `:- module/2`
and `:- dynamic` directives as s(CASP) global constraints and tells you
nothing, which is why they are tested here and not by test_scasp.pl.

These are examples rather than curated tests, and several have problems of
their own.  The point of pinning them is not that every answer is right -
for the ones that yield nothing, nobody has checked whether that is the
intended reading of the scenario - but that the outcome is recorded, so a
change in the solver is noticed instead of silently swallowed.

Each file ends with its query under a `/** <examples> */` marker; that is
the query used below.
*/

test_le :-
    run_tests([ le_answers,
                le_no_answers,
                le_broken,
                le_justification
              ]).

		 /*******************************
		 *          THE ANSWERS		*
		 *******************************/

:- begin_tests(le_answers).

test(family_grandparent, set(A-B == [john-john, mary-mary])) :-
    le_answer('family-le-scasp', is_a_grandparent_of(A,B)).

test(impossible_ancestor, set(A == [john, mary])) :-
    le_answer('impossibleancestor-scasp', is_impossible(A)).

test(isda_permission,
     set(Who-What-When ==
         [ 'Alice'-'Early_Termination_in_respect_of_all_outstanding_Transactions'
                  -'Friday' ])) :-
    le_answer('isdapermissioncorrected-scasp',
              it_is_permitted_that(designates_that(Who, occurs_at(What, When)))).

test(it_is_permitted_that,
     set(Who-Day-What-When ==
         [ 'Alice'-'Tuesday'
                  -'Early_Termination_in_respect_of_all_outstanding_Transactions'
                  -'Friday' ])) :-
    le_answer('itispermittedthat-scasp',
              'It_is_permitted_that'(
                  designates_at_that(Who, Day, occurs_at(What, When)))).

test(minicontract, all(A == [the_contract])) :-
    le_answer('minicontract-scasp', is_a_valid_contract(A)).

% The payer and the cured failure are left unbound, so this one needs
% variant rather than structural equality.

test(obligation, set(When-Amount-Failure =@= [1464825600.0-525-_])) :-
    le_answer('obligation-scasp',
              cures_the_failure_of_on_that(
                  the_borrower, Failure, When,
                  pays_to_on(the_borrower, Amount, the_lender, _))).

% Four answers, two distinct payouts: `set` is the point here, not `all`.

test(simple_rps, set(Who-Amount == [bob-1000, miguel-100])) :-
    le_answer('simpleRPS-scasp', gets(Who, Amount)).

:- end_tests(le_answers).

		 /*******************************
		 *        THE EMPTY ONES	*
		 *******************************/

% PINNED, no semantics branch yet.  These scenarios yield no answer.
% Whether that is the intended reading of the Logical English source has
% not been established - each needs reading against its own text - so
% they are pinned at the current behaviour and a change shows up.

:- begin_tests(le_no_answers).

test(citizenship_trust, fail) :-
    le_answer('citizenshiptrust-scasp',
              acquires_British_citizenship_on(_,_)).
test(criminal_justice, fail) :-
    le_answer('criminaljustice-scasp',
              exception_to_applies_to_under_of(_,_,_,_)).
test(loan_with_cure, fail) :-
    le_answer('loanwithcure-scasp', defaults_on(_,_)).
test(turing_complete, fail) :-
    le_answer('turingcomplete-scasp', some_tm_goes_from_to([1,1,1,b], _)).

:- end_tests(le_no_answers).

		 /*******************************
		 *         THE BROKEN ONES	*
		 *******************************/

% Two do not run at all.  Both are limitations of the s(CASP) side rather
% than of the scenario, so they are recorded as errors rather than as an
% absence of answers, which would look the same as the group above.

:- begin_tests(le_broken).

test(list_uses_once,
     [ throws(error(permission_error(scasp, procedure, system:once/1), _)) ]) :-
    le_answer('list-scasp', is_a_subset_of(_,_)).

test(subset_defines_forall,
     [ throws(error(permission_error(define, scasp_control_construct, forall/2), _)) ]) :-
    le_answer('subset-scasp', is_a_subset_of(_,_)).

:- end_tests(le_broken).

		 /*******************************
		 *        JUSTIFICATIONS	*
		 *******************************/

% An answer is only as good as its reason.  These name the steps the proof
% has to contain rather than pinning the shape of the whole tree, which
% moves whenever the solver changes.

:- begin_tests(le_justification).

test(grandparent_is_justified_by_two_parent_steps) :-
    le_tree('family-le-scasp', is_a_grandparent_of(john, john), Tree),
    proof_literals(Tree, Literals),
    assertion(memberchk(plain-is_a_parent_of(john, mary), Literals)),
    assertion(memberchk(plain-is_a_parent_of(mary, john), Literals)).

test(grandparent_conclusion_is_in_its_own_justification) :-
    le_tree('family-le-scasp', is_a_grandparent_of(john, john), Tree),
    proof_literals(Tree, Literals),
    assertion(memberchk(plain-is_a_grandparent_of(john, john), Literals)).

test(rps_payout_is_justified_by_the_throw, [nondet]) :-
    le_tree('simpleRPS-scasp', gets(bob, 1000), Tree),
    proof_literals(Tree, Literals),
    assertion(memberchk(plain-gets(bob, 1000), Literals)).

test(minicontract_justification_is_a_tree) :-
    le_tree('minicontract-scasp', is_a_valid_contract(the_contract), Tree),
    assertion(Tree = _-_).

% family-le-scasp.pl declares `:- module('family-scasp+http://tests.com', [])`.
% modules.pl interns the module into the functor name as `Module:Name` and
% used to split it again at the *first* colon, so every literal in the model
% and the justification came back as
% `'family-scasp+http':'//tests.com:is_a_parent_of'(john,mary)`.

test(literals_are_unqualified_despite_a_colon_in_the_module_name) :-
    le_tree('family-le-scasp', is_a_grandparent_of(john, john), Tree),
    proof_literals(Tree, Literals),
    assertion(\+ ( member(_-Literal, Literals),
                   Literal = _:_
                 )).

:- end_tests(le_justification).

		 /*******************************
		 *           HELPERS		*
		 *******************************/

%!  le_answer(+Base, :Query) is nondet.
%
%   Backtrack over the answers scasp/2 finds for Query in example Base, so
%   that a plunit `all`/`set`/`fail` condition can inspect the bindings
%   rather than a count.

le_answer(Base, Query) :-
    le_module(Base, Module),
    scasp(Module:Query, []).

%!  le_tree(+Base, :Query, -Tree) is nondet.
%
%   As le_answer/2, but also gives the justification of the answer.

le_tree(Base, Query, Tree) :-
    le_module(Base, Module),
    scasp(Module:Query, [tree(Tree)]).

%!  le_module(+Base, -Module) is det.
%
%   Load example Base and give the module it defines.  The module name
%   cannot be derived from the file name: family-le-scasp.pl declares
%   `:- module('family-scasp+http://tests.com', [])`.

le_module(Base, Module) :-
    le_file(Base, File),
    load_files(File, [silent(true), if(not_loaded)]),
    (   module_property(Module, file(File))
    ->  true
    ;   existence_error(le_module, Base)
    ).

le_file(Base, File) :-
    source_file(le_file(_,_), Here),
    file_directory_name(Here, TestDir),
    atomic_list_concat([TestDir, '/le_programs/', Base, '.pl'], File).
