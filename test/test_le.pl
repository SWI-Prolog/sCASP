:- module(test_le,
          [ test_le/0
          ]).
:- use_module(under_test).
:- use_module(scasp_plunit).
:- use_module(le_corpus).
:- use_module(library(plunit)).
:- use_module(library(lists), [memberchk/2, member/2]).
:- use_module(library(debug), [assertion/1]).

/** <module> The Logical English examples, against either library

test/le_programs/ holds 13 s(CASP) programs exported from Logical
English.  They are *embedded* programs: ordinary Prolog modules that load
library(scasp), carry `#pred` templates, and are queried with scasp/2.
Running them through the file front end instead reads their `:- module/2`
and `:- dynamic` directives as s(CASP) global constraints and tells you
nothing, which is why they are tested here and not by
`scasp/test_scasp.pl`.

**What each one answers is in `le_corpus.pl`**, and so is what the new
library answers where it differs (le_differs/4).  This suite runs against
whichever library is under test: each program says
`:- use_module(library(scasp))`, which `swipl -Dsscasp` makes the new
library.

Each file ends with its query under a `/** <examples> */` marker; that is
the query `le_corpus.pl` records.

It is the same kind of evidence as `sscasp/test_corpus.pl`: the two
implementations share every design decision, so agreeing says they did
not drift and nothing about whether either is right.  What makes it
worth having anyway is the *programs* — generated from Logical English
rather than written to exercise a solver, and so crossing features in
ways nothing in `all_programs/` does.
*/

test_le :-
    run_tests([ le_answers,
                le_no_answers,
                le_broken,
                le_differences,
                le_justification
              ]).

%!  answers(+Base, -Answers) is det.
%
%   Every answer to Base's query, as the query term instantiated.
%   `le_matches/2` compares the **set**, by variant.
%
%   Outside the units, not inside one: plunit gives each unit a module
%   of its own, and all of them below ask this.

answers(Base, Answers) :-
    le_program(Base, Query),
    le_module(Base, Module),
    findall(Query, scasp(Module:Query, []), Answers).

%!  expected(?Base, ?Expected) is nondet.
%
%   What Base answers against the library under test.

expected(Base, Expected) :-
    library_under_test(Library),
    le_answer(Library, Base, Expected).

		 /*******************************
		 *          THE ANSWERS		*
		 *******************************/

:- begin_tests(le_answers).

test(answers_what_le_corpus_records,
     [ forall(( expected(Base, Expected),
                Expected = answers(_)
              ))
     ]) :-
    answers(Base, Answers),
    assertion(le_matches(Expected, Answers)).

:- end_tests(le_answers).

		 /*******************************
		 *        THE EMPTY ONES	*
		 *******************************/

% PINNED, no semantics branch yet.  These scenarios yield no answer.
% Whether that is the intended reading of the Logical English source has
% not been established - each needs reading against its own text - so
% they are pinned at the current behaviour and a change shows up.

:- begin_tests(le_no_answers).

test(yields_nothing, [forall(expected(Base, none))]) :-
    answers(Base, Answers),
    assertion(Answers == []).

:- end_tests(le_no_answers).

		 /*******************************
		 *         THE BROKEN ONES	*
		 *******************************/

% Two do not run at all.  Both are limitations of the s(CASP) side rather
% than of the scenario, so they are recorded as errors rather than as an
% absence of answers, which would look the same as the group above.

:- begin_tests(le_broken).

test(does_not_run,
     [ forall(expected(Base, error(Formal))),
       throws(error(Formal, _))
     ]) :-
    answers(Base, _).

:- end_tests(le_broken).

		 /*******************************
		 *        THE DIFFERENCES	*
		 *******************************/

/** le_differs/4 records what one library answers where the other
    answers something else.  The tests above compare against it, which
    already stops an entry from outliving its difference; this checks
    that each entry is one, so the list says what is different and
    nothing else.
*/

:- begin_tests(le_differences).

test(a_difference_differs, [forall(le_differs(_, Base, Differs, _))]) :-
    le_expected(Base, Expected),
    assertion(Differs \=@= Expected).

:- end_tests(le_differences).

		 /*******************************
		 *        JUSTIFICATIONS	*
		 *******************************/

% An answer is only as good as its reason.  These name the steps the proof
% has to contain rather than pinning the shape of the tree, which moves
% whenever the solver changes, and read the steps with literals/2, which
% takes both libraries' trees apart.

:- begin_tests(le_justification).

test(grandparent_is_justified_by_two_parent_steps) :-
    once(le_tree('family-le-scasp', is_a_grandparent_of(john, john), Tree)),
    literals(Tree, Literals),
    assertion(memberchk(is_a_parent_of(john, mary), Literals)),
    assertion(memberchk(is_a_parent_of(mary, john), Literals)).

test(grandparent_conclusion_is_in_its_own_justification) :-
    once(le_tree('family-le-scasp', is_a_grandparent_of(john, john), Tree)),
    literals(Tree, Literals),
    assertion(memberchk(is_a_grandparent_of(john, john), Literals)).

test(rps_payout_is_justified_by_the_throw) :-
    once(le_tree('simpleRPS-scasp', gets(bob, 1000), Tree)),
    literals(Tree, Literals),
    assertion(memberchk(gets(bob, 1000), Literals)).

% PINNED: a justification is one tree.  The new library's is a forest,
% the query beside the NMR check, which gaps.pl records.

test(minicontract_justification_is_a_tree) :-
    once(le_tree('minicontract-scasp', is_a_valid_contract(the_contract),
                 Tree)),
    assertion(Tree = _-_).

% family-le-scasp.pl declares `:- module('family-scasp+http://tests.com', [])`.
% The old implementation interns the module into the functor name as
% `Module:Name` and used to split it again at the *first* colon, so every
% literal in the model and the justification came back as
% `'family-scasp+http':'//tests.com:is_a_parent_of'(john,mary)`.  The new
% one interns nothing, but the program is the one that would show it.

test(literals_are_unqualified_despite_a_colon_in_the_module_name) :-
    once(le_tree('family-le-scasp', is_a_grandparent_of(john, john), Tree)),
    literals(Tree, Literals),
    assertion(\+ ( member(Literal, Literals),
                   Literal = _:_
                 )).

:- end_tests(le_justification).

		 /*******************************
		 *           HELPERS		*
		 *******************************/

%!  le_tree(+Base, :Query, -Tree) is nondet.
%
%   The justification of an answer to Query.

le_tree(Base, Query, Tree) :-
    le_module(Base, Module),
    scasp(Module:Query, [tree(Tree)]).

%!  literals(+Tree, -Literals) is det.
%
%   Every goal in the justification, with `not`, `-` and the `chs/1`,
%   `proved/1` and `assume/1` markers taken off, so a test can name a
%   step without knowing how it was reached.  A tree is `Node-Children`
%   or a list of them; the old library's nodes are also wrapped in
%   `goal_origin/2`.

literals(Tree, Literals) :-
    findall(Literal, tree_literal(Tree, Literal), Literals).

tree_literal(Forest, Literal) :-
    is_list(Forest),
    !,
    member(Node, Forest),
    tree_literal(Node, Literal).
tree_literal(Goal-Children, Literal) :-
    (   bare(Goal, Literal)
    ;   tree_literal(Children, Literal)
    ).

bare(Var, _) :- var(Var), !, fail.
bare(not(Goal), Literal)            :- !, bare(Goal, Literal).
bare(-(Goal), Literal)              :- !, bare(Goal, Literal).
bare(chs(Goal), Literal)            :- !, bare(Goal, Literal).
bare(proved(Goal), Literal)         :- !, bare(Goal, Literal).
bare(assume(Goal), Literal)         :- !, bare(Goal, Literal).
bare(goal_origin(Goal, _), Literal) :- !, bare(Goal, Literal).
bare(Goal, Goal) :- callable(Goal).
