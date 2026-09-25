:- module(test_corpus,
          [ test_corpus/0
          ]).
:- use_module('../scasp_plunit', [with_scasp_flag/3]).
:- use_module(library(sscasp/ops)).
:- use_module(corpus).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(lists), [member/2]).
:- use_module(library(sscasp/run), [scasp/3]).
:- use_module(library(sscasp/source), [read_program/2]).

/** <module> The new implementation against the golden corpus

One test per corpus program with golden data, comparing what `scasp/3`
answers with what the old implementation recorded.

`corpus.pl` says which programs, what each is called, and which do not
agree.  This file is only the plunit units that run it, so that the list of
what does not agree — the to-do list for finishing the reimplementation —
is somewhere a person would look for it rather than among test bodies.
*/

test_corpus :-
    run_tests([ corpus_answers,
                corpus_models,
                corpus_dispositions,
                corpus_without_nmr
              ]).

		 /*******************************
		 *        THE ANSWERS		*
		 *******************************/

%   Whether the two implementations bind the **query** the same way.
%   That is what was asked for, and a difference in it means something
%   about the program rather than about the route taken to it.

:- begin_tests(corpus_answers).

test(answers, [forall(comparable(_Name, File))]) :-
    answers_agree(File).

:- end_tests(corpus_answers).

		 /*******************************
		 *        THE MODELS		*
		 *******************************/

%   The stricter check, where it holds.  Worth keeping separate rather
%   than relaxing everything to the answers: a model that changes when it
%   should not is a regression, and only a test that compares models
%   catches it.

:- begin_tests(corpus_models).

test(models, [forall(( strict(Name), corpus_program(Name, File) ))]) :-
    agrees(File).

%   Every strict program is comparable, so the list cannot drift out of
%   step with the gaps.

test(strict_programs_are_comparable, [forall(strict(Name))]) :-
    assertion(comparable(Name, _)).

:- end_tests(corpus_models).

		 /*******************************
		 *      WHAT DOES NOT AGREE	*
		 *******************************/

%   Declared `fail`, not `blocked` or `fixme`: plunit does not run the
%   body of those, so they could never report that the problem has gone
%   away.  These run, and the day one starts agreeing the suite says so —
%   which is as much the point for a `golden` disposition as for a `gap`,
%   since the day the old implementation is fixed its `.pass` changes.
%
%   An error counts as a disagreement, which is why the body catches one.
%   That is not swallowing it: `corpus.pl` records what the error is, and
%   what this asserts is only that the program does not yet agree.

:- begin_tests(corpus_dispositions).

test(still_disagrees,
     [ forall(( disposition(Name, Disposition),
                \+ slow(Disposition),
                corpus_program(Name, File)
              )),
       fail
     ]) :-
    catch(answers_agree(File), _, fail).

slow(gap(slow)).

%   Every name in the list has to be a program, or a typo would quietly
%   excuse nothing and the program it meant to excuse would fail.

test(every_disposition_names_a_program, [forall(disposition(Name, _))]) :-
    assertion(corpus_program(Name, _)).

%   A `slow` one is not run at all: a test has a time limit and there is
%   nothing to learn from watching it expire.  `bec_loan` is the only one
%   left, and `corpus.pl` says what is known about it.
%
%   What closed the rest of the event calculus family is in `CLAUDE.md`
%   under the quantifier: three readings of one mistake — negating a
%   constraint that relates the quantified variable to something the
%   answer itself invented — and then `covered/7`, the complement no
%   longer leaving its constraint store behind.

%   Nor is a corpus useful if nobody looks at it: a directory listed in
%   `corpus/3` that turns up no programs is a path that has gone stale.

test(every_corpus_has_programs, [forall(corpus(Corpus, _, _))]) :-
    assertion(corpus_program(Corpus/_, _)).

:- end_tests(corpus_dispositions).

		 /*******************************
		 *      WITHOUT THE CHECK	*
		 *******************************/

/** What the solver answers when the NMR check is not in the way

`scasp/3` appends the NMR check to every query, so a program whose check
does not terminate has no answerable query -- which says nothing about
whether the solver can answer the query.  Here it can: `bec_tap_01` is one
of the four `slow` gaps and every query in its comments comes back in
milliseconds.

Two of the expected values in those comments are the file's, not ours, and
one of them is wrong: `holdsAt(leak(L), 14)` is commented `L = 0`, and both
implementations answer no.  The test asserts what both answer.

This is a *query* test, not a semantics one -- `scasp_compile_nmr` false is
not a mode to run a program in, since a constraint the answer violates is
then not checked.  It is here to keep the gap honest about what it is.
*/

:- begin_tests(corpus_without_nmr).

%!  expected(?Query, ?Answers) is nondet.
%
%   The queries written in `bec_tap_01_overflow.pl`, with what both
%   implementations answer.

%   The variable is underscored and still named: it is what the answer
%   binds, and `level(_H)` says which argument that is where `level(_)`
%   would not.

expected(holdsAt(level(_H), -1), []).
expected(holdsAt(level(_H), 4),  [0]).
expected(holdsAt(level(_H), 8),  [3]).
expected(holdsAt(level(_H), 14), [9]).
expected(holdsAt(level(12), _),  []).
expected(holdsAt(leak(_L), 14),  []).
expected(holdsAt(leak(_L), 19),  [4]).
expected(holdsAt(leak(_L), 22),  []).

%   `bec_loan` is the remaining gap, and it is worth a test of its own:
%   its query is `?- holdsAt(F, T).` with both arguments free, and what
%   used to happen to it was not a search but a descent —
%   `not holdsAt(requested_on(the_borrower, 1000, 2), T)` calling
%   `not initiates(ends(1), is_liable_to_litigation(the_lender), T)`
%   calling the same pair one step earlier in time, 6,677 frames deep.
%   `solve.pl:same_question/2` stops it; what is left is the size of the
%   search its NMR check opens, which is a different problem.

test(the_loan_query_is_a_search_and_no_longer_a_descent) :-
    once(corpus_program(all_programs/bec_loan, File)),
    read_program(File, Source),
    with_scasp_flag(scasp_compile_nmr, false,
                    assertion(scasp(Source.terms, holdsAt(_, _), _))).

%   The two that answer with a *time* rather than a level.

test(a_level_is_reached_at_the_time_it_should_be) :-
    without_nmr(holdsAt(level(7), T), T, Times),
    assertion(Times == [12]).

test(and_spilling_holds_from_some_time_on) :-
    without_nmr(holdsAt(spilling, T), T, [Answer]),
    assertion(var(Answer)).

test(the_queries_in_its_comments, [forall(expected(Query, Answers))]) :-
    query_variable(Query, Var),
    without_nmr(Query, Var, Got),
    assertion(Got == Answers).

%!  without_nmr(+Query, -Var, -Answers) is det.

without_nmr(Query, Var, Answers) :-
    once(corpus_program(all_programs/bec_tap_01_overflow, File)),
    read_program(File, Source),
    with_scasp_flag(scasp_compile_nmr, false,
                    findall(Var, scasp(Source.terms, Query, _), Answers)).

%   The argument each query asks about: the level, the leak or the time.

query_variable(holdsAt(level(H), _), H) :- var(H), !.
query_variable(holdsAt(leak(L), _), L) :- var(L), !.
query_variable(_, x).

:- end_tests(corpus_without_nmr).
