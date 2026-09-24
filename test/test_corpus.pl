:- module(test_corpus,
          [ test_corpus/0
          ]).
:- use_module(scasp_plunit, [with_scasp_flag/3]).
:- use_module(library(sscasp/ops)).
:- use_module(corpus).
:- use_module(library(plunit)).
:- use_module(library(lists), [member/2]).
:- use_module(library(sscasp/run), [sscasp/3]).
:- use_module(library(sscasp/source), [read_program/2]).

/** <module> The new implementation against the golden corpus

One test per program of `test/all_programs/` that has golden data, comparing
the models `sscasp/3` answers with against the ones the old implementation
records.

A program the new implementation cannot yet match is listed in `gap/2` with
the reason, and its test asserts that it still **fails** — so a gap cannot
outlive the problem it records.  That is the discipline `known_issues.pl`
uses, for the same reason: a list of what is missing is only useful while
something checks it is still missing.

The gap list *is* the to-do list for finishing the reimplementation.
*/

test_corpus :-
    run_tests([corpus_answers, corpus_models, corpus_gaps, corpus_without_nmr]).

%!  gap(?Name, ?Reason) is nondet.
%
%   Programs the new implementation does not yet match, and why.
%
%   | `feature`    | a language feature that is not implemented |
%   | `slow`       | not answered within the time a test may take |
%   | `incomplete` | finishes, and misses answers it should have found |
%   | `upstream`   | a library will not do it |
%   | `comparison` | the golden data cannot answer the question asked of it |

%   One of the event calculus family is left, and what closed the other
%   three is worth reading.  They answered in milliseconds until
%   `solve.pl:disequality_goals/2` stopped calling `dif:attribute_goals/3`
%   directly — a hook that **deletes** the constraints it reports, so a
%   quantifier reading a variable's disequalities removed them and covered
%   a domain it had not covered.  Reading them properly is not optional,
%   and what it cost was the measure of what those programs had been
%   getting for free.
%
%   Paying it turned out to be affordable once the complement stopped
%   leaving its constraint store behind (`solve.pl:covered/7`).  A branch
%   invents variables, CLP(Q) keeps every variable a constraint ever
%   reached in one class, and nothing collects the ones that go out of
%   scope — so the class grew by everything every branch had ever
%   touched, and `dump/3` is Fourier–Motzkin over that class, asked once
%   per covering.  `bec_tap_01` went from no answer in 20 s to 0.087 s.
%
%       bec_tap_01     1 answer,  0.087 s      (was no answer in 20 s)
%       bec_tap_02     1 answer,  0.086 s      (was no answer in 20 s)
%       bec_tap_03     2 answers, 0.171 s      (was no answer in 20 s)
%       bec_light      2 answers, 0.40 s against the old 0.087 s
%       bec_tap_04     2 answers, 0.032 s against 0.049 s
%       lion           1 answer,  0.021 s against 0.018 s   (was a hang)
%
%   `bec_loan` is the one left, and it is no longer a descent: its query
%   `?- holdsAt(F, T).` answers in 0.027 s with the check off, where it
%   used to spend 6,677 ancestor frames walking backwards in time.  With
%   the check on it still answers -- in **376 s**, against 0.85 s for the
%   old implementation, so what is left is a factor of 440 and not a
%   hang.  `CLAUDE.md` has the measurements.

gap(bec_loan,               slow).

gap(hanoi_scasp_7,          comparison).

%!  strict(?Name) is nondet.
%
%   Programs whose *models* match as well, which is more than agreement
%   on the answer requires.  Not every program can: a model is a function
%   of the proof, so a solver reaching the same answer another way
%   carries a different model with it.

strict(classic_negation_inconstistent).
strict(forall_arity).
strict(hanoi).
strict(hanoi_7).
strict(haplotype).
strict(min_model).
strict(olon_unreachable).
strict(pq).
strict(rat).
strict(solve_findall).
strict(vars).

%!  comparable(?Name, ?File) is nondet.

comparable(Name, File) :-
    corpus_program(Name, File),
    \+ gap(Name, _).

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
		 *          GAPS		*
		 *******************************/

%   Declared `fail`, not `blocked` or `fixme`: plunit does not run the
%   body of those, so they could never report that the problem has gone
%   away.  These run, and the day one starts agreeing the suite says so.
%
%   An error counts as a gap, which is why the body catches one.  That is
%   not swallowing it: the reason in `gap/2` records what the error is,
%   and what this asserts is only that the program does not yet agree.

:- begin_tests(corpus_gaps).

test(still_a_gap,
     [ forall(( gap(Name, Reason), Reason \== slow,
                corpus_program(Name, File) )),
       fail
     ]) :-
    catch(answers_agree(File), _, fail).

%   The `slow` ones are not run.  The four that are left are all event
%   calculus; `queens.pl` was here too until the two things that made it
%   never finish were found and fixed — an unindexed lookup over the
%   whole proof, and a quantifier whose branches accumulated proofs that
%   mostly repeated.  It now answers in 0.26 s where the old
%   implementation takes 0.95 s.
%
%   The four are event calculus, and they no longer **loop**.  That took
%   three fixes, all of one mistake — negating a constraint that relates
%   the quantified variable to something the answer itself invented:
%
%     - `copy_term/3` does not project.  It reports the CLP(Q) store in
%       internal form, slack variables and all.  `region/2` uses
%       `dump/3`.
%     - `dif(T, X)` for a **free** X excludes one unknown point, so it
%       says nothing about T alone.  The excluded value must be ground.
%     - `dump/3` cannot eliminate a variable from a CLP(Q)
%       **disequality**, so it hands back `Slot + A =\= 15` for a free A.
%       Those are dropped, which *is* the projection they should have
%       had.
%
%   The last one took `bec_tap_01` from a narrowing 154 deep carrying 155
%   constraints to a narrowing of 2 carrying 2, and from 180 projections
%   in ten seconds to 22,971.
%
%   What is left is search size, and two experiments say where to look.
%
%   Committing to the first covering — the `!` the old implementation has
%   in `solve_other_forall/11` — makes all four finish in hundredths of a
%   second and answer **nothing**, where the old one answers 4, 1, 1 and
%   2.  So the cut is not transferable on its own: upstream it is paired
%   with a mechanism this design has no analogue for.  Having cut, the
%   old code produces its alternative answers by dualising the answer's
%   constraints on the *non-quantified* variables and re-solving under
%   each (`solve_other_forall/11`'s second disjunct).  Here alternatives
%   come from backtracking into the covering, which is the whole search.
%
%   Committing only to *branch* coverage, whose proof is discarded
%   anyway, changes nothing.
%
%   So the next step is that mechanism rather than another patch: commit
%   to one covering, then enumerate alternatives by partitioning the
%   other variables.

test(slow_programs_are_named, [forall(gap(Name, slow))]) :-
    assertion(corpus_program(Name, _)).

:- end_tests(corpus_gaps).

		 /*******************************
		 *      WITHOUT THE CHECK	*
		 *******************************/

/** What the solver answers when the NMR check is not in the way

`sscasp/3` appends the NMR check to every query, so a program whose check
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
    once(corpus_program(bec_loan, File)),
    read_program(File, Source),
    with_scasp_flag(scasp_compile_nmr, false,
                    assertion(sscasp(Source.terms, holdsAt(_, _), _))).

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
    once(corpus_program(bec_tap_01_overflow, File)),
    read_program(File, Source),
    with_scasp_flag(scasp_compile_nmr, false,
                    findall(Var, sscasp(Source.terms, Query, _), Answers)).

%   The argument each query asks about: the level, the leak or the time.

query_variable(holdsAt(level(H), _), H) :- var(H), !.
query_variable(holdsAt(leak(L), _), L) :- var(L), !.
query_variable(_, x).

:- end_tests(corpus_without_nmr).
