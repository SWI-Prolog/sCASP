:- module(le_corpus,
          [ le_program/2,               % ?Base, ?Query
            le_expected/2,              % ?Base, ?Expected
            le_differs/4,               % ?Library, ?Base, ?Expected, ?Reason
            le_answer/3,                % ?Library, ?Base, ?Expected
            le_module/2,                % +Base, -Module
            le_file/2,                  % ?Base, -File
            le_matches/2                % +Expected, +Answers
          ]).
:- use_module(library(lists), [member/2]).

/** <module> The Logical English examples, as a corpus

`test/le_programs/` holds 13 s(CASP) programs exported from Logical
English.  They are *embedded* programs — ordinary Prolog modules that
load a library, carry `#pred` templates and are queried through
`scasp/2` — so the file front end has nothing to say about them:
reading them that way takes their `:- module/2` and `:- dynamic`
directives for s(CASP) global constraints.

They are examples rather than curated tests, and several have problems of
their own.  The point of pinning them is not that every answer is right —
for the ones that yield nothing, nobody has checked whether that is the
intended reading of the scenario — but that the outcome is **recorded**,
so a change in either solver is noticed rather than silently swallowed.

This module is the description and `test_le.pl` runs it, against
whichever library is under test.  Each program says
`:- use_module(library(scasp))`, which `swipl -Dsscasp` makes the new
library, so the files run as they are.

Keeping it apart is worth a file for the reason `sscasp/corpus.pl` is a
file: what does *not* agree is the to-do list, and a to-do list spread
across test bodies is one nobody reads.
*/

%!  le_program(?Base, ?Query) is nondet.
%
%   The programs and the query each is asked.  Each file ends with its
%   own query under a `/** <examples> */` marker; that is this one.

le_program('citizenshiptrust-scasp',       acquires_British_citizenship_on(_, _)).
le_program('criminaljustice-scasp',        exception_to_applies_to_under_of(_, _, _, _)).
le_program('family-le-scasp',              is_a_grandparent_of(_, _)).
le_program('impossibleancestor-scasp',     is_impossible(_)).
le_program('isdapermissioncorrected-scasp',
           it_is_permitted_that(designates_that(_, occurs_at(_, _)))).
le_program('itispermittedthat-scasp',
           'It_is_permitted_that'(designates_at_that(_, _, occurs_at(_, _)))).
le_program('list-scasp',                   is_a_subset_of(_, _)).
le_program('loanwithcure-scasp',           defaults_on(_, _)).
le_program('minicontract-scasp',           is_a_valid_contract(_)).
le_program('obligation-scasp',
           cures_the_failure_of_on_that(the_borrower, _, _,
                                        pays_to_on(the_borrower, _, the_lender, _))).
le_program('simpleRPS-scasp',              gets(_, _)).
le_program('subset-scasp',                 is_a_subset_of(_, _)).
le_program('turingcomplete-scasp',         some_tm_goes_from_to([1, 1, 1, b], _)).

%!  le_expected(?Base, ?Expected) is nondet.
%
%   What the query answers, as one of
%
%   | `answers(List)` | the query's own term, one per answer, sorted |
%   | `none`          | it holds of nothing |
%   | `error(Formal)` | the program does not run, and why |
%
%   An answer is written as the **query term instantiated**, so the
%   query is its own template and nothing has to name the variables.
%   `le_matches/2` compares by variant, since an answer may leave a
%   variable free — `obligation-scasp` leaves two.
%
%   These are `library(scasp)`'s answers.  Where the reimplementation
%   differs, le_differs/4 says what it answers instead, and why.

le_expected('citizenshiptrust-scasp', none).
le_expected('criminaljustice-scasp',  none).
le_expected('family-le-scasp',
            answers([ is_a_grandparent_of(john, john),
                      is_a_grandparent_of(mary, mary)
                    ])).
le_expected('impossibleancestor-scasp',
            answers([ is_impossible(john),
                      is_impossible(mary)
                    ])).
le_expected('isdapermissioncorrected-scasp',
            answers([ it_is_permitted_that(
                          designates_that(
                              'Alice',
                              occurs_at('Early_Termination_in_respect_of_all_\c
                                         outstanding_Transactions',
                                        'Friday')))
                    ])).
le_expected('itispermittedthat-scasp',
            answers([ 'It_is_permitted_that'(
                          designates_at_that(
                              'Alice', 'Tuesday',
                              occurs_at('Early_Termination_in_respect_of_all_\c
                                         outstanding_Transactions',
                                        'Friday')))
                    ])).

%   The payer and the cured failure are left unbound, which is why the
%   comparison is by variant rather than by equality.

le_expected('obligation-scasp',
            answers([ cures_the_failure_of_on_that(
                          the_borrower, _, 1464825600.0,
                          pays_to_on(the_borrower, 525, the_lender, _))
                    ])).
le_expected('loanwithcure-scasp',     none).
le_expected('minicontract-scasp',
            answers([is_a_valid_contract(the_contract)])).

%   Four answers, two distinct payouts: what is pinned is the set.

le_expected('simpleRPS-scasp',
            answers([gets(bob, 1000), gets(miguel, 100)])).
le_expected('turingcomplete-scasp',   none).

%   Two do not run at all.  Both are limitations of the s(CASP) side
%   rather than of the scenario, so they are recorded as errors rather
%   than as an absence of answers, which would look the same as the
%   group above.

le_expected('list-scasp',
            error(permission_error(scasp, procedure, system:once/1))).
le_expected('subset-scasp',
            error(permission_error(define, scasp_control_construct, forall/2))).

		 /*******************************
		 *      WHAT DOES NOT AGREE	*
		 *******************************/

%!  le_differs(?Library, ?Base, ?Expected, ?Reason) is nondet.
%
%   Where Library does not answer what le_expected/2 records, what it
%   answers instead, and why.  The tests compare against Expected, so an
%   entry cannot outlive the difference it records: the day Library
%   answers like the other, it no longer answers Expected.
%
%   **12 of 13 agree.**  `list-scasp` defines its own `forall/2` and
%   calls it.  Both libraries refuse to run it, which is the outcome that
%   matters; they differ in which unsupported thing they name first.
%   `library(scasp)` reaches `once/1`, called from the `wrong/2` that
%   this `forall/2` calls; `library(sscasp)` stops at the redefinition
%   itself, since that is what would have been silently reinterpreted.
%   `subset-scasp` does the same, and there both name the redefinition.

le_differs(sscasp, 'list-scasp',
           error(permission_error(define, scasp_control_construct, forall/2)),
           "both refuse; ours names the redefined forall/2 where the old \c
            names the once/1 under it").

%   `turingcomplete-scasp` calls `append/3` and neither defines nor
%   imports it deliberately.  The two agree on its answer, by different
%   routes: `library(scasp)` reads `append/3` as a predicate the program
%   never defined and closes it to false, and `library(sscasp)` follows
%   the name into `library(lists)` and collects its clauses.  It used to
%   be refused here as a program with two definitions of `append/3`: the
%   collector asked `lists`' ancestor `system` before `lists` itself.
%   Collecting a *library's* clauses is still the hazard `declared/2`
%   exists for, and what is missing is a way to say "this name is not my
%   program's" without a declaration in a generated file.

%!  le_answer(?Library, ?Base, ?Expected) is nondet.
%
%   What Base answers against Library: le_expected/2, unless le_differs/4
%   says otherwise.

le_answer(Library, Base, Expected) :-
    member(Library, [scasp, sscasp]),
    le_program(Base, _),
    (   le_differs(Library, Base, Differs, _)
    ->  Expected = Differs
    ;   le_expected(Base, Expected)
    ).

		 /*******************************
		 *        THE COMPARISON		*
		 *******************************/

%!  le_matches(+Expected, +Answers) is semidet.
%
%   Answers is the query term as each answer instantiated it.  Two
%   things about the comparison:
%
%   **The set, not the list.**  `simpleRPS-scasp` answers four times
%   with two distinct payouts under both libraries, and how many answers
%   a solution set arrives in is an implementation choice — the set is
%   the semantics.
%
%   **By variant, not by equality.**  An answer may leave a variable
%   free — `obligation-scasp` leaves two — and two runs name a free
%   variable differently.

le_matches(none, Answers) :-
    !,
    Answers == [].
le_matches(answers(Expected), Answers) :-
    !,
    sort(Expected, Wanted),
    sort(Answers, Got),
    Wanted =@= Got.
le_matches(error(_), _) :-
    fail.                               % an error is not a set of answers

		 /*******************************
		 *          THE FILES		*
		 *******************************/

%!  le_file(?Base, -File) is det.

le_file(Base, File) :-
    source_file(le_file(_, _), Here),
    file_directory_name(Here, TestDir),
    atomic_list_concat([TestDir, '/le_programs/', Base, '.pl'], File).

%!  le_module(+Base, -Module) is det.
%
%   Load Base and give the module it defines.  The module name cannot be
%   derived from the file name: `family-le-scasp.pl` declares
%   `:- module('family-scasp+http://tests.com', [])`.

le_module(Base, Module) :-
    le_file(Base, File),
    load_files(File, [silent(true), if(not_loaded)]),
    (   module_property(Module, file(File))
    ->  true
    ;   existence_error(le_module, Base)
    ).
