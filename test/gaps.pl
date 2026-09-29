:- module(gaps,
          [ gap/3,                      % ?Library, ?Unit:Test, ?Reason
            load_errors/4               % ?Library, ?File, ?Count, ?Reason
          ]).
:- use_module(library(lists), [member/2]).

/** <module> Shared tests an implementation does not pass yet

The suites in `test/` state what s(CASP) means and run against either
implementation (`under_test.pl`).  Where one of them does not pass, the
test is listed here with the reason, which keeps `make check` green
without weakening the test: test_all/1 runs every listed test on its
own and **fails if it passes**, so an entry cannot outlive the problem
it records.  Remove the entry when that happens.

This is the same discipline as `known_issues.pl` for the golden corpus
and `sscasp/corpus.pl:disposition/2` for the reimplementation's view of
it.  Do not use plunit's `fixme` instead: a `fixme` test whose only
failure is an assertion/1 is counted as passed.
*/

%!  gap(?Library, ?Unit:Test, ?Reason) is nondet.
%
%   The shared test Unit:Test fails against Library for Reason.  Reason
%   is one of
%
%     - missing(What): the library does not have it yet
%     - differs(What): it has it under another name or in another shape
%     - pinned(What): the test pins the old implementation's answer,
%       which is known to be wrong, and the new one answers otherwise
%     - bug(What): a defect in the library

gap(Library, Unit:Test, Reason) :-
    gap_(Library, Unit, Tests, Reason),
    member(Test, Tests).

%   `@</2` and friends are not built-ins of the new compiler, so the
%   collector follows the name into `system` and clause/2 refuses.

gap_(sscasp, builtin_standard_order,
     [ atoms_are_ordered_alphabetically, and_not_the_other_way,
       numbers_precede_atoms, compound_terms_are_ordered_by_argument,
       strict_order_excludes_equal, less_or_equal_admits_equal,
       greater_than_is_the_mirror, greater_or_equal_admits_equal,
       the_dual_of_less_is_greater_or_equal, the_dual_fails_where_less_holds,
       a_comparison_the_functors_settle_holds, so_the_negation_fails,
       an_unbound_variable_precedes_an_atom,
       and_nothing_precedes_an_unbound_variable,
       and_an_unbound_deciding_argument_goes_undetected,
       nor_is_one_hidden_behind_a_later_clash
     ],
     missing("standard order comparisons (@</2, ...) as built-ins")).
gap_(sscasp, builtin_interval,
     [ infimum_of_an_open_interval, supremum_of_an_open_interval,
       the_constraint_survives_alongside_the_bound
     ],
     missing("inf/2 and sup/2 as built-ins")).
gap_(sscasp, builtin_findall,
     [ negated_findall_always_fails ],
     differs("the negation of findall/3 raises rather than failing")).
gap_(sscasp, diseq_not_in,
     [ the_constraint_excludes_the_list, a_value_in_the_list_is_refused,
       a_value_outside_the_list_is_admitted,
       an_earlier_binding_outside_the_list_is_kept
     ],
     differs("∉/2 is part of the language, not an exported predicate")).
gap_(sscasp, diseq_not_in,
     [ not_in_is_not_usable_in_a_program ],
     pinned("∉ in a program body works here, and raises in the old one")).
gap_(sscasp, compound_negation,
     [ the_unbound_answer_admits_only_the_f_shape ],
     pinned("doc/bugs.md #1: the old answer to not p(X) is incomplete")).
gap_(sscasp, compound_head_rewriting_is_neutral,
     [ all_three_give_the_same_single_negative_answer ],
     pinned("doc/bugs.md #1: the old answer to not p(X) is incomplete")).
gap_(sscasp, prolog_control,
     [ if_then_else_takes_the_then_branch, if_then_else_takes_a_later_branch,
       if_then_else_takes_the_else_branch, disjunction_gives_both,
       cut_keeps_only_the_first, prolog_negation_inside_the_goal,
       prolog_negation_inside_the_goal_fails
     ],
     differs("`:- prolog p/N` is spelled `:- sld p/N`; see load_errors/4")).
gap_(sscasp, prolog_opaque,
     [ the_justification_is_still_a_tree ],
     differs("the justification is a forest, the query beside the NMR check")).
gap_(sscasp, le_justification,
     [ minicontract_justification_is_a_tree ],
     differs("the justification is a forest, the query beside the NMR check")).
gap_(sscasp, diff_known_divergence,
     [ undefined_atom_in_a_query_raises_rather_than_answering ],
     differs("the program is closed: an undefined atom is false, not an error")).

gap_(sscasp, dyncall_collection,
     [ transitively_reachable_is_collected ],
     differs("scasp_query_clauses/2 returns the terms, not source(Ref, Clause)")).
gap_(sscasp, dyncall_errors,
     [ undefined_predicate_is_reported ],
     missing("scasp/2's unknown(error) option: the program is closed")).
gap_(sscasp, dyncall_errors,
     [ two_calls_in_one_conjunction_fail ],
     pinned("the old solver's determinism error, which the new one has not")).

%!  load_errors(?Library, ?File, ?Count, ?Reason) is nondet.
%
%   Loading the shared suite File (a base name) against Library gives
%   Count errors, for Reason.  A clause that does not load is a test that
%   is not there, so these are counted rather than ignored.

load_errors(sscasp, 'test_prolog_calls.pl', 7,
            differs("`prolog` is not a prefix operator; it is `sld`")).
