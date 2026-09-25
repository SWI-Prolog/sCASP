:- module(test_annotations,
          [ test_annotations/0
          ]).
:- use_module('../scasp_plunit', []).        % sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/annotations)).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(lists), [member/2]).

/** <module> The annotation table

`sscasp/annotations.pl` collects a program's domain declarations, checks them
against each other and against the program, and answers what the domain of an
argument is.

The table is a value rather than a store, so these tests hand it a list of
terms and look at what comes back — no state to set up and none to tear down.
*/

test_annotations :-
    run_tests([ annotations_collect,
                annotations_query,
                annotations_clauses,
                annotations_errors
              ]).

		 /*******************************
		 *         COLLECTING		*
		 *******************************/

:- begin_tests(annotations_collect).

program([ (:- domain day = [mon, tue, wed]),
          (:- domain hour = 0..23),
          (:- pred meeting(day, hour)),
          meeting(mon, 9),
          meeting(tue, 14)
        ]).

test(domains_are_collected) :-
    program(P),
    program_annotations(P, A),
    assertion(domain_kind(A, day, enum([mon, tue, wed]))),
    assertion(domain_kind(A, hour, range(0, 23))).

test(an_undeclared_domain_has_no_kind) :-
    program_annotations([ (:- pred wrap(shape)),
                          shape(a), shape(b),
                          wrap(a)
                        ], A),
    assertion(\+ domain_kind(A, shape, _)).

test(a_program_without_annotations_gives_an_empty_table) :-
    program_annotations([p, (q :- p)], A),
    assertion(\+ domain_kind(A, _, _)),
    assertion(\+ argument_domain(A, p/0, 1, _)).

test(a_global_constraint_is_not_an_annotation) :-
    program_annotations([p, (:- p)], A),
    assertion(\+ domain_kind(A, _, _)).

test(two_arities_of_one_name_each_have_a_signature) :-
    program_annotations([ (:- domain d = [1, 2]),
                          (:- pred p(d)),
                          (:- pred p(d, d)),
                          p(1), p(1, 2)
                        ], A),
    assertion(argument_domain(A, p/1, 1, d)),
    assertion(argument_domain(A, p/2, 2, d)).

:- end_tests(annotations_collect).

		 /*******************************
		 *          QUERYING		*
		 *******************************/

:- begin_tests(annotations_query).

annotation_table(A) :-
    program_annotations([ (:- domain day = [mon, tue]),
                          (:- domain hour = 0..23),
                          (:- pred meeting(day, hour)),
                          (:- pred note(day, _)),
                          meeting(mon, 9),
                          note(mon, hello)
                        ], A).

test(each_argument_resolves) :-
    annotation_table(A),
    assertion(argument_domain(A, meeting/2, 1, day)),
    assertion(argument_domain(A, meeting/2, 2, hour)).

%   An `_' position has no domain.  It fails rather than answering `any',
%   so a caller reads "no domain here" as failure.

test(an_anonymous_position_has_no_domain) :-
    annotation_table(A),
    assertion(argument_domain(A, note/2, 1, day)),
    assertion(\+ argument_domain(A, note/2, 2, _)).

test(a_predicate_without_a_signature_has_no_domains) :-
    annotation_table(A),
    assertion(\+ argument_domain(A, meeting/3, 1, _)).
test(a_position_out_of_range_has_no_domain) :-
    annotation_table(A),
    assertion(\+ argument_domain(A, meeting/2, 3, _)).

:- end_tests(annotations_query).

		 /*******************************
		 *          CLAUSES		*
		 *******************************/

:- begin_tests(annotations_clauses).

test(an_enumeration_contributes_its_facts) :-
    program_annotations([(:- domain day = [mon, tue])], A),
    annotation_clauses(A, Clauses),
    assertion(Clauses == [day(mon), day(tue)]).

test(a_range_contributes_one_guarded_clause) :-
    program_annotations([(:- domain hour = 0..2)], A),
    annotation_clauses(A, Clauses),
    assertion(Clauses = [(hour(_) :- _)]).

test(a_signature_contributes_nothing) :-
    program_annotations([(:- pred p(d)), (:- domain d = [1]), p(1)], A),
    annotation_clauses(A, Clauses),
    assertion(Clauses == [d(1)]).

test(no_declarations_no_clauses) :-
    program_annotations([p], A),
    annotation_clauses(A, Clauses),
    assertion(Clauses == []).

:- end_tests(annotations_clauses).

		 /*******************************
		 *          REJECTED		*
		 *******************************/

%   A domain is a unary predicate, so there are two ways for one to exist
%   and it must be exactly one of them.

:- begin_tests(annotations_errors).

test(a_declared_domain_the_program_also_defines,
     [ throws(error(permission_error(redefine, domain, day), _)) ]) :-
    program_annotations([ (:- domain day = [mon]),
                          day(tue)
                        ], _).

test(a_domain_declared_twice,
     [ throws(error(permission_error(redefine, domain, day), _)) ]) :-
    program_annotations([ (:- domain day = [mon]),
                          (:- domain day = [tue])
                        ], _).

test(a_signature_given_twice,
     [ throws(error(permission_error(redefine, signature, p/1), _)) ]) :-
    program_annotations([ (:- domain d = [1]),
                          (:- pred p(d)),
                          (:- pred p(d)),
                          p(1)
                        ], _).

test(a_signature_naming_a_domain_that_does_not_exist,
     [ throws(error(existence_error(domain, shape), _)) ]) :-
    program_annotations([ (:- pred wrap(shape)),
                          wrap(a)
                        ], _).

test(a_signature_for_a_predicate_that_does_not_exist,
     [ throws(error(existence_error(procedure, wrap/1), _)) ]) :-
    program_annotations([ (:- domain shape = [a]),
                          (:- pred wrap(shape))
                        ], _).

%   A domain the program defines as an ordinary predicate is fine — that
%   is the case needing no declaration at all.

test(a_domain_the_program_defines_is_accepted) :-
    program_annotations([ (:- pred wrap(shape)),
                          shape(a), shape(b),
                          wrap(a)
                        ], A),
    assertion(argument_domain(A, wrap/1, 1, shape)).

:- end_tests(annotations_errors).
