:- module(test_domains,
          [ test_domains/0
          ]).
:- use_module(scasp_plunit, []).        % sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/domains)).
:- use_module(library(plunit)).

/** <module> Domain annotations

`sscasp/domains.pl` is the first piece of the fresh implementation
(`doc/design-next.md`).  It has no dependency on the solver and none on the
current tree, so it is tested on its own terms: given a directive, what
annotation comes back, and given an annotation, what clauses define it.
*/

test_domains :-
    run_tests([ domains_enum,
                domains_range,
                domains_signature,
                domains_syntax_errors,
                domains_expansion
              ]).

		 /*******************************
		 *        ENUMERATIONS		*
		 *******************************/

:- begin_tests(domains_enum).

test(a_list_of_atoms) :-
    annotation(domain(day = [mon, tue, wed]), A),
    assertion(A == domain(day, enum([mon, tue, wed]))).
test(elements_may_be_compound) :-
    annotation(domain(shape = [f(a), g(b), c]), A),
    assertion(A == domain(shape, enum([f(a), g(b), c]))).
test(elements_may_be_numbers) :-
    annotation(domain(small = [1, 2, 3]), A),
    assertion(A == domain(small, enum([1, 2, 3]))).
test(a_single_element_is_a_domain) :-
    annotation(domain(unit = [only]), A),
    assertion(A == domain(unit, enum([only]))).

:- end_tests(domains_enum).

		 /*******************************
		 *           RANGES		*
		 *******************************/

:- begin_tests(domains_range).

test(an_integer_range) :-
    annotation(domain(hour = 0..23), A),
    assertion(A == domain(hour, range(0, 23))).
test(a_range_may_be_a_single_value) :-
    annotation(domain(one = 1..1), A),
    assertion(A == domain(one, range(1, 1))).
test(a_range_may_be_negative) :-
    annotation(domain(t = -5.. -1), A),
    assertion(A == domain(t, range(-5, -1))).

:- end_tests(domains_range).

		 /*******************************
		 *         SIGNATURES		*
		 *******************************/

%   A signature binds each argument position to a domain.  It defines
%   nothing; the domain named must exist, either declared or as an
%   ordinary predicate.

:- begin_tests(domains_signature).

test(each_argument_takes_a_domain) :-
    annotation(pred(meeting(day, hour)), A),
    assertion(A == signature(meeting/2, [day, hour])).
test(one_argument) :-
    annotation(pred(wrap(shape)), A),
    assertion(A == signature(wrap/1, [shape])).
test(an_anonymous_argument_is_unconstrained) :-
    annotation(pred(edge(node, _)), A),
    assertion(A == signature(edge/2, [node, any])).
test(the_same_domain_twice) :-
    annotation(pred(edge(node, node)), A),
    assertion(A == signature(edge/2, [node, node])).

:- end_tests(domains_signature).

		 /*******************************
		 *          REJECTED		*
		 *******************************/

%   A malformed annotation raises rather than failing: at the point where
%   the directive is recognised as one of ours, silence would hide a typo.
%   A directive that is *not* ours fails, so a caller can use annotation/2
%   as the test in a chain.

:- begin_tests(domains_syntax_errors).

test(an_unrelated_directive_is_not_an_annotation, [fail]) :-
    annotation(dynamic(foo/1), _).
test(an_unbound_directive_is_not_an_annotation, [fail]) :-
    annotation(_, _).

test(a_domain_without_a_body,
     [ throws(error(domain_error(domain_definition, _), _)) ]) :-
    annotation(domain(day), _).
test(an_empty_enumeration,
     [ throws(error(domain_error(non_empty_domain, _), _)) ]) :-
    annotation(domain(day = []), _).
test(a_non_ground_element,
     [ throws(error(type_error(ground_domain_element, _), _)) ]) :-
    annotation(domain(day = [mon, _]), _).
test(a_body_that_is_neither_list_nor_range,
     [ throws(error(domain_error(domain_body, _), _)) ]) :-
    annotation(domain(day = mon), _).

test(a_range_over_non_integers,
     [ throws(error(type_error(integer_range, _), _)) ]) :-
    annotation(domain(hour = a..z), _).
test(a_backwards_range,
     [ throws(error(domain_error(non_empty_range, _), _)) ]) :-
    annotation(domain(hour = 23..0), _).

test(a_signature_that_is_not_compound,
     [ throws(error(type_error(predicate_signature, _), _)) ]) :-
    annotation(pred(meeting), _).
test(a_signature_argument_that_is_not_a_name,
     [ throws(error(type_error(domain_name, _), _)) ]) :-
    annotation(pred(meeting(1)), _).

%   `$` separates the fields of the mangling scheme, which is unambiguous
%   only because no name contains one.  Rejected where the message can
%   still name the declaration.

test(a_dollar_in_a_domain_name,
     [ throws(error(permission_error(use, reserved_character($), _), _)) ]) :-
    annotation(domain('a$b' = [x]), _).
test(a_dollar_in_a_predicate_name,
     [ throws(error(permission_error(use, reserved_character($), _), _)) ]) :-
    annotation(pred('a$b'(day)), _).
test(a_dollar_in_a_domain_reference,
     [ throws(error(permission_error(use, reserved_character($), _), _)) ]) :-
    annotation(pred(meeting('a$b')), _).

:- end_tests(domains_syntax_errors).

		 /*******************************
		 *         EXPANSION		*
		 *******************************/

:- begin_tests(domains_expansion).

test(an_enumeration_becomes_facts) :-
    annotation(domain(day = [mon, tue]), A),
    domain_clauses(A, Clauses),
    assertion(Clauses == [day(mon), day(tue)]).

%   A range becomes a CLP(Q) region rather than a fact per value.  That is
%   the point of writing `0..23': `not busy(H)' over a region is one
%   constrained answer, not twenty-four.

test(a_range_becomes_a_constraint) :-
    annotation(domain(hour = 0..23), A),
    domain_clauses(A, [Clause]),
    assertion(Clause = (hour(_) :- _)),
    Clause = (_ :- Body),
    assertion(Body = (_ #>= 0, _ #=< 23)).

test(a_signature_defines_nothing) :-
    annotation(pred(wrap(shape)), A),
    domain_clauses(A, Clauses),
    assertion(Clauses == []).

%   Both kinds are finite, so both can be listed when a `forall' is to be
%   expanded into a conjunction -- a range only when asked, since avoiding
%   the enumeration is why one writes it that way.

test(the_elements_of_an_enumeration) :-
    domain_elements(enum([mon, tue]), E),
    assertion(E == [mon, tue]).
test(the_elements_of_a_range) :-
    domain_elements(range(1, 4), E),
    assertion(E == [1, 2, 3, 4]).

test(the_guard_for_an_argument) :-
    domain_goal(shape, X, Goal),
    assertion(Goal == shape(X)).

:- end_tests(domains_expansion).
