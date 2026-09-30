:- module(test_listing,
          [ test_listing/0
          ]).
:- use_module('../scasp_plunit', [with_scasp_flag/3]). % and the search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/listing)).
:- use_module(library(sscasp/compile), [compile_program/3]).
:- use_module(library(sscasp/source), [read_program/2]).
:- use_module(library(sscasp/render), [portray_rule/2]).
:- use_module(library(plunit)).
:- use_module(library(lists), [member/2, memberchk/2, append/3]).
:- use_module(library(apply), [convlist/3]).
:- use_module(library(debug), [assertion/1]).

/** <module> The compiled program, read back

`compile.pl` produces a program over mangled functors because the solver
works on functor names.  `listing.pl` reads it back into the language it was
written in, which is the only form worth showing anyone.

The one thing it does that is more than demangling is **inlining the
helpers as a disjunction**: `not p` is the conjunction of one generated
helper per clause of `p`, and each helper has one clause per way its body
can fail.  Printed as it stands that is a hierarchy of generated names; as
a disjunction it is the dual, which is what a reader is asking about.

Disjunction and not separate clauses, and that is the whole trick — a
conjunction of two three-clause helpers is one body with two `;` groups
where splitting it into clauses would be nine of them.
*/

test_listing :-
    run_tests([ listing_sections,
                listing_duals,
                listing_checks,
                listing_corpus
              ]).

%!  listing(+Program, -Listing) is det.

listing(Terms, Listing) :-
    compile_program(scasp, Terms, Program),
    program_listing(Program, Listing).

%!  printed(+Clauses, -Text) is det.

printed(Clauses, Text) :-
    with_output_to(string(Text),
                   forall(member(Clause, Clauses),
                          portray_rule(Clause, []))).

%!  section(+Terms, +Section, -Text) is det.

section(Terms, Section, Text) :-
    listing(Terms, Listing),
    Predicates = Listing.get(Section),
    findall(Clause,
            ( member(Predicate, Predicates),
              clauses_of(Predicate, Clauses),
              member(Clause, Clauses)
            ),
            All),
    printed(All, Text).

clauses_of(title(_)-Clauses, Clauses) :- !.
clauses_of(Clauses, Clauses).

contains(Text, Needle) :-
    sub_string(Text, _, _, _, Needle).

		 /*******************************
		 *         THE SECTIONS		*
		 *******************************/

:- begin_tests(listing_sections).

program([ p(1), p(2),
          (q(X) :- p(X)),
          (-r(X) :- p(X)),
          (:- q(1))
        ]).

test(the_user_predicates_are_what_was_written) :-
    program(P),
    section(P, user, Text),
    assertion(contains(Text, "p(1).")),
    assertion(contains(Text, "q(A) :-")),
    assertion(\+ contains(Text, "$")).

%   `-r/1` and `r/1` are two predicates.  A listing that ran their clauses
%   together would say they were one.

test(classical_negation_is_its_own_predicate) :-
    program(P),
    section(P, user, Text),
    assertion(contains(Text, "-r(A) :-")).

test(the_duals_are_a_section_of_their_own) :-
    program(P),
    section(P, duals, Text),
    assertion(contains(Text, "not p(A)")),
    assertion(contains(Text, "not q(A)")),
    assertion(\+ contains(Text, "not p(1).")).

test(and_the_check_another) :-
    program(P),
    section(P, constraints, Text),
    assertion(contains(Text, "check(1)")),
    assertion(contains(Text, "not q(1)")).

%   Nothing generated is left visible: a mangled name in a listing is the
%   listing having given up.

test(no_mangled_name_survives, [forall(member(Section, [user, duals, constraints]))]) :-
    program(P),
    section(P, Section, Text),
    assertion(\+ contains(Text, "$")),
    assertion(\+ contains(Text, "generated(")).

:- end_tests(listing_sections).

		 /*******************************
		 *          THE DUALS		*
		 *******************************/

:- begin_tests(listing_duals).

%   `not p` is the conjunction of one helper per clause of `p`, and each
%   helper is a disjunction over the ways that clause can fail.  Both
%   halves have to come out, and the second as a `;` rather than as
%   separate clauses.

test(a_fact_dualises_to_a_disequality) :-
    section([p(1), p(2)], duals, Text),
    assertion(contains(Text, "not p(A) :-")),
    assertion(contains(Text, "A\\=1")),
    assertion(contains(Text, "A\\=2")).

test(a_body_dualises_to_a_disjunction) :-
    section([p(1), q(1), (r(X) :- p(X), q(X))], duals, Text),
    assertion(contains(Text, "not r(A) :-")),
    assertion(contains(Text, ";")),
    assertion(contains(Text, "not p(A)")),
    assertion(contains(Text, "not q(A)")).

%   A body-only variable becomes a quantifier, and the disjunction goes
%   inside it.

test(a_body_variable_becomes_a_quantifier) :-
    with_scasp_flag(
        sscasp_unfold_facts, false,
        section([p(1), t(a), (q(Z) :- t(Z)), (r(X) :- p(X), q(Y), not s(X, Y))],
                duals, Text)),
    assertion(contains(Text, "forall(")),
    assertion(contains(Text, ";")).

%   ... unless a fact predicate bounds it, when it is enumerated.  That
%   is where the literal is not unfolded (`unfold.pl'), which would bind
%   Y to `a' at compile time and leave no quantifier at all.

test(a_variable_a_fact_bounds_is_enumerated) :-
    with_scasp_flag(
        sscasp_unfold_facts, false,
        section([p(1), q(a), (r(X) :- p(X), q(Y), not s(X, Y))],
                duals, Text)),
    assertion(contains(Text, "for_domain(")),
    assertion(contains(Text, "q(")),
    assertion(\+ contains(Text, "scasp$")).

%   One body per clause of the predicate, joined by `,`: `not p` holds
%   when *every* clause fails.

test(several_clauses_are_a_conjunction) :-
    section([(p(X) :- q(X)), (p(X) :- r(X)), q(1), r(2)], duals, Text),
    assertion(contains(Text, "not q(A)")),
    assertion(contains(Text, "not r(A)")).

%   Under `scasp_domain_error` a signature's guard is preceded by a
%   check, which is shown as the `must_be/2` it amounts to and has no
%   dual branch of its own: its negation is `fail`.

test(a_domain_check_reads_as_must_be) :-
    with_scasp_flag(scasp_domain_error, true,
                    section([ (:- domain d = [1, 2]),
                              (:- pred q(d)),
                              q(1)
                            ], duals, Text)),
    assertion(contains(Text, "must_be(d, A)")),
    assertion(\+ contains(Text, "domain_check")),
    assertion(\+ contains(Text, "scasp$")).

test(a_predicate_with_no_clauses_is_false_of_nothing) :-
    section([(p(X) :- unheard_of_predicate_4711(X))], duals, Text),
    assertion(contains(Text, "not p(A)")).

:- end_tests(listing_duals).

		 /*******************************
		 *         THE CHECKS		*
		 *******************************/

/** Each sub-check is a group of its own, headed by what it came from —
    which `nmr.checks` records and nothing else can reconstruct.
*/

:- begin_tests(listing_checks).

%   The dict is read into a variable first.  `Listing.get(constraints)`
%   *inside* `findall/3`'s goal is a meta-argument, and the expansion
%   does not reach there.

titles(Terms, Titles) :-
    listing(Terms, Listing),
    Constraints = Listing.constraints,
    convlist(title_of, Constraints, Titles).

title_of(title(Title)-_, Title).

%   The destructuring is outside `assertion/1`, which runs its goal in a
%   double negation and lets no binding out.

test(a_global_constraint_says_so) :-
    titles([p(1), (:- p(1))], Titles),
    Titles = [Title],
    assertion(contains(Title, "global constraint")).

test(a_consistency_check_names_its_predicate) :-
    titles([p(1), (-p(2))], Titles),
    Titles = [Title|_],
    assertion(contains(Title, "consistency of p/1")).

test(an_odd_loop_names_its_predicate) :-
    titles([(p :- not p)], Titles),
    Titles = [Title],
    assertion(contains(Title, "odd loop")).

test(each_check_is_a_group_of_its_own) :-
    titles([p(1), (-p(2)), q(1), (-q(2))], Titles),
    assertion(length(Titles, 2)).

:- end_tests(listing_checks).

		 /*******************************
		 *         THE CORPUS		*
		 *******************************/

/** A listing must not **fail**, whatever the compiler put in front of it:
    it is what a reader falls back on when something is wrong, so failing
    is the one thing it cannot do.  `wrap/3` has a catch-all for a kind
    nobody thought of, and this is the test that would have caught the
    four abducible kinds it did not have.
*/

:- begin_tests(listing_corpus).

test(every_corpus_program_lists, [forall(corpus_program(File))]) :-
    read_program(File, Source),
    listing(Source.terms, Listing),
    forall(member(Section, [user, duals, constraints]),
           ( section(Source.terms, Section, Text),
             assertion(\+ contains(Text, "$")),
             assertion(\+ contains(Text, "generated("))
           )),
    assertion(is_dict(Listing)).

corpus_program(File) :-
    source_file(corpus_program(_), Here),
    file_directory_name(Here, Sscasp),
    file_directory_name(Sscasp, Dir),
    atomic_list_concat([Dir, '/all_programs/*.pl'], Pattern),
    expand_file_name(Pattern, Files),
    member(File, Files),
    \+ blocked(File).

%   The one program the solver does not terminate on is not a problem
%   here — nothing is solved — but it is excluded from the corpus tests
%   everywhere else and consistency is worth more than one extra case.

blocked(File) :-
    file_base_name(File, Base),
    memberchk(Base, ['clpq_equality.pl']).

:- end_tests(listing_corpus).
