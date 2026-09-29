:- module(test_compile,
          [ test_compile/0
          ]).
:- use_module('../scasp_plunit', [with_scasp_flag/3]).  % and sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/compile)).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(lists), [member/2, memberchk/2, subtract/3]).

/** <module> Compiling a whole program

`sscasp/compile.pl` is the pass that turns a list of terms into the program
the solver runs: every clause translated, every predicate dualised, every
called-but-undefined predicate closed, and the global constraints set aside
for the NMR check.

The tests below mostly ask *which clauses exist* rather than matching whole
programs, because the order of the clauses is not part of the contract and
pinning it would make every later change to the pass a failure.  Where the
shape matters — the dual of a fact, the guard on a domained argument — it is
destructured, for the reason `test_abstract.pl` gives: two `_` in a pattern
are two different variables.
*/

test_compile :-
    run_tests([ compile_structure,
                compile_domains,
                compile_closure,
                compile_nmr,
                compile_abducibles,
                compile_sld,
                compile_unbounded
              ]).

%!  compiled(+Terms, -Program) is det.

compiled(Terms, Program) :-
    compile_program(m, Terms, Program).

%!  defines(+Program, ?Name/?Arity) is nondet.
%
%   Name/Arity is a predicate the compiled program defines.

defines(Program, Name/Arity) :-
    member(Clause, Program.clauses),
    head_of(Clause, Head),
    functor(Head, Name, Arity).

head_of((Head :- _), Head) :- !.
head_of(Fact, Fact).

%!  clause_for(+Program, +Name, -Clause) is nondet.

clause_for(Program, Name, Clause) :-
    member(Clause, Program.clauses),
    head_of(Clause, Head),
    functor(Head, Name, _).

		 /*******************************
		 *         STRUCTURE		*
		 *******************************/

:- begin_tests(compile_structure).

%   The positive clauses are translated too.  That sounds obvious, and it
%   is the thing the pass was written for: dualising alone leaves a program
%   whose duals call predicates nothing defines.

test(a_clause_is_translated_head_and_body) :-
    compiled([(p(X) :- q(X))], P),
    once(clause_for(P, 'm$plain$p$1', Clause)),
    Clause = ('m$plain$p$1'(A) :- Body),
    assertion(Body == 'm$plain$q$1'(A)).

test(a_fact_stays_a_fact) :-
    compiled([q(1)], P),
    assertion(memberchk('m$plain$q$1'(1), P.clauses)).

%   The head of a positive clause is *not* abstracted: only the dual needs
%   the unification made explicit, and `q(1)' is what the author wrote.

test(the_positive_head_is_not_abstracted) :-
    compiled([q(1)], P),
    assertion(\+ clause_for(P, 'm$plain$q$1', (_ :- _))).

test(a_predicate_gets_its_duals) :-
    compiled([q(1)], P),
    assertion(defines(P, 'm$dual$q$1'/1)),
    assertion(defines(P, 'm$dual.1$q$1'/1)).

%   `p' and `-p' are different predicates, so they compile to different
%   names and get duals of their own.

test(classical_negation_is_a_predicate_of_its_own) :-
    compiled([cold, (-(cold) :- hot)], P),
    assertion(defines(P, 'm$plain$cold$0'/0)),
    assertion(defines(P, 'm$neg$cold$0'/0)),
    assertion(defines(P, 'm$dual$cold$0'/0)),
    assertion(defines(P, 'm$dualneg$cold$0'/0)).

%   The dual of `-p' is built from the clause with the `-' taken off: it is
%   `p/0' that names the predicate, and `dualneg' that carries the polarity.

test(the_negative_dual_calls_the_dual_of_its_body) :-
    compiled([(-(cold) :- hot)], P),
    once(clause_for(P, 'm$dualneg.1$cold$0', Clause)),
    Clause = ('m$dualneg.1$cold$0' :- Body),
    assertion(Body == 'm$dual$hot$0').

test(several_clauses_of_one_predicate_stay_together) :-
    compiled([p(1), q(a), p(2)], P),
    findall(C, clause_for(P, 'm$plain$p$1', C), Cs),
    assertion(Cs == ['m$plain$p$1'(1), 'm$plain$p$1'(2)]),
    assertion(defines(P, 'm$dual.2$p$1'/1)).

:- end_tests(compile_structure).

		 /*******************************
		 *          DOMAINS		*
		 *******************************/

:- begin_tests(compile_domains).

%   A domain declaration brings clauses of its own.

test(a_domain_becomes_a_predicate) :-
    compiled([(:- domain thing = [a, b])], P),
    assertion(memberchk('m$plain$thing$1'(a), P.clauses)),
    assertion(memberchk('m$plain$thing$1'(b), P.clauses)).

%   And goes through the same path as the user's clauses, so it has a dual:
%   a domain is a predicate, and `not thing(X)' must be constructive like
%   any other negation.

test(a_domain_predicate_has_a_dual) :-
    compiled([(:- domain thing = [a, b])], P),
    once(clause_for(P, 'm$dual.1$thing$1', Clause)),
    Clause = ('m$dual.1$thing$1'(A) :- Body),
    assertion(Body == (A \= a)).

%   The point of the annotation: a signature is shorthand for a guard in
%   every clause of the predicate (decision B, `doc/design-next.md' open
%   question 3).  Outside the domain `t' is false, so its negation holds.

test(an_annotated_argument_is_guarded) :-
    compiled([ (:- domain thing = [a, b]),
               (:- pred t(_, thing)),
               (t(X, Y) :- u(X, Y))
             ], P),
    once(clause_for(P, 'm$plain$t$2', Clause)),
    assertion(Clause =@= ('m$plain$t$2'(A, B) :-
                              'm$plain$thing$1'(B), 'm$plain$u$2'(A, B))).

%   The guard comes *before* the head unification, so a guarded argument
%   that is not a variable is unified after the guard has generated or tested
%   it.  Unifying first would guard only the shapes the head admits, and
%   `not t(X)' would lose every value of the domain with another shape.

test(the_guard_precedes_the_head_unification) :-
    compiled([ (:- domain thing = [a, b]),
               (:- pred t(_, thing)),
               t(1, a)
             ], P),
    once(clause_for(P, 'm$plain$t$2', Clause)),
    assertion(Clause =@= ('m$plain$t$2'(1, B) :-
                              'm$plain$thing$1'(B), B = a)).

test(and_the_dual_has_no_guard_of_its_own) :-
    compiled([ (:- domain thing = [a, b]),
               (:- pred t(_, thing)),
               t(1, a)
             ], P),
    once(clause_for(P, 'm$dual$t$2', Clause)),
    assertion(Clause =@= ('m$dual$t$2'(A, B) :- 'm$dual.1$t$2'(A, B))).

:- end_tests(compile_domains).

		 /*******************************
		 *          CLOSURE		*
		 *******************************/

%   A predicate called but never defined is false of everything, so its
%   negation is true of everything.  Writing both out is what makes the
%   result a closed program: running it cannot raise an existence error,
%   and the solver need not treat "no clauses" as a case of its own.

:- begin_tests(compile_closure).

test(an_undefined_predicate_is_false_of_everything) :-
    compiled([(p(X) :- q(X))], P),
    clause_for(P, 'm$plain$q$1', Clause),
    assertion(Clause = ('m$plain$q$1'(_) :- fail)).

test(and_its_dual_is_true_of_everything) :-
    compiled([(p(X) :- q(X))], P),
    once(clause_for(P, 'm$dual$q$1', Clause)),
    assertion(Clause = 'm$dual$q$1'(_)).

test(a_defined_predicate_is_not_closed_over) :-
    compiled([(p(X) :- q(X)), q(1)], P),
    findall(C, clause_for(P, 'm$plain$q$1', C), Cs),
    assertion(Cs == ['m$plain$q$1'(1)]).

%   The call may be inside a quantifier, which is where an undefined
%   predicate is most likely to hide: the inner dual is generated code and
%   nothing else mentions what it calls.

test(a_call_inside_a_quantifier_is_found) :-
    compiled([(p(X) :- q(X), not t(X, _))], P),
    assertion(defines(P, 'm$plain$t$2'/2)),
    assertion(defines(P, 'm$dual$t$2'/2)).

test(a_generator_inside_for_domain_is_found) :-
    compiled([ (:- domain thing = [a, b]),
               (:- pred t(_, thing)),
               (p(X) :- q(X), t(X, Y), not r(Y)),
               t(1, a)
             ], P),
    once(clause_for(P, 'm$dual.1$p$1', ('m$dual.1$p$1'(_) :- for_domain(_, Gen, _)))),
    functor(Gen, Name, _),
    assertion(defines(P, Name/1)).

%   Every call in the compiled program resolves.  That is the property the
%   closure exists for, so it is asserted directly rather than inferred
%   from the cases above.

test(every_call_has_a_definition) :-
    compiled([ (:- domain thing = [a, b]),
               (:- pred t(_, thing)),
               (p(X) :- q(X), t(X, Y), not s(Y)),
               (-(r) :- p(_)),
               q(1),
               t(1, a)
             ], P),
    findall(N/A,
            ( member(C, P.clauses),
              body_of(C, B),
              body_goal(B, G),
              callable(G),
              functor(G, N, A),
              sub_atom(N, _, _, _, '$')
            ),
            Called),
    findall(N/A, defines(P, N/A), Defined),
    sort(Called, Cs), sort(Defined, Ds),
    subtract(Cs, Ds, Missing),
    assertion(Missing == []).

body_of((_ :- Body), Body) :- !.
body_of(_, true).

body_goal(Var, _) :- var(Var), !, fail.
body_goal((A, B), G) :- !, (body_goal(A, G) ; body_goal(B, G)).
body_goal(forall(_, A), G) :- !, body_goal(A, G).
body_goal(for_domain(_, Gen, A), G) :- !, (body_goal(Gen, G) ; body_goal(A, G)).
body_goal(G, G).

:- end_tests(compile_closure).

		 /*******************************
		 *        THE NMR CHECK		*
		 *******************************/

%   A global constraint and a rule on an odd loop both say that something
%   must not hold, and neither is tested by proving the query.  Both
%   become a denial, and the NMR check is the conjunction of their
%   negations.  `graph.pl' finds the loops, `nmr.pl' builds the check;
%   what is tested here is that the program reaches both.

:- begin_tests(compile_nmr).

test(a_global_constraint_becomes_a_sub_check) :-
    compiled([p(1), (:- p(2))], P),
    assertion(P.nmr.checks == [check(1, constraint, p(2))]).

test(an_annotation_is_not_a_constraint) :-
    compiled([(:- domain thing = [a, b]), (:- pred t(_, thing)), t(1, a)], P),
    assertion(P.nmr.checks == []).

test(a_constraint_is_not_a_clause) :-
    compiled([(:- p(2))], P),
    assertion(\+ defines(P, 'm$plain$p$1'/1)).

%   A rule on an odd loop over negation is checked as well, and a rule on
%   no loop is not: a model satisfies those anyway.

test(a_rule_on_an_odd_loop_becomes_a_sub_check) :-
    compiled([(p :- not p)], P),
    assertion(P.nmr.checks == [check(1, p/0, (not p))]).

test(an_ordinary_rule_does_not) :-
    compiled([(p :- q), q], P),
    assertion(P.nmr.checks == []).

test(an_even_loop_does_not) :-
    compiled([(p :- not q), (q :- not p)], P),
    assertion(P.nmr.checks == []).

%   The check is always there, so the solver has one goal to call rather
%   than a case to test, and it is part of the program like anything else.

test(a_program_with_nothing_to_check_still_has_one) :-
    compiled([p(1)], P),
    assertion(P.nmr.goal == 'm$nmr$$0'),
    assertion(defines(P, 'm$nmr$$0'/0)).

test(the_sub_check_duals_are_in_the_program) :-
    compiled([p(1), (:- p(2))], P),
    assertion(defines(P, 'm$dualchk.1$$0'/0)),
    assertion(defines(P, 'm$dualchk.1.1$$0'/0)).

%   And the closure covers them: a constraint may be the only mention of
%   a predicate in the whole program.

%   `:- q(2)' denies one literal, so its dual calls only q's dual — the
%   positive q is never reached and is not closed over.  A second literal
%   brings it in, since the clause denying that one calls the first.

%   `p' and `-p' are different atoms, and nothing but this stops a
%   program deriving both.  It is a constraint the language imposes
%   rather than the program, so it is generated rather than written.

test(a_classically_negated_predicate_gets_a_consistency_check) :-
    compiled([(p(X) :- q(X)), (-(p(X)) :- r(X)), q(1), r(2)], P),
    assertion(P.nmr.checks =@= [check(1, consistency(p/1), (-(p(A)), p(A)))]).

%   Only where both polarities have clauses: if either has none it is
%   false of everything and the conjunction could not hold anyway.

test(but_not_when_only_one_polarity_is_defined) :-
    compiled([(p(X) :- q(X)), q(1)], P),
    assertion(P.nmr.checks == []).

test(nor_when_the_negative_is_only_called) :-
    compiled([(p(X) :- q(X), not(-(p(X)))), q(1)], P),
    assertion(P.nmr.checks == []).

%   A predicate that is **only** an abducible does not get one, because
%   the check is *entailed*: the four rules an `#abducible` compiles to
%   have the answer sets `{p}` and `{-p}` and never both, so `:- p, -p`
%   holds in each.  Generating it anyway is a free choice the solver has
%   to make, once per query that drags the declaration in, and the
%   answers double.

test(a_pure_abducible_does_not_get_one) :-
    compiled([(:- abducible(maybe)), (rain :- maybe)], P),
    assertion(P.nmr.checks == []).

%   Both halves have to be abducible-only.  Rules for the negative half
%   give a second route to it, and then the two really can interact.

test(unless_the_negative_half_has_rules_too) :-
    compiled([(:- abducible(maybe)), (-(maybe) :- storm), storm], P),
    assertion(P.nmr.checks =@= [check(1, consistency(maybe/0), (-maybe, maybe))]).

%   And so do rules for the **positive** half, which is the half that was
%   not being looked at: `maybe` is derivable from `storm`, so assuming
%   `-maybe` gives a model holding both and the check is what says so.
%   `test_abducible.pl` shows what it costs to leave out.

test(or_the_positive_half) :-
    compiled([(:- abducible(maybe)), (maybe :- storm), storm], P),
    assertion(P.nmr.checks =@= [check(1, consistency(maybe/0), (-maybe, maybe))]).

test(a_predicate_only_a_constraint_mentions_is_closed_over) :-
    compiled([p(1), (:- q(2))], P),
    assertion(defines(P, 'm$dual$q$1'/1)).

test(and_the_positive_when_a_later_literal_needs_it) :-
    compiled([p(1), (:- q(2), r(2))], P),
    assertion(defines(P, 'm$plain$q$1'/1)),
    assertion(defines(P, 'm$dual$r$1'/1)).

:- end_tests(compile_nmr).

		 /*******************************
		 *        ABDUCIBLES		*
		 *******************************/

%   `#abducible p(X)` is written out as four rules.  Two of them give the
%   two halves their name, and the other two are an even loop over
%   negation between those halves — which is the whole mechanism: an even
%   loop is a free choice, so there is nothing in the solver to add.
%
%   `abducible/1` is a wrapper on the *head*, like `-`/1, rather than a
%   predicate of its own.  Two abducibles have to be two predicates: as
%   clauses of one, assuming either would decide the other.

:- begin_tests(compile_abducibles).

test(the_positive_half_proves_the_predicate) :-
    compiled([(:- abducible(p(_)))], P),
    once(clause_for(P, 'm$plain$p$1', Clause)),
    Clause = ('m$plain$p$1'(A) :- Body),
    assertion(Body == 'm$abducible$p$1'(A)).

test(and_the_negative_half_its_classical_negation) :-
    compiled([(:- abducible(p(_)))], P),
    once(clause_for(P, 'm$neg$p$1', Clause)),
    Clause = ('m$neg$p$1'(A) :- Body),
    assertion(Body == 'm$abducibleneg$p$1'(A)).

%   Each half is proved by the negation of the other, which is the loop.

test(the_two_halves_are_an_even_loop) :-
    compiled([(:- abducible(p(_)))], P),
    once(clause_for(P, 'm$abducible$p$1', Positive)),
    once(clause_for(P, 'm$abducibleneg$p$1', Negative)),
    assertion(Positive =@= ('m$abducible$p$1'(A) :- 'm$dualabducibleneg$p$1'(A))),
    assertion(Negative =@= ('m$abducibleneg$p$1'(B) :- 'm$dualabducible$p$1'(B))).

%   They are predicates like any other, so they are dualised like any
%   other rather than by a rule written here.

test(and_are_dualised_like_anything_else) :-
    compiled([(:- abducible(p(_)))], P),
    assertion(defines(P, 'm$dualabducible$p$1'/1)),
    assertion(defines(P, 'm$dualabducibleneg$p$1'/1)).

%   An even loop, so `graph.pl` must not read it as one to check.

test(the_loop_is_not_an_odd_one) :-
    compiled([(:- abducible(p(_)))], P),
    assertion(P.nmr.checks == []).

%   The declared predicate may have rules of its own, and then its dual
%   has to account for both — which is why the four rules are generated
%   as source and dualised, rather than emitted already compiled.

test(a_predicate_may_be_both_abducible_and_derived) :-
    compiled([(:- abducible(p(_))), (p(X) :- q(X))], P),
    findall(C, clause_for(P, 'm$plain$p$1', C), Clauses),
    assertion(length(Clauses, 2)),
    assertion(defines(P, 'm$dual.1$p$1'/1)),
    assertion(defines(P, 'm$dual.2$p$1'/1)).

:- end_tests(compile_abducibles).

		 /*******************************
		 *           :- SLD		*
		 *******************************/

/** One `:- sld` may declare several predicates, as `:- dynamic` does.
    `as` binds tighter than `,`, so it covers the whole list only in
    parentheses.
*/

:- begin_tests(compile_sld).

test(several_share_a_type_in_parentheses) :-
    sld_declarations(((p/1, q/2) as opaque), List),
    assertion(List == [p/1 as opaque, q/2 as opaque]).

test(without_them_as_is_the_last_one_s) :-
    sld_declarations((p/1, q/2 as opaque), List),
    assertion(List == [p/1, q/2 as opaque]).

test(each_gets_its_bridges) :-
    compiled([(:- sld((p/1, q/2) as opaque))], P),
    assertion(defines(P, 'm$plain$p$1'/_)),
    assertion(defines(P, 'm$dual$q$2'/_)).

:- end_tests(compile_sld).

		 /*******************************
		 *        THE REPORT		*
		 *******************************/

%   A quantifier with a domain is a `for_domain/3', so a surviving
%   `forall/2' means one that nothing bounds — decision 5's undecidable
%   case.  Listing the predicates whose duals contain one *is* the
%   analysis, which is why the report costs nothing.

:- begin_tests(compile_unbounded).

test(an_undomained_body_variable_is_reported) :-
    compiled([(p(X) :- q(X), not t(X, _))], P),
    assertion(P.unbounded == [p/1]).

test(a_domain_bounds_it) :-
    compiled([ (:- domain thing = [a, b]),
               (:- pred t(_, thing)),
               (p(X) :- q(X), t(X, Y), not r(Y)),
               t(1, a)
             ], P),
    assertion(P.unbounded == []).

%   A negated literal bounds nothing by itself — `not t(X, Y)' holds of
%   every Y outside `thing' — but the signature puts the guard `thing(Y)'
%   before it, and that does.

test(a_negated_literal_is_bounded_by_its_guard) :-
    compiled([ (:- domain thing = [a, b]),
               (:- pred t(_, thing)),
               (p(X) :- q(X), not t(X, _)),
               t(1, a)
             ], P),
    assertion(P.unbounded == []).

%   The guard goes before every `not t(...)' a body writes, and before
%   none of the positive ones: those `t''s own clauses guard.

test(the_use_site_is_guarded) :-
    compiled([ (:- domain thing = [a, b]),
               (:- pred t(_, thing)),
               (p(X) :- q(X), t(X, _), not t(X, _)),
               t(1, a)
             ], P),
    once(clause_for(P, 'm$plain$p$1', Clause)),
    assertion(Clause =@= ('m$plain$p$1'(X) :-
                              'm$plain$q$1'(X), 'm$plain$t$2'(X, _),
                              'm$plain$thing$1'(Y), 'm$dual$t$2'(X, Y))).

test(a_program_with_no_body_variables_reports_nothing) :-
    compiled([p(1), (q(X) :- p(X))], P),
    assertion(P.unbounded == []).

%   The report names the *user's* predicate, not the generated helper the
%   quantifier is actually in — that is what the author can act on.

test(the_report_names_the_source_predicate) :-
    compiled([(-(r) :- p(_))], P),
    assertion(P.unbounded == [r/0]).

%   A sub-check has no name of its own to give, so it is reported by
%   index and by what it is for — two global constraints would otherwise
%   be told apart only by a number nothing else mentions.

test(a_sub_check_is_reported_by_what_it_checks) :-
    compiled([s(1), (q(Y) :- s(Y)), (:- q(X), r(X))], P),
    assertion(P.unbounded == [check(1, constraint)]).

%   A positive literal of a predicate defined by ground facts bounds a
%   variable to their values, so nothing is left unbounded ...

test(a_fact_literal_bounds_its_variable) :-
    compiled([q(1), (:- q(X), r(X))], P),
    assertion(P.unbounded == []).

%   ... and a negated one does not: outside q's facts `not q(X)` holds.

test(a_negated_fact_literal_does_not) :-
    compiled([q(1), s(1), (:- s(Y), not q(X), r(X, Y))], P),
    assertion(P.unbounded == [check(1, constraint)]).

test(several_are_sorted_and_unique) :-
    compiled([ (p(X) :- q(X), not t(X, _)),
               (a(X) :- q(X), not t(X, _)),
               (b(X) :- q(X), not t(X, _, _))
             ], P),
    assertion(P.unbounded == [a/1, b/1, p/1]).

:- end_tests(compile_unbounded).

/** The dynamic consistency check's rules: one per user literal of a
    global constraint, saying what else would complete it.  Made only
    under `sscasp_dcc`, on by default, so a program compiled with it off
    is unchanged.
*/

:- begin_tests(compile_dcc).

test(no_rules_without_the_flag) :-
    with_scasp_flag(sscasp_dcc, false,
                    compile_program(m, [p, q, (:- p, q)], Program)),
    assertion(Program.dcc == []).

test(one_rule_per_literal_of_a_constraint) :-
    with_scasp_flag(sscasp_dcc, true,
                    compile_program(m, [p, q, (:- p, q)], Program)),
    length(Program.dcc, N),
    assertion(N == 2).

%   A built-in completes a constraint but does not trigger it: it is not
%   proved, so there is no moment at which it is established.

test(a_builtin_is_checked_but_does_not_trigger) :-
    with_scasp_flag(sscasp_dcc, true,
                    compile_program(m, [p(1), (:- p(X), X > 0)], Program)),
    assertion(Program.dcc = [dcc(_, [_ > 0])]).

:- end_tests(compile_dcc).
