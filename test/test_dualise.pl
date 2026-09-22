:- module(test_dualise,
          [ test_dualise/0
          ]).
:- use_module(scasp_plunit, []).        % sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/dualise)).
:- use_module(library(sscasp/annotations), [program_annotations/2]).
:- use_module(library(plunit)).
:- use_module(library(lists), [member/2, nth1/3]).

/** <module> Dualising a predicate

`sscasp/dualise.pl` is where head abstraction, body dualising, mangling and
the annotation table meet.  What comes out is the clauses of the predicates
that prove a predicate's negation.

These destructure results rather than matching whole terms, for the reason
`test_abstract.pl` gives: two `_` in a pattern are two *different* variables,
so `Body == (d(A), call(A, _))` cannot match a body whose second argument is
already bound and the test fails for the wrong reason.

They name the generated predicates by mangled functor.  That is the one place
in the new tree where a test depends on the naming scheme — which is the point
of having decision 2 written down, and of `mangle.pl` being tested on its own.
*/

test_dualise :-
    run_tests([ dualise_structure,
                dualise_goals,
                dualise_domains,
                dualise_edges
              ]).

%!  duals(+Program, +PI, +Clauses, -Duals) is det.

duals(Program, PI, Clauses, Duals) :-
    program_annotations(Program, Annotations),
    predicate_duals(m, Annotations, plain, PI, Clauses, Duals).

%!  head_of(+Clause, -Head) is det.

head_of((Head :- _), Head) :- !.
head_of(Fact, Fact).

functor_names(Clauses, Names) :-
    findall(Name,
            ( member(C, Clauses), head_of(C, H), functor(H, Name, _) ),
            Names).

		 /*******************************
		 *         STRUCTURE		*
		 *******************************/

%   The worked example from doc/design.md, in the new naming.

:- begin_tests(dualise_structure).

example(Duals) :-
    duals([], p/1, [p(0), (p(X) :- q(X), not t(X, _Y))], Duals).

test(three_levels_of_predicate) :-
    example(Duals),
    functor_names(Duals, Names),
    assertion(Names == [ 'm$dual$p$1',
                         'm$dual.1$p$1',
                         'm$dual.2$p$1',
                         'm$dual.2.inner$p$1',
                         'm$dual.2.inner$p$1'
                       ]).

test(the_predicate_dual_conjoins_the_clause_duals) :-
    example([Top|_]),
    Top = ('m$dual$p$1'(A) :- Body),
    assertion(Body == ('m$dual.1$p$1'(A), 'm$dual.2$p$1'(A))).

%   Clause 1 is the fact p(0), abstracted to p(A) :- A = 0, so its dual is
%   the disequality.

test(a_fact_dualises_to_a_disequality) :-
    example(Duals),
    nth1(2, Duals, Clause),
    Clause = ('m$dual.1$p$1'(A) :- Body),
    assertion(Body == (A \= 0)).

%   Clause 2 has a body-only variable, so its dual hands off through a
%   forall to an innermost dual carrying the extra argument.

test(a_body_variable_becomes_a_forall) :-
    example(Duals),
    nth1(3, Duals, Clause),
    Clause = ('m$dual.2$p$1'(A) :- forall(B, Inner)),
    assertion(Inner == 'm$dual.2.inner$p$1'(A, B)).

test(the_innermost_dual_has_one_clause_per_literal) :-
    example(Duals),
    nth1(4, Duals, First),
    nth1(5, Duals, Second),
    First = ('m$dual.2.inner$p$1'(A, _) :- B1),
    assertion(B1 == 'm$dual$q$1'(A)),
    Second = ('m$dual.2.inner$p$1'(A2, B) :- B2),
    assertion(B2 == ('m$plain$q$1'(A2), 'm$plain$t$2'(A2, B))).

%   Two body variables share one innermost dual, nested outermost first,
%   rather than taking one each.

test(two_body_variables_nest_and_share) :-
    duals([], p/1, [(p(X) :- t(X, Y, Z), u(Y, Z))], Duals),
    nth1(2, Duals, Clause),
    Clause = ('m$dual.1$p$1'(_) :- forall(_, forall(_, Inner))),
    functor(Inner, Name, Arity),
    assertion(Name == 'm$dual.1.inner$p$1'),
    assertion(Arity == 3).

:- end_tests(dualise_structure).

		 /*******************************
		 *           GOALS		*
		 *******************************/

%   A dual is a predicate, so calling one is an ordinary call rather than
%   a not/1 term for an interpreter to take apart.

:- begin_tests(dualise_goals).

test(a_positive_goal_becomes_a_plain_call) :-
    translate_goal(m, q(X), Call),
    assertion(Call == 'm$plain$q$1'(X)).
test(a_negated_goal_becomes_a_dual_call) :-
    translate_goal(m, not(q(X)), Call),
    assertion(Call == 'm$dual$q$1'(X)).
test(a_zero_arity_goal) :-
    translate_goal(m, q, Call),
    assertion(Call == 'm$plain$q$0').

%   Classical negation is a literal of its own, so it takes the negative
%   kinds rather than being confused with default negation.

test(classical_negation_takes_the_neg_kind) :-
    translate_goal(m, -(cold(X)), Call),
    assertion(Call == 'm$neg$cold$1'(X)).
test(and_its_negation_takes_dualneg) :-
    translate_goal(m, not(-(cold(X))), Call),
    assertion(Call == 'm$dualneg$cold$1'(X)).

%   Built-ins mean what they mean.

test(builtins_are_left_alone, [forall(member(G, [ (_ = _), (_ \= _),
                                                  (_ #= _), (_ #< _),
                                                  true, fail ]))]) :-
    translate_goal(m, G, Call),
    assertion(Call == G).

test(a_goal_that_is_not_a_builtin_is_renamed) :-
    translate_goal(m, edge(a, b), Call),
    assertion(Call == 'm$plain$edge$2'(a, b)).

:- end_tests(dualise_goals).

		 /*******************************
		 *          DOMAINS		*
		 *******************************/

%   The whole point of the annotation: the dual of an argument with a
%   domain is guarded by it, so `not wrap(T)` enumerates rather than
%   trying to represent the complement of a shape.

:- begin_tests(dualise_domains).

wrap_program([ (:- domain shape = [f(a), g(a), b]),
               (:- pred wrap(shape)),
               (wrap(f(X)) :- item(X)),
               item(a)
             ]).

%   The guard is a call like any other, so it is mangled: the compiled
%   program defines `m$plain$shape$1', not `shape'.

test(a_declared_domain_guards_the_dual) :-
    wrap_program(P),
    duals(P, wrap/1, [(wrap(f(X)) :- item(X))], [Top|_]),
    Top = ('m$dual$wrap$1'(A) :- Body),
    assertion(Body == ('m$plain$shape$1'(A), 'm$dual.1$wrap$1'(A))).

%   The guard must constrain the head argument, not a fresh variable —
%   which is what collecting the guards with findall/3 would have given,
%   since it copies.

test(the_guard_shares_the_head_variable) :-
    wrap_program(P),
    duals(P, wrap/1, [(wrap(f(X)) :- item(X))], [Top|_]),
    Top = ('m$dual$wrap$1'(A) :- Guard, _),
    Guard = 'm$plain$shape$1'(G),
    assertion(G == A).

test(an_unannotated_predicate_has_no_guard) :-
    duals([], p/1, [p(0)], [Top|_]),
    Top = ('m$dual$p$1'(A) :- Body),
    assertion(Body == 'm$dual.1$p$1'(A)).

test(only_the_annotated_positions_are_guarded) :-
    Program = [ (:- domain d = [1]),
                (:- pred e(d, _)),
                e(1, x)
              ],
    duals(Program, e/2, [e(1, x)], [Top|_]),
    Top = ('m$dual$e$2'(A, B) :- Guard, Call),
    assertion(Guard == 'm$plain$d$1'(A)),
    assertion(Call == 'm$dual.1$e$2'(A, B)).

%   A body-only variable with a domain gets `for_domain/3' rather than
%   `forall/2': the domain is named in the term and what to do with it —
%   enumerate the facts, or post the region and label — is the solver's
%   business.  Expanding here would commit to enumeration, which is right
%   for [a,b] and wrong for 0..23.

domained_body_var([ (:- domain thing = [a, b]),
                    (:- pred t(_, thing)),
                    (p(X) :- q(X), not t(X, _)),
                    q(1),
                    t(1, a)
                  ]).

test(a_domained_body_variable_gets_for_domain) :-
    domained_body_var(P),
    duals(P, p/1, [(p(X) :- q(X), not t(X, _Y))], Duals),
    nth1(2, Duals, Clause),
    Clause = ('m$dual.1$p$1'(A) :- for_domain(B, Generator, Inner)),
    assertion(Generator == 'm$plain$thing$1'(B)),
    assertion(Inner == 'm$dual.1.inner$p$1'(A, B)).

%   So `forall/2' surviving into a compiled program means exactly one
%   thing: a quantifier nothing bounds, which is decision 5's undecidable
%   case.

test(and_no_forall_remains) :-
    domained_body_var(P),
    duals(P, p/1, [(p(X) :- q(X), not t(X, _Y))], Duals),
    assertion(\+ ( member(C, Duals), has_forall(C) )).

has_forall(Term) :-
    compound(Term),
    (   Term = forall(_, _)
    ->  true
    ;   arg(_, Term, Arg),
        has_forall(Arg)
    ).

test(an_undomained_body_variable_keeps_its_forall) :-
    duals([], p/1, [(p(X) :- q(X), not t(X, _Y))], Duals),
    nth1(2, Duals, Clause),
    Clause = ('m$dual.1$p$1'(_) :- Body),
    assertion(has_forall(Body)).

%   A range domain takes the same shape.  Nothing is enumerated at
%   compile time, which is the point: the generator posts the region.

test(a_range_domain_looks_the_same) :-
    Program = [ (:- domain hour = 0..23),
                (:- pred at(_, hour)),
                (busy(D) :- day(D), not at(D, _)),
                day(mon),
                at(mon, 9)
              ],
    duals(Program, busy/1, [(busy(D) :- day(D), not at(D, _H))], Duals),
    nth1(2, Duals, Clause),
    Clause = ('m$dual.1$busy$1'(_) :- for_domain(B, Generator, _)),
    assertion(Generator == 'm$plain$hour$1'(B)).

:- end_tests(dualise_domains).

		 /*******************************
		 *           EDGES		*
		 *******************************/

:- begin_tests(dualise_edges).

%   A clause with no body is unconditionally true, so it contributes no
%   dual bodies.  The clause dual is written out as `fail' rather than
%   left undefined, so the program says what it means instead of relying
%   on what an undefined predicate does.

test(a_universal_fact_dualises_to_fail) :-
    duals([], edge/2, [edge(_X, _Y)], Duals),
    Duals = [Top, Clause],
    Top = ('m$dual$edge$2'(A, B) :- Call),
    assertion(Call == 'm$dual.1$edge$2'(A, B)),
    Clause = ('m$dual.1$edge$2'(_, _) :- Body),
    assertion(Body == fail).

%   A predicate with no clauses at all is false of everything, so its
%   negation is true of everything: a dual with an empty body.

test(a_predicate_with_no_clauses) :-
    duals([], p/1, [], Duals),
    Duals = [Only],
    functor(Only, Name, Arity),
    assertion(Name == 'm$dual$p$1'),
    assertion(Arity == 1).

test(a_zero_arity_predicate) :-
    duals([], p/0, [(p :- q)], Duals),
    functor_names(Duals, Names),
    assertion(Names == ['m$dual$p$0', 'm$dual.1$p$0']).

test(several_clauses_are_numbered_in_order) :-
    duals([], p/1, [p(1), p(2), p(3)], Duals),
    functor_names(Duals, Names),
    assertion(Names == [ 'm$dual$p$1', 'm$dual.1$p$1',
                         'm$dual.2$p$1', 'm$dual.3$p$1' ]).

:- end_tests(dualise_edges).
