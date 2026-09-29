:- module(test_herbrand,
          [ test_herbrand/0
          ]).
:- use_module('../scasp_plunit', []).        % sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/herbrand)).
:- use_module(library(sscasp/run), [scasp/3]).
:- use_module(library(sscasp/render), [scasp_readable/2]).
:- use_module(library(plunit)).
:- use_module(library(dif), [dif/2]).
:- use_module(library(lists), [member/2, memberchk/2]).
:- use_module(library(debug), [assertion/1]).

/** <module> The Herbrand store

Two backings behind one interface: `dif/2` for a disequality between whole
terms, which is a disjunction, and `∉` for the ground values one variable
may not take.  What is worth testing is the **routing** — which of the two
gets a given constraint, and that the answer is the same either way.

`store/2` reads a variable's constraints with `copy_term/3`, which is the
only safe way to look at one: `dif:attribute_goals//1` deletes each or-node
as it reports it, and `copy_term/3` undoes that.
*/

test_herbrand :-
    run_tests([ herbrand_routing,
                herbrand_unification,
                herbrand_reading,
                herbrand_normalising,
                herbrand_language
              ]).

%!  store(+Term, -Goals) is det.

store(Term, Goals) :-
    copy_term(Term, _, Goals).

		 /*******************************
		 *         THE ROUTING		*
		 *******************************/

/** `X ∉ [a,b]` is always `dif(X,a), dif(X,b)`, and a `dif/2` is a
    prohibited value only when the two terms disagree in **one** place,
    that place being a variable against a ground term.  Anything else is
    a disjunction and no set of values per variable can hold one.
*/

:- begin_tests(herbrand_routing).

test(one_variable_against_a_ground_term_is_a_value) :-
    apart(p(X), p(a)),
    store(X, Goals),
    assertion(Goals = ['∉'(_, [a])]).

test(either_way_round) :-
    apart(p(a), p(X)),
    store(X, Goals),
    assertion(Goals = ['∉'(_, [a])]).

test(a_compound_disagreement_stays_a_disjunction) :-
    apart(q(X, Y), q(a, b)),
    store(f(X, Y), Goals),
    assertion(Goals = [dif(_, _)]).

test(and_two_variables_do_too) :-
    apart(p(X), p(Y)),
    store(f(X, Y), Goals),
    assertion(Goals = [dif(_, _)]).

%   The point of the exercise: a set is idempotent where a conjunction of
%   or-nodes is not.  `consistent/3` posts one disequality per proved
%   literal a goal might contradict, and several collapse to the same
%   thing once the answer has bound what they differ in.

test(the_same_value_twice_is_said_once) :-
    apart(p(X), p(a)),
    apart(p(X), p(a)),
    apart(q(X, 1), q(a, 1)),
    store(X, Goals),
    assertion(Goals = ['∉'(_, [a])]).

test(different_values_accumulate) :-
    apart(p(X), p(b)),
    apart(p(X), p(a)),
    store(X, Goals),
    assertion(Goals = ['∉'(_, [a, b])]).

test(both_backings_on_one_variable) :-
    apart(p(X), p(a)),
    apart(q(X, _Y), q(1, 2)),
    store(X, Goals),
    assertion(memberchk('∉'(_, [a]), Goals)),
    assertion(memberchk(dif(_, _), Goals)).

%   Two terms that cannot become the same need no constraint, and two
%   that already are cannot be kept apart.

test(what_cannot_unify_needs_nothing) :-
    apart(p(a), p(b)),
    assertion(true).

test(what_is_already_the_same_fails, [fail]) :-
    apart(p(a), p(a)).

test(a_non_ground_value_is_a_disjunction) :-
    apart(p(X), p(f(_))),
    store(X, Goals),
    assertion(Goals = [dif(_, _)]).

:- end_tests(herbrand_routing).

		 /*******************************
		 *        UNIFICATION		*
		 *******************************/

:- begin_tests(herbrand_unification).

test(a_prohibited_value_is_refused, [fail]) :-
    not_in(X, a),
    X = a.

test(and_anything_else_is_not) :-
    not_in(X, a),
    X = b,
    assertion(X == b).

test(two_sets_meeting_are_unioned) :-
    not_in(X, a),
    not_in(Y, b),
    X = Y,
    store(X, Goals),
    assertion(Goals = ['∉'(_, [a, b])]).

%   A set meeting a `dif/2` keeps both: they are independent conjuncts
%   and nothing has to reconcile them.  Which of the two SWI binds to the
%   other is not ours to choose, so both directions are checked.

test(a_set_meeting_a_disjunction_keeps_both,
     [ forall(member(Order, [ours_first, theirs_first])) ]) :-
    not_in(X, a),
    dif(Y, b),
    (   Order == ours_first
    ->  X = Y
    ;   Y = X
    ),
    store(X, Goals),
    assertion(memberchk('∉'(_, [a]), Goals)),
    assertion(memberchk(dif(_, b), Goals)),
    assertion(X \= a),
    assertion(X \= b),
    assertion(\+ X \= c).

%   The case the hook exists for.  A variable bound to a term that is
%   **not ground** cannot keep a set of prohibited values, because
%   whether it takes one of them depends on what the term's own variables
%   become.  So the set becomes that many `dif/2`s.

test(a_set_handed_to_a_non_ground_term) :-
    not_in(X, f(a)),
    X = f(Y),
    store(X, Goals),
    assertion(Goals = [dif(_, _)]),
    assertion(Y \= a),
    assertion(\+ Y \= b).

:- end_tests(herbrand_unification).

		 /*******************************
		 *          READING		*
		 *******************************/

:- begin_tests(herbrand_reading).

%   One vocabulary out, whichever backing holds it — and `apart/2`
%   rather than `dif/2`, because the same term is what a caller posts to
%   say it again, which is how the quantifier builds a complement.

test(both_backings_read_as_one_vocabulary) :-
    apart(p(X), p(a)),
    apart(q(X, _), q(1, 2)),
    \+ \+ ( read_store([X], Goals),
            assertion(memberchk(apart(_, a), Goals)),
            assertion(( member(apart(A, B), Goals),
                        compound(A),
                        compound(B)
                      ))
          ).

%   `library(dif)` reports its or-nodes in a normal form of its own, so
%   what comes back for a compound disequality is the same relation and
%   not necessarily the same term.  The reader is asking what the answer
%   says, and `descriptions/4` takes it apart from either end.

test(a_set_reads_as_one_goal_per_value) :-
    apart(p(X), p(a)),
    apart(p(X), p(b)),
    \+ \+ ( read_store([X], Goals),
            assertion(Goals = [apart(_, a), apart(_, b)])
          ).

%   Reading the `dif/2` half **is** destructive — that is
%   `dif:attribute_goals//1`'s doing — so it is asked for inside a double
%   negation and the constraint survives.

test(reading_leaves_the_answer_alone) :-
    apart(p(X), p(a)),
    apart(q(X, _), q(1, 2)),
    \+ \+ read_store([X], _),
    assertion(X \= a),
    store(X, Goals),
    assertion(memberchk(dif(_, _), Goals)).

test(the_ground_values_come_from_both) :-
    apart(p(X), p(a)),
    apart(p(X), p(b)),
    dif(X, c),
    excluded_values(X, Values),
    assertion(msort(Values, [a, b, c])).

test(the_size_counts_both) :-
    apart(p(X), p(a)),
    node_count(X, One),
    apart(p(X), p(b)),
    node_count(X, Two),
    apart(q(X, _), q(1, 2)),
    node_count(X, Three),
    assertion(One < Two),
    assertion(Two < Three).

test(an_unconstrained_variable_says_nothing) :-
    assertion(\+ constrained(_)),
    assertion(\+ constrained(a)),
    apart(p(X), p(a)),
    assertion(constrained(X)).



:- end_tests(herbrand_reading).

		 /*******************************
		 *        NORMALISING		*
		 *******************************/

/** A disequality between two **variables** is `dif/2`'s — neither side is
    ground, so there is no value to prohibit.  Once one of them is bound
    there is, and `library(dif)` simplifies internally without telling
    anyone, so nothing in the unification hook can see it happen.
*/

:- begin_tests(herbrand_normalising).

test(a_collapsed_disjunction_becomes_a_value) :-
    apart(p(X), p(Y)),
    Y = a,
    store(X, Before),
    assertion(Before = [dif(_, a)]),
    normalise(X),
    store(X, After),
    assertion(After = ['∉'(_, [a])]).

test(and_joins_what_is_already_there) :-
    apart(p(X), p(b)),
    apart(p(X), p(Y)),
    Y = a,
    normalise(X),
    store(X, Goals),
    assertion(Goals = ['∉'(_, [a, b])]).

test(what_has_not_collapsed_stays_where_it_is) :-
    apart(q(X, _), q(1, 2)),
    normalise(X),
    store(X, Goals),
    assertion(Goals = [dif(_, _)]).

test(and_it_still_constrains) :-
    apart(p(X), p(Y)),
    Y = a,
    normalise(X),
    assertion(X \= a),
    assertion(\+ X \= b).

:- end_tests(herbrand_normalising).

		 /*******************************
		 *        THE LANGUAGE		*
		 *******************************/

/** `X ∉ [a, b]` is what a program writes, and `∈` is what it dualises to
    — a dual has to be something to *run*, and the negation of "not one of
    these" is a generator.
*/

:- begin_tests(herbrand_language).

program([ (ok(X)  :- X ∉ [a, b]),
          (sel(X) :- X ∈ [a, b, c])
        ]).

test(a_ground_value_outside_the_set) :-
    program(P),
    assertion(scasp(P, ok(c), _)).

test(and_one_inside_it) :-
    program(P),
    assertion(\+ scasp(P, ok(a), _)).

test(the_negation_enumerates_the_set, set(X == [a, b])) :-
    program(P),
    scasp(P, not ok(X), _).

%   Unbound, the answer is the constraint itself — which is the whole
%   point of a store that constrains rather than tests.

test(unbound_it_constrains_the_answer) :-
    program(P),
    scasp(P, ok(X), _),
    !,
    scasp_readable(X, Readable),
    assertion(Readable = '| '(_, {'∉'(_, [a, b])})).

test(the_generator_generates, set(X == [a, b, c])) :-
    program(P),
    scasp(P, sel(X), _).

test(and_its_negation_is_the_constraint) :-
    program(P),
    assertion(scasp(P, not sel(d), _)),
    assertion(\+ scasp(P, not sel(a), _)).

%   A set of one reads as the disequality the program wrote; from two up
%   the set is the shorter statement and keeps its own spelling.

test(a_set_of_one_reads_as_a_disequality) :-
    scasp([(p(X) :- X \= a)], p(V), _),
    !,
    scasp_readable(V, Readable),
    assertion(Readable = '| '(_, {_ \= a})).

:- end_tests(herbrand_language).
