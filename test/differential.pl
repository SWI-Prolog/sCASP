:- module(scasp_differential,
          [ scasp_brave/2,              % +Program, +Literal
            disagreements/2,            % +Program, -Disagreements
            agree/1,                    % +Program
            random_program/3,           % +NAtoms, +NConstraints, -Program
            random_program/4            % +NAtoms, +NConstraints, +Options, -Program
          ]).
:- use_module(stable_models, [stable_models/2, models_entail/2,
                              program_literals/2, op(900, fy, not),
                              op(700, xfx, [#=, #<>, #<, #>, #=<, #>=])]).
:- use_module(scasp_plunit, []).
:- use_module(under_test, [scasp/2, scasp_assert/1]).
:- use_module(library(lists), [member/2, numlist/3]).
:- use_module(library(apply), [maplist/3, include/3, convlist/3]).
:- use_module(library(random), [random_between/3, random_member/2]).
:- use_module(library(option), [option/3]).

/** <module> s(CASP) against the definition

Runs a propositional program through s(CASP) and through the brute-force
oracle in `stable_models.pl`, and reports where they disagree.

The comparison is **brave entailment**, which is what a goal-directed solver
computes: a query succeeds when *some* answer set supports it.  So for every
atom `a` of the program, both of these must hold:

    scasp succeeds on a        <=>  a is in some answer set
    scasp succeeds on not a    <=>  a is absent from some answer set

That is sharp in both directions.  It catches unsoundness (s(CASP) answers
where no answer set supports it) and incompleteness (an answer set exists
that s(CASP) cannot find), which counting models would not: a solver that
found the right number of wrong models would pass a count.

Each program is asserted into a module of its own with `scasp_assert/1` and
queried with `scasp/2`.  Nothing is shared between calls, so two may run
concurrently -- which is why the tests are free to sit in three plunit units.
They were forced into one while this went through `scasp_load/2`, which
compiles into its caller's module and wipes what was there (`doc/bugs.md` #5).

`scasp_assert/1` rather than `assertz/1`, because it understands the two
shapes a program written for the oracle uses: `:- Body` for a global
constraint, which it rewrites to the internal headless form, and `-p` for
classical negation, which it interns.  Plain `assertz/1` raises on the first --
`false/0` is a static system predicate -- and would leave the second as an
ordinary `-`/1 term.

## Scope

`scasp/2` is *relevance-scoped*: it collects the clauses reachable from the
query plus the global constraints overlapping them, and solves that.  The
oracle computes answer sets of the whole program.  Comparing the two directly
would be comparing different questions, and the gap is not subtle:

    [p, -p, q]   ?- q.   whole program: no answer set, so q is false
                         scoped:        q is true, the contradiction is
                                        unreachable from q

The same holds for an odd loop elsewhere, and for a constraint mentioning
nothing the query reaches.  `whole_program/2` therefore names every predicate
the program defines and passes it to `scasp/2`'s `collect` option, which
widens the collection without widening the query.
*/

%!  scasp_brave(+Program, +Literal) is semidet.
%
%   True when s(CASP) answers Literal against Program.  Program is a list
%   of terms as `stable_models.pl` takes them; Literal is an atom or
%   `not Atom`.
%
%   Errors are *not* caught.  Since doc/bugs.md #17 they propagate rather
%   than turning into failure, and a differential run that silently read
%   an error as "no" would be measuring the wrong thing.

scasp_brave(Program, Literal) :-
    whole_program(Program, Collect),
    in_temporary_module(
        Module,
        assert_program(Module, Program),
        \+ \+ scasp(Module:Literal, [collect(Collect)])).

assert_program(Module, Program) :-
    forall(member(Clause, Program),
           scasp_assert(Module:Clause)).

whole_program(Program, Goal) :-
    convlist(head_goal, Program, Goals0),
    sort(Goals0, Goals),
    (   Goals == []
    ->  Goal = true
    ;   conjunction(Goals, Goal)
    ).

head_goal((:- _), _) :- !, fail.
head_goal((Head :- _), Goal) :- !, most_general(Head, Goal).
head_goal(Fact, Goal) :- most_general(Fact, Goal).

most_general(-(Head), -(Goal)) :- !, functor(Head, N, A), functor(Goal, N, A).
most_general(Head, Goal) :- functor(Head, N, A), functor(Goal, N, A).

%!  disagreements(+Program, -Disagreements) is det.
%
%   Disagreements is a list of `Literal-scasp(Bool)-oracle(Bool)` for
%   every literal the two do not agree on.  `[]` means they agree
%   everywhere, which is the result to want.

disagreements(Program, Disagreements) :-
    program_literals(Program, Atoms),
    stable_models(Program, Models),     % once, not once per literal:
    findall(Literal-scasp(S)-oracle(O), % enumerating them is exponential
            ( member(Atom, Atoms),
              ( Literal = Atom ; Literal = not(Atom) ),
              ( scasp_brave(Program, Literal) -> S = true ; S = false ),
              ( models_entail(Models, Literal) -> O = true ; O = false ),
              S \== O
            ),
            Disagreements).

%!  agree(+Program) is semidet.
%
%   True when s(CASP) and the oracle agree on every literal.  Prints the
%   disagreements when they do not, so a failing test says what differs
%   rather than merely that something does.

agree(Program) :-
    disagreements(Program, Disagreements),
    (   Disagreements == []
    ->  true
    ;   print_message(error,
                      format("s(CASP) and the stable-model definition differ", [])),
        print_message(error, format("  program:       ~q", [Program])),
        print_message(error, format("  disagreements: ~q", [Disagreements])),
        fail
    ).


		 /*******************************
		 *        RANDOM PROGRAMS	*
		 *******************************/

%!  random_program(+NAtoms, +NConstraints, -Program) is det.
%!  random_program(+NAtoms, +NConstraints, +Options, -Program) is det.
%
%   A random propositional program over NAtoms atoms named `a`, `b`, ...,
%   plus NConstraints constraints.  Seed with set_random/1 so a failure
%   is reproducible from the seed alone.  Options:
%
%     - first_order(Constants)
%       Generate predicates of arity 1 over Constants rather than
%       propositions.  A domain predicate `dom/1` is emitted as facts and
%       put at the front of every rule body, which keeps every rule safe:
%       s(CASP) reads an unsafe rule over an extended Herbrand universe,
%       so a finite grounding would disagree with it by design rather
%       than by defect (see stable_models.pl).
%
%     - clp(Boolean)
%       Add CLP(Q) comparisons to some rule bodies, over the numeric
%       constants of first_order/1.  Requires first_order/1 with numeric
%       constants; it is what brings the *duals* of constraints into
%       range, since `not p(X)` then has to answer for the values the
%       constraint excludes.
%
%     - classical(Boolean)
%       Use classical negation, so a head may be `-a` as well as `a`.
%       Each atom independently gets one polarity or both; where it gets
%       both, the program can derive `a` and `-a` together and so have no
%       answer set at all, which is the case worth generating.
%
%   Every literal used in a body is also the head of some rule.  That is
%   not a limitation of the oracle but a way of steering clear of
%   doc/bugs.md #17 - querying a literal with no rules raises rather than
%   answering - which would otherwise be the only thing these ever found.
%   Drop the guarantee once that is fixed and body-only literals become
%   interesting again.
%
%   Bodies are short on purpose.  What is being probed is the interaction
%   of negation, loops and constraints, and those show up in two- and
%   three-literal bodies; longer ones mostly make the program
%   unsatisfiable.

random_program(NAtoms, NConstraints, Program) :-
    random_program(NAtoms, NConstraints, [], Program).

random_program(NAtoms, NConstraints, Options, Program) :-
    option(first_order(Constants), Options, []),
    Constants \== [],
    !,
    option(classical(Classical), Options, false),
    option(clp(Clp), Options, false),
    head_literals(NAtoms, Classical, Preds),
    maplist(dom_fact, Constants, Facts),
    findall(Rule,
            ( member(Pred, Preds),
              random_between(1, 2, NRules),
              between(1, NRules, _),
              fo_rule(Pred, Preds, Clp, Constants, Rule)
            ),
            Rules),
    findall(Constraint,
            ( between(1, NConstraints, _),
              fo_constraint(Preds, Constants, Constraint)
            ),
            Constraints),
    append([Facts, Rules, Constraints], Program).
random_program(NAtoms, NConstraints, Options, Program) :-
    option(classical(Classical), Options, false),
    head_literals(NAtoms, Classical, Literals),
    findall(Rule,
            ( member(Head, Literals),
              random_between(1, 2, NRules),
              between(1, NRules, _),
              random_rule(Head, Literals, Rule)
            ),
            Rules),
    findall(Constraint,
            ( between(1, NConstraints, _),
              random_constraint(Literals, Constraint)
            ),
            Constraints),
    append(Rules, Constraints, Program).

dom_fact(C, dom(C)).

%!  fo_rule(+Pred, +Preds, -Rule) is det.
%
%   `p(X) :- dom(X), <literals over X>.`  The dom/1 goal is what makes
%   the rule safe, and it is always first so the body reads the way a
%   person would write it.

fo_rule(Pred, Preds, Clp, Constants, (Head :- Body)) :-
    random_between(0, 2, N),
    findall(P-Sign,                     % names and signs only: findall
            ( between(1, N, _),         % copies, so literals built inside
              random_member(P, Preds),  % it would not share the head's
              random_between(0, 1, Sign)% variable
            ),
            Choices),
    random_constraint_spec(Clp, Constants, Spec),
    apply_pred(Pred, X, Head),
    maplist(fo_literal(X), Choices, Rest),
    maplist(clp_goal(X), Spec, Clps),
    append([dom(X)|Rest], Clps, Literals),
    conjunction(Literals, Body).

%!  random_constraint_spec(+Clp, +Constants, -Spec) is det.
%
%   Zero or one comparison, as an Op-Value pair so that findall/3 cannot
%   copy the variable away.  One is enough: two over a domain of three
%   values mostly leaves the rule with no instances at all.

random_constraint_spec(false, _, []) :- !.
random_constraint_spec(true, Constants, Spec) :-
    include(number, Constants, Numbers),
    (   Numbers == []
    ->  Spec = []
    ;   random_between(0, 1, Add),
        (   Add =:= 0
        ->  Spec = []
        ;   random_member(Op, [#=, #<>, #<, #>, #=<, #>=]),
            random_member(Value, Numbers),
            Spec = [Op-Value]
        )
    ).

clp_goal(X, Op-Value, Goal) :-
    Goal =.. [Op, X, Value].

fo_literal(X, Pred-Sign, Literal) :-
    apply_pred(Pred, X, Atom),
    (   Sign =:= 0
    ->  Literal = Atom
    ;   Literal = not(Atom)
    ).

%!  fo_constraint(+Preds, +Constants, -Constraint) is det.
%
%   A ground constraint, so that it constrains rather than quantifying.

fo_constraint(Preds, Constants, (:- Literal)) :-
    random_member(Pred, Preds),
    random_member(Constant, Constants),
    apply_pred(Pred, Constant, Atom),
    random_between(0, 1, Sign),
    (   Sign =:= 0
    ->  Literal = Atom
    ;   Literal = not(Atom)
    ).

%!  apply_pred(+NameOrNegated, ?Arg, -Literal) is det.
%
%   `a` and `Arg` give `a(Arg)`; `-a` gives `-a(Arg)`, keeping the
%   classical negation outermost where s(CASP) expects it.

apply_pred(-Name, Arg, -Literal) :-
    !,
    Literal =.. [Name, Arg].
apply_pred(Name, Arg, Literal) :-
    Literal =.. [Name, Arg].

%!  head_literals(+NAtoms, +Classical, -Literals) is det.
%
%   The literals that will have rules.  Without classical negation this
%   is just the atoms; with it, each atom contributes `a`, or `-a`, or
%   both.

head_literals(NAtoms, Classical, Literals) :-
    numlist(1, NAtoms, Ns),
    maplist(nth_atom, Ns, Atoms),
    findall(L,
            ( member(A, Atoms),
              polarity(Classical, A, L)
            ),
            Literals).

polarity(false, A, A) :- !.
polarity(true, A, L) :-
    random_between(0, 5, Choice),
    (   Choice =< 2
    ->  L = A
    ;   Choice =< 4
    ->  L = -A
    ;   ( L = A ; L = -A )              % both: a contradiction is possible
    ).
%   One atom in six gets both polarities.  Giving every atom both makes a
%   contradiction near-certain, and a program with no answer set agrees
%   with anything - two thirds of the sample said nothing at that rate.

nth_atom(N, Atom) :-
    Code is 0'a + N - 1,
    char_code(Atom, Code).

random_rule(Head, Literals, Rule) :-
    random_body(Literals, Body),
    (   Body == true
    ->  Rule = Head
    ;   Rule = (Head :- Body)
    ).

random_constraint(Literals, (:- Body)) :-
    random_body(Literals, Body0),
    (   Body0 == true
    ->  random_literal(Literals, Body)  % a constraint needs a body
    ;   Body = Body0
    ).

random_body(Literals, Body) :-
    random_between(0, 2, N),
    findall(L, (between(1, N, _), random_literal(Literals, L)), Chosen),
    (   Chosen == []
    ->  Body = true
    ;   conjunction(Chosen, Body)
    ).

%!  conjunction(+Literals, -Body) is det.
%
%   Right-nested, the way a person writes a body.

conjunction([L], L) :- !.
conjunction([L|T], (L, Body)) :-
    conjunction(T, Body).

random_literal(Literals, Literal) :-
    random_member(Base, Literals),
    random_between(0, 1, Sign),
    (   Sign =:= 0
    ->  Literal = Base
    ;   Literal = not(Base)
    ).
