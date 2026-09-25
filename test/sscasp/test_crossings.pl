:- module(test_crossings,
          [ test_crossings/0
          ]).
:- use_module('../scasp_plunit', []).        % sets the library search path
:- use_module(oracle_agreement, [agrees_with_oracle/1]).
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/run)).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(lists), [member/2, memberchk/2]).
:- use_module(library(apply), [maplist/2]).
:- use_module(library(clpq), [{}/1]).   % a store is called, not only read
:- use_module(library(sscasp/herbrand), [('∉')/2]).  % ... and so is this one

/** <module> Language features crossed with each other

`doc/test-strategy.md` calls this Gap 1: the suites are one feature per file,
so feature *crossings* fall between them.  The golden corpus does exercise
them — a real program uses several features at once — but a golden failure
says *"queens.pl changed"*, not *"a disequality inside a quantifier over a
compound broke"*.  Regression detection without localisation, which during a
rewrite is most of the debugging cost.

Two kinds of test here, and the difference matters:

  - Where the program is in the oracle's fragment — finitely groundable, safe
    rules, arithmetic over numbers — the test asks
    `oracle_agreement.pl:agrees_with_oracle/1`, which compares **every**
    literal against the Gelfond-Lifschitz definition.  Nothing is pinned;
    these say what any correct implementation must do, and they name the
    crossing that broke.
  - Where it is not — an unbound answer, a dense domain, a constraint store in
    the answer, `findall/3`, `#abducible`, a declared domain — the expectation
    is written out, with the reasoning beside it.

The crossings are the ones `doc/test-strategy.md` counted at **zero** tests,
plus the ones this implementation's own architecture makes interesting: the
quantifier is where constraints, compound terms and negation all meet, and
`solve.pl:between_kept/6` exists because of `a_dense_region_under_a_compound_head`
below, which found the solver claiming a domain it had not covered.
*/

test_crossings :-
    run_tests([ crossings_against_the_definition,
                crossings_beyond_the_definition
              ]).

answers(Program, Query) :-
    \+ \+ scasp(Program, Query, _).

%!  answer_stores(+Program, +Query, +Variable, -Answers) is det.
%
%   One `Copy-Store` per answer: the residual goals `copy_term/3` reports
%   for Variable, together with the copy they are about.  Keeping the copy
%   is the point — a store is a list of goals over a variable, and without
%   it there is nothing to ask them about.

answer_stores(Program, Query, Variable, Answers) :-
    findall(Copy-Store,
            ( scasp(Program, Query, _),
              copy_term(Variable, Copy, Store)
            ),
            Answers).

%!  admits(+Answers, +Value) is semidet.
%
%   True when some answer's store allows Value: posting the goals against
%   it is what asking means.

admits(Answers, Value) :-
    member(Copy-Store, Answers),
    \+ \+ ( copy_term(Copy-Store, Value-Goals),
             maplist(call, Goals) ),
    !.

		 /*******************************
		 *     JUDGED BY THE ORACLE	*
		 *******************************/

/** Crossings the definition can settle

Each `crossing/2` is a whole program; the test asks the oracle about every
literal in it, positive and negated.  A failure prints the literal and which
side answered it, so the test names the crossing *and* the case.

Two of these need a word.  `olon_clpq` has **no** answer set — `p(2) :- 2 #> 1,
not p(2)` is an odd loop with no way out, and a program whose only candidate is
eliminated has none — which is as much a fact about the crossing as a program
with three.  `olon_kills_a_model` is the interesting shape: the odd loop is
reachable only when `q(2)` holds, so it eliminates one of the two candidate
models and leaves the other.
*/

:- begin_tests(crossings_against_the_definition).

%   OLON x CLP(Q).  `doc/test-strategy.md`: 0 tests.

crossing(olon_clpq,
         [ d(1), d(2),
           (p(X) :- d(X), X #> 1, not p(X)) ]).

crossing(olon_kills_a_model,
         [ d(1), d(2),
           (p :- not p, q(X), d(X), X #> 1),
           (q(X) :- d(X), not r(X)),
           (r(X) :- d(X), not q(X)) ]).

crossing(olon_compound,
         [ d(a),
           (p(f(X)) :- d(X), not p(f(X))) ]).

crossing(olon_classical,
         [ d(1),
           (-(p(X)) :- d(X), not -(p(X))) ]).

crossing(classical_olon_clpq,
         [ d(2),
           (-(p(X)) :- d(X), X #> 1, not -(p(X))) ]).

%   Coinduction (an even loop) x CLP(Q).  `doc/test-strategy.md`: 0 tests.

crossing(evenloop_clpq,
         [ d(1), d(2),
           (p(X) :- d(X), X #> 1, not q(X)),
           (q(X) :- d(X), X #> 1, not p(X)) ]).

crossing(evenloop_compound,
         [ d(1),
           (p(f(X)) :- d(X), not q(f(X))),
           (q(f(X)) :- d(X), not p(f(X))) ]).

crossing(evenloop_classical,
         [ d(1),
           (p(X) :- d(X), not -(p(X))),
           (-(p(X)) :- d(X), not p(X)) ]).

%   Three predicates on one cycle, each guarded by a different comparison,
%   so that which of them is on the loop depends on the arithmetic.

crossing(clpq_three_way,
         [ d(1), d(2), d(3),
           (p(X) :- d(X), X #> 1, not q(X)),
           (q(X) :- d(X), X #< 3, not r(X)),
           (r(X) :- d(X), X #= 2, not p(X)) ]).

%   Compound terms x CLP(Q).  `doc/test-strategy.md`: 0 tests.

crossing(compound_clpq,
         [ d(1), d(3),
           (p(f(X)) :- d(X), X #< 2),
           (q :- p(f(1))),
           (r :- p(f(3))) ]).

crossing(compound_negation_clpq,
         [ d(1), d(2),
           (p(f(X)) :- d(X), X #> 1),
           (s :- not p(f(1))),
           (t :- not p(f(2))) ]).

%   Classical negation x CLP(Q), consistent and not.  The second has no
%   answer set at all: `p(2)` and `-p(2)` are both derivable, and that is a
%   constraint the *language* imposes rather than the program.

crossing(classical_clpq,
         [ d(1), d(3),
           (p(X) :- d(X), X #< 2),
           (-(p(X)) :- d(X), X #> 2) ]).

crossing(classical_clash_clpq,
         [ d(2),
           (p(X) :- d(X), X #> 1),
           (-(p(X)) :- d(X), X #> 1) ]).

%   The CLP(Q) disequality, which is a different store from `dif/2` and a
%   different dual.

crossing(clpq_disequality,
         [ d(1), d(2),
           (p(X) :- d(X), X #<> 1) ]).

%   A global constraint x CLP(Q), and x a quantifier: the second constraint
%   has a body-only variable, so checking it is a `forall`.

crossing(constraint_clpq,
         [ d(1), d(3),
           (p(X) :- d(X), not q(X)),
           (q(X) :- d(X), not p(X)),
           (:- p(X), X #> 2) ]).

crossing(constraint_forall,
         [ d(1), d(2),
           (p(X) :- d(X), not q(X)),
           (q(X) :- d(X), not p(X)),
           (:- p(X), d(X), d(Y), Y #> X) ]).

%   A body-only variable under a comparison is a quantifier in the dual, so
%   these are `forall` x CLP(Q) with the oracle as referee rather than a
%   hand-written expectation.

crossing(forall_clpq,
         [ d(1), d(2),
           (p(X) :- d(X), d(Y), Y #> X),
           (s :- not p(2)),
           (t :- not p(1)) ]).

crossing(classical_forall_clpq,
         [ d(1), d(2),
           (p(X) :- d(X), d(Y), Y #>= X),
           (-(p(X)) :- d(X), X #> 1) ]).

crossing(classical_constraint_clpq,
         [ d(1), d(2),
           (p(X) :- d(X), not -(p(X))),
           (-(p(X)) :- d(X), not p(X)),
           (:- p(X), d(X), X #> 1) ]).

test(agrees, [forall(crossing(_Name, Program))]) :-
    agrees_with_oracle(Program).

:- end_tests(crossings_against_the_definition).

		 /*******************************
		 *   BEYOND THE DEFINITION	*
		 *******************************/

/** Crossings the oracle cannot settle

Unbound answers, dense domains, constraint stores in the answer, and the three
constructs the oracle does not model — `findall/3`, `#abducible` and a declared
domain.  Here the expectation is written out and the reasoning with it.
*/

:- begin_tests(crossings_beyond_the_definition).

%   ## A dense region under a compound head
%
%   This is the one that found a defect.  `p(f(X)) :- X #> 3, X #< 10`
%   dualises through head abstraction to
%   `not p(A) :- forall(X, A \= f(X))`, and asked for `not p(f(V))` that
%   quantifier's answer leaves `dif(V, X)` — its region is everything but
%   the single point `X = V`.
%
%   Read as "no constraint left" that says the answer covered the whole
%   domain, and `not p(f(V))` came back as one answer constraining nothing
%   while `not p(f(5))` correctly failed: the general answer contradicting
%   the specific one.  `solve.pl:between_kept/6` describes the pair instead,
%   the point gets covered, and the two regions fall out of covering it.
%
%   The old implementation answers the same two regions.  What it gets wrong
%   on this shape is the *unbound* case of `doc/bugs.md` §15, which is
%   incompleteness; this was unsoundness.

dense_compound([ (p(f(X)) :- X #> 3, X #< 10) ]).

test(a_dense_region_under_a_compound_head) :-
    dense_compound(Program),
    answer_stores(Program, not p(f(V)), V, Answers),
    assertion(Answers = [_, _]),
    assertion(admits(Answers, 2)),
    assertion(admits(Answers, 12)),
    assertion(\+ admits(Answers, 5)).

test(and_agrees_with_itself_on_the_points) :-
    dense_compound(Program),
    assertion(answers(Program, p(f(5)))),
    assertion(\+ answers(Program, not p(f(5)))),
    assertion(answers(Program, not p(f(2)))),
    assertion(answers(Program, not p(f(12)))).

%   The same shape with no arithmetic at all, so that a regression in the
%   pair description shows up without CLP(Q) in the way.

test(a_compound_head_over_a_plain_body) :-
    Program = [ (q(f(X)) :- r(X)), r(1) ],
    assertion(answers(Program, q(f(1)))),
    assertion(\+ answers(Program, not q(f(1)))),
    assertion(answers(Program, not q(f(2)))),
    answer_stores(Program, not q(f(V)), V, Answers),
    assertion(\+ admits(Answers, 1)),
    assertion(admits(Answers, 2)).

%   ## Herbrand disequality x CLP(Q)
%
%   `doc/test-strategy.md`: 0 tests.  The two stores are separate — `dif/2`
%   for terms and `library(clpq)` for the dense order — and an answer may
%   carry both at once.  Out of the oracle's reach because it has no
%   Herbrand disequality.

test(the_two_stores_constrain_one_answer) :-
    Program = [ (p(X) :- X \= a, X #> 1) ],
    assertion(answers(Program, p(2))),
    assertion(\+ answers(Program, p(a))),
    answer_stores(Program, p(V), V, [_-Store]),
    assertion(memberchk('∉'(_, [a]), Store)),
    assertion(memberchk({_ > 1}, Store)).

%   ## `findall/3` x CLP(Q)
%
%   `findall/3` solves its goal rather than calling it, so the comparison in
%   the collected goal's body has to run inside the collection.

test(a_collection_over_a_constrained_goal) :-
    Program = [ d(1), d(2), d(3),
                (big(X) :- d(X), X #> 1),
                (all(L) :- findall(Y, big(Y), L)) ],
    findall(L, scasp(Program, all(L), _), Ls),
    assertion(Ls == [[2, 3]]).

%   ## `#abducible` x classical negation
%
%   An abducible gets no `:- p, -p` sub-check of its own — proving one means
%   proving `not -p(X)` for a quantified X, which decides the abducible for
%   every individual at once — so the crossing is worth pinning: assuming the
%   abducible must still carry the classically negated literal it supports.

test(an_abducible_supports_a_classically_negated_literal) :-
    Program = [ (:- abducible(a(1))),
                (p :- a(1)),
                (-(q) :- a(1)) ],
    assertion(answers(Program, p)),
    assertion(answers(Program, -(q))),
    assertion(answers(Program, not p)).

%   ## A declared domain x CLP(Q)
%
%   The domain makes the quantifier an enumeration and the comparison filters
%   it, so both halves have to work on the same variable.

test(a_domain_filtered_by_a_comparison) :-
    Program = [ (:- domain t = [1, 2, 3]),
                (p(X) :- t(X), X #> 1) ],
    findall(X, scasp(Program, p(X), _), Xs),
    assertion(Xs == [2, 3]),
    assertion(answers(Program, not p(1))).

%   ## A quantifier over a compound, answering unbound
%
%   Out of the oracle's reach for a different reason: `q(f(1, 2))` puts a
%   compound among the program's constants and naive grounding then squares.

test(a_quantifier_over_a_compound_argument) :-
    Program = [ d(1), d(2),
                (p(X) :- d(X), q(f(X, Y)), d(Y)),
                q(f(1, 2)) ],
    findall(X, scasp(Program, p(X), _), Xs),
    assertion(Xs == [1]),
    assertion(\+ answers(Program, not p(1))),
    assertion(answers(Program, not p(2))).

:- end_tests(crossings_beyond_the_definition).
