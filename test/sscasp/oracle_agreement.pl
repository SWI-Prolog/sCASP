:- module(oracle_agreement,
          [ agrees_with_oracle/1,       % +Program
            disagreement/3              % +Program, -Literal, -Which
          ]).
:- use_module('../scasp_plunit', []).        % sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/run), [scasp/3]).
:- use_module('../stable_models', [stable_models/2, models_entail/2,
                              program_literals/2]).
:- use_module(library(lists), [member/2]).

/** <module> Does the solver answer what the definition says?

One comparison, used by two suites, because it is the only check in this
project that could catch a defect the design has always had.  Every other
oracle here descends from the implementation it judges — `.pass` data records
what one solver produces, and the sibling checkout is the same codebase —
while `stable_models.pl` is written from the Gelfond-Lifschitz definition and
shares no decision with either.

What is compared is **brave entailment**, which is what a goal-directed solver
computes: it answers `a` iff `a` is in some answer set, and `not a` iff `a` is
absent from some answer set.  That is sharp in both directions — it catches an
answer no answer set supports *and* an answer set the solver cannot reach.
Counting models would not: two solvers can agree on how many there are and
disagree about what is in them.

Every literal of the program is asked, positive and negated, so a program that
is only *partly* wrong still reports.

The fragment is the oracle's: finitely groundable, with CLP(Q) comparisons
evaluated while grounding, and **unsafe rules refused**.  That last is about
s(CASP) rather than about the oracle — s(CASP) reads an unsafe rule over an
*extended* Herbrand universe, so `:- not married(X).` means "everyone is
married" and a finite grounding would disagree by design.  A crossing that
needs an unbound answer, a dense domain or a constraint store in the answer is
outside it, and `test_crossings.pl` says so where it asserts by hand instead.
*/

%!  agrees_with_oracle(+Program) is semidet.
%
%   True when the solver and the definition give the same verdict on every
%   literal of Program.  On a difference it prints which literal and which
%   way round before failing, because a crossing test that only says "failed"
%   costs as much to debug as no test at all.

agrees_with_oracle(Program) :-
    \+ disagreement(Program, _, _).

%!  disagreement(+Program, -Literal, -Which) is nondet.
%
%   Literal is one the two do not agree on; Which is `solver` when the solver
%   answers it and the oracle does not, `oracle` the other way round.

disagreement(Program, Literal, Which) :-
    stable_models(Program, Models),
    program_literals(Program, Atoms),
    member(Atom, Atoms),
    (   Literal = Atom
    ;   Literal = not(Atom)
    ),
    (   \+ \+ scasp(Program, Literal, _)
    ->  Solver = yes
    ;   Solver = no
    ),
    (   models_entail(Models, Literal)
    ->  Oracle = yes
    ;   Oracle = no
    ),
    Solver \== Oracle,
    (   Solver == yes
    ->  Which = solver
    ;   Which = oracle
    ),
    print_message(error,
                  format("~p: ~p  solver=~w  oracle=~w",
                         [Program, Literal, Solver, Oracle])).
