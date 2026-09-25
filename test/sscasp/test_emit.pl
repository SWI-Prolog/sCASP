:- module(test_emit,
          [ test_emit/0
          ]).
:- use_module('../scasp_plunit', []).        % sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/emit)).
:- use_module(library(sscasp/compile), [compile_program/3]).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(lists), [member/2, memberchk/2, append/3]).

/** <module> Making the compiled program runnable

`sscasp/emit.pl` is the only place that knows how the solver carries its
state.  It turns the program `compile.pl` produces as *logic* into clauses
Prolog can run: three arguments for the ancestor chain and the proof, and a
wrapper per literal so that the coinductive check happens before the clauses
are tried.

Keeping it a pass of its own is what lets the tests above it read the logical
form and these read the threading, rather than every test in the project
having to know both.

## Two kinds of module qualification, and only one of them is written

A goal this pass writes **into a clause body** — `check/7`, `forall/5`,
`apart/2` — is written bare, because `run.pl:load_program/2` imports
`sscasp_solve`, `sscasp_herbrand` and `clpq` into the module before asserting
and the name resolves there.  A goal a quantifier **holds as a term** keeps
its `M:`, because nothing resolves it until the runtime calls it and a
`meta_predicate` declaration cannot help: it applies when a clause is
*compiled*, and these are `assertz/1`-ed.

So `forall(_, Module:Goal, ...)` below is the shape to expect, with exactly
one qualifier in it, and the tests are written to show which one.
*/

test_emit :-
    run_tests([ emit_threading,
                emit_wrappers,
                emit_builtins,
                emit_quantifiers
              ]).

%!  emitted(+Terms, -Clauses) is det.

emitted(Terms, Clauses) :-
    compile_program(m, Terms, Program),
    runnable_program(program, Program, Clauses).

clause_for(Clauses, Name, Clause) :-
    member(Clause, Clauses),
    head_of(Clause, Head),
    functor(Head, Name, _).

head_of((Head :- _), Head) :- !.
head_of(Fact, Fact).

defines(Clauses, Name/Arity) :-
    member(Clause, Clauses),
    head_of(Clause, Head),
    functor(Head, Name, Arity).

		 /*******************************
		 *        THREADING		*
		 *******************************/

%   Every compiled predicate gains three arguments: `Anc` going down and
%   `S0`/`S` coming up.  They are different things with different modes,
%   so they are different arguments — nothing is reconstructed from
%   anything else.

:- begin_tests(emit_threading).

test(a_predicate_gains_three_arguments) :-
    emitted([(p :- q), q], Clauses),
    assertion(defines(Clauses, 'm$plain$p$0'/3)),
    assertion(defines(Clauses, 'm$body.plain$p$0'/3)).

test(and_keeps_its_own) :-
    emitted([q(1)], Clauses),
    assertion(defines(Clauses, 'm$plain$q$1'/4)).

%   The ancestor chain goes to every goal unchanged; the proof is
%   threaded through them in order.

test(the_body_threads_the_proof_in_order) :-
    emitted([(p :- q, r), q, r], Clauses),
    once(clause_for(Clauses, 'm$body.plain$p$0', Clause)),
    Clause = ('m$body.plain$p$0'(Anc, S0, S) :- Goals),
    Goals = ('m$plain$q$0'(A1, In1, Mid), 'm$plain$r$0'(A2, In2, Out)),
    assertion(A1 == Anc),
    assertion(A2 == Anc),
    assertion(In1 == S0),
    assertion(In2 == Mid),
    assertion(Out == S).

test(a_fact_passes_the_proof_through) :-
    emitted([q(1)], Clauses),
    once(clause_for(Clauses, 'm$body.plain$q$1', Clause)),
    Clause = ('m$body.plain$q$1'(1, _, S0, S) :- true),
    assertion(S0 == S).

%   Machinery is threaded like anything else but gets no wrapper: a
%   clause dual is not a literal.

test(the_dual_helpers_are_threaded_without_wrappers) :-
    emitted([(p :- q), q], Clauses),
    assertion(defines(Clauses, 'm$dual.1$p$0'/3)),
    assertion(\+ defines(Clauses, 'm$body.dual.1$p$0'/3)).

test(the_nmr_check_is_threaded) :-
    emitted([p], Clauses),
    assertion(defines(Clauses, 'm$nmr$$0'/3)).

:- end_tests(emit_threading).

		 /*******************************
		 *        WRAPPERS		*
		 *******************************/

%   A literal is checked against the coinductive hypothesis before its
%   clauses are tried, so the predicate a caller calls is not the one
%   holding the clauses.

:- begin_tests(emit_wrappers).

wrapper(Terms, Name, Goal, Neg, Polarity) :-
    emitted(Terms, Clauses),
    once(clause_for(Clauses, Name, Clause)),
    Clause = (_ :- Body),
    Body = (check(Goal, Neg, Polarity, _, _, _, _), _).

%   The goal, its negation and its polarity are constants the compiler
%   knows from the `Kind` field, so nothing is demangled while solving.

test(a_literal_knows_its_own_negation) :-
    wrapper([(p :- q), q], 'm$plain$p$0', Goal, Neg, Pol),
    assertion(Goal == 'm$plain$p$0'),
    assertion(Neg == 'm$dual$p$0'),
    assertion(Pol == +).

test(and_a_dual_knows_it_the_other_way) :-
    wrapper([(p :- q), q], 'm$dual$p$0', Goal, Neg, Pol),
    assertion(Goal == 'm$dual$p$0'),
    assertion(Neg == 'm$plain$p$0'),
    assertion(Pol == -).

test(classical_negation_pairs_with_its_own_dual) :-
    wrapper([(-(p) :- q), q], 'm$neg$p$0', Goal, Neg, Pol),
    assertion(Goal == 'm$neg$p$0'),
    assertion(Neg == 'm$dualneg$p$0'),
    assertion(Pol == +).

test(the_arguments_are_shared_with_the_head) :-
    emitted([q(1)], Clauses),
    once(clause_for(Clauses, 'm$plain$q$1', Clause)),
    Clause = ('m$plain$q$1'(A, _, _, _) :- Body),
    Body = (check(Goal, Neg, _, _, _, _, _), _),
    assertion(Goal == 'm$plain$q$1'(A)),
    assertion(Neg == 'm$dual$q$1'(A)).

%   Only the four literal kinds get one.  A sub-check and a clause dual
%   are machinery: nothing in a model names them, and checking them
%   against the hypothesis would be checking something the program never
%   said.

test(only_literals_are_checked, [forall(member(Kind, [plain, neg, dual, dualneg]))]) :-
    assertion(checked_kind(Kind)).

test(machinery_is_not, [forall(member(Kind, [dual(1), dual(1, inner), nmr,
                                             chk(1), dualchk(1)]))]) :-
    assertion(\+ checked_kind(Kind)).

test(a_sub_check_has_no_wrapper) :-
    emitted([p, (:- p)], Clauses),
    assertion(defines(Clauses, 'm$dualchk.1$$0'/3)),
    assertion(\+ defines(Clauses, 'm$body.dualchk.1$$0'/3)).

%   One wrapper per predicate, however many clauses it has.

%   `proved/4` gets both the proof before the body ran and the proof
%   after, because the difference between them is the literal's
%   children — which is how the justification comes back as a tree with
%   no second structure to carry it.

test(the_wrapper_hands_the_children_to_proved) :-
    emitted([(p :- q), q], Clauses),
    once(clause_for(Clauses, 'm$plain$p$0', Clause)),
    Clause = ('m$plain$p$0'(_, S0, S) :- Body),
    Body = (_, ( _ -> 'm$body.plain$p$0'(_, In, Out),
                       proved(_, Before, After, Result)
               ; done(_, _, _, _)
               )),
    assertion(In == S0),
    assertion(Before == S0),
    assertion(After == Out),
    assertion(Result == S).

test(one_wrapper_per_predicate) :-
    emitted([p(1), p(2), p(3)], Clauses),
    findall(C, clause_for(Clauses, 'm$plain$p$1', C), Wrappers),
    assertion(Wrappers = [_]),
    findall(C, clause_for(Clauses, 'm$body.plain$p$1', C), Bodies),
    assertion(Bodies = [_, _, _]).

:- end_tests(emit_wrappers).

		 /*******************************
		 *        BUILT-INS		*
		 *******************************/

%   A built-in is a constraint rather than a literal: it does not advance
%   the proof, and nothing about it belongs in a model.

:- begin_tests(emit_builtins).

test(a_builtin_does_not_advance_the_proof) :-
    emitted([(p(X) :- X = 1)], Clauses),
    once(clause_for(Clauses, 'm$body.plain$p$1', Clause)),
    Clause = ('m$body.plain$p$1'(A, _, S0, S) :- (A = 1, Out = In)),
    assertion(In == S0),
    assertion(Out == S).

%   `\=` becomes `dif/2` — the same relation, stated so that it
%   *constrains* an unbound argument rather than merely succeeding on it.
%   That is what makes the dual of `p(0)` answer `not p(X)` with the
%   values X may take rather than with "yes".

test(disequality_goes_into_the_herbrand_store) :-
    emitted([p(0)], Clauses),
    once(clause_for(Clauses, 'm$dual.1$p$1', Clause)),
    Clause = ('m$dual.1$p$1'(A, _, S0, S)
                 :- (apart(B, 0), Out = In)),
    assertion(B == A),
    assertion(In == S0),
    assertion(Out == S).

test(fail_stays_fail) :-
    emitted([edge(_, _)], Clauses),
    once(clause_for(Clauses, 'm$dual.1$edge$2', Clause)),
    Clause = (_ :- Body),
    assertion(Body = (_ = _, fail)).

:- end_tests(emit_builtins).

		 /*******************************
		 *       QUANTIFIERS		*
		 *******************************/

%   A quantifier keeps its goal as a **term** rather than a threaded
%   call, because it re-runs that goal and each run needs its own state.

:- begin_tests(emit_quantifiers).

test(for_domain_goes_to_the_runtime) :-
    Terms = [ (:- domain d = [a, b]),
              (:- pred t(_, d)),
              (p(X) :- q(X), not t(X, _)),
              q(1), t(1, a)
            ],
    emitted(Terms, Clauses),
    once(clause_for(Clauses, 'm$dual.1$p$1', Clause)),
    Clause = (_ :- for_domain(V, Gen, Goal, Anc, S0, S)),
    assertion(Gen == program:'m$plain$d$1'(V)),
    assertion(Goal = program:'m$dual.1.inner$p$1'(_, V)),
    assertion(var(Anc)), assertion(var(S0)), assertion(var(S)).

%   Qualified with the module the clauses will live in.  A
%   `meta_predicate` declaration would be the usual way to carry the
%   module across, but it applies when a clause is *compiled* and these
%   are asserted, so the runtime would look in its own module.

test(the_quantifier_goals_say_which_module_to_call_them_in) :-
    emitted([(p(X) :- q(X), not t(X, _)), q(1)], Clauses),
    once(clause_for(Clauses, 'm$dual.1$p$1', Clause)),
    Clause = (_ :- forall(_, Module:_, _, _, _)),
    assertion(Module == program).

test(an_unbounded_quantifier_goes_to_the_runtime_too) :-
    emitted([(p(X) :- q(X), not t(X, _)), q(1)], Clauses),
    once(clause_for(Clauses, 'm$dual.1$p$1', Clause)),
    Clause = (_ :- forall(V, _:Goal, _, _, _)),
    assertion(Goal = 'm$dual.1.inner$p$1'(_, V)).

:- end_tests(emit_quantifiers).
