:- module(scasp_stable_models,
          [ op(900, fy, not),
            op(700, xfx, [#=, #<>, #<, #>, #=<, #>=]),
            stable_models/2,            % +Program, -Models
            brave_entails/2,            % +Program, +Literal
            models_entail/2,            % +Models, +Literal
            cautious_entails/2,         % +Program, +Literal
            program_literals/2,         % +Program, -Literals
            ground_program/2,           % +Program, -GroundProgram
            reduct/3,                   % +Program, +Interpretation, -Reduct
            least_model/2               % +DefiniteProgram, -Model
          ]).
:- use_module(library(lists), [member/2, append/3]).
:- use_module(library(apply), [maplist/2, maplist/3, partition/4, exclude/3,
                                convlist/3]).
:- use_module(library(ordsets), [ord_union/3, ord_memberchk/2]).
:- use_module(library(error), [instantiation_error/1, type_error/2,
                               domain_error/2]).

/** <module> Stable models by brute force, straight from the definition

An oracle for the propositional fragment, written from the Gelfond-Lifschitz
definition rather than from anything s(CASP) does.  That is the whole point:
every other oracle in this project descends from the implementation under
test - the `.pass` data records what this code produces, and the sibling
checkout at ../sCASP is the same codebase - so none of them can catch a
defect the implementation has always had.  This one shares no design
decision, no data representation and no bug with either the current solver
or its replacement.

An answer set of a program P is an interpretation I such that I is the least
model of the *reduct* P^I, where the reduct is obtained by

  - deleting every rule whose negative body meets I, and
  - deleting the negative literals from the rules that remain.

The reduct is a definite program, so its least model is just the
immediate-consequence fixpoint.  `stable_models/2` enumerates every subset
of the Herbrand base and keeps the ones that satisfy that equation, which is
exponential and entirely the point: nothing clever can be wrong in a way
that agrees with a clever solver.  Keep programs to a dozen atoms.

## Program syntax

A program is a list of terms, the same ones you would write in a file:

    [ (p :- not q),
      (q :- not p),
      r,                            % a fact
      (:- r, p)                     % a constraint
    ]

`not/1` marks default negation.  The operators are declared and exported
here rather than taken from `scasp/ops.pl`, so that nothing in this file
depends on the system it judges - an operator priority cannot introduce a
semantic bug, but the independence is worth keeping total.

### Classical negation

`-p` is a literal of its own, distinct from `p` and again from `not p`: the
Herbrand base holds both `p` and `-p`, and `not -p` asks whether `-p` is
underivable.  What classical negation adds to the definition is one
requirement, that no answer set contains both `p` and `-p`
(consistent/1), so a program deriving both has no answer set at all.

### Variables

A program with variables is grounded first, naively, over its own
constants: every atomic argument occurring anywhere in it, numbers included.
There are no function symbols, so that is the whole Herbrand universe.
Grounding is exponential in the number of variables per rule on top of the
exponential enumeration, so keep the universe to a handful of constants.

### CLP comparisons

`#=`, `#<>`, `#<`, `#>`, `#=<` and `#>=` in a body are evaluated while
grounding: an instance where one is false is dropped, and one where all hold
keeps its other literals.  They are guards, not literals, and never appear
in an answer set.

### Safety

Every variable of a rule must occur in a positive, non-comparison body
literal; ground_program/2 refuses anything else.  That is not a restriction
of the oracle but a difference of meaning: s(CASP) reads an unsafe rule over
an *extended* Herbrand universe, so `:- not married(X).` there says that
everyone is married, where a grounding over the program's constants would
say something else by design rather than by defect.  A comparison binds
nothing, so it makes no variable safe: `p(X) :- X #> 2.` is unsafe however
ground the comparison looks.

## Constraints

`:- Body` is treated by filtering: an interpretation satisfying Body is not
an answer set.  Strictly it abbreviates `f :- Body, not f`, an odd loop over
negation, but the two agree on which interpretations survive and filtering
does not need a fresh atom.
*/

%!  stable_models(+Program, -Models) is det.
%
%   Models is the sorted list of answer sets of Program, each itself a
%   sorted list of atoms.  `[]` means the program has no answer set,
%   which is different from `[[]]` - one answer set, the empty one.

stable_models(Program, Models) :-
    ground_program(Program, Ground),
    maplist(parse_statement, Ground, Statements),
    partition(is_rule, Statements, Rules, Constraints),
    program_literals(Ground, Atoms),
    findall(M,
            ( subset_of(Atoms, M),
              consistent(M),
              is_stable(Rules, M),
              \+ violated(Constraints, M)
            ),
            Models0),
    sort(Models0, Models).

is_rule(rule(_,_,_)).

%!  consistent(+Interpretation) is semidet.
%
%   No literal appears alongside its classical negation.  Vacuous for a
%   program without classical negation, which is why it costs nothing to
%   apply unconditionally.

consistent(M) :-
    \+ ( member(-(A), M),
         ord_memberchk(A, M)
       ).

%!  is_stable(+Rules, +Interpretation) is semidet.

is_stable(Rules, M) :-
    reduct(Rules, M, Reduct),
    least_model(Reduct, LFP),
    LFP == M.

%!  reduct(+Rules, +Interpretation, -Reduct) is det.
%
%   The Gelfond-Lifschitz reduct: keep the rules whose negative body is
%   disjoint from the interpretation, and drop their negative literals.

reduct(Rules, M, Reduct) :-
    convlist(reduct_rule(M), Rules, Reduct).

reduct_rule(M, rule(H, Pos, Neg), definite(H, Pos)) :-
    \+ ( member(A, Neg), ord_memberchk(A, M) ).

%!  least_model(+DefiniteProgram, -Model) is det.
%
%   Immediate-consequence fixpoint of a program with no negation.

least_model(Rules, Model) :-
    least_model_(Rules, [], Model).

least_model_(Rules, In, Model) :-
    (   member(definite(H, Pos), Rules),
        \+ ord_memberchk(H, In),
        forall(member(B, Pos), ord_memberchk(B, In))
    ->  ord_union([H], In, Next),
        least_model_(Rules, Next, Model)
    ;   Model = In
    ).

%!  violated(+Constraints, +Interpretation) is semidet.

violated(Constraints, M) :-
    member(constraint(Pos, Neg), Constraints),
    forall(member(A, Pos), ord_memberchk(A, M)),
    forall(member(B, Neg), \+ ord_memberchk(B, M)),
    !.

%!  brave_entails(+Program, +Literal) is semidet.
%!  cautious_entails(+Program, +Literal) is semidet.
%
%   Literal holds in *some* answer set, respectively in *every* one.
%   Literal is an atom or `not Atom`.  Brave entailment is what s(CASP)
%   computes: a query succeeds when some answer set supports it.
%
%   Note that cautious entailment over *no* answer sets is vacuously
%   true, which is the usual convention and worth remembering when a
%   program turns out to be inconsistent.

brave_entails(Program, Literal) :-
    stable_models(Program, Models),
    models_entail(Models, Literal).

%!  models_entail(+Models, +Literal) is semidet.
%
%   Brave entailment against models already computed.  Enumerating the
%   answer sets is exponential, so a caller asking about many literals of
%   one program should call stable_models/2 once and this per literal
%   rather than brave_entails/2 per literal.

models_entail(Models, Literal) :-
    member(M, Models),
    holds(Literal, M),
    !.

cautious_entails(Program, Literal) :-
    stable_models(Program, Models),
    forall(member(M, Models), holds(Literal, M)).

holds(not A, M) :- !, \+ ord_memberchk(A, M).
holds(A, M) :- ord_memberchk(A, M).

%!  ground_program(+Program, -Ground) is det.
%
%   Program instantiated over its own constants.  A program that is
%   already ground comes back unchanged, so the propositional case pays
%   nothing.  Throws if a rule is unsafe - see *Safety* above - or if a
%   rule has variables and the program has no constants to fill them.

ground_program(Program, Ground) :-
    maplist(check_safe, Program),
    (   ground(Program),
        \+ ( member(S0, Program), has_clp(S0) )
    ->  Ground = Program
    ;   program_constants(Program, Constants),
        (   Constants == []
        ->  domain_error(program_with_constants, Program)
        ;   true
        ),
        findall(G,
                ( member(R, Program),
                  instance(R, Constants, G0),
                  eval_constraints(G0, G)
                ),
                Ground0),
        sort(Ground0, Ground)
    ).

%!  eval_constraints(+GroundStatement, -Statement) is semidet.
%
%   Evaluate the CLP comparisons in a ground body.  Fails - dropping the
%   instance - when one of them is false; otherwise strips them, since
%   they have served their purpose as a guard.

eval_constraints(Statement, Stripped) :-
    statement_parts(Statement, Head, Body),
    comma_list(Body, Literals),
    partition(clp_literal, Literals, Constraints, Rest),
    (   Constraints == []
    ->  Stripped = Statement
    ;   forall(member(C, Constraints), clp_true(C)),
        rebuild(Head, Rest, Stripped)
    ).

rebuild(true, [], _) :- !, fail.            % a constraint-only constraint
rebuild(true, Rest, (:- Body)) :- !,
    conjunction(Rest, Body).
rebuild(Head, [], Head) :- !.
rebuild(Head, Rest, (Head :- Body)) :-
    conjunction(Rest, Body).

%!  conjunction(+Literals, -Body) is det.
%
%   The inverse of comma_list/2, which cannot run backwards: it guards
%   against an unbound first argument, since without that a variable
%   statement unifies with (A,B) and recurses forever.

conjunction([L], L) :- !.
conjunction([L|T], (L, Body)) :-
    conjunction(T, Body).

has_clp(Statement) :-
    statement_parts(Statement, _, Body),
    comma_list(Body, Literals),
    member(L, Literals),
    clp_literal(L),
    !.

%!  clp_literal(@Literal) is semidet.

clp_literal(L) :-
    nonvar(L),
    functor(L, Name, 2),
    clp_op(Name).

clp_op(#=).
clp_op(#<>).
clp_op(#<).
clp_op(#>).
clp_op(#=<).
clp_op(#>=).

%!  clp_true(+Constraint) is semidet.
%
%   A comparison whose operands are not both numbers is **false**, not an
%   error.  Grounding is naive — every variable takes every constant of the
%   program — so a rule that constrains a number is offered every atom and
%   compound term in it as well, and `q #< 2` is simply an instance that does
%   not apply.  Throwing there would make the oracle refuse any program that
%   mixes arithmetic with terms, which is most of a crossing.

clp_true(Constraint) :-
    Constraint =.. [Operator, A, B],
    number(A),
    number(B),
    compare_numbers(Operator, A, B).

compare_numbers(#=,  A, B) :- A =:= B.
compare_numbers(#<>, A, B) :- A =\= B.
compare_numbers(#<,  A, B) :- A <  B.
compare_numbers(#>,  A, B) :- A >  B.
compare_numbers(#=<, A, B) :- A =< B.
compare_numbers(#>=, A, B) :- A >= B.

instance(Rule, Constants, Ground) :-
    copy_term(Rule, Ground),
    term_variables(Ground, Vars),
    maplist(from(Constants), Vars).

from(Constants, Var) :-
    member(Var, Constants).

%!  program_constants(+Program, -Constants) is det.
%
%   The Herbrand universe: every atomic term appearing as an argument,
%   numbers included.  There are no function symbols, so this is all of
%   it.

program_constants(Program, Constants) :-
    findall(C,
            ( member(Statement, Program),     % not sub_term/2 over Program:
              sub_term(T, Statement),         % that walks the list spine and
              compound(T),                    % collects its [] as a constant
              arg(_, T, C),
              atomic(C)
            ),
            Cs),
    sort(Cs, Constants).

%!  check_safe(+Statement) is det.
%
%   Every variable must occur in a positive body literal.

check_safe(Statement) :-
    (   var(Statement)
    ->  instantiation_error(Statement)
    ;   true
    ),
    statement_parts(Statement, Head, Body),
    body_positive(Body, Positive),
    term_variables(Positive, Safe),
    term_variables(Head-Body, All),
    (   exclude(in_set(Safe), All, [])
    ->  true
    ;   domain_error(safe_rule, Statement)
    ).

in_set(Set, Var) :-
    memberchk_eq(Var, Set).

memberchk_eq(X, [Y|T]) :-
    (   X == Y
    ->  true
    ;   memberchk_eq(X, T)
    ).

statement_parts((:- Body), true, Body) :- !.
statement_parts((Head :- Body), Head, Body) :- !.
statement_parts(Fact, Fact, true).

%   A constraint does not bind anything, so it cannot make a variable
%   safe: `p(X) :- X #> 2.' is unsafe however ground the comparison looks.

body_positive(Body, Positive) :-
    comma_list(Body, Literals),
    exclude(negated, Literals, Positive0),
    exclude(clp_literal, Positive0, Positive).

negated(not(_)).

%!  program_literals(+Program, -Literals) is det.
%
%   Every literal occurring in Program, sorted: the Herbrand base, with
%   `p` and `-p` counted separately.  Atoms sort before compounds, so a
%   base of both comes back as `[p, q, -p]`.

program_literals(Program, Atoms) :-
    ground_program(Program, Ground),
    maplist(parse_statement, Ground, Statements),
    findall(A, (member(S, Statements), statement_atom(S, A)), As),
    sort(As, Atoms).

statement_atom(rule(H, Pos, Neg), A) :-
    (   A = H
    ;   member(A, Pos)
    ;   member(A, Neg)
    ).
statement_atom(constraint(Pos, Neg), A) :-
    (   member(A, Pos)
    ;   member(A, Neg)
    ).

		 /*******************************
		 *           SYNTAX		*
		 *******************************/

%!  parse_statement(+Term, -Statement) is det.

parse_statement(Term, _) :-
    var(Term),
    !,
    instantiation_error(Term).
parse_statement((:- Body), Statement) :-
    !,
    body_literals(Body, Pos, Neg),
    Statement = constraint(Pos, Neg).
parse_statement((Head :- Body), Statement) :-
    !,
    must_be_atom(Head),
    body_literals(Body, Pos, Neg),
    Statement = rule(Head, Pos, Neg).
parse_statement(Fact, Statement) :-
    must_be_atom(Fact),
    Statement = rule(Fact, [], []).

body_literals(Body, Pos, Neg) :-
    comma_list(Body, Literals),
    partition_literals(Literals, Pos, Neg).

comma_list(Var, _) :-
    var(Var),
    !,
    instantiation_error(Var).
comma_list((A,B), List) :-
    !,
    comma_list(A, LA),
    comma_list(B, LB),
    append(LA, LB, List).
comma_list(true, []) :- !.
comma_list(A, [A]).

partition_literals([], [], []).
partition_literals([not A|T], Pos, [A|Neg]) :-
    !,
    must_be_atom(A),
    partition_literals(T, Pos, Neg).
partition_literals([A|T], [A|Pos], Neg) :-
    must_be_atom(A),
    partition_literals(T, Pos, Neg).

%   Called after grounding, so anything non-ground is a bug rather than a
%   variable waiting to be filled.

must_be_atom(A) :-
    (   var(A)
    ->  instantiation_error(A)
    ;   \+ ground(A)
    ->  instantiation_error(A)
    ;   atom(A)
    ->  true
    ;   A = -(Inner)
    ->  must_be_atom(Inner)
    ;   compound(A)
    ->  true
    ;   type_error(literal, A)
    ).

%!  subset_of(+Set, -Subset) is nondet.
%
%   Every subset of Set, each in the order Set is in - so a sorted Set
%   yields sorted subsets, which is what ord_memberchk/2 needs.

subset_of([], []).
subset_of([X|Xs], [X|Ys]) :- subset_of(Xs, Ys).
subset_of([_|Xs], Ys) :- subset_of(Xs, Ys).
