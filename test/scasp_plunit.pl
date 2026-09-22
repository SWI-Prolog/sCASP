:- module(scasp_plunit,
          [ scasp_models/2,             % :Query, -Models
            scasp_models/3,             % :Query, -Models, +Options
            scasp_answers/3,            % :Query, -Templates, -Models
            scasp_proofs/3,             % :Query, +Level, -Skeleton
            scasp_tree/2,               % :Query, -Tree
            models_equal/2,             % +ModelsA, +ModelsB
            model_has/2,                % +Models, +Literal
            constrained_to/3,           % +Term, +Admits, +Excludes
            scasp_flags/1,              % -Flags
            set_scasp_flags/1,          % +Flags
            with_forall/2,              % +Algorithm, :Goal
            answers_admit/4,            % :Query, ?Var, +Candidates, -Admitted
            expect_semantics/0          % for :- if/1
          ]).
:- reexport(proof_skeleton, [proof_literals/2]).
:- use_module('../prolog/scasp', [scasp/2]).

% Make library(scasp) resolve to *this* tree.  Without it the name
% resolves through the installed pack, which is a different checkout, so
% a program under test that says use_module(library(scasp)) loads a second
% copy of the library and collides with the one already loaded here.
%
% `prolog/' is the pack's library root, so this one path serves both
% trees: library(scasp/...) and library(sscasp/...) alike.

set_scasp_library_path :-
    source_file(set_scasp_library_path, Here),
    file_directory_name(Here, TestDir),
    file_directory_name(TestDir, Root),
    atom_concat(Root, '/prolog', Lib),
    add_library_path(Lib).

%   asserta/1, not assertz/1.  These have to come *before* whatever the
%   installed pack put there, or library(scasp) resolves to the other
%   checkout and a module gets loaded twice under two paths.

add_library_path(Dir) :-
    (   user:file_search_path(library, Dir)
    ->  true
    ;   asserta(user:file_search_path(library, Dir))
    ).

:- set_scasp_library_path.
:- use_module(proof_skeleton,
              [ answers_model_key/2, answers_skeleton/3, proof_literals/2 ]).
:- use_module(library(apply), [maplist/3, include/3]).
:- use_module(library(lists), [member/2]).
:- use_module(library(pairs), [pairs_keys_values/3]).

%!  answers_admit(:Query, ?Var, +Candidates, -Admitted) is det.
%
%   Admitted is the sublist of Candidates that *some* answer to Query
%   still allows for Var, which must be a variable occurring in Query.
%
%   This is the union over all answers, so it says what the solution set
%   is without depending on how many answers it arrives in.  That
%   distinction matters: `X #< 1 ; X #> 1` and a single disequality
%   describe the same set, and CLP(Q) splits the dual of an equality in
%   two only because it has no native disequality.  A reimplementation
%   may legitimately answer in one where this one answers in two, so a
%   test that counts answers pins an implementation choice while one that
%   compares the set states the semantics.
%
%   findall/3 keeps the constraints on the collected variables, so an
%   answer of `X #=< 10` is still testable against each candidate.

:- meta_predicate answers_admit(0, ?, +, -).

answers_admit(Query, Var, Candidates, Admitted) :-
    findall(Var, Query, Answers),
    include(admitted_by(Answers), Candidates, Admitted).

admitted_by(Answers, Value) :-
    member(Answer, Answers),
    \+ \+ Answer = Value,
    !.

%!  expect_semantics is semidet.
%
%   True when the suite is being run in *semantics* mode, i.e. when the
%   Prolog flag `scasp_expect` is `semantics`:
%
%       swipl -Dscasp_expect=semantics ...
%
%   Used as the condition of a `:- if/1` around a test whose current
%   answer is known to be wrong.  The two branches hold the two
%   expectations - what the reasoner does today, and what it ought to do:
%
%       :- if(expect_semantics).
%       test(the_answer_covers_every_shape) :- ...
%       :- else.
%       test(the_answer_is_only_the_f_shape) :- ...
%       :- endif.
%
%   Default (flag unset) is the current behaviour, so `make check` stays
%   green.  In semantics mode those tests are *expected to fail*: the set
%   that fails is the list of defects a reimplementation still has to fix.
%   See doc/test-strategy.md.

expect_semantics :-
    current_prolog_flag(scasp_expect, semantics).

/** <module> Helpers for testing s(CASP) through the embedded interface

`test_scasp.pl` drives the *file* front end: scasp_load/2 on a source
file, compared against golden `.pass` data.  The suites that use this
module drive the two *embedded* front ends instead - scasp/2 over
ordinary Prolog clauses, and begin_scasp/end_scasp blocks - which is how
library(scasp) is actually used and which almost nothing tested before.

The model of an answer is compared with models_equal/2, which is the same
exact comparison test_scasp.pl uses.  Justifications, when a test looks at
them at all, go through proof_skeleton/3 so they are not brittle.
*/

:- meta_predicate
    scasp_tree(0, -),
    scasp_models(0, -),
    scasp_models(0, -, +),
    scasp_answers(0, -, -),
    scasp_proofs(0, +, -).

%!  scasp_models(:Query, -Models) is det.
%!  scasp_models(:Query, -Models, +Options) is det.
%
%   Models is the list of models of  every  answer to Query, in the order
%   the solver produces them.  Deterministic: a  query with no answer at
%   all gives `[]` rather than failing, so a  test can say what it means
%   without wrapping the call.

scasp_models(Query, Models) :-
    scasp_models(Query, Models, []).

scasp_models(Query, Models, Options) :-
    findall(M, scasp(Query, [model(M)|Options]), Models).

%!  scasp_answers(:Query, -Templates, -Models) is det.
%
%   As scasp_models/2, but also returns Query  as  it was bound by each
%   answer, for a test that cares about the bindings.  Templates are the
%   *plain* goals: the module qualification meta_predicate adds would
%   otherwise defeat every comparison a test wants to make.

scasp_answers(Query, Templates, Models) :-
    strip_module(Query, _Module, Plain),
    findall(Plain-M, scasp(Query, [model(M)]), Pairs),
    pairs_keys_values(Pairs, Templates, Models).

%!  scasp_proofs(:Query, +Level, -Skeleton) is det.
%
%   Skeleton is the canonical form, at Level, of the justifications of all
%   answers to Query.  See proof_skeleton.pl.

scasp_proofs(Query, Level, Skeleton) :-
    findall(T, scasp(Query, [tree(T)]), Trees),
    answers_skeleton(Trees, Level, Skeleton).

%!  scasp_tree(:Query, -Tree) is nondet.
%
%   Tree is the justification of an answer to Query.  Backtracks over the
%   answers, so it can be used directly in a plunit `all`/`set` test.

scasp_tree(Query, Tree) :-
    scasp(Query, [tree(Tree)]).

%!  models_equal(+ModelsA, +ModelsB) is semidet.
%
%   True when the two sets of models are the same: variable naming is
%   normalised and the order of the answers ignored, nothing else.

models_equal(A, B) :-
    answers_model_key(A, Key),
    answers_model_key(B, Key).

%!  model_has(+Models, +Literal) is semidet.
%
%   True when some answer's model contains Literal.  For tests on large
%   models, where spelling the whole model out says less than naming the
%   literal that matters.

model_has(Models, Literal) :-
    member(Model, Models),
    member(Literal, Model),
    !.

%!  constrained_to(+Term, +Admits, +Excludes) is semidet.
%
%   True when every value in Admits still unifies with Term and none in
%   Excludes does.  scasp/2 leaves a constrained answer as a variable
%   carrying the constraint in its attributes rather than inlining it
%   into the term, so this is how a test says what the constraint *is*:
%   by what it still allows and what it has ruled out.
%
%       scasp(not gt(X), []),
%       constrained_to(X, [10, 0, -5], [11, 1000])

constrained_to(Term, Admits, Excludes) :-
    forall(member(A, Admits),
           (   \+ \+ Term = A
           ->  true
           ;   print_message(error, scasp_test(should_admit(Term, A))),
               fail
           )),
    forall(member(E, Excludes),
           (   \+ Term = E
           ->  true
           ;   print_message(error, scasp_test(should_exclude(Term, E))),
               fail
           )).

:- multifile prolog:message//1.

prolog:message(scasp_test(should_admit(Term, Value))) -->
    [ 'constraint on ~p should still admit ~p'-[Term, Value] ].
prolog:message(scasp_test(should_exclude(Term, Value))) -->
    [ 'constraint on ~p should exclude ~p'-[Term, Value] ].

%!  with_forall(+Algorithm, :Goal) is nondet.
%
%   Run Goal with the `scasp_forall` flag set to Algorithm.  Set here
%   rather than in a unit setup/1: a Prolog flag is thread-local once
%   set, so changing it in the thread plunit runs the test in leaves
%   every other job alone, while a setup/1 runs in the main thread and
%   would leak.

:- meta_predicate with_forall(+, 0).

with_forall(Algorithm, Goal) :-
    current_prolog_flag(scasp_forall, Old),
    setup_call_cleanup(
        set_prolog_flag(scasp_forall, Algorithm),
        Goal,
        set_prolog_flag(scasp_forall, Old)).

%!  scasp_flags(-Flags) is det.
%!  set_scasp_flags(+Flags) is det.
%
%   Save and restore every `scasp_*` Prolog flag.  s(CASP) options are
%   process-global flags and some of them - scasp_forall and scasp_dcc in
%   particular - change the *semantics* of solve/4 and
%   generate_nmr_check/1.  plunit runs every unit in one process, so a
%   unit that sets an option must put it back:
%
%       :- begin_tests(dcc, [ setup(scasp_flags(F)),
%                             cleanup(set_scasp_flags(F)) ]).

scasp_flags(Flags) :-
    findall(Flag-Value,
            ( current_prolog_flag(Flag, Value),
              atom(Flag),
              sub_atom(Flag, 0, _, _, scasp_)
            ),
            Flags).

set_scasp_flags(Flags) :-
    forall(member(Flag-Value, Flags),
           set_prolog_flag(Flag, Value)).
