:- module(scasp_plunit,
          [ scasp_models/2,             % :Query, -Models
            scasp_models/3,             % :Query, -Models, +Options
            scasp_answers/3,            % :Query, -Templates, -Models
            scasp_proofs/3,             % :Query, +Level, -Skeleton
            scasp_tree/2,               % :Query, -Tree
            models_equal/2,             % +ModelsA, +ModelsB
            model_has/2,                % +Models, +Literal
            scasp_flags/1,              % -Flags
            set_scasp_flags/1           % +Flags
          ]).
:- reexport(proof_skeleton, [proof_literals/2]).
:- use_module('../prolog/scasp', [scasp/2]).

% Make library(scasp) resolve to *this* tree.  Without it the name
% resolves through the installed pack, which is a different checkout, so
% a program under test that says use_module(library(scasp)) loads a second
% copy of the library and collides with the one already loaded here.

set_scasp_library_path :-
    source_file(set_scasp_library_path, Here),
    file_directory_name(Here, TestDir),
    file_directory_name(TestDir, Root),
    atom_concat(Root, '/prolog', Lib),
    (   user:file_search_path(library, Lib)
    ->  true
    ;   asserta(user:file_search_path(library, Lib))
    ).

:- set_scasp_library_path.
:- use_module(proof_skeleton,
              [ answers_model_key/2, answers_skeleton/3, proof_literals/2 ]).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(lists), [member/2]).
:- use_module(library(pairs), [pairs_keys_values/3]).

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
