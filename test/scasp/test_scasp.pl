/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021-2024, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(test_scasp,
          [ test_scasp/0,
            golden_program/3,           % ?Name, ?File, ?Options
            save_golden/1,              % +Spec
            save_golden/2               % +Spec, +Options
          ]).
:- use_module(library(plunit)).
:- use_module(library(apply),  [maplist/3, convlist/3]).
:- use_module(library(lists),  [member/2, append/3]).
:- use_module(library(option), [option/2, option/3]).
:- use_module(library(pairs),  [pairs_keys_values/3]).
:- use_module(library(filesex), [directory_file_path/3]).

:- use_module('../proof_skeleton',
              [ answers_model_key/2, answers_skeleton/3, answers_agreement/3 ]).
:- use_module(proof_levels, [proof_level/3]).
:- use_module(known_issues, [known_issue/3]).

/** <module> Golden-output tests for the file front end

Each program under test/ is solved with scasp_load/2 + scasp_query/3 +
solve/4 - the front end the `scasp` executable uses - and compared against
its `.pass` file.  The two halves are compared differently:

  - the **model** exactly, through answers_model_key/2: variable naming is
    normalised and the order of the answers ignored, nothing else;
  - the **justification** at the level test/scasp/proof_levels.pl declares,
    `exact` unless there is a recorded reason to be looser, through
    proof_skeleton/3.  See test/proof_skeleton.pl for what each level
    absorbs and why comparing trees verbatim does not survive a change to
    the solver.

One plunit test is generated per program, so plunit's own options apply
per program: `blocked(Reason)` for a program that does not terminate and
`fixme(Reason)` for one that cannot be solved yet.  Both come from
test/scasp/known_issues.pl, which holds the reason.

Programs under test/all_programs/dcc run with `scasp_dcc`.  The flag is
set inside the test rather than in a unit setup/1, so the change stays in
the thread plunit runs the test in and this unit can run beside the
others under run_tests/2's jobs(N).

To regenerate the golden data after an intended change:

    ?- save_golden('test/all_programs').        % only where absent
    ?- save_golden('test/all_programs', [overwrite(true)]).
*/

user:file_search_path(scasp, SCASPDir) :-
    scasp_dir(SCASPDir).
user:file_search_path(library, scasp(prolog)).

scasp_dir(SCASPDir) :-
    source_file(scasp_dir(_), File),
    file_directory_name(File, Scasp),
    file_directory_name(Scasp, TestDir),
    file_directory_name(TestDir, SCASPDir).

:- use_module(library(scasp/ops)).
:- use_module(library(scasp/compile)).
:- use_module(library(scasp/solve)).
:- use_module(library(scasp/stack)).
:- use_module(library(scasp/model)).
:- use_module(library(scasp/output)).
:- use_module(library(scasp/options)).
:- use_module(library(scasp/source_ref)).

test_scasp :-
    run_tests([golden]).


		 /*******************************
		 *         THE PROGRAMS		*
		 *******************************/

%!  golden_dir(?Dir, ?Options) is nondet.
%
%   Directories holding golden-output tests, relative to the repository.
%   Options are the plunit unit options the directory needs; `dcc` needs
%   the scasp_dcc flag and therefore its own unit.

golden_dir('test/all_programs',      golden).
golden_dir('test/all_programs/sasp', golden).
golden_dir('test/min_programs',      golden).
golden_dir('test/all_programs/dcc',  golden_dcc).

%!  golden_program(?Name, ?File, ?Options) is nondet.
%
%   Name is a unique test name for the program File, and Options are the
%   plunit options it runs under.  Names are directory-qualified because
%   three base names occur in more than one directory.

golden_program(Name, File, Options) :-
    golden_program(golden, Name, File, Options).

golden_program(Unit, Name, File, Options) :-
    golden_dir(Dir, Unit),
    scasp_dir(Root),
    directory_file_path(Root, Dir, AbsDir),
    directory_file_path(AbsDir, '*.pl', Pattern),
    expand_file_name(Pattern, Files),
    member(File, Files),
    program_name(Dir, File, Name),
    program_options(File, Options).

program_name(Dir, File, Name) :-
    file_base_name(File, Base),
    file_name_extension(Plain, pl, Base),
    file_base_name(Dir, DirName),
    atomic_list_concat([DirName, /, Plain], Name).

%!  program_options(+File, -Options) is det.
%
%   A program that cannot be solved is not given golden data - that would
%   record the bug as the expected answer - so it is marked from the
%   reason in test/scasp/known_issues.pl instead.
%
%   `hang` becomes `blocked`: running it costs the whole timeout on every
%   run.  `no_models` becomes a test expected to *fail*, which is not the
%   same as `fixme`: plunit does not run the body of a fixme test, so it
%   could never report that the problem had gone away.  An expected
%   failure runs, and fails loudly the day the program yields a model.

program_options(File, Options) :-
    file_base_name(File, Base),
    known_issue(Base, Expectation, Reason),
    !,
    issue_option(Expectation, Reason, Options).
program_options(_, []).

issue_option(hang, Reason, [blocked(Reason)]).
issue_option(no_models, _Reason, [fail]).

%!  golden_goal(+Kind, +File, -Goal) is det.
%
%   How to run the program.  The dcc ones need the flag set around them;
%   a known `no_models` issue is run for the opposite reason - to check
%   it still yields nothing.

golden_goal(golden, File, golden_test(File)) :-
    \+ known_no_models(File).
golden_goal(golden, File, yields_a_model(File)) :-
    known_no_models(File).
golden_goal(golden_dcc, File, golden_dcc_test(File)).

known_no_models(File) :-
    file_base_name(File, Base),
    known_issue(Base, no_models, _).

%!  yields_a_model(+File) is semidet.
%
%   True when File yields at least one model.  Used as the body of a test
%   declared to fail, so that the suite says so the day one of the
%   programs in test/scasp/known_issues.pl starts working.

yields_a_model(File) :-
    scasp_test(File, _Trees-Models),
    Models \== [].


		 /*******************************
		 *          THE TESTS		*
		 *******************************/

% One unit for every program, including the --dcc ones.  They have to
% share a unit: scasp_load/2 asserts the compiled program into the module
% of its caller and wipes what was there first, so two *units* compiling
% into test_scasp would destroy each other's program the moment plunit
% ran them on separate jobs.  Within a unit the tests run one at a time,
% which is what keeps that safe.

:- begin_tests(golden).

term_expansion(golden_tests, Tests) :-
    findall((test(Name, Options) :- test_scasp:Goal),
            ( test_scasp:golden_program(Kind, Name, File, Options),
              test_scasp:golden_goal(Kind, File, Goal)
            ),
            Tests).

golden_tests.

:- end_tests(golden).


%!  golden_dcc_test(+File) is semidet.
%
%   Run File with scasp_dcc set, and set it *here* rather than in a unit
%   setup/1.  A Prolog flag is thread-local once set: a thread inherits
%   its creator's value and its own changes do not propagate back, so
%   setting it in the thread plunit runs the test in leaves every other
%   job alone.
%
%   A unit setup/1 would not do, because that runs in the main thread:
%   the value would leak into every job thread created after it - and
%   scasp_forall would leak with it, since scasp_set_options/1 forces
%   `prev` alongside dcc.

golden_dcc_test(File) :-
    setup_call_cleanup(
        save_scasp_flags(Saved),
        ( scasp_set_options([dcc(true)]),
          golden_test(File)
        ),
        restore_scasp_flags(Saved)).

save_scasp_flags(Flags) :-
    findall(Flag-Value,
            ( current_prolog_flag(Flag, Value),
              atom(Flag),
              sub_atom(Flag, 0, _, _, scasp_)
            ), Flags).

restore_scasp_flags(Flags) :-
    forall(member(Flag-Value, Flags), set_prolog_flag(Flag, Value)).


		 /*******************************
		 *        RUN AND COMPARE	*
		 *******************************/

%!  golden_test(+File) is semidet.
%
%   Solve File and compare both halves against its `.pass` data.

golden_test(File) :-
    pass_data(File, PassFile, PassData),
    (   nonvar(PassData),
        PassData = PassStacks-PassModels
    ->  true
    ;   print_message(error, scasp_test(no_golden_data(PassFile))),
        fail
    ),
    scasp_test(File, Stacks-Models),
    same_models(PassModels, Models),
    test_proof_level(File, Level, _Reason),
    same_proofs(PassStacks, Stacks, Level).

%!  same_models(+PassModels, +Models) is semidet.
%
%   Models are compared *exactly*.  There is no option to weaken this.

same_models(PassModels, Models) :-
    answers_model_key(PassModels, Key),
    (   answers_model_key(Models, Key)
    ->  true
    ;   print_message(error, scasp_test(model_differs)),
        fail
    ).

%!  same_proofs(+PassStacks, +Stacks, +Level) is semidet.
%
%   Report which way a justification moved.  Naming the strongest level
%   that *would* agree turns the failure into the entry
%   test/scasp/proof_levels.pl would need, so loosening a test stays a
%   deliberate act rather than a silent rewrite of the `.pass` file.

same_proofs(PassStacks, Stacks, Level) :-
    answers_skeleton(PassStacks, Level, PassSkeleton),
    answers_skeleton(Stacks, Level, Skeleton),
    (   PassSkeleton == Skeleton
    ->  true
    ;   answers_agreement(PassStacks, Stacks, Agree)
    ->  print_message(error, scasp_test(proof_agrees_at(Level, Agree))),
        fail
    ;   print_message(error, scasp_test(proof_differs(Level))),
        fail
    ).

%!  test_proof_level(+File, -Level, -Reason) is det.

test_proof_level(File, Level, Reason) :-
    file_base_name(File, Base),
    proof_level(Base, Level, Reason),
    !.
test_proof_level(_, exact, "").

%!  scasp_test(+File, -StackModelPairs) is det.
%
%   Solve File and collect a Tree-Model pair for every answer.
%
%   The source references are reset first: scasp_source_reference/3 is a
%   process-global counter that is never retracted, so the clause numbers
%   in goal_origin/2 would otherwise depend on what was loaded earlier in
%   the process, and a comparison at `exact` would depend on test order.

scasp_test(File, Trees-Models) :-
    retractall(scasp_source_reference(_, _, _)),
    scasp_load(File, [unknown(fail)]),
    scasp_query(Query, Bindings, []),
    findall(Pair, solve_one(Query, Bindings, Pair), Pairs),
    pairs_keys_values(Pairs, Trees, Models).

solve_one(Query, Bindings, Tree-Model) :-
    solve(Query, [], StackOut, ModelOut),
    justification_tree(StackOut, Tree, []),
    canonical_model(ModelOut, Model),
    All = t(Bindings, Model, Tree),
    ovar_set_bindings(Bindings),
    ovar_analyze_term(All),
    inline_constraints(All, []).


		 /*******************************
		 *         GOLDEN DATA		*
		 *******************************/

%!  pass_data(+TestFile, -PassFile, -PassData) is det.

pass_data(File, PassFile, PassData) :-
    pass_file(File, PassFile),
    (   exists_file(PassFile)
    ->  setup_call_cleanup(
            open(PassFile, read, In),
            read_term(In, PassData, [module(scasp_ops)]),
            close(In))
    ;   true
    ).

pass_file(File, PassFile) :-
    file_name_extension(Base, _, File),
    file_name_extension(Base, pass, PassFile).

save_test_data(Into, Result) :-
    setup_call_cleanup(
        open(Into, write, Out),
        write_term(Out, Result,
                   [ module(scasp_ops),
                     quoted(true),
                     fullstop(true),
                     nl(true)
                   ]),
        close(Out)).

%!  save_golden(+Spec) is det.
%!  save_golden(+Spec, +Options) is det.
%
%   Write the `.pass` data for the programs under Spec, a directory or a
%   file relative to the repository.  By default only where it is absent;
%   with `overwrite(true)` for every program, which is what to use after
%   an intended change to the solver.
%
%   A program listed in test/scasp/known_issues.pl is skipped: recording its
%   current output would record the bug as the expected answer.

save_golden(Spec) :-
    save_golden(Spec, []).

save_golden(Spec, Options) :-
    scasp_dir(Root),
    directory_file_path(Root, Spec, Abs),
    (   exists_directory(Abs)
    ->  directory_file_path(Abs, '*.pl', Pattern),
        expand_file_name(Pattern, Files)
    ;   Files = [Abs]
    ),
    forall(member(File, Files),
           save_golden_file(File, Options)).

save_golden_file(File, _Options) :-
    file_base_name(File, Base),
    known_issue(Base, _, _),
    !,
    print_message(informational, scasp_test(skipped_known_issue(File))).
save_golden_file(File, Options) :-
    pass_file(File, PassFile),
    (   exists_file(PassFile),
        \+ option(overwrite(true), Options)
    ->  true
    ;   scasp_test(File, Result),
        save_test_data(PassFile, Result),
        print_message(informational, scasp_test(saved(PassFile)))
    ).


		 /*******************************
		 *          MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(scasp_test(no_golden_data(File))) -->
    [ 'no golden data: ~w does not exist'-[File] ].
prolog:message(scasp_test(model_differs)) -->
    [ 'golden data: the model differs'-[] ].
prolog:message(scasp_test(proof_agrees_at(Level, Agree))) -->
    [ 'golden data: the justification differs at `~w\'; it agrees at \c
       `~w\'.'-[Level, Agree], nl,
      'Add an entry to test/scasp/proof_levels.pl if that is intended.'-[] ].
prolog:message(scasp_test(proof_differs(Level))) -->
    [ 'golden data: the justification differs at `~w\' and agrees at no \c
       level.'-[Level] ].
prolog:message(scasp_test(saved(File))) -->
    [ 'saved ~w'-[File] ].
prolog:message(scasp_test(skipped_known_issue(File))) -->
    [ 'skipped ~w: listed in test/scasp/known_issues.pl'-[File] ].
