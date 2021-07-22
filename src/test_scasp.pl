:- module(test_scasp,
          [ test_scasp/0,
            qtest_scasp/0,
            run_test/2                  % +File, +Options
          ]).
:- use_module(scasp, [scasp_test/2]).
:- use_module(library(lists), [member/2]).
:- use_module(library(main), [main/0, argv_options/3]).
:- use_module(library(option), [option/3, option/2]).
:- use_module(library(time), [call_with_time_limit/2]).

:- use_module(scasp_ops).
:- use_module(sasp/options).
:- use_module(sasp/variables).
:- use_module(diff).

:- initialization(main, main).

test_scasp :-
    main([]).

qtest_scasp :-
    main([ '../test/pq.pl',
           '../test/vars.pl',
           '../test/classic_negation_incostistent.pl',
           '../test/birds.pl',
           '../test/family.pl',
           '../test/hamcycle.pl',
           '../test/hamcycle_two.pl',
           '../test/hanoi.pl'
         ]).

%!  main(+Argv)
%
%   Usage: swipl test_scasp.pl [option ...] [dir ...] [file ...]
%
%   Options:
%
%     |----------------|-------------------------------------|
%     | -q             | Only run the _quick_ tests          |
%     | --timeout=Secs | Run tests with timeout (default 60) |
%     | --save         | Save result if no .pass file exists |
%     | --overwrite    | Overwrite .pass after we passed     |
%     | --pass         | Overwrite .pass after we failed     |
%     | --cov[=Dir]    | Dump coverage data in Dir (`cov`)   |
%
%   Default runs tests from `../test`

main(['-q']) :-
    !,
    qtest_scasp.
main(Argv) :-
    argv_options(Argv, Positional, Options),
    set_stack_sizes,
    test_files(Positional, Files),
    (   option(cov(Cov), Options)
    ->  cov_dir(Cov, Dir),
        show_coverage(run_tests(Files, Options),
                      [ dir(Dir) ])
    ;   run_tests(Files, Options)
    ).

cov_dir(true, Dir) => Dir = cov.
cov_dir(Dir0, Dir) => Dir = Dir0.

run_tests(Files, Options) :-
    run_tests(Files, Failed, Options),
    (   Failed == 0
    ->  format(user_error, 'All tests passed!~n', [])
    ;   format(user_error, '~D tests failed~n', [Failed]),
        (   current_prolog_flag(break_level, _)
        ->  fail
        ;   halt(1)
        )
    ).

run_tests(Files, Failed, Options) :-
    run_tests(Files, 0, Failed, Options).

run_tests([], Failed, Failed, _).
run_tests([H|T], Failed0, Failed, Options) :-
    (   run_test(H, Options)
    ->  run_tests(T, Failed0, Failed, Options)
    ;   Failed1 is Failed0+1,
        run_tests(T, Failed1, Failed, Options)
    ).

%!  run_test(+File, +Options) is semidet.
%
%   Compute all stacks and models for File.  Options:
%
%     - save(true)
%       Write new `.pass` file if there is none.
%     - overwrite(true)
%       Save the .pass file even if the test passed.
%     - show_diff(true)
%       Use `meld` to show the difference between the stacks if the
%       test failed.

run_test(File, Options) :-
    format("~w~t~45|", [File]),
    flush_output,
    option(timeout(Time), Options, 60),
    statistics(runtime, _),
    catch(call_with_time_limit(Time, scasp_test([File], Stacks-Models)),
          Error, true),
    statistics(runtime, [_,Used]),
    Result = Stacks-Models,
    pass_data(File, PassFile, PassResult),
    (   PassResult = PassStacks-PassModels
    ->  true
    ;   PassStacks = PassResult         % old format
    ),
    (   nonvar(Error)
    ->  message_to_string(Error, Msg),
        format("ERROR: ~s ~|~t~d ms~8+\n", [Msg,Used]),
        fail
    ;   var(PassStacks)
    ->  length(Models, ModelCount),
        format("~D models ~|~t~d ms~8+\n", [ModelCount,Used]),
        (   option(save(true), Options)
        ->  save_test_data(PassFile, Result)
        ;   true
        )
    ;   PassStacks =@= Stacks
    ->  format("passed ~|~t~d ms~8+\n", [Used]),
        (   option(overwrite(true), Options)
        ->  save_test_data(PassFile, Result)
        ;   true
        )
    ;   PassModels =@= Models
    ->  format("different stacks, same models ~|~t~d ms~8+\n", [Used]),
        (   option(show_diff(true), Options)
        ->  diff_terms(PassStacks, Stacks)
        ;   option(pass(true), Options)
        ->  save_test_data(PassFile, Result)
        ;   true
        )
    ;   canonical_models(PassModels, CannonicalPassModels),
        canonical_models(Models, CannonicalModels),
        CannonicalPassModels =@= CannonicalModels
    ->  format("different stacks, same models (different order) ~|~t~d ms~8+\n",
               [Used]),
        (   option(show_diff(true), Options)
        ->  diff_terms(PassStacks, Stacks)
        ;   option(pass(true), Options)
        ->  save_test_data(PassFile, Result)
        ;   true
        )
    ;   format("FAILED ~|~t~d ms~8+\n", [Used]),
        (   option(pass(true), Options)
        ->  save_test_data(PassFile, Result)
        ;   option(show_diff(true), Options)
        ->  diff_terms(PassStacks, Stacks)
        ;   option(show_diff(models), Options)
        ->  canonical_models(PassModels, CannonicalPassModels),
            canonical_models(Models, CannonicalModels),
            diff_terms(CannonicalPassModels, CannonicalModels)
        ),
        fail
    ).

canonical_models(Models, CannModels) :-
    maplist(canonical_model, Models, Models1),
    sort(Models1, CannModels).

%!  canonical_model(+Model, -CannModel) is det.
%
%   Make the model canonical. The model is a nested list of positive and
%   negative atoms as well  as  terms   proved(_).  The  model  may also
%   contain duplicates. First we flatten the model, remove the proven(_)
%   terms and the duplicates.
%
%   Next, the variable ordering  may  be   different.  We  solve this by
%   sorting the model disregarding the variables  and re-assign the VarN
%   variable names.
%
%   @bug   This is no 100% guarantee that the model is canonical. It
%          is probably good enough though.

canonical_model(Model, CannModel) :-
    flatten(Model, FlatModel),
    exclude(nonmodel_term, FlatModel, Model1),
    sort(Model1, Model2),               % Remove duplicates
    sort_model(Model2, CannModel).

nonmodel_term(proved(_)).

sort_model(ModelIn, Model) :-
    map_list_to_pairs(term_var_anon, ModelIn, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, Model1),
    revar(Model1, Model, VarNames),
    maplist(rebind_non_anon, VarNames),
    term_variables(Model, Vars),
    rename(Vars, 0).

term_var_anon(Model, Key) :-
    mapsubterms(var_anon, Model, Key).

var_anon(V, '_') :-
    is_var(V, Name),
    sub_atom(Name, 0, _, _, 'Var').

rebind_non_anon(Name=_Var) :-
    sub_atom(Name, 0, _, _, 'Var'),
    !.
rebind_non_anon(Name=Var) :-
    Var = $Name.

rename([], _).
rename([H|T], I) :-
    H = $Name,
    atom_concat('Var', I, Name),
    I2 is I+1,
    rename(T, I2).

%!  pass_data(+TestFile, -PassFile, -PassData) is det.

pass_data(File, PassFile, PassData) :-
    file_name_extension(Base, _, File),
    file_name_extension(Base, pass, PassFile),
    (   exists_file(PassFile)
    ->  setup_call_cleanup(
            open(PassFile, read, In),
            read_term(In, PassData,
                      [ module(scasp_ops)
                      ]),
            close(In))
    ;   true
    ).

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

%!  test_files(+Argv, -Files) is det.

test_files([], Files) :-
    !,
    test_files(['../test'], Files).
test_files(Spec, Files) :-
    phrase(test_files(Spec), Files).

test_files([]) -->
    [].
test_files([Dir|T]) -->
    { exists_directory(Dir) },
    !,
    findall(File, dir_test_file(Dir,File)),
    test_files(T).
test_files([File|T]) -->
    { exists_file(File) },
    !,
    [File],
    test_files(T).
test_files([H|T]) -->
    { print_message(warning, error(existence_error(file, H),_)) },
    test_files(T).

dir_test_file(Dir, File) :-
    atom_concat(Dir, '/*.pl', Pattern),
    expand_file_name(Pattern, Files),
    member(File, Files).
