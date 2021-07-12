:- module(test_scasp,
          [ test_scasp/0,
            qtest_scasp/0
          ]).
:- use_module(scasp, [scasp_test/2]).
:- use_module(library(lists), [member/2]).
:- use_module(library(main), [main/0, argv_options/3]).
:- use_module(library(option), [option/3, option/2]).
:- use_module(library(time), [call_with_time_limit/2]).

:- include(public_ops).
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
%     | --timeout=Secs | Run tests with timeout (default 60) |
%     | --save         | Save result if no .pass file exists |
%     | --overwrite    | Overwrite .pass after we passed     |
%     | --pass         | Overwrite .pass after we failed     |
%
%   Default runs tests from `../test`

main(Argv) :-
    argv_options(Argv, Positional, Options),
    test_files(Positional, Files),
    run_tests(Files, Failed, Options),
    (   Failed == 0
    ->  format(user_error, 'All tests passed!~n', [])
    ;   format(user_error, '~D tests failed~n', [Failed]),
        halt(1)
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
    ->  format("different stacks, same models ~|~t~d ms~8+\n", [Used])
    ;   format("FAILED ~|~t~d ms~8+\n", [Used]),
        (   option(pass(true), Options)
        ->  save_test_data(PassFile, Result)
        ;   option(show_diff(true), Options)
        ->  diff_terms(PassStacks, Stacks)
        ),
        fail
    ).

pass_data(File, PassFile, PassData) :-
    file_name_extension(Base, _, File),
    file_name_extension(Base, pass, PassFile),
    (   exists_file(PassFile)
    ->  setup_call_cleanup(
            open(PassFile, read, In),
            read_term(In, PassData, []),
            close(In))
    ;   true
    ).

save_test_data(Into, Result) :-
    setup_call_cleanup(
        open(Into, write, Out),
        format(Out, '~q.~n', [Result]),
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
