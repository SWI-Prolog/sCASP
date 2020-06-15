:- module(test,[main/1, generate/0]).



:- use_module(scasp).


:- op(700, xfx, ['#=',
            '#<>',
            '#<',
            '#>',
            '#=<',
            '#>='
        ]).

:- op(700, xfx, ['│']). %% such as

:- op(700, fx, [not,'$']). %% such as


%:- include(test_results).
list_tests(_).

main(_) :-
    list_tests(Tests),
    test(Tests).

test([]).
test([F=R|Ts]) :-
    scasp_test([F], Result),
%    display(a(R,Result)),nl,nl,
    ( R = Result ->
        format("~p \tpassed\n", [F])
    ;
        format("~p \tfailed\n", [F])
    ),
    test(Ts).


list_files([
    '../test/pq.pl',
    '../test/vars.pl',
    '../test/birds.pl',
    '../test/family.pl',
    '../test/hamcycle.pl',
    '../test/hamcycle_two.pl',
    '../test/hanoi.pl',
    '../test/pq.pl',
    '../test/queens.pl',
    '../test/classic_negation_incostistent.pl',
    '../test/bec_light.pl'
]).

generate :-
    list_files(Files),
    generate_test(Files).

generate_test([]).
generate_test([F|Ts]) :-
    scasp_test([F], Result),
    format("\t\'~w\' = \n\t~q,\n", [F, Result]),
    generate_test(Ts).


