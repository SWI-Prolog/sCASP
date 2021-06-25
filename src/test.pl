:- module(test,[main/1, generate/0]).
:- use_module(diff).


:- use_module(scasp).


:- op(700, xfx, ['#=',
            '#<>',
            '#<',
            '#>',
            '#=<',
            '#>='
        ]).

:- op(700, xfx, ['| ']). %% such as

:- op(700, fx, [not,'$']). %% such as


:- include(test_results).
%list_tests(_).

main([]) :-
    list_tests(Tests),
    test(Tests,St),
    end(St).

main([Test]) :-
    list_tests(Tests),
    member(Test=Result,Tests),
    test([Test=Result], St),
    end(St).

end(Var) :- var(Var), !.
end(fail) :- fail.

test([],_).
test([F=R0|Ts],St1) :-
    statistics(runtime, _),
    scasp_test([F], Result0),
    statistics(runtime, [_,Used]),
    strip_vars(R0, R),
    strip_vars(Result0, Result),
    ( R =@= Result ->
        format("~p \tpassed ~dms\n", [F,Used]),
        St1 = St0
    ;
        format("~p \tfailed ~dms\n", [F,Used]),
        diff_terms(R, Result),
        St1 = fail
    ),
    test(Ts,St0).

strip_vars($(X), X) :-
    !.
strip_vars(T0, T) :-
    compound(T0),
    !,
    T0 =.. [Name|Args0],
    maplist(strip_vars, Args0, Args),
    T =.. [Name|Args].
strip_vars(X, X).


list_files([
    '../test/pq.pl',
    '../test/vars.pl',
    '../test/birds.pl',
    '../test/family.pl',
    '../test/hamcycle.pl',
    '../test/hamcycle_two.pl',
    '../test/hanoi.pl',
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


