:- module(test,[main/1, generate/0]).



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
test([F=R|Ts],St1) :-
    statistics(runtime, _),
    scasp_test([F], Result),
    statistics(runtime, [_,Used]),
    ( cmp(R, Result) ->
        format("~p \tpassed ~dms\n", [F,Used]),
        St1 = St0
    ;
        format("~p \tfailed ~dms\n", [F, Used]),
        St1 = fail
    ),
    test(Ts,St0).

cmp(X,X) :- !.
cmp($(Var), Var) :- !.
cmp(Var, $(Var)) :- !.
cmp(C1, C2) :-
    compound(C1), compound(C2),
    forall((arg(I,C1,A1),
            arg(I,C2,A2)),
           cmp(A1,A2)).



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


