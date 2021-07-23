:- module(clp_disequality_test,
          [ run_test/0
          ]).

:- use_module(clp_disequality).

run_test :-
    template(_A = 5),
    template(_B .\=. 4),
    template((C .\=. 5, C = 5)),
    template(p(1,2) .\=. p(_D1, _D2)),
    template((E .\=. 25, E .\=. 3, [E,E,E] = [E,E,4])),
    template((F .\=. 4, F .\=. 5)),
    template(( G .\=. 6, G .\=. 5, G = 4)),
    template((H .\=. 6, H .\=. 5, p(H,3) .\=. p(4,H))),
    template((I1 .\=.3, I2 .\=. 5, I1 .\=. I2)),
    template((s(_K) = s(5))).

template(Goal) :-
    format('\n--------------------------\n'),
    copy_term(Goal, Copy),
    format('TEST ~w',Copy),nl,
    term_variables(Goal, Var),
    term_variables(Copy, CVar),
    (   intercept(call(Goal),_,fail),
        format('Result: '),
        print_list(-(Var,CVar)),nl,
        fail
    ;   \+ call(Goal),
        format('Result: \n\tfails\n')
    ).
template(_).


print_list(-([],[])).
print_list(-([X|Xs],[Cx|Cxs])) :-
    \+ \+ ( numbervars(Cx+X, 0, _, [attvar(skip)]),
            format('\n\t~p is ~p', [Cx, X])
          ),
    print_list(-(Xs,Cxs)).


