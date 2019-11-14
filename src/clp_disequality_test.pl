:- module(clp_disequality_test,_).


:- use_package(.(clp_disequality)).

:- use_module(library(terms_vars)).

run_test :-
    template(_A = 5),
    template(_B .\=. 4),
    template((_C .\=. 5, _C = 5)),
    template(p(1,2) .\=. p(_D1, _D2)),
    template((_E .\=. 25, _E .\=. 3, [_E,_E,_E] = [_E,_E,4])),
    template((_F .\=. 4, _F .\=. 5)),
    template(( _G .\=. 6, _G .\=. 5, _G = 4)),
    template((_H .\=. 6, _H .\=. 5, p(_H,3) .\=. p(4,_H))),
    template((_I1 .\=.3, _I2 .\=. 5, _I1 .\=. _I2)), 
    template((s(_K) = s(5))).

template(Goal) :-
    nl,print('--------------------------'),nl,
    copy_term(Goal, Copy),
    format('TEST ~w',Copy),nl,
    varset(Goal, Var),
    varset(Copy, CVar),
    (
        intercept(call(Goal),_,fail),
        display('Result: '),
        print_list(-(Var,CVar)),nl,
        fail
    ;
        \+ call(Goal),
        display('Result: \n\tfails'),nl
    ).
template(_).


print_list(-([],[])).
print_list(-([X|Xs],[Cx|Cxs])) :-
    nl, display('\t'),
    print(Cx),
    print(' is '),
    print(X),
    print_list(-(Xs,Cxs)).

    
