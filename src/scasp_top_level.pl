:- module(scasp_top_level,[
    main/2,
    binding/2
]).


:- use_module(scasp).


main(Args, [query(Query), answer(Answer), bindings(PVars, Bindings), model(Model)]) :-
    
    scasp_exec(Args, [Query, Answer, PVars, Bindings, Model]).


binding(Args, Bindings) :-
    scasp_exec(Args, [_Query, _Answer, _PVars, Bindings, _Model]).