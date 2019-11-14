:- module(new_hanoi_3,_).

:- dynamic size/1.

main([S|_]) :-
    atom_chars(S,Ns),number_chars(Size,Ns),
    retractall(size(_)),    
    assert(size(Size)),
    query(Size, T, _),nl,
    display(moves(T)),nl,nl.

query(S, T, L) :-
    statistics(runtime,_),
    hanoi(S, T, L),
    statistics(runtime,[_|Time]),nl,
    display(runtime(Time)),nl.

hanoi(S, T,List) :-
    gen_list(1,S,Tower),
    holds(0,T,st(Tower, [],[]),st([],[],Tower),[],List).


holds(T,T,S,S,Ac,[S|Ac]).
holds(T1, T3, St1, St2, Ac, Res) :-
    trans(St1, StX),
    \+ member(StX, Ac),
    T2 is T1 + 1,
    holds(T2, T3, StX, St2, [St1|Ac], Res).



trans(st([A|As],Bs,Cs), st(As,[A|Bs],Cs)) :-
    \+ bigger_in_B([A|Bs]),
    move(A,Bs).
trans(st([A|As],Bs,Cs), st(As,Bs,[A|Cs])) :-
    move(A,Cs).

trans(st(As,[B|Bs],Cs), st([B|As],Bs,Cs)) :-
    move(B,As).
trans(st(As,[B|Bs],Cs), st(As,Bs,[B|Cs])) :-
    move(B,Cs).

trans(st(As,Bs,[C|Cs]), st([C|As],Bs,Cs)) :-
    \+ correct_([C|Cs]),
    move(C,As).
trans(st(As,Bs,[C|Cs]), st(As,[C|Bs],Cs)) :-
    \+ bigger_in_B([C|Bs]),
    \+ correct_([C|Cs]),
    move(C,Bs).

correct_([F|Rs]) :-
    size(S),
    gen_list(F,S,[F|Rs]).

bigger_in_B([S]) :-
    size(S).

move(_,[]).
move(E,[O|_]) :-
    E < O.

gen_list(N,N,[N]).
gen_list(I,N,[I|L]) :-
    I < N,
    I1 is I + 1,
    gen_list(I1,N,L).
