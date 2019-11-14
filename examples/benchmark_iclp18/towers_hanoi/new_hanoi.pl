



query(T,L) :-
    T .<. 50, hanoi(T,L).

size(5).
stack(Stack) :- size(N), gen_list(1,N,Stack).

:- use_package(clpq).

hanoi(T,List) :-
    stack(Stack),
    holds(T,st([],[],Stack),List).

init(st(Stack,[],[])) :-
    stack(Stack).

holds(0,St,[St-0]) :-
    init(St).
holds(T1, St1,[St1-T1|Prev]) :-
    T1 .>. 0,
    T1 .=. T0 + 1,
    holds(T0, St0, Prev),
    trans(St0, St1),
    \+ member(St1-T,Prev).

trans(st(A0,B0,C0), st(A1,B1,C1)) :-
    trans_(A0,A1,B0,B1,C0,C1).
trans(st(A0,B0,C0), st(A1,B1,C1)) :-
    trans_(B0,B1,C0,C1,A0,A1).
trans(st(A0,B0,C0), st(A1,B1,C1)) :-
    trans_(C0,C1,A0,A1,B0,B1).

trans_([A|As],As,B,[A|B],C,C) :-
    not_invalid(A,B).
trans_([A|As],As,B,B,C,[A|C]) :-
    not_invalid(A,C).

% invalid(E,[O|_]) :-
%       E .>. O.

not_invalid(_,[]).
not_invalid(E,[O|_]) :-
    E .<. O.

gen_list(N,N,[N]).
gen_list(I,N,[I|L]) :-
    I < N,
    I1 is I + 1,
    gen_list(I1,N,L).
