



query(S, T, L, U) :-
    T .>=. 0, hanoi(S, T,L).

% size(7).
% stack(S, Stack) :- gen_list(1,S,Stack).

:- use_package(clpq).

hanoi(S, T,List) :-
    gen_list(1,S,Stack),
    %stack(S, Stack),
    holds(T,st(Stack, [],[]),st([],[],Stack),[],List).

% init(st(Stack,[],[])) :-
%       stack(Stack).

holds(0,S,S,Ac,[S|Ac]):- \+ member(S, Ac).
holds(T1, St1, St2, Ac, Res) :-
    T0 .>=. 0,
    T1 .=. T0 + 1,
    \+ member(St1, Ac),
    trans(St1, StX),
    holds(T0, StX, St2, [St1|Ac], Res).



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
