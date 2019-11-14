
:- use_package(tabling).
:- use_package(t_clpq).
% :- push_prolog_flag(quiet, on).

print_const(X) :-
    display(X),
    dump_constraints(X,X,C),
    display(C).

final(s0).
final(s2).

path(A,B,R) :-
    A = s(s0,_,_), B = s(F,_,_),
    reach(A,B,R),
%       print_const(R),nl,
    final(F).

:- table reach/3.
reach(A,B,[A-Action,B]) :-
    trans(A,B,Action).
reach(A,B,[A-Action|R]) :-
    trans(A,Z,Action),
    reach(Z,B,R).

trans(s(s0,T0, _C0), s(s1, T1, C1), drop(T1)) :-
    T1 .>=. T0 + 1,
    drop(T1),
    C1 .=. 0.
trans(s(s1,T1, _C1), s(s0, T00, C00), catch(T00)) :-
    T00 .>=. T1 + 1,
    C00 .=. T00 - T1,
    catch(T00),
    C00 .<. 2.
trans(s(s1,T1, _C1), s(s2, T2, C2), wait(T2)) :-
    T2 .>=. T1 + 1,
    C2 .=. T2 - T1,
    not catch(T2),
    C2 .>=. 2.

catch(T) :- not negcatch(T).
negcatch(T) :- not catch(T).


run :-
    path(s(s0,0,0),s(_S,11,_C),R), T2 .=. T1 + 5, member(s(s0,T1,_)-drop(T2),R).

% hold(T) :- T .>. 0.
% hold(T) :- T .<. Td, drop(Td), not falling(T), not broken(T).
% falling(T) :- not hold(T), not broken(T).
% broken(T) :- not hold(T), not falling(T).

% drop(2).
%drop(T) :- T1 .<. T, T .<. T2, hold(T1), falling(T2).




% hold(T) :- T .=. 0.
% hold(T1) :-
%       T1 .>. Ti,
%       T1 .<. Td,
%       hold(Ti),
%       drop(Td).



% findrange(M, N, X) :- driver(0, s0, 0, 0, X, R),
%       append(A, [f(catch, T2)|_], R), append(_, [f(drop, T1)|_], A),
%       N .<. T2 - T1, T2 .<. T1 + M, M .>. 0, N .>. 0.

% append([],    L, L).
% append([X|A], B, [X|C]) :- append(A, B, C).

% ball(s0, drop, s1, T1, T2, T3) :-
%       T3 .=. T1.
% ball(s1, catch, s0, T1, T2, T3) :-
%       T1 .=<. T2 + 1,
%       T3 .=. T2.
% ball(s1, wait, s2, T1, T2, T3) :-
%       T1 .>. T2 + 1,
%       T3 .=. T2.

% driver(0,s0,T,T0,[X|Rst], [f(X,T)|R]) :-
%       ball(S0,X,S00,T,T0,T00),
% %     action(X,T),
%       TA .>. T,
% %     X = drop, N = 0,
%       driver(1, S00,TA,T00,Rst,R).

% driver(N,S0,T,T0,[X|Rst], [f(X,T)|R]) :-
%       N > 0,
%       ball(S0,X,S00,T,T0,T00),
% %     action(X,T),
%       TA .>. T,
%       X \= drop, 
%       driver(N, S00,TA,T00,Rst,R).

% driver(N,S0,T,T0,[X|Rst], [f(X,T)|R]) :-
%       ball(S0,X,S00,T,T0,T00),
% %     action(X,T),
%       TA .>. T,
%       (X = drop ; X = wait),
%        R = [], Rst = [], N>0.




% hold(T) :-
%       T1 .<. T,
%       T .<. T2,
%       init(T1),
%       drop(T2).







% drop(T,T1,T2) :-
%       T1 .<. T2,
%       holding(T1,T),
%       not holding(T,T2).

% holding(0,5).

% not_holding(T,D) :- not holding(T,D).

% :- holding(T,D), not_holding(T,D).




% :- use_package(clpr).
% :- op(700, fx,  [(not)]). 

% n1(T) :-
%       holding(T),
%       not falling(T),
%       not broken(T).
% n2(T) :-
%       not holding(T),
%       falling(T),
%       not broken(T).
% n3(T) :-
%       not holding(T),
%       not falling(T),
%       broken(T).

% edge(n1(T1),n2(T2),T1,T2) :-
%       T .>. T1 + 4,
%       drop(T),
% %     T2 .>. T1,
%       T1 .<. T,
%       T2 .>=. T.

% edge(n2(T1), n1(T2),T1,T2) :-
%       catch(T),
% %     T2 .>. T1,
%       T1 .<. T,
%       T2 .>=. T,      
%       T2 .<. T + 1.
% edge(n2(T1), n3(T2),T1,T2) :-
%       not catch(T),
% %     T2 .>. T1,
%       T1 .<. T,
%       T2 .>=. T,      
%       T2 .>=. T + 1.

% trans(S1,S2,T1,T2) :-
%       edge(S1,S2,T1,T2).

% trans(S1,S2,T1,T2) :-
%       T2 .>. T1,
%       edge(S1,Si,T1,Ti),
%       trans(Si,S2,Ti,T2).
    
