%Code for modeling timed automata using CLP(R).
%Adapted from "A Constraint based Approach for 
%Specification and Verification for Real-time Systems"
%by G. Gupta and E. Pontelli. Realtime Systems Symposium.
%1997. 

%Copyright Gopal Gupta and Enrico Pontelli 1997, 2003.
%Please leave the above message intact, if you modify
%this code. Please do not distribute without authors'
%permission.

:- use_package(clpr).
:- push_prolog_flag(quiet, on).

train(s0, approach, s1, T1, T2, T3) :-
    T3 .=. T1.
train(s1, inn, s2, T1, T2, T3) :-
    T1 .>. T2 + 2,
    T3 .=. T2.
train(s2, out,  s3, T1, T2, T3) :-
    T3 .=. T2.
train(s3, exit, s0, T1, T2, T3) :-
    T3 .=. T2,
    T1 .<. T2 + 5.
train(X, lower, X, T1, T2, T2).
train(X, down,  X, T1, T2, T2).
train(X, raise, X, T1, T2, T2).
train(X, up,    X, T1, T2, T2).

gate(s0, lower, s1, T1, T2, T1).
gate(s1, down,  s2, T1, T2, T3) :-
    T3 .=. T2,
    T1 .<. T2 + 1.
gate(s2, raise, s3, T1, T2, T3) :-
    T3 .=. T1.
gate(s3, up, s0, T1, T2, T3) :-
    T3 .=. T2,
    T1 .>. T2 + 1,
    T1 .<. T2 + 2.
gate(X, approach, X, T1, T2, T2).
gate(X, inn,       X, T1, T2, T2).
gate(X, out,      X, T1, T2, T2).
gate(X, exit,     X, T1, T2, T2).



contr(s0, approach, s1, T1, T2, T1).
contr(s1, lower,    s2, T1, T2, T3) :-
    T3 .=. T2,
    T1 .=. T2 + 1.
contr(s2, exit,  s3, T1, T2, T1).
contr(s3, raise, s0, T1, T2, T2) :-
    T1 .<. T2 + 1.
contr(X, inn,   X, T1, T2, T2).
contr(X, out,  X, T1, T2, T2).
contr(X, up,   X, T1, T2, T2).
contr(X, down, X, T1, T2, T2).


driver(N, S0, S1, S2, T, T0, T1, T2, [X|Rest], [f(X, T)|R]) :-
    train(S0, X, S00, T, T0, T00),
    gate(S1, X, S10, T, T1, T10),
    contr(S2, X, S20, T, T2, T20),
    TA .>. T,
    X = approach, N = 0,
    driver(1, S00, S10, S20, TA, T00, T10, T20, Rest, R).

driver(N, S0, S1, S2, T, T0, T1, T2, [X|Rest], [f(X, T)|R]) :-
    N > 0,
    train(S0, X, S00, T, T0, T00),
    gate(S1, X, S10, T, T1, T10),
    contr(S2, X, S20, T, T2, T20),
    TA .>. T,
    X \= approach,
    driver(N, S00, S10, S20, TA, T00, T10, T20, Rest, R).

driver(N, S0, S1, S2, T, T0, T1, T2, [X|Rest], [f(X, T)|R]) :-
    train(S0, X, S00, T, T0, T00),
    gate(S1, X, S10, T, T1, T10),
    contr(S2, X, S20, T, T2, T20),
    TA .>. T,
    X = approach, R = [], Rest = [], N > 0.


append([],    L, L).
append([X|A], B, [X|C]) :- append(A, B, C).

%---------------------------------------------------------------
%Property 1: check that the gate will always be "down" when the
%train is "in" subject to the timing constraints. A call to
% "downbeforein" will produce the answer "no" since the property
% is negated in the query.

downbeforein(X) :- driver(0, s0, s0, s0, 0, 0, 0, 0, X, R),
    append(A, [down|_], X), append(_, [inn|_], A).
%---------------------------------------------------------------


%---------------------------------------------------------------
%Property 2: check that the gate can never be up more than 10 units 
%of time.  A call to downmorethan10 will produce the answer "no" 
%since the property is negated in the query.

downmorethan10 :- driver(0, s0, s0, s0, 0, 0, 0, 0, X, R),
    append(A, [f(up, T2)|_], R), append(_, [f(down, T1)|_], A),
    10 .<. T2 - T1.
%---------------------------------------------------------------


%---------------------------------------------------------------
%Property 3: Find the upper and lower bounds on the time the gate
%can be down (assuming there is only one train). Variables M and N 
%denote the Max time and the Min time. More precisely this query 
%will find the upper and lower bounds on the times at which 
%two approach signals can occur with no other intervening approach 
%signal. Calling "findrange(M,N)" on sicstus will produce:
%           N < 7,  M > 1, M > N
% This means that the largest value that difference of 
% T2 and T1 can have is 7, while the smallest value it can
% have is larger than 1.

findrange(M, N, X) :- driver(0, s0, s0, s0, 0, 0, 0, 0, X, R),
    append(A, [f(up, T2)|_], R), append(_, [f(down, T1)|_], A),
    N .<. T2 - T1, T2 .<. T1 + M, M .>. 0, N .>. 0.
%---------------------------------------------------------------
    
