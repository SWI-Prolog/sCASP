
final(s0).
final(s2).

path(A,B,R) :-
    A = s(s0,0,_), B = s(F,5,_),
    reach(A,B,R),
    final(F).

reach(A,B,[A-Action,B]) :-
    trans(A,B,Action).
reach(A,B,[A-Action|R]) :-
    trans(A,Z,Action),
    reach(Z,B,R).

trans(s(s0,T0, C0), s(s1, T1, C1), drop(T1)) :-
    T1 .>=. T0 + 1,
    drop(T1),
    C1 .=. 0.
trans(s(s1,T1, C1), s(s0, T00, C00), catch(T00)) :-
    T00 .>=. T1 + 1,
    C00 .=. T00 - T1,
    catch(T00),
    C00 .<. 2.
trans(s(s1,T1, C1), s(s2, T2, C2), wait(T2)) :-
    T2 .>=. T1 + 1,
    C2 .=. T2 - T1,
    not catch(T2),
    C2 .>=. 2.

catch(T) :- not wait(T).
wait(T) :- not catch(T).

drop(T) :- not negdrop(T).
negdrop(T) :- not drop(T).


p(A, B, R) :- catch(T), path(A,B,R), T < 5.



s:- r.
r:- p(1), p(1).

p(1).
