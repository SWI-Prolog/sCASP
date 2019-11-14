nqueens(N,Q) :-
    hide_nqueens(N,N,[],Q).

hide_nqueens(X,N,Qi,Qo) :-
    X>0,
    hide_pickqueen(X,Y,N),
    not(hide_attack(X,Y,Qi)),
    X1 is X-1,
    hide_nqueens(X1,N,[q(X,Y)|Qi],Qo).
hide_nqueens(0,_V1,Q,Q).

hide_pickqueen(X,Y,Y) :-
    Y>0,
    q(X,Y).
hide_pickqueen(X,Y,N) :-
    N>1,
    N1 is N-1,
    hide_pickqueen(X,Y,N1).

hide_attack(X,_V1,[q(X,_V2)|_V3]).
hide_attack(_V1,Y,[q(_V2,Y)|_V3]).
hide_attack(X,Y,[q(X2,Y2)|_V1]) :-
    Xd is X2-X,
    hide_abs(Xd,Xd2),
    Yd is Y2-Y,
    hide_abs(Yd,Yd2),
    Xd2=Yd2.
hide_attack(X,Y,[_V1|T]) :-
    hide_attack(X,Y,T).

q(X,Y) :-
    not(hide_negq(X,Y)).

hide_negq(X,Y) :-
    not(q(X,Y)).

hide_abs(X,X) :-
    X>=0.
hide_abs(X,Y) :-
    X<0,
    Y is X* -1.

not(_hide_negq1(X,Y)) :-
    q(X,Y).

not(hide_negq(_X0,_X1)) :-
    not(_hide_negq1(_X0,_X1)).

not(_hide_abs1(X,_X1)) :-
    _X1\=X.
not(_hide_abs1(X,_X1)) :-
    _X1=X,
    X<0.

not(_hide_abs2(X,Y)) :-
    X>=0.
not(_hide_abs2(X,Y)) :-
    X<0,
    not(Y is X* -1).

not(hide_abs(_X0,_X1)) :-
    not(_hide_abs1(_X0,_X1)),
    not(_hide_abs2(_X0,_X1)).

not(_q1(X,Y)) :-
    hide_negq(X,Y).

not(q(_X0,_X1)) :-
    not(_q1(_X0,_X1)).

not(_hide_attack1(X,_V1,_Z0,_V2,_V3)) :-
    _Z0\=[q(X,_V2)|_V3].

not(_hide_attack1(X,_V1,_Z0)) :-
    forall(_V2,forall(_V3,not(_hide_attack1(X,_V1,_Z0,_V2,_V3)))).

not(_hide_attack2(_V1,Y,_Z0,_V2,_V3)) :-
    _Z0\=[q(_V2,Y)|_V3].

not(_hide_attack2(_V1,Y,_Z0)) :-
    forall(_V2,forall(_V3,not(_hide_attack2(_V1,Y,_Z0,_V2,_V3)))).

not(_hide_attack3(X,Y,_Z0,X2,Y2,_V1,Xd,Xd2,Yd,Yd2)) :-
    _Z0\=[q(X2,Y2)|_V1].
not(_hide_attack3(X,Y,_Z0,X2,Y2,_V1,Xd,Xd2,Yd,Yd2)) :-
    _Z0=[q(X2,Y2)|_V1],
    not(Xd is X2-X).
not(_hide_attack3(X,Y,_Z0,X2,Y2,_V1,Xd,Xd2,Yd,Yd2)) :-
    _Z0=[q(X2,Y2)|_V1],
    Xd is X2-X,
    not(hide_abs(Xd,Xd2)).
not(_hide_attack3(X,Y,_Z0,X2,Y2,_V1,Xd,Xd2,Yd,Yd2)) :-
    _Z0=[q(X2,Y2)|_V1],
    Xd is X2-X,
    hide_abs(Xd,Xd2),
    not(Yd is Y2-Y).
not(_hide_attack3(X,Y,_Z0,X2,Y2,_V1,Xd,Xd2,Yd,Yd2)) :-
    _Z0=[q(X2,Y2)|_V1],
    Xd is X2-X,
    hide_abs(Xd,Xd2),
    Yd is Y2-Y,
    not(hide_abs(Yd,Yd2)).
not(_hide_attack3(X,Y,_Z0,X2,Y2,_V1,Xd,Xd2,Yd,Yd2)) :-
    _Z0=[q(X2,Y2)|_V1],
    Xd is X2-X,
    hide_abs(Xd,Xd2),
    Yd is Y2-Y,
    hide_abs(Yd,Yd2),
    Xd2\=Yd2.

not(_hide_attack3(X,Y,_Z0)) :-
    forall(X2,forall(Y2,forall(_V1,forall(Xd,forall(Xd2,forall(Yd,forall(Yd2,not(_hide_attack3(X,Y,_Z0,X2,Y2,_V1,Xd,Xd2,Yd,Yd2))))))))).

not(_hide_attack4(X,Y,_Z0,_V1,T)) :-
    _Z0\=[_V1|T].
not(_hide_attack4(X,Y,_Z0,_V1,T)) :-
    _Z0=[_V1|T],
    not(hide_attack(X,Y,T)).

not(_hide_attack4(X,Y,_Z0)) :-
    forall(_V1,forall(T,not(_hide_attack4(X,Y,_Z0,_V1,T)))).

not(hide_attack(_X0,_X1,_X2)) :-
    not(_hide_attack1(_X0,_X1,_X2)),
    not(_hide_attack2(_X0,_X1,_X2)),
    not(_hide_attack3(_X0,_X1,_X2)),
    not(_hide_attack4(_X0,_X1,_X2)).

not(_hide_pickqueen1(X,Y,_X2)) :-
    _X2\=Y.
not(_hide_pickqueen1(X,Y,_X2)) :-
    _X2=Y,
    Y=<0.
not(_hide_pickqueen1(X,Y,_X2)) :-
    _X2=Y,
    Y>0,
    not(q(X,Y)).

not(_hide_pickqueen2(X,Y,N,N1)) :-
    N=<1.
not(_hide_pickqueen2(X,Y,N,N1)) :-
    N>1,
    not(N1 is N-1).
not(_hide_pickqueen2(X,Y,N,N1)) :-
    N>1,
    N1 is N-1,
    not(hide_pickqueen(X,Y,N1)).

not(_hide_pickqueen2(X,Y,N)) :-
    forall(N1,not(_hide_pickqueen2(X,Y,N,N1))).

not(hide_pickqueen(_X0,_X1,_X2)) :-
    not(_hide_pickqueen1(_X0,_X1,_X2)),
    not(_hide_pickqueen2(_X0,_X1,_X2)).

not(_hide_nqueens1(X,N,Qi,Qo,Y,X1)) :-
    X=<0.
not(_hide_nqueens1(X,N,Qi,Qo,Y,X1)) :-
    X>0,
    not(hide_pickqueen(X,Y,N)).
not(_hide_nqueens1(X,N,Qi,Qo,Y,X1)) :-
    X>0,
    hide_pickqueen(X,Y,N),
    hide_attack(X,Y,Qi).
not(_hide_nqueens1(X,N,Qi,Qo,Y,X1)) :-
    X>0,
    hide_pickqueen(X,Y,N),
    not(hide_attack(X,Y,Qi)),
    not(X1 is X-1).
not(_hide_nqueens1(X,N,Qi,Qo,Y,X1)) :-
    X>0,
    hide_pickqueen(X,Y,N),
    not(hide_attack(X,Y,Qi)),
    X1 is X-1,
    not(hide_nqueens(X1,N,[q(X,Y)|Qi],Qo)).

not(_hide_nqueens1(X,N,Qi,Qo)) :-
    forall(Y,forall(X1,not(_hide_nqueens1(X,N,Qi,Qo,Y,X1)))).

not(_hide_nqueens2(_X0,_V1,Q,_X3)) :-
    _X0\=0.
not(_hide_nqueens2(_X0,_V1,Q,_X3)) :-
    _X0=0,
    _X3\=Q.

not(hide_nqueens(_X0,_X1,_X2,_X3)) :-
    not(_hide_nqueens1(_X0,_X1,_X2,_X3)),
    not(_hide_nqueens2(_X0,_X1,_X2,_X3)).

not(_nqueens1(N,Q)) :-
    not(hide_nqueens(N,N,[],Q)).

not(nqueens(_X0,_X1)) :-
    not(_nqueens1(_X0,_X1)).

not(_false).

_nmr_check.

NMR Check:
[_nmr_check]

Query
[nqueens(4,Q)]
