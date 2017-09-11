pr_rule(nqueens(N,Q),[hide_nqueens(N,N,[],Q)]).
pr_rule(hide_nqueens(X,N,Qi,Qo),[X>0,hide_pickqueen(X,Y,N),not(hide_attack(X,Y,Qi)),X1 is X-1,hide_nqueens(X1,N,[q(X,Y)|Qi],Qo)]).
pr_rule(hide_nqueens(0,_V1,Q,Q),[]).
pr_rule(hide_pickqueen(X,Y,Y),[Y>0,q(X,Y)]).
pr_rule(hide_pickqueen(X,Y,N),[N>1,N1 is N-1,hide_pickqueen(X,Y,N1)]).
pr_rule(hide_attack(X,_V1,[q(X,_V3)|_V5]),[]).
pr_rule(hide_attack(_V1,Y,[q(_V3,Y)|_V5]),[]).
pr_rule(hide_attack(X,Y,[q(X2,Y2)|_V2]),[Xd is X2-X,hide_abs(Xd,Xd2),Yd is Y2-Y,hide_abs(Yd,Yd2),Xd2=Yd2]).
pr_rule(hide_attack(X,Y,[_V2|T]),[hide_attack(X,Y,T)]).
pr_rule(q(X,Y),[not(hide_negq(X,Y))]).
pr_rule(hide_negq(X,Y),[not(q(X,Y))]).
pr_rule(hide_abs(X,X),[X>=0]).
pr_rule(hide_abs(X,Y),[X<0,Y is X* -1]).
pr_rule(not(o_hide_negq1(X,Y)),[q(X,Y)]).
pr_rule(not(hide_negq(_X0,_X1)),[not(o_hide_negq1(_X0,_X1))]).
pr_rule(not(o_hide_abs1(X,_X1)),[_X1 .\=. X]).
pr_rule(not(o_hide_abs1(X,_X1)),[_X1=X,X<0]).
pr_rule(not(o_hide_abs2(X,Y)),[X>=0]).
pr_rule(not(o_hide_abs2(X,Y)),[X<0,not(Y is X* -1)]).
pr_rule(not(hide_abs(_X0,_X1)),[not(o_hide_abs1(_X0,_X1)),not(o_hide_abs2(_X0,_X1))]).
pr_rule(not(o_q1(X,Y)),[hide_negq(X,Y)]).
pr_rule(not(q(_X0,_X1)),[not(o_q1(_X0,_X1))]).
pr_rule(not(o_hide_attack1(X,_V1,_Z0,_V3,_V5)),[_Z0 .\=. [q(X,_V3)|_V5]]).
pr_rule(not(o_hide_attack1(X,_V1,_Z0)),[forall(_V3,forall(_V5,not(o_hide_attack1(X,_V1,_Z0,_V3,_V5))))]).
pr_rule(not(o_hide_attack2(_V1,Y,_Z0,_V3,_V5)),[_Z0 .\=. [q(_V3,Y)|_V5]]).
pr_rule(not(o_hide_attack2(_V1,Y,_Z0)),[forall(_V3,forall(_V5,not(o_hide_attack2(_V1,Y,_Z0,_V3,_V5))))]).
pr_rule(not(o_hide_attack3(X,Y,_Z0,X2,Y2,_V2,Xd,Xd2,Yd,Yd2)),[_Z0 .\=. [q(X2,Y2)|_V2]]).
pr_rule(not(o_hide_attack3(X,Y,_Z0,X2,Y2,_V2,Xd,Xd2,Yd,Yd2)),[_Z0=[q(X2,Y2)|_V2],not(Xd is X2-X)]).
pr_rule(not(o_hide_attack3(X,Y,_Z0,X2,Y2,_V2,Xd,Xd2,Yd,Yd2)),[_Z0=[q(X2,Y2)|_V2],Xd is X2-X,not(hide_abs(Xd,Xd2))]).
pr_rule(not(o_hide_attack3(X,Y,_Z0,X2,Y2,_V2,Xd,Xd2,Yd,Yd2)),[_Z0=[q(X2,Y2)|_V2],Xd is X2-X,hide_abs(Xd,Xd2),not(Yd is Y2-Y)]).
pr_rule(not(o_hide_attack3(X,Y,_Z0,X2,Y2,_V2,Xd,Xd2,Yd,Yd2)),[_Z0=[q(X2,Y2)|_V2],Xd is X2-X,hide_abs(Xd,Xd2),Yd is Y2-Y,not(hide_abs(Yd,Yd2))]).
pr_rule(not(o_hide_attack3(X,Y,_Z0,X2,Y2,_V2,Xd,Xd2,Yd,Yd2)),[_Z0=[q(X2,Y2)|_V2],Xd is X2-X,hide_abs(Xd,Xd2),Yd is Y2-Y,hide_abs(Yd,Yd2),Xd2 .\=. Yd2]).
pr_rule(not(o_hide_attack3(X,Y,_Z0)),[forall(X2,forall(Y2,forall(_V2,forall(Xd,forall(Xd2,forall(Yd,forall(Yd2,not(o_hide_attack3(X,Y,_Z0,X2,Y2,_V2,Xd,Xd2,Yd,Yd2)))))))))]).
pr_rule(not(o_hide_attack4(X,Y,_Z0,_V2,T)),[_Z0 .\=. [_V2|T]]).
pr_rule(not(o_hide_attack4(X,Y,_Z0,_V2,T)),[_Z0=[_V2|T],not(hide_attack(X,Y,T))]).
pr_rule(not(o_hide_attack4(X,Y,_Z0)),[forall(_V2,forall(T,not(o_hide_attack4(X,Y,_Z0,_V2,T))))]).
pr_rule(not(hide_attack(_X0,_X1,_X2)),[not(o_hide_attack1(_X0,_X1,_X2)),not(o_hide_attack2(_X0,_X1,_X2)),not(o_hide_attack3(_X0,_X1,_X2)),not(o_hide_attack4(_X0,_X1,_X2))]).
pr_rule(not(o_hide_pickqueen1(X,Y,_X2)),[_X2 .\=. Y]).
pr_rule(not(o_hide_pickqueen1(X,Y,_X2)),[_X2=Y,Y=<0]).
pr_rule(not(o_hide_pickqueen1(X,Y,_X2)),[_X2=Y,Y>0,not(q(X,Y))]).
pr_rule(not(o_hide_pickqueen2(X,Y,N,N1)),[N=<1]).
pr_rule(not(o_hide_pickqueen2(X,Y,N,N1)),[N>1,not(N1 is N-1)]).
pr_rule(not(o_hide_pickqueen2(X,Y,N,N1)),[N>1,N1 is N-1,not(hide_pickqueen(X,Y,N1))]).
pr_rule(not(o_hide_pickqueen2(X,Y,N)),[forall(N1,not(o_hide_pickqueen2(X,Y,N,N1)))]).
pr_rule(not(hide_pickqueen(_X0,_X1,_X2)),[not(o_hide_pickqueen1(_X0,_X1,_X2)),not(o_hide_pickqueen2(_X0,_X1,_X2))]).
pr_rule(not(o_hide_nqueens1(X,N,Qi,Qo,Y,X1)),[X=<0]).
pr_rule(not(o_hide_nqueens1(X,N,Qi,Qo,Y,X1)),[X>0,not(hide_pickqueen(X,Y,N))]).
pr_rule(not(o_hide_nqueens1(X,N,Qi,Qo,Y,X1)),[X>0,hide_pickqueen(X,Y,N),hide_attack(X,Y,Qi)]).
pr_rule(not(o_hide_nqueens1(X,N,Qi,Qo,Y,X1)),[X>0,hide_pickqueen(X,Y,N),not(hide_attack(X,Y,Qi)),not(X1 is X-1)]).
pr_rule(not(o_hide_nqueens1(X,N,Qi,Qo,Y,X1)),[X>0,hide_pickqueen(X,Y,N),not(hide_attack(X,Y,Qi)),X1 is X-1,not(hide_nqueens(X1,N,[q(X,Y)|Qi],Qo))]).
pr_rule(not(o_hide_nqueens1(X,N,Qi,Qo)),[forall(Y,forall(X1,not(o_hide_nqueens1(X,N,Qi,Qo,Y,X1))))]).
pr_rule(not(o_hide_nqueens2(_X0,_V1,Q,_X3)),[_X0 .\=. 0]).
pr_rule(not(o_hide_nqueens2(_X0,_V1,Q,_X3)),[_X0=0,_X3 .\=. Q]).
pr_rule(not(hide_nqueens(_X0,_X1,_X2,_X3)),[not(o_hide_nqueens1(_X0,_X1,_X2,_X3)),not(o_hide_nqueens2(_X0,_X1,_X2,_X3))]).
pr_rule(not(o_nqueens1(N,Q)),[not(hide_nqueens(N,N,[],Q))]).
pr_rule(not(nqueens(_X0,_X1)),[not(o_nqueens1(_X0,_X1))]).
pr_rule(not(o_false),[]).
pr_rule(not(o_false),[]).
pr_rule(o_nmr_check,[]).
pr_rule(add_to_query,[o_nmr_check]).
