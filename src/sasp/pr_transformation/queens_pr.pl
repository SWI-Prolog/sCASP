pr_rule(nqueens(N,Q),[_nqueens(N,N,[],Q)]).
pr_rule(_nqueens(X,N,Qi,Qo),[X>0,_pickqueen(X,Y,N),not(_attack(X,Y,Qi)),X1 is X-1,_nqueens(X1,N,[q(X,Y)|Qi],Qo)]).
pr_rule(_nqueens(0,_V1,Q,Q),[]).
pr_rule(_pickqueen(X,Y,Y),[Y>0,q(X,Y)]).
pr_rule(_pickqueen(X,Y,N),[N>1,N1 is N-1,_pickqueen(X,Y,N1)]).
pr_rule(_attack(X,_V1,[q(X,_V3)|_V5]),[]).
pr_rule(_attack(_V1,Y,[q(_V3,Y)|_V5]),[]).
pr_rule(_attack(X,Y,[q(X2,Y2)|_V2]),[Xd is X2-X,_abs(Xd,Xd2),Yd is Y2-Y,_abs(Yd,Yd2),Xd2=Yd2]).
pr_rule(_attack(X,Y,[_V2|T]),[_attack(X,Y,T)]).
pr_rule(q(X,Y),[not(_negq(X,Y))]).
pr_rule(_negq(X,Y),[not(q(X,Y))]).
pr_rule(_abs(X,X),[X>=0]).
pr_rule(_abs(X,Y),[X<0,Y is X* -1]).
pr_rule(not(o_d__negq1(X,Y)),[q(X,Y)]).
pr_rule(not(_negq(_X0,_X1)),[not(o_d__negq1(_X0,_X1))]).
pr_rule(not(o_d__abs1(X,_X1)),[_X1\=X]).
pr_rule(not(o_d__abs1(X,_X1)),[_X1=X,X<0]).
pr_rule(not(o_d__abs2(X,Y)),[X>=0]).
pr_rule(not(o_d__abs2(X,Y)),[X<0,not(Y is X* -1)]).
pr_rule(not(_abs(_X0,_X1)),[not(o_d__abs1(_X0,_X1)),not(o_d__abs2(_X0,_X1))]).
pr_rule(not(o_q1(X,Y)),[_negq(X,Y)]).
pr_rule(not(q(_X0,_X1)),[not(o_q1(_X0,_X1))]).
pr_rule(not(o_d__attack1(X,_V1,_Z0,_V3,_V5)),[_Z0\=[q(X,_V3)|_V5]]).
pr_rule(not(o_d__attack1(X,_V1,_Z0)),[forall(_V3,forall(_V5,not(o_d__attack1(X,_V1,_Z0,_V3,_V5))))]).
pr_rule(not(o_d__attack2(_V1,Y,_Z0,_V3,_V5)),[_Z0\=[q(_V3,Y)|_V5]]).
pr_rule(not(o_d__attack2(_V1,Y,_Z0)),[forall(_V3,forall(_V5,not(o_d__attack2(_V1,Y,_Z0,_V3,_V5))))]).
pr_rule(not(o_d__attack3(X,Y,_Z0,X2,Y2,_V2,Xd,Xd2,Yd,Yd2)),[_Z0\=[q(X2,Y2)|_V2]]).
pr_rule(not(o_d__attack3(X,Y,_Z0,X2,Y2,_V2,Xd,Xd2,Yd,Yd2)),[_Z0=[q(X2,Y2)|_V2],not(Xd is X2-X)]).
pr_rule(not(o_d__attack3(X,Y,_Z0,X2,Y2,_V2,Xd,Xd2,Yd,Yd2)),[_Z0=[q(X2,Y2)|_V2],Xd is X2-X,not(_abs(Xd,Xd2))]).
pr_rule(not(o_d__attack3(X,Y,_Z0,X2,Y2,_V2,Xd,Xd2,Yd,Yd2)),[_Z0=[q(X2,Y2)|_V2],Xd is X2-X,_abs(Xd,Xd2),not(Yd is Y2-Y)]).
pr_rule(not(o_d__attack3(X,Y,_Z0,X2,Y2,_V2,Xd,Xd2,Yd,Yd2)),[_Z0=[q(X2,Y2)|_V2],Xd is X2-X,_abs(Xd,Xd2),Yd is Y2-Y,not(_abs(Yd,Yd2))]).
pr_rule(not(o_d__attack3(X,Y,_Z0,X2,Y2,_V2,Xd,Xd2,Yd,Yd2)),[_Z0=[q(X2,Y2)|_V2],Xd is X2-X,_abs(Xd,Xd2),Yd is Y2-Y,_abs(Yd,Yd2),Xd2\=Yd2]).
pr_rule(not(o_d__attack3(X,Y,_Z0)),[forall(X2,forall(Y2,forall(_V2,forall(Xd,forall(Xd2,forall(Yd,forall(Yd2,not(o_d__attack3(X,Y,_Z0,X2,Y2,_V2,Xd,Xd2,Yd,Yd2)))))))))]).
pr_rule(not(o_d__attack4(X,Y,_Z0,_V2,T)),[_Z0\=[_V2|T]]).
pr_rule(not(o_d__attack4(X,Y,_Z0,_V2,T)),[_Z0=[_V2|T],not(_attack(X,Y,T))]).
pr_rule(not(o_d__attack4(X,Y,_Z0)),[forall(_V2,forall(T,not(o_d__attack4(X,Y,_Z0,_V2,T))))]).
pr_rule(not(_attack(_X0,_X1,_X2)),[not(o_d__attack1(_X0,_X1,_X2)),not(o_d__attack2(_X0,_X1,_X2)),not(o_d__attack3(_X0,_X1,_X2)),not(o_d__attack4(_X0,_X1,_X2))]).
pr_rule(not(o_d__pickqueen1(X,Y,_X2)),[_X2\=Y]).
pr_rule(not(o_d__pickqueen1(X,Y,_X2)),[_X2=Y,Y=<0]).
pr_rule(not(o_d__pickqueen1(X,Y,_X2)),[_X2=Y,Y>0,not(q(X,Y))]).
pr_rule(not(o_d__pickqueen2(X,Y,N,N1)),[N=<1]).
pr_rule(not(o_d__pickqueen2(X,Y,N,N1)),[N>1,not(N1 is N-1)]).
pr_rule(not(o_d__pickqueen2(X,Y,N,N1)),[N>1,N1 is N-1,not(_pickqueen(X,Y,N1))]).
pr_rule(not(o_d__pickqueen2(X,Y,N)),[forall(N1,not(o_d__pickqueen2(X,Y,N,N1)))]).
pr_rule(not(_pickqueen(_X0,_X1,_X2)),[not(o_d__pickqueen1(_X0,_X1,_X2)),not(o_d__pickqueen2(_X0,_X1,_X2))]).
pr_rule(not(o_d__nqueens1(X,N,Qi,Qo,Y,X1)),[X=<0]).
pr_rule(not(o_d__nqueens1(X,N,Qi,Qo,Y,X1)),[X>0,not(_pickqueen(X,Y,N))]).
pr_rule(not(o_d__nqueens1(X,N,Qi,Qo,Y,X1)),[X>0,_pickqueen(X,Y,N),_attack(X,Y,Qi)]).
pr_rule(not(o_d__nqueens1(X,N,Qi,Qo,Y,X1)),[X>0,_pickqueen(X,Y,N),not(_attack(X,Y,Qi)),not(X1 is X-1)]).
pr_rule(not(o_d__nqueens1(X,N,Qi,Qo,Y,X1)),[X>0,_pickqueen(X,Y,N),not(_attack(X,Y,Qi)),X1 is X-1,not(_nqueens(X1,N,[q(X,Y)|Qi],Qo))]).
pr_rule(not(o_d__nqueens1(X,N,Qi,Qo)),[forall(Y,forall(X1,not(o_d__nqueens1(X,N,Qi,Qo,Y,X1))))]).
pr_rule(not(o_d__nqueens2(_X0,_V1,Q,_X3)),[_X0\=0]).
pr_rule(not(o_d__nqueens2(_X0,_V1,Q,_X3)),[_X0=0,_X3\=Q]).
pr_rule(not(_nqueens(_X0,_X1,_X2,_X3)),[not(o_d__nqueens1(_X0,_X1,_X2,_X3)),not(o_d__nqueens2(_X0,_X1,_X2,_X3))]).
pr_rule(not(o_nqueens1(N,Q)),[not(_nqueens(N,N,[],Q))]).
pr_rule(not(nqueens(_X0,_X1)),[not(o_nqueens1(_X0,_X1))]).
pr_rule(not(o_false),[]).
pr_rule(not(o_false),[]).
pr_rule(o_nmr_check,[]).
pr_rule(add_to_query,[o_nmr_check]).
