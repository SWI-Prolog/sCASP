pr_rule(nqueens(N,Q),[_nqueens(N,N,Q)]).
pr_rule(_nqueens(X,N,[q(X,Y)|T]),[X>0,_hasqueen(X,Y,N),X1 is X-1,_nqueens(X1,N,T)]).
pr_rule(_nqueens(0,X,[]),[]).
pr_rule(_hasqueen(X,Y,N),[X>0,_pickqueen(X,Y,N),_testrow(N,X,Y),_testcol(N,X,Y),_testdiag1(X,Y),_testdiag2(X,Y,N),_testdiag3(X,Y,N),_testdiag4(X,Y,N)]).
pr_rule(_hasqueen(0,Y,N),[]).
pr_rule(_pickqueen(X,Y,Y),[Y>0,q(X,Y)]).
pr_rule(_pickqueen(X,Y,N),[N>1,N1 is N-1,_pickqueen(X,Y,N1)]).
pr_rule(_testrow(C,X,Y),[C>0,C\=X,not(q(C,Y)),C1 is C-1,_testrow(C1,X,Y)]).
pr_rule(_testrow(C,X,Y),[C=X,C1 is C-1,_testrow(C1,X,Y)]).
pr_rule(_testrow(0,_V1,_V2),[]).
pr_rule(_testcol(C,X,Y),[C>0,C\=Y,not(q(X,C)),C1 is C-1,_testcol(C1,X,Y)]).
pr_rule(_testcol(C,X,Y),[C=Y,C1 is C-1,_testrow(C1,X,Y)]).
pr_rule(_testcol(0,_V1,_V2),[]).
pr_rule(_testdiag1(X,Y),[X>1,Y>1,X1 is X-1,Y1 is Y-1,not(q(X1,Y1)),_testdiag1(X1,Y1)]).
pr_rule(_testdiag1(1,X),[X\=1]).
pr_rule(_testdiag1(X,1),[]).
pr_rule(_testdiag2(X,Y,N),[X>1,Y<N,X1 is X-1,Y1 is Y+1,not(q(X1,Y1)),_testdiag2(X1,Y1,N)]).
pr_rule(_testdiag2(1,X,N),[X\=N]).
pr_rule(_testdiag2(X,N,N),[]).
pr_rule(_testdiag3(X,Y,N),[X<N,Y>1,X1 is X+1,Y1 is Y-1,not(q(X1,Y1)),_testdiag3(X1,Y1,N)]).
pr_rule(_testdiag3(X,1,N),[X\=N]).
pr_rule(_testdiag3(N,X,N),[]).
pr_rule(_testdiag4(X,Y,N),[X<N,Y<N,X1 is X+1,Y1 is Y+1,not(q(X1,Y1)),_testdiag4(X1,Y1,N)]).
pr_rule(_testdiag4(N,Y,N),[Y\=N]).
pr_rule(_testdiag4(X,N,N),[]).
pr_rule(q(X,Y),[not(_negq(X,Y))]).
pr_rule(_negq(X,Y),[not(q(X,Y))]).
pr_rule(not(o_d__negq1(X,Y)),[q(X,Y)]).
pr_rule(not(_negq(_X0,_X1)),[not(o_d__negq1(_X0,_X1))]).
pr_rule(not(o_q1(X,Y)),[_negq(X,Y)]).
pr_rule(not(q(_X0,_X1)),[not(o_q1(_X0,_X1))]).
pr_rule(not(o_d__testdiag41(X,Y,N,X1,Y1)),[X>=N]).
pr_rule(not(o_d__testdiag41(X,Y,N,X1,Y1)),[X<N,Y>=N]).
pr_rule(not(o_d__testdiag41(X,Y,N,X1,Y1)),[X<N,Y<N,not(X1 is X+1)]).
pr_rule(not(o_d__testdiag41(X,Y,N,X1,Y1)),[X<N,Y<N,X1 is X+1,not(Y1 is Y+1)]).
pr_rule(not(o_d__testdiag41(X,Y,N,X1,Y1)),[X<N,Y<N,X1 is X+1,Y1 is Y+1,q(X1,Y1)]).
pr_rule(not(o_d__testdiag41(X,Y,N,X1,Y1)),[X<N,Y<N,X1 is X+1,Y1 is Y+1,not(q(X1,Y1)),not(_testdiag4(X1,Y1,N))]).
pr_rule(not(o_d__testdiag41(X,Y,N)),[forall(X1,forall(Y1,not(o_d__testdiag41(X,Y,N,X1,Y1))))]).
pr_rule(not(o_d__testdiag42(N,Y,_X2)),[_X2\=N]).
pr_rule(not(o_d__testdiag42(N,Y,_X2)),[_X2=N,Y=N]).
pr_rule(not(o_d__testdiag43(X,N,_X2)),[_X2\=N]).
pr_rule(not(_testdiag4(_X0,_X1,_X2)),[not(o_d__testdiag41(_X0,_X1,_X2)),not(o_d__testdiag42(_X0,_X1,_X2)),not(o_d__testdiag43(_X0,_X1,_X2))]).
pr_rule(not(o_d__testdiag31(X,Y,N,X1,Y1)),[X>=N]).
pr_rule(not(o_d__testdiag31(X,Y,N,X1,Y1)),[X<N,Y=<1]).
pr_rule(not(o_d__testdiag31(X,Y,N,X1,Y1)),[X<N,Y>1,not(X1 is X+1)]).
pr_rule(not(o_d__testdiag31(X,Y,N,X1,Y1)),[X<N,Y>1,X1 is X+1,not(Y1 is Y-1)]).
pr_rule(not(o_d__testdiag31(X,Y,N,X1,Y1)),[X<N,Y>1,X1 is X+1,Y1 is Y-1,q(X1,Y1)]).
pr_rule(not(o_d__testdiag31(X,Y,N,X1,Y1)),[X<N,Y>1,X1 is X+1,Y1 is Y-1,not(q(X1,Y1)),not(_testdiag3(X1,Y1,N))]).
pr_rule(not(o_d__testdiag31(X,Y,N)),[forall(X1,forall(Y1,not(o_d__testdiag31(X,Y,N,X1,Y1))))]).
pr_rule(not(o_d__testdiag32(X,_X1,N)),[_X1\=1]).
pr_rule(not(o_d__testdiag32(X,_X1,N)),[_X1=1,X=N]).
pr_rule(not(o_d__testdiag33(N,X,_X2)),[_X2\=N]).
pr_rule(not(_testdiag3(_X0,_X1,_X2)),[not(o_d__testdiag31(_X0,_X1,_X2)),not(o_d__testdiag32(_X0,_X1,_X2)),not(o_d__testdiag33(_X0,_X1,_X2))]).
pr_rule(not(o_d__testdiag21(X,Y,N,X1,Y1)),[X=<1]).
pr_rule(not(o_d__testdiag21(X,Y,N,X1,Y1)),[X>1,Y>=N]).
pr_rule(not(o_d__testdiag21(X,Y,N,X1,Y1)),[X>1,Y<N,not(X1 is X-1)]).
pr_rule(not(o_d__testdiag21(X,Y,N,X1,Y1)),[X>1,Y<N,X1 is X-1,not(Y1 is Y+1)]).
pr_rule(not(o_d__testdiag21(X,Y,N,X1,Y1)),[X>1,Y<N,X1 is X-1,Y1 is Y+1,q(X1,Y1)]).
pr_rule(not(o_d__testdiag21(X,Y,N,X1,Y1)),[X>1,Y<N,X1 is X-1,Y1 is Y+1,not(q(X1,Y1)),not(_testdiag2(X1,Y1,N))]).
pr_rule(not(o_d__testdiag21(X,Y,N)),[forall(X1,forall(Y1,not(o_d__testdiag21(X,Y,N,X1,Y1))))]).
pr_rule(not(o_d__testdiag22(_X0,X,N)),[_X0\=1]).
pr_rule(not(o_d__testdiag22(_X0,X,N)),[_X0=1,X=N]).
pr_rule(not(o_d__testdiag23(X,N,_X2)),[_X2\=N]).
pr_rule(not(_testdiag2(_X0,_X1,_X2)),[not(o_d__testdiag21(_X0,_X1,_X2)),not(o_d__testdiag22(_X0,_X1,_X2)),not(o_d__testdiag23(_X0,_X1,_X2))]).
pr_rule(not(o_d__testdiag11(X,Y,X1,Y1)),[X=<1]).
pr_rule(not(o_d__testdiag11(X,Y,X1,Y1)),[X>1,Y=<1]).
pr_rule(not(o_d__testdiag11(X,Y,X1,Y1)),[X>1,Y>1,not(X1 is X-1)]).
pr_rule(not(o_d__testdiag11(X,Y,X1,Y1)),[X>1,Y>1,X1 is X-1,not(Y1 is Y-1)]).
pr_rule(not(o_d__testdiag11(X,Y,X1,Y1)),[X>1,Y>1,X1 is X-1,Y1 is Y-1,q(X1,Y1)]).
pr_rule(not(o_d__testdiag11(X,Y,X1,Y1)),[X>1,Y>1,X1 is X-1,Y1 is Y-1,not(q(X1,Y1)),not(_testdiag1(X1,Y1))]).
pr_rule(not(o_d__testdiag11(X,Y)),[forall(X1,forall(Y1,not(o_d__testdiag11(X,Y,X1,Y1))))]).
pr_rule(not(o_d__testdiag12(_X0,X)),[_X0\=1]).
pr_rule(not(o_d__testdiag12(_X0,X)),[_X0=1,X=1]).
pr_rule(not(o_d__testdiag13(X,_X1)),[_X1\=1]).
pr_rule(not(_testdiag1(_X0,_X1)),[not(o_d__testdiag11(_X0,_X1)),not(o_d__testdiag12(_X0,_X1)),not(o_d__testdiag13(_X0,_X1))]).
pr_rule(not(o_d__testcol1(C,X,Y,C1)),[C=<0]).
pr_rule(not(o_d__testcol1(C,X,Y,C1)),[C>0,C=Y]).
pr_rule(not(o_d__testcol1(C,X,Y,C1)),[C>0,C\=Y,q(X,C)]).
pr_rule(not(o_d__testcol1(C,X,Y,C1)),[C>0,C\=Y,not(q(X,C)),not(C1 is C-1)]).
pr_rule(not(o_d__testcol1(C,X,Y,C1)),[C>0,C\=Y,not(q(X,C)),C1 is C-1,not(_testcol(C1,X,Y))]).
pr_rule(not(o_d__testcol1(C,X,Y)),[forall(C1,not(o_d__testcol1(C,X,Y,C1)))]).
pr_rule(not(o_d__testcol2(C,X,Y,C1)),[C\=Y]).
pr_rule(not(o_d__testcol2(C,X,Y,C1)),[C=Y,not(C1 is C-1)]).
pr_rule(not(o_d__testcol2(C,X,Y,C1)),[C=Y,C1 is C-1,not(_testrow(C1,X,Y))]).
pr_rule(not(o_d__testcol2(C,X,Y)),[forall(C1,not(o_d__testcol2(C,X,Y,C1)))]).
pr_rule(not(o_d__testcol3(_X0,_V1,_V2)),[_X0\=0]).
pr_rule(not(_testcol(_X0,_X1,_X2)),[not(o_d__testcol1(_X0,_X1,_X2)),not(o_d__testcol2(_X0,_X1,_X2)),not(o_d__testcol3(_X0,_X1,_X2))]).
pr_rule(not(o_d__testrow1(C,X,Y,C1)),[C=<0]).
pr_rule(not(o_d__testrow1(C,X,Y,C1)),[C>0,C=X]).
pr_rule(not(o_d__testrow1(C,X,Y,C1)),[C>0,C\=X,q(C,Y)]).
pr_rule(not(o_d__testrow1(C,X,Y,C1)),[C>0,C\=X,not(q(C,Y)),not(C1 is C-1)]).
pr_rule(not(o_d__testrow1(C,X,Y,C1)),[C>0,C\=X,not(q(C,Y)),C1 is C-1,not(_testrow(C1,X,Y))]).
pr_rule(not(o_d__testrow1(C,X,Y)),[forall(C1,not(o_d__testrow1(C,X,Y,C1)))]).
pr_rule(not(o_d__testrow2(C,X,Y,C1)),[C\=X]).
pr_rule(not(o_d__testrow2(C,X,Y,C1)),[C=X,not(C1 is C-1)]).
pr_rule(not(o_d__testrow2(C,X,Y,C1)),[C=X,C1 is C-1,not(_testrow(C1,X,Y))]).
pr_rule(not(o_d__testrow2(C,X,Y)),[forall(C1,not(o_d__testrow2(C,X,Y,C1)))]).
pr_rule(not(o_d__testrow3(_X0,_V1,_V2)),[_X0\=0]).
pr_rule(not(_testrow(_X0,_X1,_X2)),[not(o_d__testrow1(_X0,_X1,_X2)),not(o_d__testrow2(_X0,_X1,_X2)),not(o_d__testrow3(_X0,_X1,_X2))]).
pr_rule(not(o_d__pickqueen1(X,Y,_X2)),[_X2\=Y]).
pr_rule(not(o_d__pickqueen1(X,Y,_X2)),[_X2=Y,Y=<0]).
pr_rule(not(o_d__pickqueen1(X,Y,_X2)),[_X2=Y,Y>0,not(q(X,Y))]).
pr_rule(not(o_d__pickqueen2(X,Y,N,N1)),[N=<1]).
pr_rule(not(o_d__pickqueen2(X,Y,N,N1)),[N>1,not(N1 is N-1)]).
pr_rule(not(o_d__pickqueen2(X,Y,N,N1)),[N>1,N1 is N-1,not(_pickqueen(X,Y,N1))]).
pr_rule(not(o_d__pickqueen2(X,Y,N)),[forall(N1,not(o_d__pickqueen2(X,Y,N,N1)))]).
pr_rule(not(_pickqueen(_X0,_X1,_X2)),[not(o_d__pickqueen1(_X0,_X1,_X2)),not(o_d__pickqueen2(_X0,_X1,_X2))]).
pr_rule(not(o_d__hasqueen1(X,Y,N)),[X=<0]).
pr_rule(not(o_d__hasqueen1(X,Y,N)),[X>0,not(_pickqueen(X,Y,N))]).
pr_rule(not(o_d__hasqueen1(X,Y,N)),[X>0,_pickqueen(X,Y,N),not(_testrow(N,X,Y))]).
pr_rule(not(o_d__hasqueen1(X,Y,N)),[X>0,_pickqueen(X,Y,N),_testrow(N,X,Y),not(_testcol(N,X,Y))]).
pr_rule(not(o_d__hasqueen1(X,Y,N)),[X>0,_pickqueen(X,Y,N),_testrow(N,X,Y),_testcol(N,X,Y),not(_testdiag1(X,Y))]).
pr_rule(not(o_d__hasqueen1(X,Y,N)),[X>0,_pickqueen(X,Y,N),_testrow(N,X,Y),_testcol(N,X,Y),_testdiag1(X,Y),not(_testdiag2(X,Y,N))]).
pr_rule(not(o_d__hasqueen1(X,Y,N)),[X>0,_pickqueen(X,Y,N),_testrow(N,X,Y),_testcol(N,X,Y),_testdiag1(X,Y),_testdiag2(X,Y,N),not(_testdiag3(X,Y,N))]).
pr_rule(not(o_d__hasqueen1(X,Y,N)),[X>0,_pickqueen(X,Y,N),_testrow(N,X,Y),_testcol(N,X,Y),_testdiag1(X,Y),_testdiag2(X,Y,N),_testdiag3(X,Y,N),not(_testdiag4(X,Y,N))]).
pr_rule(not(o_d__hasqueen2(_X0,Y,N)),[_X0\=0]).
pr_rule(not(_hasqueen(_X0,_X1,_X2)),[not(o_d__hasqueen1(_X0,_X1,_X2)),not(o_d__hasqueen2(_X0,_X1,_X2))]).
pr_rule(not(o_d__nqueens1(X,N,_Z0,Y,T,X1)),[_Z0\=[q(X,Y)|T]]).
pr_rule(not(o_d__nqueens1(X,N,_Z0,Y,T,X1)),[_Z0=[q(X,Y)|T],X=<0]).
pr_rule(not(o_d__nqueens1(X,N,_Z0,Y,T,X1)),[_Z0=[q(X,Y)|T],X>0,not(_hasqueen(X,Y,N))]).
pr_rule(not(o_d__nqueens1(X,N,_Z0,Y,T,X1)),[_Z0=[q(X,Y)|T],X>0,_hasqueen(X,Y,N),not(X1 is X-1)]).
pr_rule(not(o_d__nqueens1(X,N,_Z0,Y,T,X1)),[_Z0=[q(X,Y)|T],X>0,_hasqueen(X,Y,N),X1 is X-1,not(_nqueens(X1,N,T))]).
pr_rule(not(o_d__nqueens1(X,N,_Z0)),[forall(Y,forall(T,forall(X1,not(o_d__nqueens1(X,N,_Z0,Y,T,X1)))))]).
pr_rule(not(o_d__nqueens2(_X0,X,_X2)),[_X0\=0]).
pr_rule(not(o_d__nqueens2(_X0,X,_X2)),[_X0=0,_X2\=[]]).
pr_rule(not(_nqueens(_X0,_X1,_X2)),[not(o_d__nqueens1(_X0,_X1,_X2)),not(o_d__nqueens2(_X0,_X1,_X2))]).
pr_rule(not(o_nqueens1(N,Q)),[not(_nqueens(N,N,Q))]).
pr_rule(not(nqueens(_X0,_X1)),[not(o_nqueens1(_X0,_X1))]).
pr_rule(not(o_false),[]).
pr_rule(not(o_false),[]).
pr_rule(o_nmr_check,[]).
pr_rule(add_to_query,[o_nmr_check]).
