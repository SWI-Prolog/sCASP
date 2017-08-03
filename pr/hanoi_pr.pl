pr_rule(hanoi(N,T),[moven(N,0,T,1,2,3)]).
pr_rule(moven(N,Ti,To,A,B,C),[N>1,N1 is N-1,moven(N1,Ti,T2,A,C,B),T3 is T2+1,move(T3,A,B),moven(N1,T3,To,C,B,A)]).
pr_rule(moven(1,Ti,To,A,B,_V1),[To is Ti+1,move(To,A,B)]).
pr_rule(move(T,P1,P2),[not(negmove(T,P1,P2))]).
pr_rule(negmove(T,P1,P2),[not(move(T,P1,P2))]).
pr_rule(not(o_negmove1(T,P1,P2)),[move(T,P1,P2)]).
pr_rule(not(negmove(_X0,_X1,_X2)),[not(o_negmove1(_X0,_X1,_X2))]).
pr_rule(not(o_move1(T,P1,P2)),[negmove(T,P1,P2)]).
pr_rule(not(move(_X0,_X1,_X2)),[not(o_move1(_X0,_X1,_X2))]).
pr_rule(not(o_moven1(N,Ti,To,A,B,C,N1,T2,T3)),[N=<1]).
pr_rule(not(o_moven1(N,Ti,To,A,B,C,N1,T2,T3)),[N>1,not(N1 is N-1)]).
pr_rule(not(o_moven1(N,Ti,To,A,B,C,N1,T2,T3)),[N>1,N1 is N-1,not(moven(N1,Ti,T2,A,C,B))]).
pr_rule(not(o_moven1(N,Ti,To,A,B,C,N1,T2,T3)),[N>1,N1 is N-1,moven(N1,Ti,T2,A,C,B),not(T3 is T2+1)]).
pr_rule(not(o_moven1(N,Ti,To,A,B,C,N1,T2,T3)),[N>1,N1 is N-1,moven(N1,Ti,T2,A,C,B),T3 is T2+1,not(move(T3,A,B))]).
pr_rule(not(o_moven1(N,Ti,To,A,B,C,N1,T2,T3)),[N>1,N1 is N-1,moven(N1,Ti,T2,A,C,B),T3 is T2+1,move(T3,A,B),not(moven(N1,T3,To,C,B,A))]).
pr_rule(not(o_moven1(N,Ti,To,A,B,C)),[forall(N1,forall(T2,forall(T3,not(o_moven1(N,Ti,To,A,B,C,N1,T2,T3)))))]).
pr_rule(not(o_moven2(_X0,Ti,To,A,B,_V1)),[_X0 .\=. 1]).
pr_rule(not(o_moven2(_X0,Ti,To,A,B,_V1)),[_X0=1,not(To is Ti+1)]).
pr_rule(not(o_moven2(_X0,Ti,To,A,B,_V1)),[_X0=1,To is Ti+1,not(move(To,A,B))]).
pr_rule(not(moven(_X0,_X1,_X2,_X3,_X4,_X5)),[not(o_moven1(_X0,_X1,_X2,_X3,_X4,_X5)),not(o_moven2(_X0,_X1,_X2,_X3,_X4,_X5))]).
pr_rule(not(o_hanoi1(N,T)),[not(moven(N,0,T,1,2,3))]).
pr_rule(not(hanoi(_X0,_X1)),[not(o_hanoi1(_X0,_X1))]).
pr_rule(not(o_false),[]).
pr_rule(not(o_false),[]).
pr_rule(o_nmr_check,[]).
pr_rule(add_to_query,[o_nmr_check]).
