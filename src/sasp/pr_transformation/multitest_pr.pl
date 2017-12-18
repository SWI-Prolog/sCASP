pr_rule((test X),[not(test2(X,Y))]).
pr_rule((test X),[(test[])]).
pr_rule((test X),[]).
pr_rule(test2(X,Y),[Y\=1,not(test3(X))]).
pr_rule(test2(X,1),[not(test3(X))]).
pr_rule(test3(X),[not((test X))]).
pr_rule(male(bob),[]).
pr_rule(male(bo),[]).
pr_rule(male(ben),[]).
pr_rule(female(may),[]).
pr_rule(female(jill),[]).
pr_rule(female(sam),[]).
pr_rule(father(bob,jill),[]).
pr_rule(father(bob,bo),[]).
pr_rule(father(ben,sam),[]).
pr_rule(mother(may,jill),[]).
pr_rule(mother(may,bo),[]).
pr_rule(mother(jill,sam),[]).
pr_rule(parent(X,Y),[father(X,Y)]).
pr_rule(parent(X,Y),[mother(X,Y)]).
pr_rule(ancestor(X,Y,[X,Y]),[parent(X,Y)]).
pr_rule(ancestor(X,Y,[X|T]),[parent(X,Z),ancestor(Z,Y,T)]).
pr_rule(arith(Z),[Z is 2**2.2]).
pr_rule(not(o_arith1(Z)),[not(Z is 2**2.2)]).
pr_rule(not(arith(_X0)),[not(o_arith1(_X0))]).
pr_rule(not(o_ancestor1(X,Y,_Z0)),[_Z0\=[X,Y]]).
pr_rule(not(o_ancestor1(X,Y,_Z0)),[_Z0=[X,Y],not(parent(X,Y))]).
pr_rule(not(o_ancestor2(X,Y,_Z0,T,Z)),[_Z0\=[X|T]]).
pr_rule(not(o_ancestor2(X,Y,_Z0,T,Z)),[_Z0=[X|T],not(parent(X,Z))]).
pr_rule(not(o_ancestor2(X,Y,_Z0,T,Z)),[_Z0=[X|T],parent(X,Z),not(ancestor(Z,Y,T))]).
pr_rule(not(o_ancestor2(X,Y,_Z0)),[forall(T,forall(Z,not(o_ancestor2(X,Y,_Z0,T,Z))))]).
pr_rule(not(ancestor(_X0,_X1,_X2)),[not(o_ancestor1(_X0,_X1,_X2)),not(o_ancestor2(_X0,_X1,_X2))]).
pr_rule(not(o_parent1(X,Y)),[not(father(X,Y))]).
pr_rule(not(o_parent2(X,Y)),[not(mother(X,Y))]).
pr_rule(not(parent(_X0,_X1)),[not(o_parent1(_X0,_X1)),not(o_parent2(_X0,_X1))]).
pr_rule(not(o_mother1(_X0,_X1)),[_X0\=may]).
pr_rule(not(o_mother1(_X0,_X1)),[_X0=may,_X1\=jill]).
pr_rule(not(o_mother2(_X0,_X1)),[_X0\=may]).
pr_rule(not(o_mother2(_X0,_X1)),[_X0=may,_X1\=bo]).
pr_rule(not(o_mother3(_X0,_X1)),[_X0\=jill]).
pr_rule(not(o_mother3(_X0,_X1)),[_X0=jill,_X1\=sam]).
pr_rule(not(mother(_X0,_X1)),[not(o_mother1(_X0,_X1)),not(o_mother2(_X0,_X1)),not(o_mother3(_X0,_X1))]).
pr_rule(not(o_father1(_X0,_X1)),[_X0\=bob]).
pr_rule(not(o_father1(_X0,_X1)),[_X0=bob,_X1\=jill]).
pr_rule(not(o_father2(_X0,_X1)),[_X0\=bob]).
pr_rule(not(o_father2(_X0,_X1)),[_X0=bob,_X1\=bo]).
pr_rule(not(o_father3(_X0,_X1)),[_X0\=ben]).
pr_rule(not(o_father3(_X0,_X1)),[_X0=ben,_X1\=sam]).
pr_rule(not(father(_X0,_X1)),[not(o_father1(_X0,_X1)),not(o_father2(_X0,_X1)),not(o_father3(_X0,_X1))]).
pr_rule(not(o_female1(_X0)),[_X0\=may]).
pr_rule(not(o_female2(_X0)),[_X0\=jill]).
pr_rule(not(o_female3(_X0)),[_X0\=sam]).
pr_rule(not(female(_X0)),[not(o_female1(_X0)),not(o_female2(_X0)),not(o_female3(_X0))]).
pr_rule(not(o_male1(_X0)),[_X0\=bob]).
pr_rule(not(o_male2(_X0)),[_X0\=bo]).
pr_rule(not(o_male3(_X0)),[_X0\=ben]).
pr_rule(not(male(_X0)),[not(o_male1(_X0)),not(o_male2(_X0)),not(o_male3(_X0))]).
pr_rule(not(o_test31(X)),[(test X)]).
pr_rule(not(test3(_X0)),[not(o_test31(_X0))]).
pr_rule(not(o_test21(X,Y)),[Y=1]).
pr_rule(not(o_test21(X,Y)),[Y\=1,test3(X)]).
pr_rule(not(o_test22(X,_X1)),[_X1\=1]).
pr_rule(not(o_test22(X,_X1)),[_X1=1,test3(X)]).
pr_rule(not(test2(_X0,_X1)),[not(o_test21(_X0,_X1)),not(o_test22(_X0,_X1))]).
pr_rule(not(o_test1(X,Y)),[test2(X,Y)]).
pr_rule(not(o_test1(X)),[forall(Y,not(o_test1(X,Y)))]).
pr_rule(not(o_test2(X)),[not((test[]))]).
pr_rule(not((test _X0)),[not(o_test1(_X0)),not(o_test2(_X0)),not(o_test3(_X0))]).
pr_rule(not(o_false),[]).
pr_rule(not(o__chk11(X)),[(test X)]).
pr_rule(not(o__chk11(X)),[not((test X)),test3(X)]).
pr_rule(not(o_chk1(_X0)),[not(o__chk11(_X0))]).
pr_rule(not(o__chk21(X,Y)),[test2(X,Y)]).
pr_rule(not(o__chk21(X,Y)),[not(test2(X,Y)),(test X)]).
pr_rule(not(o__chk21(X)),[forall(Y,not(o__chk21(X,Y)))]).
pr_rule(not(o_chk2(_X0)),[not(o__chk21(_X0))]).
pr_rule(not(o__chk31(X,_X1)),[_X1\=1]).
pr_rule(not(o__chk31(X,_X1)),[_X1=1,test3(X)]).
pr_rule(not(o__chk31(X,_X1)),[_X1=1,not(test3(X)),test2(X,1)]).
pr_rule(not(o_chk3(_X0,_X1)),[not(o__chk31(_X0,_X1))]).
pr_rule(not(o__chk41(X,Y)),[Y=1]).
pr_rule(not(o__chk41(X,Y)),[Y\=1,test3(X)]).
pr_rule(not(o__chk41(X,Y)),[Y\=1,not(test3(X)),test2(X,Y)]).
pr_rule(not(o_chk4(_X0,_X1)),[not(o__chk41(_X0,_X1))]).
pr_rule(not(o_false),[]).
pr_rule(o_nmr_check,[forall(_X0,not(o_chk1(_X0))),forall(_X0,not(o_chk2(_X0))),forall(_X0,forall(_X1,not(o_chk3(_X0,_X1)))),forall(_X0,forall(_X1,not(o_chk4(_X0,_X1))))]).
pr_rule(add_to_query,[o_nmr_check]).
