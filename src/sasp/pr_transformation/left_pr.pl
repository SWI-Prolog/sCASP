pr_rule(p(X),[p(Y)]).
pr_rule(not(o_p1(X,Y)),[not(p(Y))]).
pr_rule(not(o_p1(X)),[forall(Y,not(o_p1(X,Y)))]).
pr_rule(not(p(_X0)),[not(o_p1(_X0))]).
pr_rule(not(o_false),[]).
pr_rule(not(o_false),[]).
pr_rule(o_nmr_check,[]).
pr_rule(add_to_query,[o_nmr_check]).
