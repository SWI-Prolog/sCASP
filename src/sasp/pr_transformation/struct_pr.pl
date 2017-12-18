pr_rule(p(s(X)),[]).
pr_rule(not(o_p1(_Z0,X)),[_Z0\=s(X)]).
pr_rule(not(o_p1(_Z0)),[forall(X,not(o_p1(_Z0,X)))]).
pr_rule(not(p(_X0)),[not(o_p1(_X0))]).
pr_rule(not(o_false),[]).
pr_rule(not(o_false),[]).
pr_rule(o_nmr_check,[]).
pr_rule(add_to_query,[o_nmr_check]).
