pr_rule(p(X),[p(X)]).
pr_rule(not(o_p1(X)),[not(p(X))]).
pr_rule(not(p(_X0)),[not(o_p1(_X0))]).
pr_rule(not(o_false),[]).
pr_rule(not(o_false),[]).
pr_rule(o_nmr_check,[]).
pr_rule(add_to_query,[o_nmr_check]).
