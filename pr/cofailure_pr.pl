pr_rule(d(1),[]).
pr_rule(not(o_d_d1(_X0)),[_X0\=1]).
pr_rule(not(d(_X0)),[not(o_d_d1(_X0))]).
pr_rule(not(o_false),[]).
pr_rule(not(o_false),[]).
pr_rule(o_nmr_check,[]).
pr_rule(add_to_query,[o_nmr_check]).
