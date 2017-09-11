pr_rule(p(1),[]).
pr_rule(p(2),[]).
pr_rule(p(3),[]).
pr_rule(not(o_p1(_X0)),[_X0 .\=. 1]).
pr_rule(not(o_p2(_X0)),[_X0 .\=. 2]).
pr_rule(not(o_p3(_X0)),[_X0 .\=. 3]).
pr_rule(not(p(_X0)),[not(o_p1(_X0)),not(o_p2(_X0)),not(o_p3(_X0))]).
pr_rule(not(o_false),[]).
pr_rule(not(o_false),[]).
pr_rule(o_nmr_check,[]).
pr_rule(add_to_query,[o_nmr_check]).
