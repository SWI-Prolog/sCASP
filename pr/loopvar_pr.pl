pr_rule(p(X),[not(q(X))]).
pr_rule(q(X),[not(p(X))]).
pr_rule(r(X),[X .\=. 3,X .\=. 4,q(X)]).
pr_rule(not(o_r1(X)),[X=3]).
pr_rule(not(o_r1(X)),[X .\=. 3,X=4]).
pr_rule(not(o_r1(X)),[X .\=. 3,X .\=. 4,not(q(X))]).
pr_rule(not(r(_X0)),[not(o_r1(_X0))]).
pr_rule(not(o_q1(X)),[p(X)]).
pr_rule(not(q(_X0)),[not(o_q1(_X0))]).
pr_rule(not(o_p1(X)),[q(X)]).
pr_rule(not(p(_X0)),[not(o_p1(_X0))]).
pr_rule(not(o_false),[]).
pr_rule(not(o_false),[]).
pr_rule(o_nmr_check,[]).
pr_rule(add_to_query,[o_nmr_check]).
