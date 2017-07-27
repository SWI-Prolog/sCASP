pr_rule(r(X),[X>0]).
pr_rule(r(a),[]).
pr_rule(r(b),[]).
pr_rule(r(c),[]).
pr_rule(p(X),[r(X),not(q(X))]).
pr_rule(q(X),[not(p(X))]).
pr_rule(q(5),[]).
pr_rule(not(o_q1(X)),[p(X)]).
pr_rule(not(o_q2(_X0)),[_X0\=5]).
pr_rule(not(q(_X0)),[not(o_q1(_X0)),not(o_q2(_X0))]).
pr_rule(not(o_p1(X)),[not(r(X))]).
pr_rule(not(o_p1(X)),[r(X),q(X)]).
pr_rule(not(p(_X0)),[not(o_p1(_X0))]).
pr_rule(not(o_r1(X)),[X=<0]).
pr_rule(not(o_r2(_X0)),[_X0\=a]).
pr_rule(not(o_r3(_X0)),[_X0\=b]).
pr_rule(not(o_r4(_X0)),[_X0\=c]).
pr_rule(not(r(_X0)),[not(o_r1(_X0)),not(o_r2(_X0)),not(o_r3(_X0)),not(o_r4(_X0))]).
pr_rule(not(o_false),[]).
pr_rule(not(o_false),[]).
pr_rule(o_nmr_check,[]).
pr_rule(add_to_query,[o_nmr_check]).