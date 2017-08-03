pr_rule(vertex(0),[]).
pr_rule(vertex(1),[]).
pr_rule(vertex(2),[]).
pr_rule(edge(0,1),[]).
pr_rule(edge(1,2),[]).
pr_rule(edge(2,0),[]).
pr_rule(reachable(V),[chosen(U,V),reachable(U)]).
pr_rule(reachable(0),[chosen(V,0)]).
pr_rule(other(U,V),[vertex(U),vertex(V),vertex(W),edge(U,W),V .\=. W,chosen(U,W)]).
pr_rule(chosen(U,V),[edge(U,V),not(other(U,V))]).
pr_rule(not(o_other1(U,V,W)),[not(vertex(U))]).
pr_rule(not(o_other1(U,V,W)),[vertex(U),not(vertex(V))]).
pr_rule(not(o_other1(U,V,W)),[vertex(U),vertex(V),not(vertex(W))]).
pr_rule(not(o_other1(U,V,W)),[vertex(U),vertex(V),vertex(W),not(edge(U,W))]).
pr_rule(not(o_other1(U,V,W)),[vertex(U),vertex(V),vertex(W),edge(U,W),V=W]).
pr_rule(not(o_other1(U,V,W)),[vertex(U),vertex(V),vertex(W),edge(U,W),V .\=. W,not(chosen(U,W))]).
pr_rule(not(o_other1(U,V)),[forall(W,not(o_other1(U,V,W)))]).
pr_rule(not(other(_X0,_X1)),[not(o_other1(_X0,_X1))]).
pr_rule(not(o_chosen1(U,V)),[not(edge(U,V))]).
pr_rule(not(o_chosen1(U,V)),[edge(U,V),other(U,V)]).
pr_rule(not(chosen(_X0,_X1)),[not(o_chosen1(_X0,_X1))]).
pr_rule(not(o_reachable1(V,U)),[not(chosen(U,V))]).
pr_rule(not(o_reachable1(V,U)),[chosen(U,V),not(reachable(U))]).
pr_rule(not(o_reachable1(V)),[forall(U,not(o_reachable1(V,U)))]).
pr_rule(not(o_reachable2(_X0,V)),[_X0 .\=. 0]).
pr_rule(not(o_reachable2(_X0,V)),[_X0=0,not(chosen(V,0))]).
pr_rule(not(o_reachable2(_X0)),[forall(V,not(o_reachable2(_X0,V)))]).
pr_rule(not(reachable(_X0)),[not(o_reachable1(_X0)),not(o_reachable2(_X0))]).
pr_rule(not(o_edge1(_X0,_X1)),[_X0 .\=. 0]).
pr_rule(not(o_edge1(_X0,_X1)),[_X0=0,_X1 .\=. 1]).
pr_rule(not(o_edge2(_X0,_X1)),[_X0 .\=. 1]).
pr_rule(not(o_edge2(_X0,_X1)),[_X0=1,_X1 .\=. 2]).
pr_rule(not(o_edge3(_X0,_X1)),[_X0 .\=. 2]).
pr_rule(not(o_edge3(_X0,_X1)),[_X0=2,_X1 .\=. 0]).
pr_rule(not(edge(_X0,_X1)),[not(o_edge1(_X0,_X1)),not(o_edge2(_X0,_X1)),not(o_edge3(_X0,_X1))]).
pr_rule(not(o_vertex1(_X0)),[_X0 .\=. 0]).
pr_rule(not(o_vertex2(_X0)),[_X0 .\=. 1]).
pr_rule(not(o_vertex3(_X0)),[_X0 .\=. 2]).
pr_rule(not(vertex(_X0)),[not(o_vertex1(_X0)),not(o_vertex2(_X0)),not(o_vertex3(_X0))]).
pr_rule(not(o__false1(U)),[not(vertex(U))]).
pr_rule(not(o__false1(U)),[vertex(U),reachable(U)]).
pr_rule(not(o__false1),[forall(U,not(o__false1(U)))]).
pr_rule(not(o__false2(U,W,V)),[not(chosen(U,W))]).
pr_rule(not(o__false2(U,W,V)),[chosen(U,W),not(chosen(V,W))]).
pr_rule(not(o__false2(U,W,V)),[chosen(U,W),chosen(V,W),U=V]).
pr_rule(not(o__false2),[forall(U,forall(W,forall(V,not(o__false2(U,W,V)))))]).
pr_rule(not(o_false),[not(o__false1),not(o__false2)]).
pr_rule(not(o__chk11(U)),[not(vertex(U))]).
pr_rule(not(o__chk11(U)),[vertex(U),reachable(U)]).
pr_rule(not(o__chk11),[forall(U,not(o__chk11(U)))]).
pr_rule(not(o_chk1),[not(o__chk11)]).
pr_rule(not(o__chk21(U,W,V)),[not(chosen(U,W))]).
pr_rule(not(o__chk21(U,W,V)),[chosen(U,W),not(chosen(V,W))]).
pr_rule(not(o__chk21(U,W,V)),[chosen(U,W),chosen(V,W),U=V]).
pr_rule(not(o__chk21),[forall(U,forall(W,forall(V,not(o__chk21(U,W,V)))))]).
pr_rule(not(o_chk2),[not(o__chk21)]).
pr_rule(not(o__chk31(U,V)),[not(edge(U,V))]).
pr_rule(not(o__chk31(U,V)),[edge(U,V),other(U,V)]).
pr_rule(not(o__chk31(U,V)),[edge(U,V),not(other(U,V)),chosen(U,V)]).
pr_rule(not(o_chk3(_X0,_X1)),[not(o__chk31(_X0,_X1))]).
pr_rule(not(o__chk41(U,V,W)),[not(vertex(U))]).
pr_rule(not(o__chk41(U,V,W)),[vertex(U),not(vertex(V))]).
pr_rule(not(o__chk41(U,V,W)),[vertex(U),vertex(V),not(vertex(W))]).
pr_rule(not(o__chk41(U,V,W)),[vertex(U),vertex(V),vertex(W),not(edge(U,W))]).
pr_rule(not(o__chk41(U,V,W)),[vertex(U),vertex(V),vertex(W),edge(U,W),V=W]).
pr_rule(not(o__chk41(U,V,W)),[vertex(U),vertex(V),vertex(W),edge(U,W),V .\=. W,not(chosen(U,W))]).
pr_rule(not(o__chk41(U,V,W)),[vertex(U),vertex(V),vertex(W),edge(U,W),V .\=. W,chosen(U,W),other(U,V)]).
pr_rule(not(o__chk41(U,V)),[forall(W,not(o__chk41(U,V,W)))]).
pr_rule(not(o_chk4(_X0,_X1)),[not(o__chk41(_X0,_X1))]).
pr_rule(not(o_false),[]).
pr_rule(o_nmr_check,[not(o_chk1),not(o_chk2),forall(_X0,forall(_X1,not(o_chk3(_X0,_X1)))),forall(_X0,forall(_X1,not(o_chk4(_X0,_X1))))]).
pr_rule(add_to_query,[o_nmr_check]).
