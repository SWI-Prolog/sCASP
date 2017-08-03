pr_rule(not_p, [forall(X, np1(X))]).
pr_rule(np1(X), [q2(X)]).
pr_rule(q2(Y), [Y = a]).
pr_rule(q2(Y), [Y .\=. a]).
