% An OLON the query cannot reach.
%
% Whole-program semantics: p :- not p, q with q true has no stable model,
% so *no* query has an answer, including this one.  Compare
% test/test_olon.pl:nmr_query_scope, where scasp/2 answers the same query
% because it compiles only the clauses reachable from it.

p :- not p, q.
q.

?- q.
