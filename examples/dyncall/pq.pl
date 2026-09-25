:- use_module(library(scasp)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
An even loop over negation: p and q exclude each other, so there are two
stable models, one with p and one with q.

?- ? p(X).
?- ? q(X).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

p(X) :- not q(X).
q(X) :- not p(X).
