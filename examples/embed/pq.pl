:- use_module(library(scasp)).

:- begin_scasp(qp,[p/1,q/1]).

p(X) :- not q(X).
q(X) :- not p(X).

:- end_scasp.
