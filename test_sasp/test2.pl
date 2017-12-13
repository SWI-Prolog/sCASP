% { not p, not q, not r }
% all must fail: no rules for q and r, only an OLON rule for p.

p :- q, not p, r.

%#compute 2 {not p, not q, not r}.
comp :- not p, not q, not r.
:- not comp.
