% { p, r, not q }, { p, q, not r }
p :- not q, r.
p :- not r, q.
r :- not q.
q :- not r.
% #compute 0 { p }.
:- not p.
