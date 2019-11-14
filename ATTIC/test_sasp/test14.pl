% { t, not p, not q, not r, not s, not u }
p :- not t.
t :- not q.
q :- not r, u, not q.
r :- s.
s :- p.
% #compute 1 { not r }.
:- r.
