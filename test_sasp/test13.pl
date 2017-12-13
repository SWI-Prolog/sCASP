% { p, q, r, not s, not t }
p :- q, r.
q :- not s.
r :- not t.
r :- not p.
% #compute 1 { q }.
:- not q.
