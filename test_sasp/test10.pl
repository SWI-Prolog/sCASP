% { p, q, r, not s }
p :- q, not s.
q :- r.
r.
s :- not q.

% #compute 1 { p }.
:- not p.
