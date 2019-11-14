% { p, r, not q, not s }
p :- not q.
q :- r, s.
r :- p.
% #compute 1 { p }.
:- not p.
