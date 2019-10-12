% { q, r, not p, not s }
% Two even loops allow selection, but the OLON forces r to succeed.

p :- not q.
q :- not p.     % (p ^ not q) V (q ^ not p)
r :- not s, q.
s :- not r.     % (r ^ not s) V (s ^ not r)

% #compute 1 { r }.
:- not r.
