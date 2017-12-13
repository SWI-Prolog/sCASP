% { q, r, not p }
% Rules for p, r and q.1 are both ordinary and OLON: even loop through q.2, odd
% loop through q.1.

p :- not q.
q :- not r.
r :- not p.
q :- not p.

% #compute 1 { q }.
:- not q.
