% { q, s, not p, not r }
% for ?- q, s. : { q, not p, s, not r }

p :- not q.
q :- not p.
r :- not s.
s :- not r.

% #compute 1 {q,s}.
comp :- q, s.
:- not comp.
