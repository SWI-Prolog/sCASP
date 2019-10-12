% { p, r, not q }, { p, not q }, { q, not p, not r }
% p can succeed through not q, but not through r, since it would loop with no
% intervening negation. Similarly, r can succeed only if p succeeds through the
% first rule. Even with partial answer sets, not q will always be in any answer
% set with p or r.

p :- not q.
q :- not p.
p :- r.
r :- p.

_out :- r.
_out :- p.
_out :- q.

#compute 0 { _out }.
%:- not _out.
