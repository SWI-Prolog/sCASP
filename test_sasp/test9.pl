% false.
% Even loop with no negation through r must fail unless positive loops are
% enabled.

p :- not q, r.
q :- not p.
r :- p.

% #compute 1 { p }.
:- not p.
