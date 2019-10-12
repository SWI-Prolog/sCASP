% { p, not q }
% p will always succeed through the fact. q fails due to the OLON rule and lack
% of rules for q.

p :- q, not p.
p.

% #compute 1 {p}.
:- not p.
