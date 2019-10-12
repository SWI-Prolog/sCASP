% answer set: { not p, not q }
% Positive loop only: no success unless positive loops are enabled.

p :- q.
q :- p.

% #compute 1 { not p }.
:- p.
