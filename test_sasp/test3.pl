% false.
% Contradiction: OLON rule forces q to be false, fact forces it to be true.

p :- q, not p.
q.

_out :- p.
_out :- q.
_out :- not p.
_out :- not q.

% #compute 1 { _out }.
:- not _out.
