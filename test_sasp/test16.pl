% { a, b, c, not d, not e }, { a, c, d, not b, not e }
% test of factorization
a :- b, c.
a :- d, c.
a :- c, b, e.

c :- not e.
e :- not c.

b :- not d.
d :- not b.

% #compute 2 {a}.
:- not a.
