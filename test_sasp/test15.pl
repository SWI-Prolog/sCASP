% false.
% no answer set
a :- not b.
b :- not a.
c :- not d.
d :- not c.
:- a, d.
:- a, c.
:- b, c.
:- b, d.
