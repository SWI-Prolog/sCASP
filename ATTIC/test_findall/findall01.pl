




p :- not q.
q :- not p.


s(1) :- p.
s(2) :- p.
s(3) :- q.

l(L) :- findall(V, s(V), L).



?- l(L).
?- p, l(L).

?- not l(L).