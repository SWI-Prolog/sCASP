% Even loop through negation with variables

p(X) :- not q(X).
q(X) :- not p(X).

?- p(X).
