r(X):-X>0.
r(a).
r(b).
r(c).

p(X):-r(X), not q(X).
q(X):-not p(X).

q(5).
