r(X, Y) :- r(X, Z), p(Z, Y).     
r(X, Y) :- p(X, Y).        
% r(a,d) works if goals reversed. Likely r(X, Z) will never succeed with Z = c
r(X, Y) :- r(X, Z), q(Z, Y).   
p(a, b).   p(b, c).   q(c, d).
?- r(a,Y).
