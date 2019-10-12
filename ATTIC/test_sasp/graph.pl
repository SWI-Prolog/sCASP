% Graph

node(a).
node(b).
node(c).
node(d).
node(e).

edge(a,b).
edge(a,c).
edge(b,c).
edge(d,e).

path(X,X).
path(X,Y):-edge(X,Z),path(Z,Y).
