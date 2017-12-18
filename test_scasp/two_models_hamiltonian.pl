
reachable(V) :-
	     chosen(U, V),
	     reachable(U).

reachable(O) :-
	     chosen(V,O).

:- vertex(U), not reachable(U).

other(U,V) :-
	   vertex(U), vertex(V), vertex(W),
	   V \= W, chosen(U,W).

chosen(U,V) :-
	    vertex(U), vertex(V),
	    edge_h(U,V), not other(U,V).

:- chosen(U,W), chosen(V,W), U \= V.

vertex(0).
vertex(1).
vertex(2).
vertex(3).
vertex(4).

edge_h(0,1).
edge_h(1,2).
edge_h(2,3).
edge_h(3,4).
edge_h(4,0).
edge_h(4,1).
edge_h(4,2).
edge_h(4,3).

edge_h(0,2).
edge_h(2,1).
edge_h(1,3).

%?- reachable(0), chosen(0,1).
%?- reachable(0), chosen(0,2).