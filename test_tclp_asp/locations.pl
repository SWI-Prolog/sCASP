




% pos(X) :- loc(X), not neg_pos(X).
% neg_pos(X) :- loc(X), not pos(X).

loc(1).
loc(2).
loc(3).

leaf(l1).
leaf(l2).
leaf(l3).

% leafPos(Leaf, 1) :- leaf(Leaf), not leafPos(Leaf,2), not leafPos(Leaf,3) .
% leafPos(Leaf, 2) :- leaf(Leaf), not leafPos(Leaf,1), not leafPos(Leaf,3) .
% leafPos(Leaf, 3) :- leaf(Leaf), not leafPos(Leaf,1), not leafPos(Leaf,2) .

leafPos(Leaf, Pos) :- leaf(Leaf), loc(Pos), not neg_leafPos(Leaf, Pos).
neg_leafPos(Leaf, Pos) :- not leafPos(Leaf, Pos).
 
%1{leafPos(Leaf, Pos) : loc(Pos) }1 :- leaf(Leaf).

:- leafPos(L1,P), leafPos(L2,P), L1 \= L2.
:- leafPos(L,P1), leafPos(L,P2), P1 \= P2.


?- leafPos(l1,P), leafPos(l2,P2), leafPos(l3,P3).
