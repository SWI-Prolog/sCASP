

hanoi([], [], _).
hanoi(A, B, C):-
	any_move(A, B, C, A1, B1, C1),
	hanoi(A1, B1, C1).

any_move(A, B, C, A1, B, C1):- swap(A, C, A1, C1).
any_move(A, B, C, A1, B1, C):- swap(A, B, A1, B1).
any_move(A, B, C, A, B1, C1):- swap(C, B, C1, B1).

swap(A, B, A1, B1):- moveto(A, B, A1, B1).
swap(A, B, A1, B1):- moveto(B, A, B1, A1).

moveto([A|A1], B, A1, [A|B]).

:- hanoi(A, B, C), inverted(A).
:- hanoi(A, B, C), inverted(B).
:- hanoi(A, B, C), inverted(C).

inverted([A,B|_]):- A > B.

?- hanoi([1,2,3], [], []).