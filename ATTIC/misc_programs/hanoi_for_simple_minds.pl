/*
Dumb (but likely correct) Towers of Hanoi

This is a simple version of the Towers of Hanoi which:

- Defines the ToH as a (blind) planning: it does any moves it can do
  as long as there are discs in the pegs.

- Does not tell you the order of the moves, only if there is a solution.

- Defines global constraints to remove the incorrect states.

- I cannot (as-is) be executed in ASP as it uses lists of size in
  principle unknown, but we know that they are bounded by the number
  of discs.

However:

- As there can be repeated movements without upper bound on its
  number, it can just loop forever --> s(CASP) should be able to catch
  that.

- As the constraints on the bad movements are caught when the whole
  model is generated, it should be in any case hugely inefficient.

*/

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
