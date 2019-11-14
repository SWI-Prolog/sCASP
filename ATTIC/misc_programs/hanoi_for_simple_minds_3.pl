

hanoi([], [], _).
hanoi(A, B, C):-
    [A,B] \= [[],[]],
    any_move(A, B, C, A1, B1, C1),
%       display(hanoi(A1, B1, C1)),nl,
    hanoi(A1, B1, C1).
%       display(ok),nl.

any_move(A, B, C, A1, B, C1):- swap(A, C, A1, C1).
any_move(A, B, C, A1, B1, C):- swap(A, B, A1, B1).
any_move(A, B, C, A, B1, C1):- swap(C, B, C1, B1).

swap([A|A1], B, A1, [A|B]):- moveto([A|B]).
swap(A, [B|B1], [B|A], B1):- moveto([B|A]).

%moveto(A) :- not inverted(A).

moveto(A) :- not negmoveto(A).
negmoveto(A) :- not moveto(A).

% moveto(A, B, A1, B1) :- not negmoveto(A, B, A1, B1).
% negmoveto(A, B, A1, B1) :- not moveto(A, B, A1, B1).

:- moveto(A), inverted(A).


%:- moveto(A, B, A1, B1), inverted(B1).

inverted([A,B|_]):- A > B.

%inverted([A,B|Cs]):- A < B, inverted([B|Cs]).


?- hanoi([1,2,3], [], []).


#show hanoi/3, moveto/4.



/*
?- ?? hanoi([1,2,3],[],[]).

Answer 1        (in [23.284] ms):
{ hanoi([1,2,3],[],[]) , hanoi([2,3],[],[1]) , hanoi([3],[2],[1]) , hanoi([1,3],[2],[]) , hanoi([3],[1,2],[]) , hanoi([],[1,2],[3]) , hanoi([1],[2],[3]) , hanoi([],[2],[1,3]) , hanoi([2],[],[1,3]) , hanoi([1,2],[],[3]) , hanoi([2],[1],[3]) , hanoi([],[1],[2,3]) , hanoi([1],[],[2,3]) , hanoi([],[],[1,2,3]) }


next ? ;

Answer 2        (in [13.626] ms):
{ hanoi([1,2,3],[],[]) , hanoi([2,3],[],[1]) , hanoi([3],[2],[1]) , hanoi([1,3],[2],[]) , hanoi([3],[1,2],[]) , hanoi([],[1,2],[3]) , hanoi([1],[2],[3]) , hanoi([],[2],[1,3]) , hanoi([2],[],[1,3]) , hanoi([1,2],[],[3]) , hanoi([2],[1],[3]) , hanoi([],[1],[2,3]) , hanoi([],[],[1,2,3]) }


next ? 
 */
