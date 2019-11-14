

% Initial situtation

% const grippers=2.
% const lasttime=3.
%block(1..6).
block(1).
block(2).
block(3).
block(4).
block(5).
block(6).

% DEFINE
on(1,2,0).
on(2, 'table', 0).
on(3,4,0).
on(4, 'table', 0).
on(5,6,0).
on(6, 'table', 0).

% Goal Situation

% TEST
:- not on(3,2,3).
:- not on(2,1,3).
:- not on(1,'table',3).
:- not on(6,5,3).
:- not on(5,4,3).
:- not on(4,'table',3).

% GENERATE

%time(0..3).
time(0).
time(1).
time(2).
time(3).
location(B) :- block(B).
location('table').

% GENERATE

% { move(B,L,T) : block(B) , location(L) } 2 :-
%       time(T),
%       T<3.
move(B,L,T) :-
    block(B) ,
    location(L),
    time(T),
    T < 3, not neg_move(B,L,T).
neg_move(B,L,T) :-
    block(B),
    location(L),
    time(T),
    T < 3, not move(B,L,T).

% DEFINE

% effect of moving a block
on(B,L,T1) :-
    block(B),
    location(L),
    time(T),
    move(B,L,T),
    T1 is T + 1,
    T < 3.

% inertia
on(B,L,T1) :-
    location(L),
    block(B),
    time(T),
    on(B,L,T),
    T1 is T + 1,
    not neg_on(B,L,T1),
    T < 3.

% uniqueness of location
neg_on(B,L1,T) :-
    block(B),
    location(L),
    location(L1),
    time(T),
    on(B,L,T),
    L \= L1.

% TEST

% neg_on is the negation of on
:- block(B), location(L), time(T), on(B,L,T), neg_on(B,L,T).

% two blocks cannot be on top of the same block
:- block(B), time(T), on(B1,B,T), block(B1), on(B2,B,T), block(B2), B1 \= B2.

% a block can’t be moved unless it is clear
:- location(L), time(T), T < 3, move(B,L,T), on(B1,B,T), block(B), block(B1).

% a block can’t be moved onto a block that is being moved also
:- block(B), block(B1), location(L), time(T), T < 3, move(B,B1,T), move(B1,L,T).

?- move(A,B,L).
