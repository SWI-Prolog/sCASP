

%% Include the BASIC EVENT CALCULUS THEORY
#include '../bec_theory'.

% The domain comprises a TapOn event, which initiates a flow of liquid
% into the vessel.  The fluent Filling holds while water is flowing
% into the vessel, and the fluent Level(x) represents holds if the
% water is at level x in the vessel, where x is a real number. The
% fluent Leak(x) represents holds if x liter of water are leaked,
% where x is a real number. Level(x) finishes when the maximum level of
% the vessel is reached and the Leak(x) starts.

% An Overflow event occurs when the water reaches the rim of the
% vessel at level 10. The Overflow event initiates a period during
% which the fluent Spilling holds.

max_level(10) :- not max_level(16).      %% The even loop
max_level(16) :- not max_level(10).
    
initiates(tapOn,filling,T).
terminates(tapOff,filling,T).

initiates(overflow,spilling,T) :-
    max_level(Max),
    holdsAt(level(Max), T).

releases(tapOn,level(0),T) :- happens(tapOn, T).

% Note that (S1.3) has to be a Releases formula instead of a
% Terminates formula, so that the Level fluent is immune from the
% common sense law of inertia after the tap is turned on.

% Now we have the Trajectory formula, which describes the continuous
% variation in the Level fluent while the Filling fluent holds. The
% level is assumed to rise at one unit per unit of time until it reach
% the maximum level of the vessel.

trajectory(filling,T1,level(X2),T2) :-
    T1 .<. T2,
    X2 .=. X + T2 - T1,
    max_level(Max),
    X2 .=<. Max,
    holdsAt(level(X),T1).
trajectory(filling,T1,level(overlimit),T2) :-
    T1 .<. T2,
    X2 .=. X + T2 - T1,
    max_level(Max),
    X2 .>. Max,
    holdsAt(level(X),T1).

% Now we have the Trajectory formula, which describes the continuous
% variation in the Leaf fluent while the Spilling fluent holds. The
% water is assumed to leak at one unit per unit of time since it reach
% the maximum level of the vessel.

trajectory(spilling,T1,leak(X),T2) :-
    holdsAt(filling, T2),
    T1.<.T2,
    X .=. T2 - T1.

% The next formulae ensures the Overflow event is triggered when it
% should be.

happens(overflow,T).                            

% Here’s a simple narrative. The level is initially 0, and the tap is
% turned on at time 5.

initiallyP(level(0)).

% Using abducible the events may or may to occur (depending on the
% model/world). If a query success the partial model will contain a
% compatible planning.
happens(tapOn,5).
#abducible happens(tapOff, U).


%% Queries (with the expected result)
%% Uncomment the querie you want to check...

% ?- holdsAt(level(12),T).  % -> no      / 17s "            
?- holdsAt(spilling,T).   % -> T > 15s / T > 21s  "                         
% ?- holdsAt(leak(L),13).   % -> no   / no   "
% ?- holdsAt(leak(L),16).   % -> L = 1   / no   "
% ?- holdsAt(spilling,17), happens(tapOff,T).   % -> yes / no  "
% ?- holdsAt(level(H),19), max_level(Max).   %  -> H = overflow, Max = 10 / H = 14, Max = 16  "
