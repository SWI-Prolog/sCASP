% Basic Event Calculus

%% Include the BASIC EVENT CALCULUS THEORY
#include 'bec_theory.incl'.

%% Inspired by example 14 from Mueller (2014)

% If a light is turned on, it will be on:
initiates(turn_on, light_on, T).

% If a light is turned on, whether it is red or green will be released
% from the commonsense law of inertia:
releases(turn_on, light_red, T).
releases(turn_on, light_green, T).

% If a light is turned off, it will not be on.
terminates(turn_off, light_on, T).

% After a light is turned on, it will emit red for up to two seconds
% and green after at least two seconds:
trajectory(light_on, T1, light_red, T2) :- T1 #< T2, T2 #< T1 + 1.
trajectory(light_on, T1, light_green, T2) :- T2 #>= T1 + 1.

initiallyN(light_on).

%% Actions
happens(turn_on, 2).
happens(turn_off, 4).
happens(turn_on, 6).

?- holdsAt(light_red, T).
