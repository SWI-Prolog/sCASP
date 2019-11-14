


%% Include the BASIC EVENT CALCULUS THEORY
#include '../bec_theory'.

%% Inspired by example 4 from Mueller

% If a light is turned on, it will be on:
initiates(turn_on, light_on, T).

% If a light is turned on, whether it is red or green will be released
% from the commonsense law of inertia:
releases(turn_on, light_red, T).
releases(turn_on, light_green, T).

% If a light is turned off, it will not be on.
terminates(turn_off, light_on, T).

% After a light is turned on, it will emit red for six seconds and
% green after the first four seconds:
trajectory(light_on, T1, light_red, T2) :- T2 .>=. T1, T2.<.T1 + 6.
trajectory(light_on, T1, light_green, T2) :- T2 .>=. T1 + 4.

%% NOTE That this rule make the example inconsistent because the
%% trajectory for light_red overlap the trajectory for light_green...

:- holdsAt(light_red,T), holdsAt(light_green,T).

%% Actions
happens(turn_on, 2).




%% Queries (with the expected result)
%% Uncomment the querie you want to check...

?- holdsAt(light_green,5).
% ?- holdsAt(light_on,3).

 
