


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

% After a light is turned on, it will emit red for up to two seconds
% and green after at least two seconds:
trajectory(light_on, T1, light_red, T2) :- T2 .>=. T1, T2.<.T1 + 2.
trajectory(light_on, T1, light_green, T2) :- T2 .>=. T1 + 2.

initiallyN(light_on).

%% Actions
happens(turn_on, 1/2).
happens(turn_off, 4).
happens(turn_on, 6).





%% Queries (with the expected result)
%% Uncomment the querie you want to check...

% ?- holdsAt(light_on, -1).                           % no
 ?- holdsAt(light_on, 2).                            % success  
% ?- holdsAt(light_on, 4).                            % success 
% ?- holdsAt(light_on, 5).                            % no      
% ?- holdsAt(light_on, 6).                            % no      
% ?- holdsAt(light_on, 104/10).                       % success
                                     
% ?- -holdsAt(light_on, 1/10).                        % no
% ?- -holdsAt(light_on, 2).                           % no      
% ?- -holdsAt(light_on, 5).                           % success
                                     
% ?- T is 1/2 + 2 - 1/10, holdsAt(light_red, T).      % success
% ?- T is 1/2 + 2 + 1/10, holdsAt(light_red, T).      % no
% ?- holdsAt(light_red, 4).                           % no      
% ?- holdsAt(light_red, 6).                           % success 
% ?- holdsAt(light_red, 8).                           % no      
% ?- holdsAt(light_green, 6).                         % no      
% ?- holdsAt(light_green, 8).                         % success                   

