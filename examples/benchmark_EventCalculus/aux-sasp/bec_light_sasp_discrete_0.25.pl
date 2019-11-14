


%% Include the BASIC EVENT CALCULUS THEORY
#include 'bec_theory_discrete.pl'.

time(T) :- bet(0,5.0,T).

bet(N, M, K) :- N =< M, K is N.
bet(N, M, K) :- N < M,  N1 is N+0.25, bet(N1, M, K).

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
trajectory(light_on, T1, light_red, T2) :- T is T1 + 2, T2 >= T1, T2 < T.
trajectory(light_on, T1, light_green, T2) :- T is T1 + 2, T2 >= T.

initiallyN(light_on).

%% Actions
happens(turn_on, 0.5).  %% Modified, rationals such as 1/2 are not supported
happens(turn_off, 4.0).



%% Queries (with the expected result)
%% Uncomment the querie you want to check...

% ?- holdsAt(light_on, -1).                           % no
 ?- holdsAt(light_on, 2.0).                            % success        
% ?- holdsAt(light_on, 4).                            % success 
% ?- holdsAt(light_on, 5).                            % no      
% ?- holdsAt(light_on, 6).                            % no      
% ?- holdsAt(light_on, 104/10).                       % rationals not supported
                                     
% ?- -holdsAt(light_on, 1/10).                        % rationals not supported
% ?- -holdsAt(light_on, 2).                           % no      
% ?- -holdsAt(light_on, 5).                           % success
                                     
% ?- T is 1/2 + 2 - 1/10, holdsAt(light_red, T).      % rationals not supported
% ?- T is 1/2 + 2 + 1/10, holdsAt(light_red, T).      % rationals not supported
% ?- holdsAt(light_red, 4).                           % no      
% ?- holdsAt(light_red, 6).                           % success 
% ?- holdsAt(light_red, 8).                           % no      
% ?- holdsAt(light_green, 6).                         % no      
% ?- holdsAt(light_green, 8).                         % success                   

