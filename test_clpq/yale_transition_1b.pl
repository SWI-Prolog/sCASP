



action(load).
action(shoot).
action(wait).

duration(load,25).
duration(shoot,5).
duration(wait,36).

% do(load,Step) :- not do(shoot,Step), not do(wait,Step).
% do(shoot,Step) :- not do(wait,Step), not do(load,Step).
% do(wait,Step) :- not do(load,Step), not do(shoot,Step).

holds(0,0,State,[]) :-
	init(State).
holds(Step1, Time1, FinalState, [Action|As]) :-
	Time1 .>. 0,
	Time1 .=. Time + Duration,
	Step1 .=. Step + 1,
%	do(Action, Step),
	action(Action),
	duration(Action, Duration),
	transition(Action, PrevState, FinalState),
	holds(Step, Time, PrevState, As).

init(state(alive, unloaded, 0)).

transition(load, state(alive, unloaded, _), state(alive, loaded,FinalArmed)) :- FinalArmed = 0.
transition(wait, state(alive, Gun, PrevArmed), state(alive, Gun, FinalArmed)) :-
	duration(wait, D), FinalArmed .=. PrevArmed + D.
transition(shoot, state(alive, loaded, PrevArmed), state(dead, unloaded, 0)) :-
	PrevArmed .=<. 35.
transition(shoot, state(alive, loaded, PrevArmed), state(alive, unloaded, 0)) :-
	PrevArmed .>. 35.
