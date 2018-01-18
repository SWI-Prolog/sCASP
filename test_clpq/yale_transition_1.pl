


#table holds/4, transition/3.

action(load).
action(shoot).
action(wait).

duration(load,25).
duration(shoot,5).
duration(wait,36).

do(load,Step) :- not do(shoot,Step), not do(wait,Step).
do(shoot,Step) :- not do(wait,Step), not do(load,Step).
do(wait,Step) :- not do(load,Step), not do(shoot,Step).

holds(0,0,State,[]) :-
	init(State).
holds(Step1, Time1, FinalState, [Action|As]) :-
	Time1 .>. 0,
	Time1 .=. Time + Duration,
%	do(Action, Step),
	action(Action),
	duration(Action, Duration),
	transition(Action, PrevState, FinalState),
	holds(Step, Time, PrevState, As),
	Step1 is Step + 1.

init(state(alive, unloaded, 0)).

transition(load, state(Turkey, unloaded, _), state(Turkey, loaded,FinalArmed)) :- FinalArmed = 0.
transition(wait, state(Turkey, Gun, PrevArmed), state(Turkey, Gun, FinalArmed)) :-
	duration(wait, D), FinalArmed .=. PrevArmed + D.
transition(shoot, state(_, loaded, PrevArmed), state(dead, unloaded, 0)) :-
	PrevArmed .=<. 35.
transition(shoot, state(Turkey, loaded, PrevArmed), state(Turkey, unloaded, 0)) :-
	PrevArmed .>. 35.
