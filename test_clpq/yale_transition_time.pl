


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

holds(0,State,[]) :-
	init(State).
holds(Time1, FinalState, [a(Action,Time1)|As]) :-
	Time1 .>. 0,
	Time1 .=. Time + Duration,
	do(Action, Time1),
%	action(Action),
	duration(Action, Duration),
	transition(Action, PrevState, FinalState),
	holds(Time, PrevState, As).

init(state(unloaded, 0)).

transition(load, state(unloaded, _), state(loaded,FinalArmed)) :- FinalArmed = 0.
transition(wait, state(Gun, PrevArmed), state(Gun, FinalArmed)) :-
	duration(wait, D), FinalArmed .=. PrevArmed + D.
transition(shoot, state(loaded, PrevArmed), state(dead, 0)) :-
	PrevArmed .=<. 35.
transition(shoot, state(loaded, PrevArmed), state(unloaded, 0)) :-
	PrevArmed .>. 35.
transition(shoot, state(unloaded, _), state(unloaded, 0)).

:- T .<. 10, do(shoot,T).

?- holds(Step, Time, [dead,_], Actions).