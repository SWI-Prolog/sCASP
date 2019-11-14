
duration(load,D) :-
    D .<. 23/3, D .>. 10/3.
duration(shoot,5).
duration(wait,36).

holds(0,State,[]) :-
    init(State).
holds(Time1, FinalState, [Action|As]) :-
    Time1 .>. 0,
    Time1 .=. Time + Duration,
    duration(Action, Duration),
    not prohibited(Action, Time1),
    transition(Action, PrevState, FinalState),
    holds(Time, PrevState, As).

init(state(alive, unloaded, 0)).

transition(load, state(alive, _, _), state(alive, loaded,FinalArmed)) :-
    FinalArmed = 0.
transition(wait, state(alive, Gun, PrevArmed), state(alive, Gun, FinalArmed)) :-
    duration(wait, D), FinalArmed .=. PrevArmed + D.
transition(shoot, state(alive, loaded, PrevArmed), state(dead, unloaded, 0)) :-
    not spoiled(PrevArmed).
transition(shoot, state(alive, loaded, PrevArmed), state(alive, unloaded, 0)) :-
    spoiled(PrevArmed).

spoiled(Armed) :- Armed .>. 35.   
prohibited(shoot, T) :- T .<. 35.

?- T .<. 40, holds(T, state(dead,_,_), List).

#show holds/3.
