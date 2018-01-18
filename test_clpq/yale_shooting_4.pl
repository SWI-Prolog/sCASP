
q(N,S,List,T) :-
	T .<. 100,
	S = dead,
	holds(N,S,T,Arm,List).

holds(0,S0,0,[Arm0,W0],[S0]) :-
	init(S0,[Arm0,W0]).
holds(N1,S1,T1,[Arm1,W1],[Action-T1|As]) :-
	N1 .=. N0 + 1,
	T1 .>. 0, T1 .=. T0 + Duration,
	transition(Action,Duration,S0,[Arm0,W0],S1,[Arm1,W1]),
	action(Action,T1),
	holds(N0,S0,T0,[Arm0,W0],As).

unloaded(gun1) :- not unloaded(gun2).
unloaded(gun2) :- not unloaded(gun1).

action(A,N) :- not neg_action(A,N).
neg_action(A,N) :- not action(A,N).

% :- T2 .>. T1, action(load(gun2),T1), action(shoot(gun1),T2).

init(loaded(gun1),[0,0]) :- not unloaded(gun1).
init(loaded(gun2),[0,0]) :- not unloaded(gun2).
init(unloaded(gun1),[0,0]) :- unloaded(gun1).
init(unloaded(gun2),[0,0]) :- unloaded(gun2).


duration(load,25).
duration(shoot,5).
duration(wait,D) :- D .>. 0.

transition(load(X), Duration, unloaded(X), _, loaded(X), [Duration, 0]) :-
	duration(load, Duration).
transition(shoot(X), Duration, loaded(X), [Arm,_], dead, [0,0]) :-
	Arm .=<. 35,
	duration(shoot,Duration).
transition(shoot(X), Duration, loaded(X), [Arm,_], unloaded(X), [0,0]) :-
	Arm .>. 35,
	duration(shoot,Duration).
transition(wait(Duration), Duration, State,[Arm0,0], State, [Arm1, 1]) :-
	Arm1 .=. Arm0 + Duration,
	duration(wait, Duration).





% shoot :- not neg_shoot.
% neg_shoot :- not shoot.

% h(X) :-
% 	unloaded(X),
% 	shoot.


