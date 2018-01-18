







:- use_package(clpq).

q(S,List,T) :-
	T .<. 100,
	holds(S,List,T, _Arm,_).


duration(load,25).
duration(shoot,5).
duration(wait,D) :- D .>. 0.



holds(unloaded, [], 0, 0,0).
holds(loaded, [load|As], T1, Arm1,0) :-
	T1 .>. 0,
	T1 .=. T0 + D,
	Arm1 .=. D,
	duration(load,D),
	holds(unloaded, As, T0, _,_).
holds(dead, [shoot|As], T1, 0,0) :-
	T1 .>. 0,
	T1 .=. T0 + D,
	Arm .=<. 35,
	duration(shoot,D),
	holds(loaded, As, T0, Arm,_).
holds(unloaded, [shoot|As], T1, 0,0) :-
	T1 .>. 0,
	T1 .=. T0 + D,
	Arm .>. 35,
	duration(shoot,D),
	holds(loaded, As, T0, Arm,_).
holds(S1, [wait(D)|As], T1, Arm1,1) :-
	T1 .>. 0,
	T1 .=. T0 + D,
	Arm1 .=. Arm0 + D,
	duration(wait,D),
	holds(S1, As, T0, Arm0,0).
	








% load(gun1,0) :- not load(gun2,0).
% load(gun2,0) :- not load(gun1,0).


% load(X,5) :- load(X,0).
% load(X,5) :- not load(X,0), loaded(X,5).

% loaded(gun1,5).

% unloaded(0).

% actions(0,[]).
% actions(N,[Action|Prev]) :-
% 	N > 0,
% 	do(Action,N),
% 	N1 is N - 1,
% 	actions(N1,Prev).

% action(load).
% action(shoot).
% action(wait).

% % :- use_package(clpq).

% step(1).
% step(2).
% step(3).
% step(Step) :-
% 	 not neg_step(Step).
% neg_step(Step) :-
% 	 not step(Step).

% do(Step,Action) :-
% 	step(Step),
% 	action(Action),
% 	not neg_do(Step,Action).
% neg_do(Step,Action) :-
% 	not do(Step,Action).

%:- do(S, A1), do(S, A2), A1 \= A2.
%:- do(S1, A), do(S2, A), S1 \= S2.

% hold(0,[]).
% hold(S,[A|As]) :-
% 	S > 0,
% 	S1 is S - 1,
% 	do(S,A),
% 	hold(S1,As).



