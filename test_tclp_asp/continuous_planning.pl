


% :- use_package(clpq).

% :- use_package(tabling).
% :- use_package(t_clpq).
% :- table reach/2.

%% #table reach/2.

reach(init, 0).
reach(S1,  T1) :-
	T1 .=. T0 + T,
	edge(S0,S1,T),
	reach(S0, T0).

incident :- not neg_incident.
neg_incident :- not incident.

edge(init, server_on, 5) :-
	not incident.
edge(init, server_on, 9) :-
	incident.
edge(server_on, server_off, 8).

edge(init, at_office, 7).
edge(at_office, working, 0) :-
	Ta .>. Tb,
	reach(at_office,Ta),
	reach(server_on,Tb).

edge(at_office,at_office,T) :-
	T .>. 0,
	Ta .<. Tb,
	reach(at_office, Ta),
	reach(server_on,Tb).
edge(at_office,ready,T) :-
	Ta .<. Tb,
	T .=. Tb - Ta,
	reach(at_office, Ta),
	reach(server_on,Tb).
edge(ready,working,0).

edge(working, out_office, Ts) :-
	not incident,
	stimate(Ts).
edge(working, out_office, T) :-
	T .=. Ts * 1.2,
	incident,
	stimate(Ts).
stimate(T) :-
	T .>. 7,
	T .<. 10.
	

% server_on(T) :-
% 	reach(server_on, Tb),
% 	T .>=. Tb.

% not_server_on(T) :-
% 	T .<. Tb,
% 	reach(server_on,Tb).


% at(State, Time):-
% 	not info_at(State,_),
% 	reach(State, Time).



q1(T) :- T .<. 25, reach(out_office,T).
q2(T) :- reach(server_off,T).
