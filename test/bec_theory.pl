
%% BASIC EVENT CALCULUS (BEC) THEORY


%% BEC1 - StoppedIn(t1,f,t2)
stoppedIn(T1, Fluent, T2) :-
    T1 #< T, T #< T2,
    terminates(Event, Fluent, T),
    happens(Event, T).

stoppedIn(T1, Fluent, T2) :-
    T1 #< T, T #< T2,
    releases(Event, Fluent, T),
    happens(Event, T).

%% BEC2 - StartedIn(t1,f,t2)
startedIn(T1, Fluent, T2) :-
    T1 #< T, T #< T2,
    initiates(Event, Fluent, T),
    happens(Event, T).

startedIn(T1, Fluent, T2) :-
    T1 #< T, T #< T2,
    releases(Event, Fluent, T),
    happens(Event, T).

%% BEC3 - HoldsAt(f,t)
holdsAt(Fluent2, T2) :-
    initiates(Event, Fluent1, T1),
    happens(Event, T1),
    trajectory(Fluent1, T1, Fluent2, T2),
    not stoppedIn(T1, Fluent1, T2).
%% BEC4 - HoldsAt(f,t)
holdsAt(Fluent, T) :-
    0 #< T,
    initiallyP(Fluent),
    not stoppedIn(0, Fluent, T).

%% BEC5 - not HoldsAt(f,t)
-holdsAt(Fluent, T) :-
    0 #< T,
    initiallyN(Fluent),
    not startedIn(0, Fluent, T).

%% BEC6 - HoldsAt(f,t)
holdsAt(Fluent, T) :-
    T1 #< T,
    initiates(Event, Fluent, T1),
    happens(Event, T1),
    not stoppedIn(T1, Fluent, T).

%% BEC7 - not HoldsAt(f,t)
-holdsAt(Fluent, T) :-
    T1 #< T,
    terminates(Event, Fluent, T1),
    happens(Event, T1),
    not startedIn(T1, Fluent, T).

