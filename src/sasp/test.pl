

%% TESTING directive include...
% #include('test_include.pl').


%% PROGRAM 1:
%% two models one for ?- p. and another for ?- q.

% p :- not q.
% q :- not p.

% ?- p.
% ?- q.

%% PROGRAM 1b:
%% Previous program with predicate ASP.

% p(X) :- not q(X).
% q(X) :- not p(X).


% ?- q(X).
% ?- p(X).

%% PROGRAM 2:
%% three models (normal prolog query)

% pa(1,2).
% pa(2,3).
% pa(3,4).

% ?- pa(X,Y).


%% PROGRAM 3:
%% Hamiltonian problem with a graph with two models
%% The second model appear in the 7th answer...

% reachable(V) :-
% 	     chosen(U, V),
% 	     reachable(U).

% reachable(O) :-
% 	     chosen(V,O).

% :- vertex(U), not reachable(U).

% other(U,V) :-
% 	   vertex(U), vertex(V), vertex(W),
% 	   V \= W, chosen(U,W).

% chosen(U,V) :-
% 	    vertex(U), vertex(V),
% 	    edgeh(U,V), not other(U,V).

% :- chosen(U,W), chosen(V,W), U \= V.

% vertex(0).
% vertex(1).
% vertex(2).
% vertex(3).
% vertex(4).

% edgeh(0,1).
% edgeh(1,2).
% edgeh(2,3).
% edgeh(3,4).
% edgeh(4,0).
% edgeh(4,1).
% edgeh(4,2).
% edgeh(4,3).

% edgeh(0,2).
% edgeh(2,1).
% edgeh(1,3).

% ?- reachable(0).



%% PROGRAM 4:
%% Path distance problem with positive loops

path(X,Y,D) :-
	p(X,Z,D1),
	p(Z,Y,D2),
	D is D1 + D2.

path(X,Y,D) :-
	edge(X,Y,D).

p(A,B,C) :- path(A,B,C).

edge(a,b,5).
edge(b,c,3).

?- path(X,Y,D).

