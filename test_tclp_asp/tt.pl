% :- include('tt2.pl').

% #include('tt2.pl').
% #include 'tt2.pl'.

p(X) :- not q(X).
q(X) :- not p(X).

%?- p(X).

hola(1) :- a.
hola(2) :- b.
hola(3).


p([X|T]):- q(X), p(T).