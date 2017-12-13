alpha(a).
alpha(b).
alpha(c).

alpha_num(a).
alpha_num(d).
alpha_num(e).
alpha_num(0).
alpha_num(1).
alpha_num(2).

alpha_spec(e).
alpha_spec(f).
alpha_spec(g).
alpha_spec('#').
alpha_spec('&').

member(X,[X|T]).
member(X,[_|T]) :- member(X,T).

state1(L) :- not alpha(X), not member(X,L), not state2(L).
state1(X).
%state2(L) :- not alpha_num(X), not member(X,L), not state3(L).
state2(L) :- not state3(L).
%state3(L) :- not alpha_spec(X), not member(X,L), not state1(L).
state3(L) :- not state1(L).
