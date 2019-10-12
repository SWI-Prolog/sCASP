




% p :- not q.
% q :- not p.
% %result(S) :- S = #count{ :p}.

% r:- s_result(1).
% s:- r_result(1).

% s_result(S) :- S = #count{ :s}.
% r_result(S) :- S = #count{ :r}.




% t(a) :- N = #count{X:u(X)}, N = 0.
% u(a) :- N = #count{X:t(X)}, N = 0.

% t(a) :- N = #count{X:u(X)}, N = 0.
% u(a) :- N = #count{X:t(X)}, N = 0.



% Party

% coming(X) :- required(X,K), N= #count{ :kc(X,_)}, N >= K.
% kc(X,Y) :- knows(X,Y), coming(Y).

% %coming(a).
% required(a,1).
% required(b,1).
% knows(a,b).
% knows(b,a).


% r(a).r(b).
% p(b).
% q(b).
% p(a) :- N = #count{ r(X): q(X)}, N = 1.
% q(a) :- 1 = #count{ r(X): p(X)}.


p(0).
p(1).
p(2).

r(1..2).

p(0) :- N = #min {X :  p(X)}, N = 0.

#show p/1.
