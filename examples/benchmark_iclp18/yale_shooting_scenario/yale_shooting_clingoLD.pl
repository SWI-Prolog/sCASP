#include "incmode_lc.lp".

#program base.
action(load).
action(shoot).
action(wait).
duration(load,25).
duration(shoot,5).
duration(wait,36).
unloaded(0).
&sum { at(0) } = 0.
&sum { armed(0) } = 0.

#program step(n).
1 { do(X,n) : action(X) } 1.
&sum { at(n), -1 * at(N') } = D :-
     do(X,n),
     duration(X,D),
     N' = n - 1.

loaded(n) :-
     loaded(n-1),
     not unloaded(n).
unloaded(n) :-
     unloaded(n-1),
     not loaded(n).
dead(n) :-
     dead(n-1).

&sum { armed(n) } = 0 :-
     unloaded(n-1).
&sum { armed(n), -1 * armed(N') } = D :-
     do(X,n),
     duration(X,D),
     N' = n + 1,
     loaded(N').

loaded(n) :- 
     do(load,n).
unloaded(n) :- 
     do(shoot,n).
dead(n) :- 
     do(shoot,n), 
     &sum { armed(n) } <= 35.

:- do(shoot,n), unloaded(n-1).

#program check(n).
:- not dead(n), query(n).
:- not &sum { at(n) } <= 100, query(n).
:- do(shoot,n), not &sum { at(n) } > 35.    
