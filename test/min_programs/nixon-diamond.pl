%:- set_prolog_flag(scasp_assume, true).

pacifist(X) :- not -pacifist(X), quaker(X).
-pacifist(X) :- not pacifist(X), republican(X).
-quaker(X) :- not quaker(X).
-republican(X) :- not republican(X).
quaker(alan).
quaker(nixon).
republican(bill).
pacifist(bill).
republican(nixon). 

:- republican(X), -republican(X). 


?- pacifist(X). 