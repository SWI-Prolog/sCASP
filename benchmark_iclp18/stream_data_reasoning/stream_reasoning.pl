valid_stream(P,Data) :- 
     stream(P,Data), 
     not cancelled(P, Data).
cancelled(P, Data) :- 
     higher_prio(P1, P), 
     stream(P1, Data1), 
     incompt(Data, Data1).
higher_prio(PHi, PLo) :- 
     PHi .>. PLo.
incompt(p(X), q(X)).
incompt(q(X), p(X)).

stream(1,p(X)).
stream(2,q(a)).
stream(2,q(b)).
stream(3,p(a)).

?- valid_stream(P,Data).

