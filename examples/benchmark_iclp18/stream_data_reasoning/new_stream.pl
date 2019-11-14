



valid_stream(P,Data) :- 
    stream(P,Data), 
    not cancelled(P, Data).
cancelled(P, Data) :-
    stream(P,Data),
    stream(P1, Data1),
    P1 > P,
    incompt(Data, Data1).

incompt(p(X), q(X)) :- stream(_,p(X)).
incompt(q(X), p(X)) :- stream(_,q(X)).
incompt(a(X), b(Y)) :- stream(_,a(X)), stream(_,b(Y)), X > Y.
incompt(b(X), a(X)) :- stream(_,b(X)), stream(_,b(Y)), X > Y.
incompt(c(X), d(X)) :- stream(_,c(X)).
incompt(d(X), c(X)) :- stream(_,d(X)).
incompt(e(X), f(X)) :- stream(_,e(X)).
incompt(f(X), e(X)) :- stream(_,f(X)).
incompt(g(X), h(X)) :- stream(_,g(X)).
incompt(h(X), g(X)) :- stream(_,h(X)).

stream(1,p(c)).   
stream(2,q(b)).   
stream(2,q(a)).
stream(3,p(a)).
stream(1,a(1..100)).
stream(2,b(50..100)).
stream(3,c(1..10)).
stream(4,d(1..10)).
stream(5,e(1..10)).
stream(6,f(1..10)).
stream(7,g(1..10)).
stream(8,h(1..10)).


#show valid_stream/2.
    
