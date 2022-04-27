process(G) :-
    call(G). 
process(G) :-
    exception(G),
    not(call(G)).

exception(test). 

?- process(test).
