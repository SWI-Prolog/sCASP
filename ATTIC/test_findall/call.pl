







p(X) :-
    call(X).

q(1).
q(2).

?- p(q(X)).
?- not p(q(X)).

?- not q(X).
?- q(X).


call_list([]).
call_list([X|Xs]) :-
    call(X),
    call_list(Xs).

?- call_list([q(X),not q(Y)]).