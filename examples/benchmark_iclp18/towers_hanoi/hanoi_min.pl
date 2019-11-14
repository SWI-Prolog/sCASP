





path(A,A,0). % :-
%       edge(A,B,D).

path(A,B,D) :-
    D .=. D1 + D2,
    edge(A,Z,D1),
    path(Z,B,D2).

edge(a,b,1).
edge(a,b,2).
edge(a,c,2).
edge(a,b,0).
edge(c,d,3).
edge(d,g,1).
edge(a,g,2).

min_edge(A,B,D) :-
    edge(A,B,D),
    not neg_min_edge(A,B,D).

neg_min_edge(A,B,D) :-
    D1 .<. D,
    edge(A,B,D1).


min_path(A,B,D) :-
    edge(A,B,D),
    not neg_min_path(A,B,D).
min_path(A,B,D) :-
    edge(A,Z,D1),
    D .=. D1 + D2,
    min_path(Z,B,D2),
    not neg_min_path(A,B,D).

neg_min_path(A,B,D) :-
    D1 .<. D,
    min_path(A,B,D1).


