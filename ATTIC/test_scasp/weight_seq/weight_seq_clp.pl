:- module(weight_seq_clp,_).

:- use_package(clpq).

:- include('select_instance.pl').

test(T,W) :-
    statistics(runtime,_), query([_,_,W]), statistics(runtime, [_|T]).

query([InnerTree, Color, W]) :-
    max_total_weight(MaxW),
    W .=<. MaxW,
    weight_seq_clp:num(Lenght),
    create(0, Lenght, L),
    permutation(L, List),
    weight(List, InnerTree, Color, W).

weight([L1, L2|Tree], [innerLeftRight(1, L1, L2)|InnerTree], [innerColor(1, Color, WNode)|ColorTree], Weight) :-
    Weight .=. WNode + WMid,
    leafWeightCardinality(L1, W1, _),
    colorWeight(W1, L2, Color, WNode),
    weightAux(1, [WNode|Tree], InnerTree, ColorTree, WMid).

weightAux(N, [W1, L2|Tree], [innerLeftRight(N1, N, L2)|InnerTree], [innerColor(N1,Color, WNode)|ColorTree], Weight) :-
    N1 is N + 1,
    colorWeight(W1, L2, Color, WNode),
    weightAux(N1, [WNode|Tree], InnerTree, ColorTree, WMid),
    Weight is WNode + WMid.

weightAux(_, [_Weight], [], [], 0).


colorWeight(W1, L2, Color, W) :-
    leafWeightCardinality(L2, W2, C2),
    G is W2 + C2,
    R is W1 + W2,
    B is W1 + C2,
    (
        G .=<. R, G .=<. B ->
        Color = green,
        W = G
    ;
        R .=<. B ->
        Color = red,
        W = R
    ;
        Color = blue,
        W = B
    ).

permutation([], []).

permutation(L, [T|Q]) :-
    select(T, L, L1),
    permutation(L1, Q).

create(Max, Max, []).
create(N,   Max, [L|Ls]) :-
    N < Max,
    N1 is N + 1,
    atom_number(AN, N1),
    atom_concat(leaf, AN, L),
    create(N1, Max, Ls).


