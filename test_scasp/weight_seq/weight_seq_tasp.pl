


% :- include('instances/01-weight_assignment_tree-654-0.asp').
% :- include('instances/02-weight_assignment_tree-687-0.asp'). 
% :- include('instances/03-weight_assignment_tree-742-0.asp'). 
% :- include('instances/04-weight_assignment_tree-612-0.asp'). 
% :- include('instances/05-weight_assignment_tree-579-0.asp'). 
% :- include('instances/06-weight_assignment_tree-560-0.asp'). 
% :- include('instances/07-weight_assignment_tree-547-0.asp'). 
% :- include('instances/08-weight_assignment_tree-806-0.asp'). 
% :- include('instances/09-weight_assignment_tree-778-0.asp'). 
% :- include('instances/10-weight_assignment_tree-938-0.asp'). 
% :- include('instances/11-weight_assignment_tree-934-0.asp'). 
% :- include('instances/12-weight_assignment_tree-918-0.asp'). 
% :- include('instances/13-weight_assignment_tree-901-0.asp'). 
#include('instances/14-weight_assignment_tree-1070-0.asp').
% ...
% :- include('instances/60-weight_assignment_tree-1588-0.asp').
% :- include('instances/61-weight_assignment_tree-1542-0.asp').
% :- include('instances/62-weight_assignment_tree-2102-0.asp').

%% EXAMPLE
% leafWeightCardinality(leaf1, 4, 5).
% leafWeightCardinality(leaf2, 3, 7).

% innerNode(1).

% num(2).
% max_total_weight(9).
%% EXAMPLE

query([InnerTree, Color, W]) :-
	max_total_weight(MaxW),
	W .=<. MaxW,
	%	weight_seq_clp:num(Lenght),
	num(Lenght),
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

weightAux(_, [Weight], [], [], 0).


colorWeight(W1, L2, Color, W) :-
	leafWeightCardinality(L2, W2, C2),
	G is W2 + C2,
	R is W1 + W2,
	B is W1 + C2,
	choose(G,R,B,Color,W).

choose(G,R,B,green,G) :-
	G .=<. R, G .=<. B.
choose(G,R,B,red,R) :-
	R .=<. B,
	not choose(G,R,B,green,G).
choose(G,R,B,blue,B) :-
	not choose(G,R,B,red,R),
	not choose(G,R,B,green,G).


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
