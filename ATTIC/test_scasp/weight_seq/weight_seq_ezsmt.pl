% $domain(0..mv).
cspvar(nWeight(N),0,MV) :-      coloredPos(N),  max_total_weight(MV).

color(red).
color(blue).
color(green).

leaf(L) :- leafWeightCardinality(L,W,C).

% We mostly care about the lesser of weight and cardinality.
% Just call it leafWeight
leafCost(L,W) :- leaf(L), leafWeightCardinality(L,W,C), W <= C.
leafCost(L,C) :- leaf(L), leafWeightCardinality(L,W,C), C < W.

% locations go from 1 till N-1: assumption is that leafs are numbered 1 through N
coloredPos(L) :- leaf(L),L!=N, num(N).

% Each leaf will have a location in our sequence
% locations go from 0 till N-1
location(0). 
location(L) :- leaf(L),L!=N, num(N).

% Give each leaf a location in the sequence
1{ leafPos(L,N) : location(N) }1 :- leaf(L).

% No sharing locations
:- leafPos(L1, N), leafPos(L2, N), location(N), L1 != L2.


%%%% COLORS %%%%



% coloredPos(1) has a special case, look at locations 1 and 2 in leafPos to
% to determine color

% green if (weight(right) + card(right)) < (weight(left) + leafCost(right))
posColor(1,green) :- leafPos(L1,0), leafPos(L2,1), leafWeightCardinality(L1,WL,CL),
            leafWeightCardinality(L2,WR,CR), leafCost(L2,W3), W1=WR+CR, W2=WL+W3, W1 < W2.
% blue if not green and card(right) < weight(right)
posColor(1,blue) :- leafPos(L2,1), leafWeightCardinality(L2,W,C), C < W, 
            not posColor(1,green).
% red if not green and weight(right) < card(right)
posColor(1,red) :- leafPos(L2,1), leafWeightCardinality(L2,W,C), W <= C, 
            not posColor(1,green).

% Give each colored pos above 1 a unique color
1{ posColor(P,N) : color(N) }1 :- coloredPos(P), P>1.

% if nWeight(N-1)>WR+CR-W2 then posColor(N,green) must be green
%required(nWeight(N-1)<=WR+CR-W2):- 
required(nWeight(N2)<=WR+CR-W2):- 
    not posColor(N,green), N>1, coloredPos(N), leafPos(L,N), leafWeightCardinality(L,WR,CR),
    leafCost(L,W2),
    N2=N-1.
            

%%
%% Place for addons
%% tw.sequence++.sum-basis-addon-v1-moreseqallowed.con OR
%% tw.sequence++.sum-basis-addon-v2.ez



%%%% WEIGHTS %%%%

% nWeight for first coloredPos
required(nWeight(1)==W):- posColor(1,green), leafPos(L,1), leafWeightCardinality(L,WR,CR),
            W=WR+CR.
required(nWeight(1)==W) :- not posColor(1,green), leafPos(L1,0), leafPos(L2,1), 
            leafWeightCardinality(L1,WL,CL), leafCost(L2,WR), W=WL+WR.

% define nWeight/2
% green
required(nWeight(N)==W) :- coloredPos(N), N>1, posColor(N,green), leafPos(L,N), 
            leafWeightCardinality(L,WR,CR), W=WR+CR.

% not green
%required(nWeight(N)==nWeight(N-1)+WR)  :- coloredPos(N), N>1, not posColor(N,green), leafPos(L,N), 
required(nWeight(N)==nWeight(N2)+WR)  :- coloredPos(N), N>1, not posColor(N,green), leafPos(L,N), 
            leafCost(L,WR),
            N2=N-1.

%sum of nWeights should be less than mv
required(sum([nWeight/1],<=,MV)) :-
    max_total_weight(MV).

label_order(nWeight(N),1) :-coloredPos(N).
required(nWeight(Nm1)> W+C-W2):- not posColor(N,blue), N>1, coloredPos(N), leafPos(L,N), leafWeightCardinality(L,W,C), C < W,Nm1=N-1, leafCost(L,W2).


%%
% If it's not  green and accords red color restrictions it must be red
required(nWeight(Nm1)> W+C-W2):- not posColor(N,red), N>1, coloredPos(N), leafPos(L,N), leafWeightCardinality(L,W,C), C >= W,Nm1=N-1, leafCost(L,W2).

%% + enforce sorted order of nongreen blocks
%%
:-not posColor(N,green), not posColor(N-1,green), coloredPos(N-1),
leafPos(L1,N-1),  leafPos(L2,N), leafCost(L1,C1), leafCost(L2,C2), C1>C2.

:-not posColor(N,green), not posColor(N-1,green), coloredPos(N-1),
leafPos(L1,N-1),  leafPos(L2,N), leafCost(L1,C), leafCost(L2,C), L1>L2.

%% ++ enforcing sorted green nodes (sorting by ids)
%%
:- leafPos(L1,P1),  leafPos(L2,P2), 
   posColor(P1,green), posColor(P2,green), 
   L1<L2, P1>P2.
