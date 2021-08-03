:- module(stack,
          [ process_stack/2,
            test/1  %% To be removed
          ]).
:- op(900, fy, [not]). %% To be removed


%% process
%% [flies(tweety),bird(tweety),[],not ab(tweety),not o_ab_1(tweety),not penguin(tweety),not o_penguin_1(tweety),tweety\=sam,[],[],[],[],not o_ab_2(tweety),not wounded_bird(tweety),not o_wounded_bird_1(tweety),tweety\=john,[],[],[],[],[],[],o_nmr_check,[],[]]
%%
%% 1) by using collect_children to obtain:
%% [(query,
%%   [(flies(tweety),
%%          [(bird(tweety),[]),
%%           (not ab(tweety),
%%                [(not o_ab_1(tweety),
%%                      [(not penguin(tweety),
%%                            [(not o_penguin_1(tweety),
%%                                  [(tweety\=sam,[])])])]),
%%                 (not o_ab_2(tweety),
%%                      [(not wounded_bird(tweety),
%%                            [(not o_wounded_bird_1(tweety),
%%                                  [(tweety\=john,[])])])])])]),
%%    (o_nmr_check,[])])]
%% 
%% 2) by using collect_parents to obtain:
%% [  (query,[flies(tweety),o_nmr_check]),
%%    (flies(tweety),[bird(tweety),not ab(tweety)]),
%%    (bird(tweety),[]),
%%    (not ab(tweety),[not o_ab_1(tweety),not o_ab_2(tweety)]),
%%    (not o_ab_1(tweety),[not penguin(tweety)]),
%%    (not penguin(tweety),[not o_penguin_1(tweety)]),
%%    (not o_penguin_1(tweety),[tweety\=sam]),
%%    (tweety\=sam,[]),
%%    (not o_ab_2(tweety),[not wounded_bird(tweety)]),
%%    (not wounded_bird(tweety),[not o_wounded_bird_1(tweety)]),
%%    (not o_wounded_bird_1(tweety),[tweety\=john]),
%%    (tweety\=john,[]),
%%    (o_nmr_check,[])
%% ]
%%
%% 3) and others (enumerated list) ....


%! process_stack(:StackOut, :JustificationTree)
%
%  Process the stack

process_stack(StackOut, [EnumStack, Children, FilterChildren, Siblings]) :-
    enumerate([query|StackOut],EnumStack,1,1),
    %% chose one of the following
    collect_children(EnumStack, Children, 1),   %% default
    filter_tree(Children, FilterChildren),
    collect_parents(EnumStack, Siblings),
    %% nl,print(StackOut),nl,
    %% nl,print(EnumStack),nl,
    %% nl,print(Children),nl,
    %% nl,print(FilterChildren),nl,
    %% nl,print(Siblings),nl,
    plain_output(FilterChildren, 0),
    true.


%! enumerate(:StackOut, :EnumStack, :Parent, :Order)

enumerate([],[],_,_) :- !.
enumerate([[]],[],_,_) :- !.
enumerate([[]|Stack], Enum, P-PO, _) :- !,
    NO is PO + 1,
    enumerate(Stack, Enum, P, NO).
enumerate([Term|Stack], [(Term, P-O) | Enum], P, O) :-
    enumerate(Stack, Enum, P-O, 1).


%! collect_children(:EnumStack, :Children, :ParentId)

collect_children([], [], _) :- !.
collect_children([(Term, PId-O)|Stack], [(Term, Childs) | Cs], PId) :- !,
    collect_children(Stack, Cs, PId),
    collect_children(Stack, Childs, PId-O).
collect_children([_|Stack], Cs, PId) :-
    collect_children(Stack, Cs, PId).
    

%! collect_parents(:EnumStack, :Childs, :ParentId)

collect_parents([], []) :- !.
collect_parents([(Term, PId)|Stack], [(Term, Siblings) | Cs]) :-
    collect_parents(Stack, Cs),
    collect_siblings(Stack, Siblings, PId).

collect_siblings([], [], _) :- !.
collect_siblings([(Term, PId-_)|Stack], [Term|Siblings], PId) :- !,
    collect_siblings(Stack, Siblings, PId).
collect_siblings([_|Stack], Siblings, PId) :-
    collect_siblings(Stack, Siblings, PId).


%! filter_tree(:Children, :FilterChildren)
filter_tree([], []).
filter_tree([(Term, Childs) | Cs], [(Term, FChilds) | Fs]) :-
    selected(Term), !,
    filter_tree(Childs, FChilds),
    filter_tree(Cs, Fs).
filter_tree([(_, Childs) | Cs], FilterChildren) :-
    append(Childs, Cs, AllCs),
    filter_tree(AllCs, FilterChildren).


selected(query).
selected(flies(_)).
selected(bird(_)).
%selected(\=(_,_)).
selected(not(penguin(_))).
selected(not(ab(_))).
selected(not(wounded_bird(_))).
selected(not( o_ab_1(_))).

%! plain_output(:FilterChildren, :Index)

plain_output([A,B|Rs], I) :- !,
    plain_output_(A, I),
    format(",",[]),
    plain_output([B|Rs], I).
plain_output([A], 0) :- !,
    plain_output_(A, 0),
    format(".\n",[]).
plain_output([A], I) :- !,
    plain_output_(A, I).

plain_output_((Term, []), I) :- !,
    nl, tab(I), term_output(Term).
plain_output_((Term, Child), I) :- !,
    nl, tab(I), term_output(Term), format(" :-",[]),
    I1 is I + 3,
    plain_output(Child, I1).

term_output(Term) :-
    Term =.. [Name], !,
    format("~p",[Name]).
term_output(Term) :-
    Term =.. [Name|Args], !,
    format("~p(",[Name]),
    args_output(Args),
    format(")",[]).

args_output([A, B|Rs]) :- !,
    args_output_(A),
    format(", ",[]),
    args_output([B|Rs]).
args_output([B]) :- !,
    args_output_(B).

args_output_(A) :- ground(A), !, format("~p",[A]).
args_output_(A) :- var(A), !, format("~p",[A]).
args_output_(A) :- !, format("~p",[A]).


    


    
    


%%% TO BE REMOVED %%%

%% test(1) :-
%%     process_stack([p(X),not q(X),not o_q_1(X),chs(p(X)),[],[],[],[],o_nmr_check,[],[]], _).

test(2) :-
    process_stack([flies(tweety),bird(tweety),[],not ab(tweety),not o_ab_1(tweety),not penguin(tweety),not o_penguin_1(tweety),tweety \= sam,[],[],[],[],not o_ab_2(tweety),not wounded_bird(tweety),not o_wounded_bird_1(tweety),tweety \= john,[],[],[],[],[],[],o_nmr_check,[],[]], _).

                   
%% test(3) :-
%%     process_stack([flies(tweety),bird(tweety),[],not ab(tweety),not o_ab_1(tweety),not penguin(tweety),not o_penguin_1(tweety),tweety \= sam,[],[],[],[],not o_ab_2(tweety),not wounded_bird(tweety),not o_wounded_bird_1(tweety),tweety \= john,[],[],[],[],[],[],o_nmr_check,not o_chk_1,not o__chk_1_1,forall(Var6,not o__chk_1_1(Var6)),not o__chk_1_1(sam),not -ab(sam),not 'o_-ab_1'(sam),ab(sam),penguin(sam),[],[],[],[],[],not o__chk_1_1(john),not -ab(john),not 'o_-ab_1'(john),ab(john),wounded_bird(john),[],[],[],[],[],not o__chk_1_1(Var1 is {Var1 \= john,Var1 \= sam}),-ab(Var1 is {Var1 \= john,Var1 \= sam}),not ab(Var1 is {Var1 \= john,Var1 \= sam}),not o_ab_1(Var1 is {Var1 \= john,Var1 \= sam}),not penguin(Var1 is {Var1 \= john,Var1 \= sam}),not o_penguin_1(Var1 is {Var1 \= john,Var1 \= sam}),Var1 \= sam,[],[],[],[],not o_ab_2(Var1 is {Var1 \= john,Var1 \= sam}),not wounded_bird(Var1 is {Var1 \= john,Var1 \= sam}),not o_wounded_bird_1(Var1 is {Var1 \= john,Var1 \= sam}),Var1 \= john,[],[],[],[],[],[],not ab(Var1 is {Var1 \= john,Var1 \= sam}),not o_ab_1(Var1 is {Var1 \= john,Var1 \= sam}),not penguin(Var1 is {Var1 \= john,Var1 \= sam}),not o_penguin_1(Var1 is {Var1 \= john,Var1 \= sam}),Var1 \= sam,[],[],[],[],not o_ab_2(Var1 is {Var1 \= john,Var1 \= sam}),not wounded_bird(Var1 is {Var1 \= john,Var1 \= sam}),not o_wounded_bird_1(Var1 is {Var1 \= john,Var1 \= sam}),Var1 \= john,[],[],[],[],[],[],[],[],[],not o_chk_2,not o__chk_2_1,forall(Var7,not o__chk_2_1(Var7)),not o__chk_2_1(sam),not -penguin(sam),not 'o_-penguin_1'(sam),penguin(sam),[],[],[],[],not o__chk_2_1(Var2 is {Var2 \= sam}),-penguin(Var2 is {Var2 \= sam}),not penguin(Var2 is {Var2 \= sam}),not o_penguin_1(Var2 is {Var2 \= sam}),Var2 \= sam,[],[],[],[],not penguin(Var2 is {Var2 \= sam}),not o_penguin_1(Var2 is {Var2 \= sam}),Var2 \= sam,[],[],[],[],[],[],[],not o_chk_3,not o__chk_3_1,forall(Var8,not o__chk_3_1(Var8)),not o__chk_3_1(john),not -wounded_bird(john),not 'o_-wounded_bird_1'(john),wounded_bird(john),[],[],[],[],not o__chk_3_1(Var3 is {Var3 \= john}),-wounded_bird(Var3 is {Var3 \= john}),not wounded_bird(Var3 is {Var3 \= john}),not o_wounded_bird_1(Var3 is {Var3 \= john}),Var3 \= john,[],[],[],[],not wounded_bird(Var3 is {Var3 \= john}),not o_wounded_bird_1(Var3 is {Var3 \= john}),Var3 \= john,[],[],[],[],[],[],[],not o_chk_4,not o__chk_4_1,forall(Var9,not o__chk_4_1(Var9)),not o__chk_4_1(tweety),not -bird(tweety),not 'o_-bird_1'(tweety),bird(tweety),[],[],[],[],not o__chk_4_1(john),not -bird(john),not 'o_-bird_1'(john),bird(john),wounded_bird(john),[],[],[],[],[],not o__chk_4_1(Var4 is {Var4 \= john,Var4 \= tweety}),-bird(Var4 is {Var4 \= john,Var4 \= tweety}),not bird(Var4 is {Var4 \= john,Var4 \= tweety}),not o_bird_1(Var4 is {Var4 \= john,Var4 \= tweety}),Var4 \= tweety,[],[],not o_bird_2(Var4 is {Var4 \= john,Var4 \= tweety}),forall(Var10,forall(Var11,not o_bird_2(Var4 is {Var4 \= john,Var4 \= tweety},Var10,Var11))),not o_bird_2(Var4 is {Var4 \= john,Var4 \= tweety},Var12,Var13),not penguin(Var4 is {Var4 \= john,Var4 \= tweety},Var12),[],[],[],[],not o_bird_3(Var4 is {Var4 \= john,Var4 \= tweety}),not wounded_bird(Var4 is {Var4 \= john,Var4 \= tweety}),not o_wounded_bird_1(Var4 is {Var4 \= john,Var4 \= tweety}),Var4 \= john,[],[],[],[],[],[],not bird(Var4 is {Var4 \= john,Var4 \= tweety}),not o_bird_1(Var4 is {Var4 \= john,Var4 \= tweety}),Var4 \= tweety,[],[],not o_bird_2(Var4 is {Var4 \= john,Var4 \= tweety}),forall(Var14,forall(Var15,not o_bird_2(Var4 is {Var4 \= john,Var4 \= tweety},Var14,Var15))),not o_bird_2(Var4 is {Var4 \= john,Var4 \= tweety},Var16,Var17),not penguin(Var4 is {Var4 \= john,Var4 \= tweety},Var16),[],[],[],[],not o_bird_3(Var4 is {Var4 \= john,Var4 \= tweety}),not wounded_bird(Var4 is {Var4 \= john,Var4 \= tweety}),not o_wounded_bird_1(Var4 is {Var4 \= john,Var4 \= tweety}),Var4 \= john,[],[],[],[],[],[],[],[],[],not o_chk_5,not o__chk_5_1,forall(Var18,not o__chk_5_1(Var18)),not o__chk_5_1(tweety),not -flies(tweety),not 'o_-flies_1'(tweety),not ab(tweety),not o_ab_1(tweety),not penguin(tweety),not o_penguin_1(tweety),tweety \= sam,[],[],[],[],not o_ab_2(tweety),not wounded_bird(tweety),not o_wounded_bird_1(tweety),tweety \= john,[],[],[],[],[],[],not 'o_-flies_2'(tweety),not -bird(tweety),not 'o_-bird_1'(tweety),bird(tweety),[],[],[],[],[],[],not o__chk_5_1(sam),-flies(sam),ab(sam),penguin(sam),[],[],[],not flies(sam),not o_flies_1(sam),not bird(sam),not o_bird_1(sam),sam \= tweety,[],[],not o_bird_2(sam),forall(Var19,forall(Var20,not o_bird_2(sam,Var19,Var20))),not o_bird_2(sam,Var21,Var22),not penguin(sam,Var21),[],[],[],[],not o_bird_3(sam),not wounded_bird(sam),not o_wounded_bird_1(sam),sam \= john,[],[],[],[],[],[],[],[],not o__chk_5_1(john),-flies(john),ab(john),wounded_bird(john),[],[],[],not flies(john),not o_flies_1(john),proved(bird(john)),[],proved(ab(john)),[],[],[],[],not o__chk_5_1(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),-flies(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),-bird(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),not bird(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),not o_bird_1(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),Var5 \= tweety,[],[],not o_bird_2(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),forall(Var23,forall(Var24,not o_bird_2(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety},Var23,Var24))),not o_bird_2(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety},Var25,Var26),not penguin(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety},Var25),[],[],[],[],not o_bird_3(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),not wounded_bird(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),not o_wounded_bird_1(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),Var5 \= john,[],[],[],[],[],[],[],not flies(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),not o_flies_1(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),not bird(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),not o_bird_1(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),Var5 \= tweety,[],[],not o_bird_2(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),forall(Var27,forall(Var28,not o_bird_2(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety},Var27,Var28))),not o_bird_2(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety},Var29,Var30),not penguin(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety},Var29),[],[],[],[],not o_bird_3(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),not wounded_bird(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),not o_wounded_bird_1(Var5 is {Var5 \= john,Var5 \= sam,Var5 \= tweety}),Var5 \= john,[],[],[],[],[],[],[],[],[],[],[],[],[]], _).