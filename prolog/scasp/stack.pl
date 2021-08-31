:- module(stack,
          [ justification_tree/3,		% :Stack, -JustTree, +Options
            print_justification_tree/1          % +JustTree
          ]).
:- use_module(predicates).
:- use_module(common).
:- op(900, fy, [not]). %% To be removed
:- meta_predicate
    justification_tree(:, -, +).

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


%!  justification_tree(+Stack, -JustificationTree, +Options)
%
%   Process Stack as produced by solve/4 into a justification tree.
%   Options include:
%
%     - format(+Format)
%       One of `tree` or `list`.

justification_tree(M:Stack, JustificationTree, Options) :-
    reverse(Stack, RevStack),
    enumerate([query|RevStack],EnumStack,1,1),
    (   option(format(tree), Options, tree)
    ->  collect_children(EnumStack, Children, 1),
        filter_tree(Children, M, JustificationTree)
    ;   collect_parents(EnumStack, JustificationTree)
    ).

%!  enumerate(+StackOut, -EnumStack, +Parent, +Order)
%
%   Give each node in the stack a rank,   which is a I-J-K-... term that
%   represents its position in  the   top-down/left-right  layout of the
%   tree.
%
%   @arg EnumStack is a list `Term=Place`

enumerate([],[],_,_) :- !.
enumerate([[]],[],_,_) :- !.
enumerate([[]|Stack], Enum, P-PO, _) :- !,
    NO is PO + 1,
    enumerate(Stack, Enum, P, NO).
enumerate([Term|Stack], [Term=P-O|Enum], P, O) :-
    enumerate(Stack, Enum, P-O, 1).


%!  collect_children(+EnumStack, -Tree, +ParentId) is det.
%
%   Process the enumerated tree into a proper tree.  The tree takes
%   the shape `Node - Children`

collect_children([], [], _) :- !.
collect_children([Term=PId-O|Stack], [Term-Children | Cs], PId) :- !,
    collect_children(Stack, Cs, PId),
    collect_children(Stack, Children, PId-O).
collect_children([_|Stack], Cs, PId) :-
    collect_children(Stack, Cs, PId).


%! collect_parents(:EnumStack, :Childs, :ParentId)

collect_parents([], []) :- !.
collect_parents([Term=PId|Stack], [(Term, Siblings) | Cs]) :-
    collect_parents(Stack, Cs),
    collect_siblings(Stack, Siblings, PId).

collect_siblings([], [], _) :- !.
collect_siblings([(Term, PId-_)|Stack], [Term|Siblings], PId) :- !,
    collect_siblings(Stack, Siblings, PId).
collect_siblings([_|Stack], Siblings, PId) :-
    collect_siblings(Stack, Siblings, PId).


%! filter_tree(+Children, +Module, -FilteredChildren)

filter_tree([],_,[]).
filter_tree([Term0-Childs|Cs], M, [Term-FChilds|Fs]) :-
    selected(Term0, M), !,
    raise_negation(Term0, Term),
    filter_tree(Childs, M, FChilds),
    filter_tree(Cs, M, Fs).
filter_tree([_-Childs|Cs], M, FilterChildren) :-
    append(Childs, Cs, AllCs),
    filter_tree(AllCs, M, FilterChildren).


selected(query, _) => true.
selected(not(Goal), M) =>
    selected(Goal, M).
selected(Goal, M) =>
    user_predicate(M:Goal).

%!  print_justification_tree(+Tree) is det.
%
%   Print the justification tree as returned by process_stack/3

print_justification_tree(Tree) :-
    plain_output(Tree, 1).

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

plain_output_(Term-[], I) :- !,
    nl, tab(I), print(Term).
plain_output_(Term-Child, I) :- !,
    nl, tab(I), print(Term), format(" :-",[]),
    I1 is I + 3,
    plain_output(Child, I1).
