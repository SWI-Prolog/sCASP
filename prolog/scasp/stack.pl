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
%       One of `tree` (default) or `list`.

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
    raise_negation(Term0, Term),
    selected(Term, M), !,
    filter_tree(Childs, M, FChilds),
    filter_tree(Cs, M, Fs).
filter_tree([_-Childs|Cs], M, FilterChildren) :-
    append(Childs, Cs, AllCs),
    filter_tree(AllCs, M, FilterChildren).


selected(query, _) => true.
selected(proved(_), _) => true.
selected(chs(_), _) => true.
selected(assume(_), _) => true.
selected(not(Goal), M) =>
    selected(Goal, M).
selected(-(Goal), M) =>
    selected(Goal, M).
selected(Goal, M) =>
    (   user_predicate(M:Goal)
    ->  true
    ;   is_global_constraint(Goal)
    ).

is_global_constraint(Atom) :-
    atom(Atom),
    atom_concat(o_chk_, NA, Atom),
    atom_number(NA, _).

%!  print_justification_tree(+Tree) is det.
%!  print_justification_tree(+Tree, +Options) is det.
%
%   Print the justification tree as returned by process_stack/3

print_justification_tree(Tree) :-
    print_justification_tree(Tree, []).

print_justification_tree(Tree, Options) :-
    plain_output(Tree, [depth(1)|Options]).

%!  plain_output(+FilterChildren, +Options)

plain_output([A,B|Rs], Options) :- !,
    plain_output_(A, Options),
    connector(and, Conn, Options),
    format("~w",[Conn]),
    plain_output([B|Rs], Options).
plain_output([A], 0) :- !,
    plain_output_(A, 0),
    format(".\n",[]).
plain_output([A], Options) :- !,
    plain_output_(A, Options).

plain_output_(Term-[], Options) :- !,
    option(depth(D), Options),
    Indent is D*3,
    nl, tab(Indent),
    term(Term, Options).
plain_output_(Term-Child, Options) :- !,
    select_option(depth(D), Options, Options1),
    Indent is D*3,
    connector(implies, Conn, Options),
    nl, tab(Indent), term(Term, Options), format(" ~w",[Conn]),
    D1 is D+1,
    plain_output(Child, [depth(D1)|Options1]).

term(not(Term), Options) :-
    !,
    format("not ", []),
    term(Term, Options).
term(Term, _Options) :-
    print(Term).

connector(Semantics, Conn, Options) :-
    option(format(Format), Options, unicode),
    connector_string(Semantics, Format, Conn).

connector_string(implies, ascii, ':-').
connector_string(and,     ascii, ',').
connector_string(implies, unicode, '\u2190').
connector_string(and,     unicode, ' \u2227').
