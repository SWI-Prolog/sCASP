:- module(stack,
          [ justification_tree/3,		% :Stack, -JustTree, +Options
            print_justification_tree/1,         % :JustTree
            print_justification_tree/2,         % :JustTree, +Options
            unqualify_justitication_tree/3      % +TreeIn, +Module, -TreeOut
          ]).
:- use_module(predicates).
:- use_module(common).
:- use_module(modules).

:- meta_predicate
    justification_tree(:, -, +),
    print_justification_tree(:),
    print_justification_tree(:, +).

%!  justification_tree(:Stack, -JustificationTree, +Options)
%
%   Process Stack as produced by solve/4 into a justification tree.
%   Options include:
%
%     - format(+Format)
%       One of `tree` (default) or `list`.
%
%   The `tree` format is defined as follows.
%
%     - The tree as a whole is module-qualified with the same
%       module as the input Stack.
%     - Each node takes the shape `Node-Children`, where `Node`
%       is an _atom_ (in the logical sense) and `Children` is
%       a (possibly empty) list of sub-trees that justify the
%       `Node`.
%     - Nodes (_atom_) may be wrapped in one or more of
%       - not(Node)
%         NAF negation
%       - -(Node)
%         Classical negation
%       - chs(Node)
%         Node was proven by co-induction ("it is assumed that ...")
%       - proved(Node)
%         Node was proven before ("justified above")
%
%   The root node has the atom `query`  and has two children: the actual
%   query  and  the  atom  `o_nmr_check`  which  represents  the  global
%   constraints.

:- det(justification_tree/3).

justification_tree(M:Stack, M:JustificationTree, Options) :-
    reverse(Stack, RevStack),
    enumerate([query|RevStack],EnumStack,1,1),
    (   option(format(tree), Options, tree)
    ->  collect_children(EnumStack, Children, 1),
        filter_tree(Children, M, [JustificationTree])
    ;   collect_parents(EnumStack, JustificationTree)
    ).

%!  enumerate(+StackOut, -EnumStack, +Parent, +Order)
%
%   Give each node in the stack a rank, which is a I-J-K-... term that+M
%   represents its position in the top-down/left-right layout of the
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
filter_tree([Term0-Children|Cs], M, [Term-FChildren|Fs]) :-
    raise_negation(Term0, Term),
    selected(Term, M), !,
    filter_tree(Children, M, FChildren),
    filter_tree(Cs, M, Fs).
filter_tree([_-Childs|Cs], M, FilterChildren) :-
    append(Childs, Cs, AllCs),
    filter_tree(AllCs, M, FilterChildren).


selected(query, _) => true.
selected(proved(_), _) => true.
selected(chs(_), _) => true.
selected(assume(_), _) => true.
selected(not(Goal), _) =>
    \+ aux_predicate(Goal).
selected(-(Goal), M) =>
    selected(Goal, M).
selected(Goal, M) =>
    (   user_predicate(M:Goal)
    ->  true
    ;   is_global_constraint(Goal)
    ).

aux_predicate(-(o_,_)) :- !.                    % copied from io.pl
aux_predicate(A) :-
    functor(A, Name, _Arity),
    sub_atom(Name, 0, _, _, o_),
    \+ is_global_constraint(Name).

is_global_constraint(Atom) :-
    atom(Atom),
    atom_concat(o_chk_, NA, Atom),
    atom_number(NA, _).

%!  print_justification_tree(:Tree) is det.
%!  print_justification_tree(:Tree, +Options) is det.
%
%   Print the justification tree as returned by justification_tree/3 or
%   scasp_justification/2.

print_justification_tree(Tree) :-
    print_justification_tree(Tree, []).

:- det(print_justification_tree/2).

print_justification_tree(M:Tree, Options) :-
    plain_output(Tree, [depth(1),module(M)|Options]).

%!  plain_output(+FilterChildren, +Options)

plain_output(Term-[], Options) :-
    !,
    option(depth(D), Options),
    Indent is D*3,
    nl, tab(Indent),
    term(Term, Options).
plain_output(Term-Children, Options) :-
    !,
    select_option(depth(D), Options, Options1),
    Indent is D*3,
    connector(implies, Conn, Options),
    nl, tab(Indent), term(Term, Options), format(" ~w",[Conn]),
    D1 is D+1,
    plain_output_children(Children, [depth(D1)|Options1]),
    (   D == 0
    ->  format(".", [])
    ;   true
    ).

plain_output_children([A,B|Rs], Options) :-
    !,
    plain_output(A, Options),
    connector(and, Conn, Options),
    format("~w",[Conn]),
    plain_output_children([B|Rs], Options).
plain_output_children([A], Options) :-
    !,
    plain_output(A, Options).

%!  term(+Node, +Options) is det.
%
%   Print a, possibly annotated, atom.

term(not(Term), Options) :-
    !,
    format("not ", []),
    term(Term, Options).
term(-Term, Options) :-
    !,
    connector(negation, Conn, Options),
    format('~w', [Conn]),
    print(Term).
term(proved(Term), Options) :-
    !,
    term1(proved, Term, Options).
term(assume(Term), Options) :-
    !,
    term1(assume, Term, Options).
term(chs(Term), Options) :-
    !,
    term1(chs, Term, Options).
term(Term, _Options) :-
    print(Term).

term1(Functor, Arg, Options) :-
    format('~w(', [Functor]),
    term(Arg, Options),
    format(')', []).

connector(Semantics, Conn, Options) :-
    option(format(Format), Options, unicode),
    connector_string(Semantics, Format, Conn).

connector_string(implies,  ascii, ':-').
connector_string(and,      ascii, ',').
connector_string(negation, ascii, '-').
connector_string(implies,  unicode, '\u2190').   % <-
connector_string(and,      unicode, ' \u2227').  % /\
connector_string(negation, unicode, '\u00ac ').  % -

%!  unqualify_justitication_tree(:TreeIn, +Module, -TreeOut) is det.
%
%   Unqualify the nodes in TreeIn, turning the nodes qualified to module
%   Module into plain nodes.

unqualify_justitication_tree(_:Tree0, Module, Tree) :-
    is_list(Tree0),
    !,
    maplist(unqualify_just(Module), Tree0, Tree).
unqualify_justitication_tree(_:Tree0, Module, Tree) :-
    unqualify_just(Module, Tree0, Tree).

unqualify_just(M, Node0-Children0, Node-Children) :-
    unqualify_model_term(M, Node0, Node),
    maplist(unqualify_just(M), Children0, Children).
