:- module(scasp_stack,
          [ justification_tree/3,		% :Stack, -JustTree, +Options
            print_justification_tree/1,         % :JustTree
            print_justification_tree/2,         % :JustTree, +Options
            unqualify_justitication_tree/3      % +TreeIn, +Module, -TreeOut
          ]).
:- use_module(predicates).
:- use_module(common).
:- use_module(modules).
:- use_module(output).

:- autoload(library(apply), [maplist/3]).
:- autoload(library(lists), [reverse/2, append/3]).
:- autoload(library(option),
            [option/2, merge_options/3, option/3, select_option/3]).

:- meta_predicate
    justification_tree(:, -, +),
    print_justification_tree(:),
    print_justification_tree(:, +).

:- multifile
    justification_tree_hook/2.

%!  justification_tree(:Stack, -JustificationTree, +Options)
%
%   Process Stack as produced by solve/4 into a justification tree.
%   Options include:
%
%      <none yet>
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
%       - assume(Node)
%         Node was assumed (matching chs(Node)).
%
%   The root node has the atom `query`  and has two children: the actual
%   query  and  the  atom  `o_nmr_check`  which  represents  the  global
%   constraints.

:- det(justification_tree/3).

justification_tree(M:Stack, M:JustificationTree, Options) :-
    reverse(Stack, RevStack),
    stack_tree([query|RevStack], Trees),
    filter_tree(Trees, M, [JustificationTree], Options).

%!  stack_tree(+Stack, -Tree) is det.
%
%   Translate the solver  Stack  into  a   tree.  The  solver  stack  is
%   represented as a flat list  of   proved  goals  where `[]` indicates
%   _this branch is complete_.  Here are some examples
%
%     [p, []]				p-[]
%     [p, q, [], []]			p-[q-[]]
%     [p, q, [], r, [], []]		p-[q-[],r-[]]
%
%     We maintain a stack of  difference  lists   in  the  4th  argument.
%     On encountering a `[]` we pop this stack.

stack_tree(Stack, Tree) :-
    stack_tree(Stack, Tree, [], []).

stack_tree([], Children0, Children, []) =>
    Children = Children0.
stack_tree([[]|Stack], Children0, Children, [T0/T|Parents]) =>
    Children = Children0,
    stack_tree(Stack, T0, T, Parents).
stack_tree([H|Stack], Tree, T, Parents) =>
    Tree = [H-Children|T0],
    stack_tree(Stack, Children, [], [T0/T|Parents]).


%!  filter_tree(+Children, +Module, -FilteredChildren, +Options)
%
%   Clean the tree from less  interesting   details.  By default removes
%   auxiliary nodes created as part of the compilation and the NMR proof
%   if this is empty. Additional filtering is based on Options:
%
%      - pos(true)
%        Remove all not(_) nodes from the tree.
%      - long(true)
%        Keep the full tree, including forall() and intermediate nodes.

filter_tree(Tree, _, Tree, Options) :-
    option(long(true), Options), !.
filter_tree([],_,[], _) :- !.
filter_tree([goal_origin(Atom0,_)-[goal_origin(Abd, O)-_]|Cs],
            M,
            [goal_origin(abduced(Atom), O)-[]|Fs], Options) :-
    abduction_justification(Abd),
    !,
    raise_negation(Atom0, Atom),
    filter_tree(Cs, M, Fs, Options).
filter_tree([ proved(Atom0)-[],
              not(_Neg)-[ not(_) - [proved(not(Abd))-[]]]],
            _M,
            [abduced(Atom)-[]], _Options) :-
    abduction_justification(Abd),
    !,
    raise_negation(Atom0, Atom).
filter_tree([ not(Atom0)-[not(_Neg)-[proved(not(Abd))-[]]]],
            _M,
            [abduced(Atom)-[]], _Options) :-
    raise_negation(Atom0, Atom1),
    neg(Atom1, Atom),
    abduction_justification(Abd),
    !.
filter_tree([goal_origin(Term0,O)-Children|Cs], M, Tree, Options) :-
    filter_pos(Term0, Options),
    raise_negation(Term0, Term),
    selected(Term, M), !,
    filter_tree(Children, M, FChildren, Options),
    Tree = [goal_origin(Term, O)-FChildren|Fs],
    filter_tree(Cs, M, Fs, Options).
filter_tree([Term0-Children|Cs], M, Tree, Options) :-
    filter_pos(Term0, Options),
    raise_negation(Term0, Term),
    selected(Term, M), !,
    filter_tree(Children, M, FChildren, Options),
    (   Term == o_nmr_check, FChildren == []
    ->  Tree = Fs
    ;   Tree = [Term-FChildren|Fs]
    ),
    filter_tree(Cs, M, Fs, Options).
filter_tree([_-Childs|Cs], M, FilterChildren, Options) :-
    append(Childs, Cs, AllCs),
    filter_tree(AllCs, M, FilterChildren, Options).

neg(-A, Neg) => Neg = A.
neg(A, Neg) => Neg = -A.

%!  filter_pos(+Node, +Options) is semidet.
%
%   Filter negated nodes when  ``--pos``  is   active.  We  should _not_
%   filter the global constraint nodes.

filter_pos(not(GC), _Options), is_global_constraint(GC) =>
    true.
filter_pos(not(_), Options) =>
    \+ option(pos(true), Options).
filter_pos(_, _) =>
    true.

selected(query, _) => true.
selected(proved(_), _) => true.
selected(chs(_), _) => true.
selected(assume(_), _) => true.
selected(not(-Goal), _) =>
    \+ aux_predicate(Goal).
selected(not(Goal), _) =>
    \+ aux_predicate(Goal).
selected(-(Goal), M) =>
    selected(Goal, M).
selected(findall(_,_,_), _) => true.
selected(is(_,_), _) => true.
selected(_>_, _) => true.
selected(_>=_, _) => true.
selected(_<_, _) => true.
selected(_=<_, _) => true.
selected(Goal, M) =>
    (   aux_predicate(Goal)
    ->  fail
    ;   (   current_predicate(M:pr_user_predicate/1)
        ->  user_predicate(M:Goal)
        ;   true
        )
    ->  true
    ;   is_global_constraint(Goal)
    ).

aux_predicate(-(o_,_)) :- !.                    % copied from io.pl
aux_predicate(A) :-
    functor(A, Name, _Arity),
    sub_atom(Name, 0, _, _, o_),
    \+ is_global_constraint(Name).

is_global_constraint(o_nmr_check) :-
    !.
is_global_constraint(Atom) :-
    atom(Atom),
    atom_concat(o_chk_, NA, Atom),
    atom_number(NA, _).


abduction_justification(Abd) :-
    atom(Abd),
    sub_atom(Abd, _, _, _, ':abducible$'),
    !.


%!  print_justification_tree(:Tree) is det.
%!  print_justification_tree(:Tree, +Options) is det.
%
%   Print the justification tree as  returned by justification_tree/3 or
%   scasp_justification/2. The tree is  printed   to  the current output
%   stream. Options:
%
%     - format(+Format)
%       One of `unicode` (default) or `ascii`.
%     - depth(+Integer)
%       Initial indentation (0)
%     - indent(+Integer)
%       Indent increment per level (3).
%     - full_stop(+Bool)
%       End the tree with a full stop and newline.  If `false`,
%       it ends with the last atom.

print_justification_tree(Tree) :-
    print_justification_tree(Tree, []).

:- det(print_justification_tree/2).

print_justification_tree(Tree, Options) :-
    justification_tree_hook(Tree, Options),
    !.
print_justification_tree(M:Tree, Options) :-
    merge_options(Options, [depth(0),module(M)], Options1),
    plain_output(Tree, Options1),
    (   option(full_stop(true), Options, true)
    ->  format(".~n", [])
    ;   true
    ).

%!  plain_output(+FilterChildren, +Options)

plain_output(goal_origin(Term, _)-Children, Options) :-
    !,
    plain_output(Term-Children, Options).
plain_output(Term-[], Options) :-
    !,
    option(depth(D), Options),
    Indent is D*3,
    nl_indent(Indent),
    term(Term, Options).
plain_output(Term-Children, Options) :-
    !,
    select_option(depth(D), Options, Options1),
    option(indent(Width), Options1, 3),
    Indent is D*Width,
    connector(implies, Conn, Options),
    nl_indent(Indent), term(Term, Options), format(" ~w",[Conn]),
    D1 is D+1,
    plain_output_children(Children, [depth(D1)|Options1]).

nl_indent(Indent) :-
    format('~N~t~*|', [Indent]).

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
%   Print a, possibly annotated, stack atom.

term(proved(Term), Options) =>
    term1(proved, Term, Options).
term(assume(Term), Options) =>
    term1(assume, Term, Options).
term(chs(Term), Options) =>
    term1(chs, Term, Options).
term(Term, Options) =>
    print_model_term(Term, Options).

term1(Functor, Arg, Options) :-
    print_connector(Functor, Options),
    format('(', []),
    print_model_term(Arg, Options),
    format(')', []).

%!  unqualify_justitication_tree(:TreeIn, +Module, -TreeOut) is det.
%
%   Unqualify the nodes in TreeIn, turning the nodes qualified to module
%   Module into plain nodes.

:- det(unqualify_justitication_tree/3).

unqualify_justitication_tree(_:Tree0, Module, Tree) :-
    is_list(Tree0),
    !,
    maplist(unqualify_just(Module), Tree0, Tree).
unqualify_justitication_tree(_:Tree0, Module, Tree) :-
    unqualify_just(Module, Tree0, Tree).

unqualify_just(M, Node0-Children0, Node-Children) :-
    unqualify_model_term(M, Node0, Node),
    maplist(unqualify_just(M), Children0, Children).
