:- module(scasp_proof_skeleton,
          [ proof_skeleton/3,           % +Tree,  +Level, -Skeleton
            answers_skeleton/3,         % +Trees, +Level, -Skeleton
            model_key/2,                % +Model, -Key
            answers_model_key/2,        % +Models, -Key
            skeleton_level/1,           % ?Level
            weakest_agreement/3,        % +TreeA,  +TreeB,  -Level
            answers_agreement/3,        % +TreesA, +TreesB, -Level
            proof_literals/2            % +Tree, -Literals
          ]).
:- use_module(library(apply), [maplist/3, maplist/2, foldl/4, foldl/5, foldl/6]).
:- use_module(library(lists), [append/3, member/2, reverse/2]).
:- use_module(library(pairs), [pairs_keys_values/3]).

/** <module> Rewrite-stable abstractions of an s(CASP) justification

The model of an answer is compared exactly.  Its justification is not:
the *location* of individual steps moves whenever the solver changes,
even though the proof is the same.  Two drifts already present in the
test data show the two shapes this takes:

  - `test/all_programs/solve_findall.pl` gained a `findall/3`
    bookkeeping node *between* a literal and its children.
  - `test/all_programs/sasp/abdbirds.pl` had an abduction subtree
    expanded from `abduced(X)` into the raw `abducible`/`chs` machinery.

proof_skeleton/3 maps a justification onto a canonical term at a chosen
level of abstraction, so a test can say how much drift it tolerates.
Variable *naming* is normalised at every level: only the sharing pattern
is compared.  Levels, strict to loose:

  | `exact`    | the whole tree, incl. clause refs and sibling order  |
  | `refless`  | additionally: no clause refs, abduction collapsed    |
  | `literals` | additionally: bookkeeping nodes deleted              |
  | `support`  | the set of literal parent->child edges               |
  | `reached`  | the set of (literal,status) pairs; no structure      |

`support` is the default for tests.  It tolerates sibling reordering,
inserted bookkeeping nodes and clause renumbering, but still fails when
a literal is proved by a different route, moves to another branch, or a
justification step disappears.

@see test/test_proof_skeleton.pl for the behaviour this guarantees.
*/

%!  skeleton_level(?Level) is nondet.
%
%   True when Level is a known abstraction level, strongest first.

skeleton_level(exact).
skeleton_level(refless).
skeleton_level(literals).
skeleton_level(support).
skeleton_level(reached).

%!  proof_skeleton(+Tree, +Level, -Skeleton) is det.
%
%   Skeleton is the canonical form of  justification Tree at Level.  Two
%   justifications represent the same proof at Level iff their skeletons
%   are `==`.  The level name is part of  the  skeleton, so skeletons of
%   different levels never compare equal by accident.

proof_skeleton(M:Tree, Level, Skeleton) :-
    !,
    ignore(M = M),
    proof_skeleton(Tree, Level, Skeleton).
proof_skeleton(Tree, exact, Skeleton) :-
    !,
    canonical_vars(Tree, Canonical),
    Skeleton = exact(Canonical).
proof_skeleton(Tree, Level, Skeleton) :-
    normalise(Tree, Norm),
    reduce(Level, Norm, Reduced),
    canonical_vars(Reduced, Canonical),
    Skeleton =.. [Level, Canonical].

%!  answers_skeleton(+Trees, +Level, -Skeleton) is det.
%
%   As proof_skeleton/3 for the list of  justifications of all answers.
%   The answers are sorted, so their *order* does not matter while the
%   multiset of proofs does.

answers_skeleton(Trees, Level, answers(Level, Sorted)) :-
    maplist(answer_skeleton(Level), Trees, Skeletons),
    msort(Skeletons, Sorted).

answer_skeleton(Level, Tree, Inner) :-
    proof_skeleton(Tree, Level, Skeleton),
    Skeleton =.. [_, Inner].

%!  model_key(+Model, -Key) is det.
%!  answers_model_key(+Models, -Key) is det.
%
%   Canonical key for a model.  Models  are  compared *exactly*: only
%   variable naming is normalised.  answers_model_key/2 additionally
%   sorts the answers, so answer order does not matter.

model_key(Model, Key) :-
    canonical_vars(Model, Key).

answers_model_key(Models, models(Sorted)) :-
    maplist(model_key, Models, Keys),
    msort(Keys, Sorted).

%!  weakest_agreement(+TreeA, +TreeB, -Level) is semidet.
%
%   Level is the strongest level at which  TreeA and TreeB agree.  Fails
%   when they disagree even at the weakest level.  Used by the `--relax`
%   reporting so that loosening a test is a deliberate act.

weakest_agreement(A, B, Level) :-
    skeleton_level(Level),
    proof_skeleton(A, Level, SA),
    proof_skeleton(B, Level, SB),
    SA == SB,
    !.

%!  proof_literals(+Tree, -Literals) is det.
%
%   Literals is the sorted set of  Status-Literal pairs appearing anywhere
%   in Tree, with Status one of `plain`, `chs`, `proved` or `assume`.  For
%   a test that wants to name the steps a proof must contain without
%   pinning the shape of the whole tree.

proof_literals(Tree, Literals) :-
    proof_skeleton(Tree, reached, reached(Literals)).

%!  answers_agreement(+TreesA, +TreesB, -Level) is semidet.
%
%   As weakest_agreement/3 over the justifications of all answers.

answers_agreement(A, B, Level) :-
    skeleton_level(Level),
    answers_skeleton(A, Level, SA),
    answers_skeleton(B, Level, SB),
    SA == SB,
    !.


		 /*******************************
		 *          NORMALISE		*
		 *******************************/

%!  normalise(+Tree, -Normalised) is det.
%
%   Remove the accidental differences:  clause  reference numbers, which
%   renumber whenever clauses are reordered, and  the two spellings of an
%   abduced literal.  Keeps the tree shape.

normalise(Node0-Children0, Tree) :-
    !,
    maplist(normalise, Children0, Children1),
    strip_ref(Node0, Node1),
    (   abduction_collapse(Node1, Children1, Collapsed)
    ->  Tree = Collapsed
    ;   Tree = Node1-Children1
    ).
normalise(Node0, Node) :-
    strip_ref(Node0, Node).

%!  strip_ref(+Node, -Plain) is det.
%
%   Drop the clause index of goal_origin/2.   It is a positional clause
%   number: reordering the clauses of  a  predicate renumbers it without
%   changing the proof.

strip_ref(Var, Plain) :-
    var(Var),
    !,
    Plain = Var.
strip_ref(goal_origin(Node0, _Ref), Plain) :-
    !,
    strip_ref(Node0, Plain).
strip_ref(Node, Plain) :-
    wrapper(Node, Inner0),
    !,
    strip_ref(Inner0, Inner),
    functor(Node, Name, 1),
    Plain =.. [Name, Inner].
strip_ref(Node, Node).

%!  wrapper(?Node, ?Inner) is semidet.
%
%   Node is Inner wrapped in one of the node decorations of the `tree`
%   format documented in stack.pl.

wrapper(not(A),    A).
wrapper(-(A),      A).
wrapper(chs(A),    A).
wrapper(proved(A), A).
wrapper(assume(A), A).

%!  abduction_collapse(+Node, +Children, -Tree) is semidet.
%
%   An abduced literal is shown either as  a bare `abduced(L)` leaf or as
%   the raw machinery that abduces it. Normalise both to the leaf.

abduction_collapse(abduced(L), _, abduced(L)-[]) :-
    !.
abduction_collapse(Node, Children, abduced(Node)-[]) :-
    bare_literal(Node),
    Children \== [],
    abduction_support(Children, Node),
    \+ \+ ( member(C, Children),
            abduction_evidence(C, Node)
          ).

%!  abduction_support(+Trees, +Literal) is semidet.
%
%   True when every node below Trees is part of the abduction machinery
%   for Literal, so the whole subtree says no more than "L was abduced".

abduction_support([], _).
abduction_support([Node-Children|T], L) :-
    abduction_node(Node, L),
    abduction_support(Children, L),
    abduction_support(T, L).

abduction_evidence(Node-Children, L) :-
    (   abduction_marker(Node, L)
    ->  true
    ;   member(C, Children),
        abduction_evidence(C, L)
    ).

abduction_node(Node, L) :- abduction_marker(Node, L), !.
abduction_node(chs(L0), L)    :- !, variant_of(L0, L).
abduction_node(proved(L0), L) :- !, variant_of(L0, L).

abduction_marker(Node, L) :-
    strip_negation(Node, Inner),
    compound(Inner),
    compound_name_arguments(Inner, Name, [Arg]),
    abducible_functor(Name),
    variant_of(Arg, L).

abducible_functor(abducible).
abducible_functor(o_abducible).
abducible_functor(abduced).
abducible_functor(Name) :-
    sub_atom(Name, _, _, 0, abducible).

strip_negation(not(A), I) :- !, strip_negation(A, I).
strip_negation(-(A),   I) :- !, strip_negation(A, I).
strip_negation(chs(A), I) :- !, strip_negation(A, I).
strip_negation(A, A).

variant_of(A, B) :- A =@= B.

%!  bare_literal(+Node) is semidet.
%
%   A node that carries a program literal rather than solver bookkeeping.

bare_literal(Node) :-
    nonvar(Node),
    \+ bookkeeping(Node),
    \+ wrapper(Node, _).


		 /*******************************
		 *           REDUCE		*
		 *******************************/

reduce(refless, Tree, Tree).
reduce(literals, Tree, Reduced) :-
    prune([Tree], [Reduced0]),
    !,
    Reduced = Reduced0.
reduce(literals, _, none).
reduce(support, Tree, Roots-Edges) :-
    reduce(literals, Tree, Pruned),
    tree_roots(Pruned, Roots0),
    findall(P-C, edge(Pruned, P, C), Edges0),
    canonical_vars(Roots0-Edges0, Roots-Edges1),
    msort(Edges1, Edges).
reduce(reached, Tree, Nodes) :-
    reduce(literals, Tree, Pruned),
    findall(Status-Literal, reached_node(Pruned, Status, Literal), Nodes0),
    canonical_vars(Nodes0, Nodes1),
    msort(Nodes1, Nodes).

%!  prune(+Trees, -Pruned) is det.
%
%   Delete solver bookkeeping nodes, lifting  their children onto the
%   nearest surviving ancestor.  This is  the operation that makes the
%   inserted findall/3 node of solve_findall.pl invisible.

prune([], []).
prune([Node-Children|T], Pruned) :-
    prune(Children, PrunedChildren),
    (   bookkeeping(Node)
    ->  prune(T, PrunedT),
        append(PrunedChildren, PrunedT, Pruned)
    ;   prune(T, PrunedT),
        Pruned = [Node-PrunedChildren|PrunedT]
    ).
prune([Node|T], [Node|PrunedT]) :-
    prune(T, PrunedT).

%!  bookkeeping(+Node) is semidet.
%
%   Nodes that record *how* the solver proceeded rather than *what* was
%   proved.  Their presence is an implementation detail.

bookkeeping(Node) :-
    nonvar(Node),
    compound(Node),
    functor(Node, Name, Arity),
    bookkeeping_functor(Name, Arity).

bookkeeping_functor(findall,  3).
bookkeeping_functor(forall,   2).
bookkeeping_functor(c_forall, 2).
bookkeeping_functor(prolog,   1).

tree_roots(Node-_, [Node]) :- !.
tree_roots(List, Roots) :-
    is_list(List),
    !,
    findall(N, (member(E, List), tree_roots(E, Ns), member(N, Ns)), Roots).
tree_roots(Node, [Node]).

%!  edge(+Tree, -Parent, -Child) is nondet.
%
%   Parent immediately dominates Child, in pre-order.

edge(Node-Children, Parent, Child) :-
    !,
    (   member(C-GCs, Children),
        (   Parent = Node,
            Child = C
        ;   edge(C-GCs, Parent, Child)
        )
    ).
edge(List, Parent, Child) :-
    is_list(List),
    member(E, List),
    edge(E, Parent, Child).

reached_node(Node-Children, Status, Literal) :-
    !,
    (   node_status(Node, Status, Literal)
    ;   member(C, Children),
        reached_node(C, Status, Literal)
    ).
reached_node(List, Status, Literal) :-
    is_list(List),
    member(E, List),
    reached_node(E, Status, Literal).

node_status(chs(L),    chs,    L) :- !.
node_status(proved(L), proved, L) :- !.
node_status(assume(L), assume, L) :- !.
node_status(L,         plain,  L).


		 /*******************************
		 *        CANONICAL VARS	*
		 *******************************/

%!  canonical_vars(+Term, -Canonical) is det.
%
%   Rename every variable and every  '$VAR'(Name)  by  its first
%   occurrence in pre-order, so that variable *names* do not matter while
%   the sharing pattern still does.  Constraint stores attached with the
%   `'| '` operator are sorted, as the CLP(Q) dump order is not stable.

canonical_vars(Term0, Term) :-
    copy_term(Term0, Term1),
    sort_constraints(Term1, Term2),
    devar(Term2, Term3, [], _),
    collect_vars(Term3, [], Vars0),
    reverse(Vars0, Vars),
    foldl(bind_canonical, Vars, 0, _),
    Term = Term3.

bind_canonical(Var, N0, N) :-
    Var = '$VAR'(N0),
    N is N0+1.

%!  devar(+Term, -Term, +Map0, -Map) is det.
%
%   Replace each '$VAR'(Name) by a fresh variable, sharing one variable
%   per distinct Name.  Written names are ground, so they cannot be bound
%   to a canonical name in place.

devar(Term, Out, Map0, Map) :-
    var(Term),
    !,
    Out = Term,
    Map = Map0.
devar('$VAR'(Name), Out, Map0, Map) :-
    (   atom(Name)
    ;   integer(Name)
    ),
    !,
    (   memberchk_key(Name, Map0, Var)
    ->  Out = Var,
        Map = Map0
    ;   Out = Var,
        Map = [Name-Var|Map0]
    ).
devar(Term, Out, Map0, Map) :-
    compound(Term),
    !,
    compound_name_arguments(Term, Name, Args0),
    foldl(devar_, Args0, Args, Map0, Map),
    compound_name_arguments(Out, Name, Args).
devar(Term, Term, Map, Map).

devar_(In, Out, Map0, Map) :-
    devar(In, Out, Map0, Map).

memberchk_key(Name, [K-V|T], Var) :-
    (   K == Name
    ->  Var = V
    ;   memberchk_key(Name, T, Var)
    ).

collect_vars(Term, Acc, Vars) :-
    var(Term),
    !,
    add_var(Term, Acc, Vars).
collect_vars(Term, Acc, Vars) :-
    compound(Term),
    !,
    compound_name_arguments(Term, _, Args),
    foldl(collect_vars_, Args, Acc, Vars).
collect_vars(_, Acc, Acc).

collect_vars_(Term, Acc, Vars) :-
    collect_vars(Term, Acc, Vars).

add_var(V, Acc, Acc) :-
    memberchk_eq(V, Acc),
    !.
add_var(V, Acc, [V|Acc]).

memberchk_eq(X, [Y|T]) :-
    (   X == Y
    ->  true
    ;   memberchk_eq(X, T)
    ).

%!  sort_constraints(+Term, -Sorted) is det.
%
%   Sort the conjunction inside an inlined  constraint store, written by
%   output.pl as ``Var '| ' {Constraints}``.

sort_constraints(Term, Sorted) :-
    var(Term),
    !,
    Sorted = Term.
sort_constraints('| '(V, {C0}), Sorted) :-
    !,
    sort_constraints(V, V1),
    conj_list(C0, L0),
    maplist(sort_constraints, L0, L1),
    msort(L1, L2),
    conj_list(C1, L2),
    Sorted = '| '(V1, {C1}).
sort_constraints(Term, Sorted) :-
    compound(Term),
    !,
    compound_name_arguments(Term, Name, Args0),
    maplist(sort_constraints, Args0, Args),
    compound_name_arguments(Sorted, Name, Args).
sort_constraints(Term, Term).

conj_list(Conj, List) :-
    nonvar(Conj),
    !,
    conj_to_list(Conj, List).
conj_list(Conj, List) :-
    list_to_conj(List, Conj).

conj_to_list((A,B), List) :-
    !,
    conj_to_list(A, LA),
    conj_to_list(B, LB),
    append(LA, LB, List).
conj_to_list(A, [A]).

list_to_conj([One], One) :- !.
list_to_conj([H|T], (H,Conj)) :-
    list_to_conj(T, Conj).
