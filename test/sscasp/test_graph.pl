:- module(test_graph,
          [ test_graph/0
          ]).
:- use_module('../scasp_plunit', []).        % sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/graph)).
:- use_module(library(plunit)).
:- use_module(library(random), [random_member/2, maybe/0]).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(lists), [member/2, memberchk/2, append/3, numlist/3,
                               nth1/3, last/2]).
:- use_module(library(apply), [maplist/3]).

/** <module> The call graph and its odd loops

`sscasp/graph.pl` decides which clauses are on a loop over negation with an
odd number of negations — the loops that have no model when the rules are
read as implications, and so the ones the NMR check must be built from.

It decides it by *balance* rather than by enumerating cycles: a component's
cycles are all even exactly when its nodes can be assigned parities consistent
with every arc.  That argument is the load-bearing part, so the last unit here
checks it against a brute-force enumeration of the simple cycles, on
hand-written graphs and then on random ones.  Nothing clever can be wrong in a
way that agrees with something that enumerates.
*/

%!  program(-Clauses) is nondet.
%
%   The worked programs, at file level because two units use them.

program([(p :- not p)]).
program([(p :- not p), (p :- b), b]).
program([(p :- not q), (q :- not r), (r :- not p)]).
program([a, (p :- not p), b, (q :- not q), c]).
program([(p :- q), q]).
program([]).

test_graph :-
    run_tests([ graph_arcs,
                graph_odd_loops,
                graph_olon_clauses,
                graph_against_enumeration
              ]).

		 /*******************************
		 *         THE GRAPH		*
		 *******************************/

:- begin_tests(graph_arcs).

arcs(Clauses, Arcs) :-
    call_graph(Clauses, G),
    Arcs = G.arcs.

nodes(Clauses, Nodes) :-
    call_graph(Clauses, G),
    Nodes = G.nodes.

test(a_positive_call_is_a_positive_arc) :-
    arcs([(p :- q)], Arcs),
    assertion(Arcs == [arc(key(plain, p/0), key(plain, q/0), +)]).

test(a_negated_call_is_a_negative_arc) :-
    arcs([(p :- not q)], Arcs),
    assertion(Arcs == [arc(key(plain, p/0), key(plain, q/0), -)]).

%   `p' and `-p' are different atoms — nothing but the consistency
%   constraint relates them — so they are different nodes.

test(classical_negation_is_a_node_of_its_own) :-
    arcs([(p :- -(q))], Arcs),
    assertion(Arcs == [arc(key(plain, p/0), key(neg, q/0), +)]).

test(a_negated_classical_literal_is_a_negative_arc_to_it) :-
    arcs([(p :- not(-(q)))], Arcs),
    assertion(Arcs == [arc(key(plain, p/0), key(neg, q/0), -)]).

test(a_negative_head_is_a_node_of_its_own) :-
    arcs([(-(p) :- q)], Arcs),
    assertion(Arcs == [arc(key(neg, p/0), key(plain, q/0), +)]).

%   A built-in calls nothing, so an arc to one could not be part of a
%   cycle and it is not a node.

test(builtins_are_not_nodes) :-
    nodes([(p(X) :- X = 1, q(X))], Nodes),
    assertion(Nodes == [key(plain, p/1), key(plain, q/1)]).

test(a_fact_is_a_node_with_no_arcs) :-
    call_graph([q(1)], G),
    assertion(G.nodes == [key(plain, q/1)]),
    assertion(G.arcs == []).

%   A predicate that is called but never defined cannot be on a loop,
%   having no arcs out — but leaving it out would make the graph disagree
%   with the program.

test(an_undefined_callee_is_still_a_node) :-
    nodes([(p :- q)], Nodes),
    assertion(Nodes == [key(plain, p/0), key(plain, q/0)]).

test(arity_distinguishes_predicates) :-
    nodes([(p :- q(1)), (q(_) :- q(1, 2))], Nodes),
    assertion(memberchk(key(plain, q/1), Nodes)),
    assertion(memberchk(key(plain, q/2), Nodes)).

test(duplicate_arcs_are_merged) :-
    arcs([(p :- q, q)], Arcs),
    assertion(Arcs == [arc(key(plain, p/0), key(plain, q/0), +)]).

%   Calling a predicate both ways is two arcs, not one: the sign is part
%   of the arc.

test(the_same_callee_both_ways_is_two_arcs) :-
    arcs([(p :- q, not q)], Arcs),
    assertion(Arcs == [ arc(key(plain, p/0), key(plain, q/0), +),
                        arc(key(plain, p/0), key(plain, q/0), -)
                      ]).

:- end_tests(graph_arcs).

		 /*******************************
		 *         ODD LOOPS		*
		 *******************************/

:- begin_tests(graph_odd_loops).

odd(Clauses, Components) :-
    call_graph(Clauses, G),
    odd_components(G, Components).

%   The smallest odd loop there is.

test(a_negative_self_call) :-
    odd([(p :- not p)], Cs),
    assertion(Cs == [[key(plain, p/0)]]).

test(a_positive_self_call_is_not_one) :-
    odd([(p :- p)], Cs),
    assertion(Cs == []).

test(two_negations_make_an_even_loop) :-
    odd([(p :- not q), (q :- not p)], Cs),
    assertion(Cs == []).

test(three_negations_make_an_odd_one) :-
    odd([(p :- not q), (q :- not r), (r :- not p)], Cs),
    assertion(Cs == [[key(plain, p/0), key(plain, q/0), key(plain, r/0)]]).

test(one_negation_in_a_longer_loop) :-
    odd([(p :- q), (q :- r), (r :- not p)], Cs),
    assertion(Cs == [[key(plain, p/0), key(plain, q/0), key(plain, r/0)]]).

test(a_loop_with_no_negation_at_all) :-
    odd([(p :- q), (q :- p)], Cs),
    assertion(Cs == []).

test(a_path_with_a_negation_but_no_loop) :-
    odd([(p :- not q), (q :- not r)], Cs),
    assertion(Cs == []).

%   A `forall/2` the program writes is a control construct like `,`:
%   the literals under it are the clause's dependencies.

test(a_loop_through_a_written_forall) :-
    odd([(p :- forall(X, not r(X))), (r(_) :- p)], Cs),
    assertion(Cs == [[key(plain, p/0), key(plain, r/1)]]).

%   And `not forall(...)` negates what is under it: a negative arc.

test(a_loop_through_a_negated_forall) :-
    odd([(p :- not forall(X, r(X))), (r(_) :- p)], Cs),
    assertion(Cs == [[key(plain, p/0), key(plain, r/1)]]).

test(a_single_node_with_no_arcs) :-
    odd([p], Cs),
    assertion(Cs == []).

%   A component holding both an even and an odd cycle is unbalanced: it is
%   the odd one that decides.

test(an_even_cycle_does_not_cancel_an_odd_one) :-
    odd([(p :- not p), (p :- q), (q :- p)], Cs),
    assertion(Cs == [[key(plain, p/0), key(plain, q/0)]]).

%   Two independent loops are two components, and only the odd one is
%   reported.

test(two_loops_are_judged_separately) :-
    odd([ (p :- not p),
          (a :- not b), (b :- not a)
        ], Cs),
    assertion(Cs == [[key(plain, p/0)]]).

test(and_both_when_both_are_odd) :-
    odd([ (p :- not p),
          (a :- not a)
        ], Cs),
    assertion(Cs == [[key(plain, a/0)], [key(plain, p/0)]]).

%   `p' and `-p' are different nodes, so this is a path, not a loop.

test(a_literal_and_its_classical_negation_do_not_loop) :-
    odd([(p :- not(-(p)))], Cs),
    assertion(Cs == []).

test(but_they_can_loop_through_each_other) :-
    odd([(p :- not(-(p))), (-(p) :- p)], Cs),
    assertion(Cs == [[key(neg, p/0), key(plain, p/0)]]).

:- end_tests(graph_odd_loops).

		 /*******************************
		 *       OLON CLAUSES		*
		 *******************************/

:- begin_tests(graph_olon_clauses).

test(the_clause_on_the_loop) :-
    olon_clauses([(p :- not p)], Olon, Ordinary),
    assertion(Olon == [(p :- not p)]),
    assertion(Ordinary == []).

%   A clause whose head is on a loop but whose body does not lead back
%   into it is not itself on the loop.

test(a_clause_whose_body_leaves_the_component) :-
    olon_clauses([(p :- not p), (p :- b), b], Olon, Ordinary),
    assertion(Olon == [(p :- not p)]),
    assertion(Ordinary == [(p :- b), b]).

test(a_fact_is_never_on_a_loop) :-
    olon_clauses([p, (p :- not p)], Olon, Ordinary),
    assertion(Olon == [(p :- not p)]),
    assertion(Ordinary == [p]).

test(every_clause_of_a_longer_loop) :-
    olon_clauses([(p :- not q), (q :- not r), (r :- not p)], Olon, Ordinary),
    assertion(Ordinary == []),
    assertion(Olon == [(p :- not q), (q :- not r), (r :- not p)]).

test(order_is_preserved) :-
    olon_clauses([a, (p :- not p), b, (q :- not q), c], Olon, Ordinary),
    assertion(Olon == [(p :- not p), (q :- not q)]),
    assertion(Ordinary == [a, b, c]).

test(a_program_with_no_loops_at_all) :-
    olon_clauses([(p :- q), q], Olon, Ordinary),
    assertion(Olon == []),
    assertion(Ordinary == [(p :- q), q]).

%   Every clause is in exactly one of the two lists.  Worth asserting
%   directly: the two are what the NMR check and the solver each get, and
%   a clause falling between them would go missing silently.

test(the_two_lists_partition_the_program, [forall(program(Clauses))]) :-
    olon_clauses(Clauses, Olon, Ordinary),
    append(Olon, Ordinary, Both),
    msort(Both, Sorted),
    msort(Clauses, Expected),
    assertion(Sorted == Expected).

:- end_tests(graph_olon_clauses).

		 /*******************************
		 *   AGAINST AN ENUMERATION	*
		 *******************************/

%   The balance criterion replaces enumerating cycles with a 2-colouring,
%   which is the one step in this module that is an argument rather than a
%   transcription.  So it is checked against an enumeration.
%
%   The property is at the level of a *component*: a component is
%   unbalanced exactly when it contains an odd simple cycle.  It is not
%   true node by node — a node can sit in an unbalanced component and lie
%   on no odd *simple* cycle itself, reachable only by going round the odd
%   cycle and back, which is a walk and not a cycle.

:- begin_tests(graph_against_enumeration).

%!  odd_cycle(+Graph, -Cycle) is nondet.
%
%   A simple directed cycle of Graph with an odd number of negations, as
%   a sorted list of its nodes.  Brute force: every path from every node
%   that returns to it without repeating one.

odd_cycle(Graph, Nodes) :-
    member(Start, Graph.nodes),
    path(Graph.arcs, Start, Start, [Start], 0, Parity, Path),
    Parity =:= 1,
    msort(Path, Nodes0),
    sort(Nodes0, Nodes).

path(Arcs, From, Target, _Seen, P0, P, [From]) :-
    member(arc(From, Target, Sign), Arcs),
    parity(Sign, P0, P).
path(Arcs, From, Target, Seen, P0, P, [From|Path]) :-
    member(arc(From, Next, Sign), Arcs),
    Next \== Target,
    \+ memberchk(Next, Seen),
    parity(Sign, P0, P1),
    path(Arcs, Next, Target, [Next|Seen], P1, P, Path).

parity(+, P, P).
parity(-, P0, P) :- P is 1 - P0.

%   Soundness: every component reported holds an odd simple cycle.
%   Completeness: every odd simple cycle lies inside a reported one.
%   Together they pin the criterion exactly.

check(Clauses) :-
    call_graph(Clauses, Graph),
    odd_components(Graph, Components),
    forall(member(C, Components),
           assertion(( odd_cycle(Graph, Cycle),
                       forall(member(N, Cycle), memberchk(N, C))
                     ))),
    forall(odd_cycle(Graph, Cycle),
           assertion(( member(C, Components),
                       forall(member(N, Cycle), memberchk(N, C))
                     ))).

test(worked_examples, [forall(program(P))]) :-
    check(P).

%   The case the component-level phrasing exists for: `c' is in the
%   unbalanced component {a,b,c} but its only simple cycle, a->c->a, is
%   even.  A node-by-node comparison would call this a disagreement.

test(a_node_in_an_unbalanced_component_need_not_be_on_an_odd_cycle) :-
    Clauses = [(a :- not b), (b :- a), (a :- c), (c :- a)],
    check(Clauses),
    call_graph(Clauses, Graph),
    odd_components(Graph, [Component]),
    assertion(memberchk(key(plain, c/0), Component)),
    assertion(\+ ( odd_cycle(Graph, Cycle),
                   memberchk(key(plain, c/0), Cycle)
                 )).

%!  random_clauses(+Seed, +Predicates, +Arcs, -Clauses) is det.
%
%   A reproducible random program of one-literal clauses, so that a
%   failure names the seed that repeats it.  One literal per clause keeps
%   the arcs independent, which is what makes a small graph interesting.

random_clauses(Seed, NPreds, NArcs, Clauses) :-
    set_random(seed(Seed)),
    numlist(1, NPreds, Ns),
    maplist(pred_name, Ns, Names),
    findall(Clause, ( between(1, NArcs, _),
                      random_clause(Names, Clause)
                    ), Clauses).

pred_name(N, Name) :-
    atom_concat(p, N, Name).

random_clause(Names, Clause) :-
    random_member(Head, Names),
    random_member(Body, Names),
    (   maybe
    ->  Clause = (Head :- not Body)
    ;   Clause = (Head :- Body)
    ).

seed(Shape, Seed) :-
    shape(Shape, NPreds, NArcs, Count),
    between(1, Count, N),
    Seed is N * 7919 + NPreds * 31 + NArcs.

%   Small graphs on purpose: the enumeration is exponential in the number
%   of nodes, while the balance test is not.  Three to five predicates
%   with four to eight arcs is dense enough that most graphs have several
%   components and at least one loop.

shape(tiny,   3, 4, 40).
shape(small,  4, 6, 40).
shape(denser, 5, 8, 20).

test(random_graphs, [forall(( shape(Shape, NPreds, NArcs, _),
                              seed(Shape, Seed) ))]) :-
    random_clauses(Seed, NPreds, NArcs, Clauses),
    check(Clauses).

:- end_tests(graph_against_enumeration).
