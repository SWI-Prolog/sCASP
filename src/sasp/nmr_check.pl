:- module(nmr_check, [generate_nmr_check/0]).

/** <module> Detect OLON rules and construct nmr_check

Detect OLON rules and construct nmr_check.

@author Kyle Marple
@version 20170127
@license BSD-3
*/

/*
* Copyright (c) 2016, University of Texas at Dallas
* All rights reserved.
*  
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the University of Texas at Dallas nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*  
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY OF TEXAS AT DALLAS BE LIABLE FOR
* ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

:- use_module(library(lists)).
:- use_module(call_graph).
:- use_module(common).
:- use_module(comp_duals).
:- use_module(program).

%! generate_nmr_check is det
% Compute the nmr_check for a program. Wrapper for generate_nmr_check2/0.
generate_nmr_check :-
        write_verbose(0, 'Generating NMR check...\n'),
        generate_nmr_check2,
        !.
generate_nmr_check :-
        write_error('could not generate NMR check'),
        !,
        fail.

%! generate_nmr_check2 is det
% Get the rules in the program containing odd loops and compute the NMR check.
% After this step, headless rules are useless, so remove them and add a fact for
% the negation of the dummy head (_false_0). Call generate_nmr_check/0 instead
% of this.
generate_nmr_check2 :-
        findall(R, (defined_rule(_, H, B), rule(R, H, B)), Rs), % get all rules
        once(olon_rules(Rs, Rc, [e('_false_0')])),
        once(nmr_check(Rc, Nmrchk)),
        once(retractall(defined_rule('_false_0', _, _))), % remove headless rules
        negate_functor('_false_0', Nf),
        predicate(Np, Nf, []),
        rule(Nr, Np, []),
        assert_rule(Nr), % assert fact for negation of dummy head.
        assert_nmr_check(Nmrchk).

%! nmr_check(+OLONrules:list, -NmrCheck:list) is det
% Build the nmr_check.
%
% @param OLONrules List of rules to create NMR sub-checks for.
% @param NmrCheck List of NMR sub-check goals.
nmr_check(Rc, Nmrchk) :-
        Rc \= [],
        !,
        write_verbose(1, 'Creating sub-checks...\n'),
        olon_chks(Rc, Nmrchk, 1).
nmr_check([], []).

%! olon_rules(+Rules:list, -OLONrules:list, +ExcludeList:list) is det
% Determine which of the original rules are part of odd loops, and return them
% in a list.
%
% @param RuleIn Input list of rules.
% @param OLONrules Rules for which NMR sub-checks will need to be created.
% @param ExcludeList List of elements of the form e(X), where X is a literal to
%        exclude when building the call graph. These are literals created when
%        factorizing headless rules, so they will always be part of the NMR
%        check; we don't need to test them.
olon_rules(R, Rc, E) :-
        write_verbose(1, 'Detecting rules that contain odd loops over negation...\n'),
        once(assign_unique_ids(R, R1)),
        once(sort(R1, R2)), % ensure that all rules with the same head are together
        write_verbose(2, 'Building call graph...\n'),
        once(build_call_graph(R2, Ns, E, 1)), % build call graph, skipping duals.
        dfs(Ns, Pc, _, _),
        !,
        extract_ids(Pc, Ic),
        divide_rules(R2, Ic, Rc1, _), % get OLON rules
        get_headless_rules(R2, Rc1, Rc),
        destroy_call_graph.

%! dfs(+Nodes:list, -OLONs:list, -OrdinaryPaths:list, -PositiveLoops:list) is det
% Use depth first search to detect: cycles with no negation, cycles with even
% (>2) negation, paths with no cycle and cycles with odd negation. A list of
% paths will be returned for each type. Wrapper for dfs2/8.
%
% @param Nodes Nodes in the call graph.
% @param OLONs Paths containing odd loops. A list of lists of arcs, with each
%        sublist representing a path containing an odd loop.
% @param OrdinaryPaths Paths containing no odd loops and no even loops without
%        an intervening negation. A list of lists of arcs, with each sublist
%        representing a path containing only ordinary rules.
% @param PositiveLoops Path with cycles that have no negation. A list of lists
%        of arcs, with each sublist representing a path containing a positive
%        loop.
dfs(N, Pc, Po, Pr) :-
        write_verbose(2, 'Detecting cycles in call graph...\n'),
        dfs2(N, [], [], Pc, [], Po, [], Pr).

%! dfs2(+Nodes:list, +Tested:list, +OlonIn:list, -OlonOut:list, +OrdIn:list, -OrdOut:list, +PosIn:list, -PosOut:list) is det
% Test each node in case the graph isn't connected. For a visited node, dfs3/11
% will return quickly as no paths will be expanded.
%
% @param Nodes Nodes in the call graph.
% @param Tested List of visited nodes with negation, so that we only visit each
%        node at most once for each negation option: no negation, odd negation
%        and even negation. Elements are of the form v(X, N), where X is the
%        node and N is 0 = no negations, 1 = odd negs or 2 = even > 0 negs.
% @param OlonIn Input list of paths containing OLONs.
% @param OlonOut Output list of paths containing OLONs.
% @param OrdIn Input list of ordinary paths.
% @param OrdOut Output list of ordinary paths.
% @param PosIn Input list of paths with cycles and no negations.
% @param PosOut Output list of paths with cycles and no negations.
dfs2([X | T], V, Pci, Pco, Poi, Poo, Pri, Pro) :-
        findall(a(X, Y, N, I), a(X, Y, N, I), As), % Get all arcs from X
        dfs3(As, [v(X, 0) | V], Vo, [], 0, Pci, Pc1, Poi, Po1, Pri, Pr1),
        dfs2(T, Vo, Pc1, Pco, Po1, Poo, Pr1, Pro).
dfs2([], _, Pc, Pc, Po, Po, Pr, Pr).

%! dfs3(+Arcs:list, +VisitedIn:list, -VisitedOut:list, +Path:list, +Negations:int, +OlonIn:list, -OlonOut:list, +OrdIn:list, -OrdOut:list, +PosIn:list, -PosOut:list) is det
% The main traversal. Traverse each arc for a node, but don't recursively search
% previously visited nodes.
%
% @param Arcs The list of arcs in the call graph that originate at the last node
%        in the current path.
% @param VisitedIn Input list of visited nodes with negation, so that we only
%        visit each node at most once for each negation option: no negation, odd
%        negation and even negation. Elements are of the form v(X, N), where X
%        is the node and N is 0 = no negations, 1 = odd negs or 2 = even > 0
%        negs.
% @param VisitedOut Output list of visited nodes.
% @param Path List of arcs forming the path currently being examined.
% @param Negations 0 = no negations, 1 = odd negs or 2 = even > 0 negs
% @param OlonIn Input list of paths containing OLONs.
% @param OlonOut Output list of paths containing OLONs.
% @param OrdIn Input list of ordinary paths.
% @param OrdOut Output list of ordinary paths.
% @param PosIn Input list of paths with cycles and no negations.
% @param PosOut Output list of paths with cycles and no negations.
dfs3([A | T], Vi, Vo, P, N, Pci, Pco, Poi, Poo, Pri, [[A] | Pro]) :- % rule calls itself directly with no negation
        A = a(X, X, 0, _),
        !,
        dfs3(T, Vi, Vo, P, N, Pci, Pco, Poi, Poo, Pri, Pro).
dfs3([A | T], Vi, Vo, P, N, Pci, [[A] | Pco], Poi, Poo, Pri, Pro) :- % rule calls itself directly with a negation
        A = a(X, X, 1, _),
        !,
        dfs3(T, Vi, Vo, P, N, Pci, Pco, Poi, Poo, Pri, Pro).
dfs3([A | T], Vi, Vo, P, N, Pci, Pco, Poi, Poo, Pri, Pro) :-
        A = a(_, Y, _, _),
        member(a(Y, _, _, _), P), % cycle
        !,
        check_cycle(Y, [A | P], Pci, Pc1, Poi, Po1, Pri, Pr1),
        dfs3(T, Vi, Vo, P, N, Pc1, Pco, Po1, Poo, Pr1, Pro).
dfs3([A | T], Vi, Vo, P, N, Pci, Pco, Poi, Poo, Pri, Pro) :-
        A = a(_, Y, N2, _),
        update_negation(N, N2, N3),
        member(v(Y, N3), Vi), % previously visited node, but not a cycle
        !,
        dfs3(T, Vi, Vo, P, N, Pci, Pco, Poi, Poo, Pri, Pro).
dfs3([A | T], Vi, Vo, P, N, Pci, Pco, Poi, Poo, Pri, Pro) :-
        A = a(_, Y, N2, _), % not previously visited or a cycle; expand
        !,
        update_negation(N, N2, N3),
        set_append(v(Y, N3), Vi, V1),
        findall(a(Y, Y2, Y3, Y4), a(Y, Y2, Y3, Y4), As), % Get all arcs from Y
        !,
        once(dfs3(As, V1, V2, [A | P], N3, Pci, Pc1, Poi, Po1, Pri, Pr1)),
        dfs3(T, V2, Vo, P, N, Pc1, Pco, Po1, Poo, Pr1, Pro).
dfs3([], V, V, _, _, Pc, Pc, Po, Po, Pr, Pr).

%! check_cycle(+Node:list, +Path:list, +OlonIn:list, -OlonOut:list, +OrdIn:list, -OrdOut:list, +PosIn:list, -PosOut:list) is det
% Get the cycle and classify by number of negations.
%
% @param Node A node in the call graph.
% @param Path List of arcs forming the path currently being examined.
% @param OlonIn Input list of paths containing OLONs.
% @param OlonOut Output list of paths containing OLONs.
% @param OrdIn Input list of ordinary paths.
% @param OrdOut Output list of ordinary paths.
% @param PosIn Input list of paths with cycles and no negations.
% @param PosOut Output list of paths with cycles and no negations.
check_cycle(X, P, Pci, Pco, Poi, Poo, Pri, Pro) :-
        get_cycle(X, P, C, N),
        classify_cycle(N, C, Pci, Pco, Poi, Poo, Pri, Pro).

%! get_cycle(+Node:list, +Path:list, -Cycle:list, -Negations:int) is det
% Get the portion of the path forming the cycle on node X. Count the negations
% as we go. Will fail if path doesn't have a cycle over X. Wrapper for
% get_cycle2/5.
%
% @param Node A node in Path.
% @param Path List of arcs forming the path currently being examined.
% @param Cycle List of arcs forming a cycle on Node.
% @param Negations The number of negations in Cycle.
get_cycle(X, P, C, N) :-
        get_cycle2(X, P, C, 0, N).

%! get_cycle2(+Node:list, +Path:list, -Cycle:list, +NegsIn:int, -NegsOut:int) is det
% See get_cycle/4. Call that instead.
%
% @param Node A node in Path.
% @param Path List of arcs forming the path currently being examined.
% @param Cycle List of arcs forming a cycle on the original node.
% @param NegsIn Input number of negations in Cycle.
% @param NegsOut Output number of negations in Cycle.
get_cycle2(X, [A | P], [A | C], Ni, No) :-
        A = a(Y, _, N, _),
        X \= Y,
        N2 is N + Ni,
        get_cycle2(X, P, C, N2, No).
get_cycle2(X, [A | _], [A], Ni, No) :-
        A = a(X, _, N, _),
        No is Ni + N.

%! classify_cycle(+Negs:int, +Cycle:list, +OlonIn:list, -OlonOut:list, +OrdIn:list, -OrdOut:list, +PosIn:list, -PosOut:list) is det
% Put the cycle into the right group based on number of negations.
%
% @param Negs Number of negations in Cycle.
% @param Cycle List of arcs forming a cycle.
% @param OlonIn Input list of paths containing OLONs.
% @param OlonOut Output list of paths containing OLONs.
% @param OrdIn Input list of ordinary paths.
% @param OrdOut Output list of ordinary paths.
% @param PosIn Input list of paths with cycles and no negations.
% @param PosOut Output list of paths with cycles and no negations.
classify_cycle(0, C, Pc, Pc, Po, Po, Pr, [C | Pr]).
classify_cycle(N, C, Pc, Pc, Po, [C | Po], Pr, Pr) :-
        N > 0,
        N mod 2 =:= 0.
classify_cycle(N, C, Pc, [C | Pc], Po, Po, Pr, Pr) :-
        N mod 2 =:= 1.

%! update_negation(+NegsIn1:int, +NegsIn2:int, +NegsOut:int) is det
% Update negation value. 0 = no negations, 1 = odd negs, 2 = even > 0 negs.
%
% @param NegsIn Input negation value 1.
% @param NegsIn2 Input negation value 2.
% @param NegsOut Output negation value.
update_negation(2, 1, 1) :-
        !.
update_negation(X, Y, Z) :-
        Z is X + Y,
        !.

%! set_append(+Element:callable, +Set:list, +SetOut:list) is det
% Append Elements only if not already present in Set.
%
% @param Element The element to append.
% @param Set Input set.
% @param SetOut Output set.
set_append(X, Y, Y) :-
        member(X, Y),
        !.
set_append(X, Y, [X | Y]).

%! extract_ids(+Cycles:list, -IDs:list) is det
% Given a list of cycles, get the unique rules IDs.
%
% @param Cycles List of cycles in the call graph. Each cycle is a list of arcs.
% @param IDs List of rule IDs from Cycles.
extract_ids(C, I) :-
        extract_ids2(C, [], I2),
        sort(I2, I). % remove duplicates

%! extract_ids2(+Cycles:list, +IdsIn:list, -IdsOut:list) is det
% Get the lists of ids from each path and merge them.
%
% @param Cycles List of cycles in the call graph. Each cycle is a list of arcs.
% @param IdsIn Input list of rules IDs from Cycles.
% @param IdsOut Output list of rules IDs from Cycles.
extract_ids2([X | T], Ii, Io) :-
        extract_ids3(X, [], I),
        append(I, Ii, I1),
        extract_ids2(T, I1, Io).
extract_ids2([], I, I).

%! extract_ids3(+Cycle:list, +IdsIn:list, -IdsOut:list) is det
% Get a list of IDs from a single path, extracting the rule ID for each arc.
%
% @param Cycle A list of arcs representing a cycle in the call graph.
% @param IdsIn Input list of rules IDs from Cycles.
% @param IdsOut Output list of rules IDs from Cycles.
extract_ids3([X | T], Ii, Io) :-
        X = a(_, _, _, I), % I is the arc ID
        ar(I, Ri), % ar/2 associates an arc ID with a list of rule IDs
        append(Ri, Ii, I1),
        extract_ids3(T, I1, Io).
extract_ids3([], I, I).

%! divide_rules(+RulesIn:list, +IDs:list, -Members:list, -Nonmembers:list) is det
% Split rules based on ID list membership.
%
% @param RulesIn Input list of rules.
% @param IDs A list of rule IDs.
% @param Members Rules whose ID is a member of IDs.
% @param Nonmembers Rules whose ID is not in IDs.
divide_rules([X | T], Is, [X | To], To2) :- % rule in list
        rule(X, _, I, _),
        member(I, Is),
        !,
        divide_rules(T, Is, To, To2).
divide_rules([X | T], Is, To, [X | To2]) :- % rule not in list
        !,
        divide_rules(T, Is, To, To2).
divide_rules([], _, [], []).

%! get_headless_rules(+RulesIn:list, +HeadlessIn:list, -HeadlessOut:list) is det
% Get rules with the head '_false', indicating the rule was headless in the
% original program, and thus an OLON rule.
%
% @param RulesIn Input rule list.
% @param HeadlessIn Input list of headless rules (head = 1).
% @param HeadlessOut Output list of headless rules (head = 1).
get_headless_rules([X | T], Rci, [X | Rco]) :-
        rule(X, H, _, _),
        predicate(H, '_false_0', _), % headless rule
        !,
        get_headless_rules(T, Rci, Rco).
get_headless_rules([_ | T], Rci, Rco) :-
        get_headless_rules(T, Rci, Rco).
get_headless_rules([], Rc, Rc).

%! olon_chks(+RulesIn:list, -NMRCheck:list, +Counter:int) is det
% For each OLON rule, create a check that contains the negation of the rule's
% head by copying the rule and adding the negation if not present. Create the
% duals here since we can discard the unused non-dual. Add the new (dual) rule's
% head to the list of nmr checks. Use the original rule's ID for the goal ID in
% the NMR check. When adding the negation of the head to a rule, set the ID to
% ensure that it will be the last goal in the rule.
%
% @param RulesIn List of rules to create NMR sub-checks for.
% @param NMRCheck List of NMR sub-check goals.
% @param Counter Counter used to ensure sub-check heads are unique.
olon_chks([R | T], [not(G) | Nmr], C) :-
        rule(R, X, _, Y),
        predicate(X, '_false_0', _), % headless rule
        !,
        rule(R2, X, Y), % strip ID for comp_duals3/2
        create_unique_functor('_chk_0', C, H), % Create functor for sub-check head
        comp_duals3(H, [R2]),
        predicate(G, H, []), % Create goal for NMR check
        C1 is C + 1,
        olon_chks(T, Nmr, C1).
olon_chks([R | T], [Go | Nmr], C) :-
        rule(R, X, _, Y),
        !,
        (once(member(not(X), Y)) -> % negated head must match exactly, including args
                rule(R2, X, Y)
        ;
                append(Y, [not(X)], Y2),
                rule(R2, X, Y2) % add negated head to body
        ),
        predicate(X, Hi, _),
        split_functor(Hi, _, A), % get arity of head
        number_chars(A, Ac),
        append(['_', 'c', 'h', 'k'], ['_' | Ac], Hc),
        atom_chars(Hb, Hc), % Create base sub-check functor with correct arity
        create_unique_functor(Hb, C, H), % Create functor for sub-check head
        comp_duals3(H, [R2]),
        var_list(A, 0, [], V), % Get place holder args for NMR check goal
        predicate(G, H, V),
        define_forall(not(G), Go, V), % forall for every variable in the head.
        C1 is C + 1,
        olon_chks(T, Nmr, C1).
olon_chks([], [], _).

%! assign_unique_ids(+ListIn:list, -ListOut:list) is det
% Give each rule or goal a unique ID to allow individual members to be
% identified later. Wrapper for assign_unique_ids2/3. Note that list order is
% not changed. The IDs are sequential, and allow the original order to be
% restored by sorting later.
%
% @param ListIn A list of rules or goals without attached IDs.
% @param ListOut A list of rules or goals with unique IDs attached.
assign_unique_ids(Ri, Ro) :-
        assign_unique_ids2(Ri, Ro, 1).

%! assign_unique_ids2(+ListIn:list, -ListOut:list, +Counter:int) is det
% Assign each rule or goal (int) a unique ID, the current value of Counter. Call
% assign_unique_ids/2 instead of this predicate.
%
% @param ListIn A list of rules or goals without attached IDs.
% @param ListOut A list of rules or goals with unique IDs attached.
% @param Counter The next ID to assign.
assign_unique_ids2([X | T], [X2 | T2], C) :-
        rule(X, H, B), % rule
        !,
        rule(X2, H, C, B),
        C1 is C + 1,
        assign_unique_ids2(T, T2, C1).
assign_unique_ids2([X | T], [X2 | T2], C) :-
        !, % goal
        X2 = -(X, C),
        C1 is C + 1,
        assign_unique_ids2(T, T2, C1).
assign_unique_ids2([], [], _).
