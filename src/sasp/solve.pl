:- module(solve, [
                        solve/1,
                        solve_unify/5,
                        solve_subdnunify/5,
                        solve_dnunify/5,
                        occurs_check/3
                 ]).

/** <module> Solve input ASP program

With the program asserted and the NMR check computed, solve for a partial answer
set using the provided query.

@author Kyle Marple
@version 20170510
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
:- use_module(ciao_auxiliar).
:- use_module(chs).
:- use_module(common).
:- use_module(debug).
:- use_module(interactive).
:- use_module(options).
:- use_module(output).
:- use_module(program).
:- use_module(variables).

%! solve(+Mode:ground) is nondet
% This predicate serves as a wrapper for solve2/1, to provide an error message
% if it fails.
%
% @param Mode 'auto' or 'user', indicating if the built-in query should be used
%        or if the user should be asked to provide one.
solve(Mode) :-
        if_debug(0, write_program),
        solve2(Mode),
        !.
solve(_) :-
        write_error('could not run solver'),
        !,
        fail.

%! solve2(+Mode:ground) is det
% Execute in interactive or automatic mode according to the value of Mode.
%
% @param Mode 'auto' or 'user', indicating if the built-in query should be used
%        or if the user should be asked to provide one.
solve2(auto) :- % get built-in query and run it
        defined_query(_, N), % get hardcoded answer set count
        (user_option(ascount) -> % Force ascount specified by user
                user_option(ascount, N1)
        ;
                N1 = N
        ),
        (N1 = 0 -> % When finding all answer sets, Ensure a negative value is passed.
                N2 is -1
        ;
                N2 = N1
        ),
        nb_setval(ans_cnt, 0), % Initialize global variable ans_cnt
        !,
	auto_solve(N2).

solve2(user) :-
        get_user_query(Q),
        !,
	user_solve(Q).

	    

%! auto_solve(+MaxAS:int)
% Use the hard-coded or default query to find the specified number of answer
% sets without user interaction. A non-backtracking global variable, 'ans_cnt',
% is used to track progress.
%
% @param MaxAS The number of answer sets to find. MaxAS < 0 for all.
auto_solve(N) :-
        defined_query(Q, _),
        defined_nmr_check(NMR),
        body_vars2(Q, [], [], Qv), % get query variables for printing with solutions
        append(Q, NMR, Q2),
        new_var_struct(Vi),
        new_chs(CHSi),
        %force(write('as\n')),
	(
	    user_option(statistics_run_time, true) ->
	    statistics(runtime,_),
	    solve_goals(Q2, Vi, Vo, CHSi, CHSo, [], _, J, 0), % find an answer set
	    statistics(runtime,[_,T]),
	    nl, print(solve_run_time(T,'00 ms')), nl, nl
	;
	    solve_goals(Q2, Vi, Vo, CHSi, CHSo, [], _, J, 0) % find an answer set
	),
        if_debug(1, write('SOLUTION FOUND! PRINTING CHS:\n')),
        b_getval(ans_cnt, I), % ans_cnt won't reset on backtracking
        I2 is I + 1,
        nb_setval(ans_cnt, I2),
	format('Answer: ~w\n',[I2]),
        once(print_chs(CHSo, Vo, 0)),
        once(print_vars(Qv, Vo)),
        if_debug(2, print_var_struct(Vo)),
        (user_option(list_abducibles, true) ->
                print_abducibles(CHSo, Vo)
        ;
                true
        ),
        (user_option(justification, true) ->
                print_justification(J)
        ;
                true
        ),
        (user_option(html_justification, true) ->
                print_html(J,[Q,CHSo,Qv,Vo])
        ;
                true
        ),
        nl, nl,
        (I2 =\= N -> % Never true for N < 0 (find all)
                fail % backtrack to find the required number of answer sets
        ;
                true % succeed when required number of sets found
        ).
auto_solve(-1) :-
        b_getval(ans_cnt, I),
        I > 0, % we were finding all answer sets, and got at least 1.
        !.
auto_solve(_) :-
        write('false.\n\n'), % No remaining answer sets, and we didn't hit our target.
        !.

%! user_solve(+Query:list) is det
% Execute Query in interactive mode.
%
% @param Query The user-supplied query. May be a list of goals or a one-element
%        list containing calling a built-in such as 'exit'.
user_solve(['exit_0']) :-
        !. % exit
user_solve(['halt_0']) :-
        !. % exit
user_solve(Q) :-
        body_vars2(Q, [], [], Qv), % get query variables for printing with solutions
        defined_nmr_check(NMR),
        append(Q, NMR, Q2),
        new_var_struct(Vi),
        new_chs(CHSi),
	(
	    user_option(statistics_run_time, true) ->
	    statistics(runtime,_),
	    solve_goals(Q2, Vi, Vo, CHSi, CHSo, [], _, J, 0), % find an answer set
	    statistics(runtime,[_,T]),
	    nl, print(solve_run_time(T,'00 ms')), nl, nl
	;
	    solve_goals(Q2, Vi, Vo, CHSi, CHSo, [], _, J, 0) % find an answer set
	),
        %force(write('post solve_goals\n')),
        if_debug(1, write('SOLUTION FOUND! PRINTING CHS:\n')),
        once(print_chs(CHSo, Vo, 0)),
        once(print_vars(Qv, Vo)),
        (user_option(list_abducibles, true) ->
                print_abducibles(CHSo, Vo)
        ;
                true
        ),
        (user_option(justification, true) ->
                print_justification(J)
        ;
                true
        ),
        (user_option(html_justification, true) ->
                print_html(J,[Q,CHSo,Qv,Vo])
        ;
                true
        ),
        (get_user_response ->
                !, % user accepted answer set
                solve2('user') % do it all over again
        ;
                fail % user rejected answer set
        ).
user_solve(_) :-
        write('false.\n'), % No remaining answer sets.
        !,
        solve2(user). % Get another query.

%! solve_goals(+Goals:list, +VarsIn:compound, -VarsOut:compound, +CHSin:list, -CHSout:list, +CallStack:list, -EvenLoops:list, -Justification:list, +InNMR:int)
% Solve a list of goals.
%
% @param Goals The list of goals to solve.
% @param VarsIn Input var struct.
% @param VarsOut Output var struct.
% @param CHSin Input CHS.
% @param CHSout Output CHS.
% @param CallStack The list of ancestor calls. Used to ensure that positive
%        loops fail.
% @param EvenLoops List of even loops involving parent goal.
% @param Justification A list of structures representing the rules and goals
%        used to satisfy the current goal.
% @param InNMR 1 if the goal was first added after entering the NMR check, 0
%        otherwise.
solve_goals([X | T], Vi, Vo, CHSi, CHSo, Cs, E, [Jg | Jt], NMR) :-
        if_debug(1, (
                format_term(X, X2, C, Vi),
                length(Cs, L),
                indent(L),
                writef('CALL (~w): current goal is ~w (', [L, X2]),
                print_var_constraints(C),
                write(')'),
                nl,
                !
                )),
        (X = '_nmr_check_0' -> % set flag when we enter the NMR check
                NMR2 = 1
        ;
                NMR2 = NMR
        ),
        %force(write('sgs\n')),
        solve_goal(X, Vi, V1, CHSi, CHS1, Cs, E1, Jg, NMR2),
        %force(writef('sgs post ~w\n', [X])),
        %fill_in_variable_values(X, Xp, [], _, V1),
        %force(writef('post-s2s3: ~w\n', [Xp])),
        if_debug(1, (
                format_term(X, X3, C2, V1),
                format_term_list(T, T2, _, V1),
                indent(L),
                writef('FIN: (~w): fin goal ~w (', [L, X3]),
                print_var_constraints(C2),
                writef('), T = ~w\n', [T2])
                )),
        %force(write('solve_goals solve_goals\n')),
        solve_goals(T, V1, Vo, CHS1, CHSo, Cs, E2, Jt, NMR),
        append(E1, E2, E).
solve_goals([], V, V, C, C, _, [], [], _). % no goals left; inductive success

%! solve_goal(+Goal:compound, +VarsIn:compound, -VarsOut:compound, +CHSin:list, -CHSout:list, +CallStack:list, -EvenLoops:list, -Justification:compound, +InNMR:int)
% Solve a single goal. Determine if it's a predicate or an expression, then call
% the appropriate solver.
%
% @param Goal The list of goal to solve.
% @param VarsIn Input variables.
% @param VarsOut Output variables.
% @param CHSin Input CHS.
% @param CHSout Output CHS.
% @param CallStack The list of ancestor calls. Used to ensure that positive
%        loops fail.
% @param EvenLoops List of even loops involving parent goal.
% @param Justification A struct of the form -(Goal, Constraints, List) where
%        list contains info on the rules used to satisfy Goal.
% @param InNMR 1 if the goal was first added after entering the NMR check, 0
%        otherwise.
solve_goal(G, Vi, Vo, CHSi, CHSo, Cs, E, J, NMR) :-
        G = forall(V, G2), % forall
        !,
	print(solve_goal(V,G2)),nl,
	(
	    solve_forall(V, G2, Vi, Vo, CHSi, CHSo, Cs, E, J, NMR)->
	    print('OK_solve_goal'(V,G2)),nl
	;
	    print('NO_solve_goal'(V,G2)),nl,fail
	).
solve_goal(G, V, V, CHS, CHS, _, [], -(builtin(G3), C, []), _) :-
        G = builtin_1(G2), % Call to builtin
        !,
        format_term(G2, G3, C, V), % fill in variables, strip prefixes, etc.
        call(G3).
solve_goal(G, Vi, Vo, CHS, CHS, _, [], -(G2, C, []), _) :-
        G =.. [O | _],
        operator(O, _, _),
        !,
        format_term(G, G2, C, Vi), % fill in variables, strip prefixes, etc.
        solve_expression(G, Vi, Vo).
solve_goal(G, Vi, Vo, CHS, CHS, _, [], -(expression(G2), C, []), _) :-
        G = not(is(_, _)),
        !,
        format_term(G, G2, C, Vi), % fill in variables, strip prefixes, etc.
        solve_expression(G, Vi, Vo),
        !.
solve_goal(G, Vi, Vo, CHSi, CHSo, Cs, E, J, NMR) :-
        G = not(G2),
        predicate(G2, _, _), % negated predicate
        !,
        solve_predicate(G, Vi, Vo, CHSi, CHSo, Cs, E, J, NMR).
solve_goal(G, Vi, Vo, CHSi, CHSo, Cs, E, J, NMR) :-
        predicate(G, _, _), % predicate
        G \= not(_),
        !,
        %force(write('sg\n')),
        solve_predicate(G, Vi, Vo, CHSi, CHSo, Cs, E, J, NMR).
solve_goal(G, _, _, _, _, _, _, _, _) :-
        write_error('encountered invalid goal ~w', [G]),
        !,
        fail.

%! solve_predicate(+Goal:compound, +VarsIn:compound, -VarsOut:compound, +CHSin:list, -CHSout:list, +CallStack:list, -EvenLoops:list, -Justification:compound, +InNMR:int)
% Solve a single goal that we know is a predicate. Check the CHS first, then
% act based on the result.
%
% @param Goal The goal being solved.
% @param VarsIn Input variables.
% @param VarsOut Output variables.
% @param CHSin Input CHS.
% @param CHSout Output CHS.
% @param CallStack The list of ancestor calls. Used to ensure that positive
%        loops fail.
% @param EvenLoops List of even loops involving parent goal.
% @param Justification A struct of the form -(Goal, Constraints, List) where
%        list contains info on the rules used to satisfy Goal.
% @param InNMR 1 if the goal was first added after entering the NMR check, 0
%        otherwise.
solve_predicate(G, Vi, Vo, CHSi, CHSo, Cs, E, -(Gj, Cj, [J]), NMR) :-
        G = not(G2),
        !,
        predicate(G2, F, A),
        negate_functor(F, Fn), % get dual functor
        format_term(G, Gj, Cj, Vi),
        if_debug(5, (
                format_term(G, Gp, C, Vi),
                writef('SP: check chs: ~w (', [Gp]),
                print_var_constraints(C),
                write(') against: '),
                once(print_chs(CHSi, Vi, 0)), nl
                )),
        %force(write('pre-chsa\n')),
        check_chs(Fn, A, Vi, V1, CHSi, Cs, Cflag, El),
        %force(writef('post-chs a: ~w\n', [Fn])),
        if_debug(5, (
                format_term(G, Gp2, C2, V1),
                writef('SP: chs check got flag: ~w for ~w (', [Cflag, Gp2]),
                print_var_constraints(C2),
                write(')'), nl
                )),
        if_debug(5, (
                (Cflag =:= 1 ->
                        G3 =.. [Fn | A],
                        format_term(G3, G4, C3, V1),
                        writef('SP: chs success, goal is now ~w (', [G4]),
                        print_var_constraints(C3),
                        write(')\n')
                ;
                        true
                )
                )),
        predicate(Go, Fn, A), % pack negated goal as dual
        expand_call(Cflag, Go, V1, Vo, CHSi, CHSo, Cs, El, E, J, NMR).
solve_predicate(G, Vi, Vo, CHSi, CHSo, Cs, E, -(Gj, Cj, [J]), NMR) :-
        G \= not(_),
        !,
        predicate(G, F, A),
        format_term(G, Gj, Cj, Vi),
        if_debug(5, (
                format_term(G, Gp, C, Vi),
                writef('SP: check chs: ~w (', [Gp]),
                print_var_constraints(C),
                write(') against: '),
                once(print_chs(CHSi, Vi, 0)), nl
                )),
        %force(write('pre-chs b\n')),
        check_chs(F, A, Vi, V1, CHSi, Cs, Cflag, El),
        %force(writef('post-chs b: ~w\n', [F])),
        if_debug(5, (
                format_term(G, Gp2, C2, V1),
                writef('SP: chs check got flag: ~w for ~w (', [Cflag, Gp2]),
                print_var_constraints(C2),
                write(')'), nl
                )),
        expand_call(Cflag, G, V1, Vo, CHSi, CHSo, Cs, El, E, J, NMR).

%! expand_call(+CHSflag:int, +Goal:compound, +VarsIn:compound, -VarsOut:compound, +CHSin:list, -CHSout:list, +CallStack:list, +EvenLoopIn:compound, -EvenLoopOut:list, -Justification:compound, +InNMR:int)
% Solve a single goal that we know is a predicate. Check the CHS first, then
% act based on the result.
%
% @param CHSflag Result of checking the CHS for the call. -1, 0, 1 or 2,
% indicating possible positive loop, not present, coinductive success, or check
% intervening negations, respectively.
% @param Goal The goal being solved.
% @param VarsIn Input variables.
% @param VarsOut Output variables.
% @param CHSin Input CHS.
% @param CHSout Output CHS.
% @param CallStack The list of ancestor calls. Used to ensure that positive
%        loops fail.
% @param EvenLoopIn Even loop involving current goal, if one exists.
% @param EvenLoopOut List of even loops involving parent goal.
% @param Justification A struct of the form -(Goal, Constraints, List) where
%        list contains info on the rules used to satisfy Goal.
% @param InNMR 1 if the goal was first added after entering the NMR check, 0
%        otherwise.
expand_call(1, G, Vi, Vo, CHS, CHS, _, Ei, Eo, -(chs__success, G2, C), _) :- % coinductive success
        Ei = -(L, Lv),
        (Lv = [] ->
                Eo = [],
                Vo = Vi
        ;
                gen_sub_vars(Lv, Lv2, Vi, Vo),
                E2 = -(L, Lv2),
                Eo = [E2]
        ),
        format_term(G, G2, C, Vi), % fill in variables, strip prefixes, etc.
        !.
expand_call(Cflag, G, Vi, Vo, CHSi, CHSo, Cs, _, Eo, J, NMR) :- % not present
        predicate(G, F, A),
        format_term(G, G2, C, Vi),
        if_debug(4, (
                format_term(G, G2, C, Vi),
                writef('EC: add to chs ~w (', [G2]),
                print_var_constraints(C),
                write(')'), nl
                )),
        once(add_to_chs(F, A, 0, NMR, E, [], Vi, V1, CHSi, CHS1)),
        if_debug(4, (
                format_term(G, G3, C2, V1),
                writef('EC: added to chs ~w (', [G3]),
                print_var_constraints(C2),
                write(')'), nl
                )),
        once(findall(R, (defined_rule(F, H, B), rule(R, H, B)), Rs)), % get potentially matching clauses
        (Cflag =:= -1 ->
	      fail % !!! REMOVE ONCE CALL BELOW COMPLETE !!!
	%%	      strip_used_rules(Cs, Rs, Rs2, G, V1),
        ;
                Rs2 = Rs
        ),
        if_debug(4, (
                format_term(G, G4, C3, V1),
                format_term_list(Rs2, Rs3, _, V1),
                writef('EC: expanding ~w (', [G4]),
                print_var_constraints(C3),
                writef(') with rules ~w\n', [Rs3])
                )),
        expand_call2(G, Rs2, V1, V2, CHS1, CHS2, Cs, E1, J, NMR), % expand
        remove_from_chs(E, CHS2, CHS3), % remove original goal from CHS
        get_sub_vars(E1, Ev, V2, V3),
        once(add_to_chs(F, A, 1, NMR, _, Ev, V3, Vo, CHS3, CHSo)), % add succeeding goal to CHS
        remove_cycle_heads(E1, Eo),
        if_debug(3, (
                format_term(G, G5, C4, Vo),
                writef('EC: CHS marked success for ~w (\n', [G5]),
                print_var_constraints(C4),
                write(')'), nl
                )).

%! expand_call2(+Goal:compound, +Rules:list, +VarsIn:compound, -VarsOut:compound, +CHSin:list, -CHSout:list, +CallStack:list, -EvenLoops:list, -Justification:compound, +InNMR:int)
% Given a list of rules whose heads have the same name and arity as the call
% being expanded, check that the args unify and then expand. Skip any rules
% whose args don't unify.
%
% @param Goal The original call.
% @param Rules List of rules matching the head/arity of goal being expanded.
% @param VarsIn Input variables.
% @param VarsOut Output variables.
% @param CHSin Input CHS.
% @param CHSout Output CHS.
% @param CallStack The list of ancestor calls. Used to ensure that positive
%        loops fail.
% @param EvenLoops List of even loops involving parent goal.
% @param Justification A struct of the form -(Goal, Constraints, List) where
%        list contains info on the rules used to satisfy Goal.
% @param InNMR 1 if the goal was first added after entering the NMR check, 0
%        otherwise.
expand_call2(G, [X | _], Vi, Vo, CHSi, CHSo, Cs, E, -(expand__call(G2), C, Js), NMR) :- % match
        rule(X, H, B),
        once(get_unique_vars(H, H2, B, B2, Vi, V1)),
        if_debug(3, (
                format_term(G, G3, C2, V1),
                format_term(H2, H3, _, V1),
                writef('trying to unify ~w (', [G3]),
                print_var_constraints(C2),
                writef(') with ~w\n', [H3])
                )),
        solve_unify(G, H2, V1, V2, 0), % unify goal and head args
        (
	    user_option(html_justification, true) ->
	    format_term(H2, Head, C, V1),
	    format_term(B2, Body, C, V1),
	    G2 = Head-Body
	;
	    format_term(G, G2, C, V2) % fill in variables, strip prefixes, etc.
	),
        solve_goals(B2, V2, Vo, CHSi, CHSo, [-(G, X) | Cs], E, Js, NMR).
expand_call2(G, [_ | T], Vi, Vo, CHSi, CHSo, Cs, E, J, NMR) :- % not a match or backtracking
        if_debug(2, (
                format_term(G, G2, C, Vi),
                format_term_list(T, T2, _, Vi),
                writef('no match or backtracking for ~w (', [G2]),
                print_var_constraints(C),
                writef('), T = ~w!\n', [T2])
                )),
        !,
        expand_call2(G, T, Vi, Vo, CHSi, CHSo, Cs, E, J, NMR).

%! strip_used_rules(+CallStack:list, +RulesIn:list, -RulesOut:list, +Goal:compound, +Vars:compound) is det
% When encountering a potential positive loop, instead of failing, we want to
% force it to execute *while skipping rules that have already been tried*. To
% do this, check matching call stack entries and remove their rules from the
% list of rules produced by expand_call/11. The process is very similar to
% chs:check_negations/8.
%
% @param CallStack The list of ancestor calls. Used to ensure that positive
%        loops fail.
% @param RulesIn Input rule list.
% @param RulesOut Output rule list.
% @param Goal The goal to process.
% @param Vars The var struct to use.
% strip_used_rules([-(X, R) | T], Rsi, Rso, G, V) :-
%         % recreate positive loop check from chs:check_negations/8
%         predicate(G, F, A1),
%         predicate(X, F, A2),
%         solve_unify(G, X, V, V2, 1),
% 	nl,display(rule(R,'\n',Rsi)),nl,
%         chs_entry(E1, F, A1, _, _),
%         chs_entry(E2, F, A2, _, _),
%         % !!!STOPPED HERE!!!
%         !,
%         strip_used_rules(T, Rsi, Rso, G, V).
% strip_used_rules([_ | T], Rsi, Rso, G, V) :- % current entry isn't an exact match
%         !,
%         strip_used_rules(T, Rsi, Rso, G, V).
% strip_used_rules([], Rs, Rs, _, _) :-
%         !.

%! gen_sub_vars(+LoopVars:list, -SubVars:list, +VarsIn:compound, -VarsOut:compound)
% For each loop variable, create a unique variable to substitute in the CHS.
% Ignore variables marked as part of a forall (loopvar flag = -1).
%
% @param LoopVars The list of loop variables.
% @param SubVars The list of pairs for substitutions.
% @param VarsIn Input variables.
% @param VarsOut Output variables.
gen_sub_vars([X | T], [-(X, Y) | T2], Vi, Vo) :-
        is_unbound(X, Vi, Con, F, Lv), % get value
        Lv =\= -1,
        !,
        var_con(Val, Con, F, 1), % Flag as loop variable.
        generate_unique_var(Y, Vi, V2, '_'), % get new variable
        update_var_value(Y, Val, V2, V3), % copy value to new variable
        gen_sub_vars(T, T2, V3, Vo).
gen_sub_vars([X | T], T2, Vi, Vo) :-
        is_unbound(X, Vi, _, _, -1), % variable cannot be made a loop variable
        !,
        gen_sub_vars(T, T2, Vi, Vo).
gen_sub_vars([], [], V, V) :-
        !.

%! get_sub_vars(+Cycles:list, -SubVars:list, +VarsIn:compound, -VarsOut:compound)
% Given a list of cycle structures, get one substitution variable per value ID.
% If the value IDs of two non-substitution variables in different cycles match,
% unify them.
%
% @param Cycles The list of cycle structures.
% @param SubVars The list of structs for variable substitution.
% @param VarsIn Input variables.
% @param VarsOut Output variables.
get_sub_vars([], [], V, V) :- % even loops should be uncommon, so start with base case
        !.
get_sub_vars([X], Y, V, V) :- % only one cycle, just return subst. structs
        X = -(_, Y),
        !.
get_sub_vars([X, Y | T], Sv, Vsi, Vso) :-
        X = -(_, X2),
        Y = -(_, Y2),
        get_sub_vars2(X2, Y2, Z, Vsi, Vs1),
        !,
        get_sub_vars([-(_, Z) | T], Sv, Vs1, Vso).

%! get_sub_vars2(+VarsA:list, +VarsB:list, -VarsC:list, +VarsIn:compound, -VarsOut:compound)
% Test each member of the first list against every member of the second to see
% if they have the same value ID. If so, unify their substitution variables and
% remove the element from the second list. When the first list is empty, return
% the remaining elements from the second list.
%
% @param VarsA First list of subst. structs.
% @param VarsB Second list of subst. structs.
% @param VarsC Output list of subst. structs.
% @param VarsIn Input variables.
% @param VarsOut Output variables.
get_sub_vars2([X | T], Y, [X | T2], Vsi, Vso) :-
        X = -(Ax, Bx),
        get_value_id(Ax, Xi, Vsi),
        get_sub_vars3(Y, Y2, Xi, Bx, Vsi, Vs1),
        !,
        get_sub_vars2(T, Y2, T2, Vs1, Vso).
get_sub_vars2([], Y, Y, V, V) :-
        !.

%! get_sub_vars3(+SubsIn:list, -SubsOut:list, +ID:int, +SubVar:ground, +VarsIn:compound, -VarsOut:compound)
% Given a list of variable substitution structs, get the value ID for each
% non-sub variable and test against the given ID. If they match, unify the
% substitution variable with SubVar and remove the struct from the list.
%
% @param SubsIn Input list of sub structs.
% @param SubsOut Output list of sub structs.
% @param ID The ID to test against.
% @param SubVar The variable to unify matches with.
% @param VarsIn Input variables.
% @param VarsOut Output variables.
get_sub_vars3([X | T], [X | T2], I, S, Vi, Vo) :- % Not a match
        X = -(Xa, _),
        get_value_id(Xa, I2, Vi),
        I \= I2,
        !,
        get_sub_vars3(T, T2, I, S, Vi, Vo).
get_sub_vars3([X | T], T2, I, S, Vi, Vo) :- % Match
        X = -(Xa, Xb),
        get_value_id(Xa, I2, Vi),
        I = I2,
        !,
        unify_vars(S, Xb, Vi, V1, 0),
        !,
        get_sub_vars3(T, T2, I, S, V1, Vo).
get_sub_vars3([], [], _, _, V, V) :-
        !.

%! remove_cycle_heads(+EvenLoopsIn:list, -EvenLoopsOut:list)
% For each even loop struct in the list, remove the first goal in the cycle. If
% the cycle would be empty, drop it from the list.
%
% @param EvenLoopsIn Input cycles.
% @param EvenLoopsOut Output cycles.
remove_cycle_heads([E | T], Eo) :-
        E = -([_ | Ct], Cv),
        (Ct = [] ->
                Eo = T2
        ;
                E2 = -(Ct, Cv),
                Eo = [E2 | T2]
        ),
        !,
        remove_cycle_heads(T, T2).
remove_cycle_heads([], []).

%! solve_expression(Goal:compound, +VarsIn:compound, -VarsOut:compound)
% Solve a single goal that we know is an expression.
%
% @param Goal An expression to solve.
% @param VarsIn Input variable list.
% @param VarsOut Output variable list.
solve_expression(=(G1, G2), Vi, Vo) :-
        solve_unify(G1, G2, Vi, Vo, 0),
        !.
solve_expression(\=(G1, G2), Vi, Vo) :-
        solve_dnunify(G1, G2, Vi, Vo, _).
solve_expression(not(is(G1, G2)), Vi, Vo) :-
        G2 =.. [O | _],
        operator(O, _, _), % RHS is an expression, solve it before continuing
        !,
        solve_subexpr(G2, Vi, V2),
        solve_expression(\=(G1, V2), Vi, Vo).
solve_expression(not(is(G1, G2)), Vi, Vo) :- % non-expression RHS
        !,
        solve_expression(\=(G1, G2), Vi, Vo).
solve_expression(is(G1, G2), Vi, Vo) :-
        is_unbound(G1, Vi, [], F, _), % G1 is unbound
        !,
        F =:= 0, % bindable; else fail
        solve_subexpr(G2, Vi, V2),
        catch( % test types
                check_type(V2, number),
                invalid_type,
                (
                        format_term(V2, V22, _, Vi),
                        fatal_error('is/2 expects RHS to be a function or a number, got ~w', [V22])
                )
             ),
        update_var_value(G1, val(V2), Vi, Vo),
        !.
solve_expression(is(G1, G2), Vi, Vo) :-
        is_unbound(G1, Vi, V1, F, _),
        V1 = [_ | _], % G1 is constrained
        !,
        F =:= 0, % bindable; else fail
        once(solve_subexpr(G2, Vi, V2)),
        catch( % test types
                check_type(V2, number),
                invalid_type,
                (
                        format_term(V2, V22, _, Vi),
                        fatal_error('is/2 expects RHS to be a function or a number, got ~w', [V22])
                )
             ),
        \+member(V2, V1), % V2 is not ruled out by constraints
        update_var_value(G1, val(V2), Vi, Vo),
        !.
solve_expression(is(G1, G2), Vi, Vo) :-
        is_var(G1), % G1 is a bound variable
        var_value(G1, Vi, val(V1)), % has a value
        !,
        catch( % test types
                check_type(V1, number),
                invalid_type,
                (
                        format_term(V1, V12, _, Vi),
                        fatal_error('is/2 expects LHS to be a number when bound, got ~w', [V12])
                )
             ),
        solve_subexpr(G2, Vi, V2),
        catch( % test types
                check_type(V2, number),
                invalid_type,
                (
                        format_term(V2, V22, _, Vi),
                        fatal_error('is/2 expects RHS to be a function or a number, got ~w', [V22])
                )
             ),
        solve_unify(V1, V2, Vi, Vo, 0),
        !.
solve_expression(is(G1, G2), Vi, Vo) :- % possibly covered by case 3 above
        is_var(G1), % G1 is a variable
        !, % No entry for G1 in Vi
        solve_subexpr(G2, Vi, V2),
        catch( % test types
                check_type(V2, number),
                invalid_type,
                (
                        format_term(V2, V22, _, Vi),
                        fatal_error('is/2 expects RHS to be a function or a number, got ~w', [V22])
                )
             ),
        update_var_value(G1, val(V2), Vi, Vo),
        !.
solve_expression(is(G1, G2), Vi, Vo) :-
        catch( % test types, LHS must be a number at this point
                check_type(G1, number),
                invalid_type,
                (
                        format_term(G1, G12, _, Vi),
                        fatal_error('is/2 expects LHS to be a number when bound, got ~w', [G12])
                )
             ),
        !,
        solve_subexpr(G2, Vi, V2),
        catch( % test types
                check_type(V2, number),
                invalid_type,
                (
                        format_term(V2, V22, _, Vi),
                        fatal_error('is/2 expects RHS to be a function or a number, got ~w', [V22])
                )
             ),
        solve_unify(G1, V2, Vi, Vo, 0),
        !.
solve_expression(=:=(G1, G2), V, V) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, number), check_type(V2, number)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('=:=/2 expects both arguments to be numbers, got ~w and ~w', [V12, V22])
                )
             ),
        V1 =:= V2,
        !.
solve_expression(=\=(G1, G2), V, V) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, number), check_type(V2, number)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('=\\=/2 expects both arguments to be numbers, got ~w and ~w', [V12, V22])
                )
             ),
        V1 =\= V2,
        !.
solve_expression(>(G1, G2), V, V) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, number), check_type(V2, number)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('>/2 expects both arguments to be numbers, got ~w and ~w', [V12, V22])
                )
             ),
        V1 > V2,
        !.
solve_expression(>=(G1, G2), V, V) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, number), check_type(V2, number)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('>=/2 expects both arguments to be numbers, got ~w and ~w', [V12, V22])
                )
             ),
        V1 >= V2,
        !.
solve_expression(<(G1, G2), V, V) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, number), check_type(V2, number)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('</2 expects both arguments to be numbers, got ~w and ~w', [V12, V22])
                )
             ),
        V1 < V2,
        !.
solve_expression(=<(G1, G2), V, V) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, number), check_type(V2, number)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('=</2 expects both arguments to be numbers, got ~w and ~w', [V12, V22])
                )
             ),
        V1 =< V2,
        !.
solve_expression(@>(G1, G2), V, V) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        V1 @> V2,
        !.
solve_expression(@>=(G1, G2), V, V) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        V1 @>= V2,
        !.
solve_expression(@<(G1, G2), V, V) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        V1 @< V2,
        !.
solve_expression(@=<(G1, G2), V, V) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        V1 @=< V2,
        !.

%! solve_subexpr(+Goal:compound, +Vars:list, -Value:compound)
% Solve a sub-expression, returning a value. Note that any variables must be
% bound. Fail otherwise.
% 
% @param Goal An expression to solve.
% @param Vars Variable list.
% @param Value The value returned.
solve_subexpr(G, _, G) :-
        number(G),
        !.
solve_subexpr(G, V, Val) :-
        is_var(G),
        var_value(G, V, val(Val)),
        !.
solve_subexpr(G, _, G) :-
        is_var(G), % not a number, but don't throw error yet.
        !.
solve_subexpr(+(G1, G2), V, Val) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, number), check_type(V2, number)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('+/2 expects both arguments to be numbers, got ~w and ~w', [V12, V22])
                )
             ),
        Val is V1 + V2,
        !.
solve_subexpr(-(G1, G2), V, Val) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, number), check_type(V2, number)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('-/2 expects both arguments to be numbers, got ~w and ~w', [V12, V22])
                )
             ),
        Val is V1 - V2,
        !.
solve_subexpr(*(G1, G2), V, Val) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, number), check_type(V2, number)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('*/2 expects both arguments to be numbers, got ~w and ~w', [V12, V22])
                )
             ),
        Val is V1 * V2,
        !.
solve_subexpr(/(G1, G2), V, Val) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, number), check_type(V2, number)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('=:=/2 expects both arguments to be numbers, got ~w and ~w', [V12, V22])
                )
             ),
        Val is V1 / V2,
        !.
solve_subexpr(//(G1, G2), V, Val) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, integer), check_type(V2, integer)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('///2 expects both arguments to be integers, got ~w and ~w', [V12, V22])
                )
             ),
        Val is V1 // V2,
        !.
solve_subexpr(rem(G1, G2), V, Val) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, integer), check_type(V2, integer)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('rem/2 expects both arguments to be integers, got ~w and ~w', [V12, V22])
                )
             ),
        Val is V1 rem V2,
        !.
solve_subexpr(mod(G1, G2), V, Val) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, integer), check_type(V2, integer)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('mod/2 expects both arguments to be integers, got ~w and ~w', [V12, V22])
                )
             ),
        Val is V1 mod V2,
        !.
solve_subexpr(<<(G1, G2), V, Val) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, integer), check_type(V2, integer)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('<</2 expects both arguments to be integers, got ~w and ~w', [V12, V22])
                )
             ),
        Val is V1 << V2,
        !.
solve_subexpr(>>(G1, G2), V, Val) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, integer), check_type(V2, integer)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('>>/2 expects both arguments to be integers, got ~w and ~w', [V12, V22])
                )
             ),
        Val is V1 >> V2,
        !.
solve_subexpr('**'(G1, G2), V, Val) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, number), check_type(V2, number)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('**/2 expects both arguments to be numbers, got ~w and ~w', [V12, V22])
                )
             ),
        Val is V1 ** V2,
        !.
solve_subexpr('^'(G1, G2), V, Val) :-
        solve_subexpr(G1, V, V1),
        solve_subexpr(G2, V, V2),
        catch( % test types
                (check_type(V1, number), check_type(V2, number)),
                invalid_type,
                (
                        format_term(V1, V12, _, V),
                        format_term(V2, V22, _, V),
                        fatal_error('^/2 expects both arguments to be numbers, got ~w and ~w', [V12, V22])
                )
             ),
        Val is V1 ^ V2,
        !.
solve_subexpr(G, _, G) :- % G is not a variable, number or expression. For now, assume atom.
        !.

%! check_type(+Goal, +Type:atom)
% Given a goal and a type, ensure that Goal is of type Type. Otherwise, print an
% exception and halt.
%
% @param Goal The goal to test.
% @param Type The type to match.
check_type(X, number) :-
        number(X),
        !.
check_type(X, integer) :-
        integer(X),
        !.
check_type(_, _) :-
        throw(invalid_type),
        !.

%! solve_unify(+Goal1:compound, +Goal2:compound, +VarsIn:compound, -VarsOut:compound, +OccursCheck:int)
% Solve a unification (=(G1, G2)).
%
% @param Goal1 A goal to unify.
% @param Goal2 A goal to unify.
% @param VarsIn Input variable list.
% @param VarsOut Output variable list.
% @param OccursCheck 1 or 0 indicating whether or not to perform the occurs
%        check when unifying a variable with a structure.
solve_unify(G1, G2, Vi, Vo, O) :- % var var, binding doesn't matter
        is_var(G1),
        is_var(G2),
        !,
        %(G2 = 'Var17' -> var_value(G2, Vi, Val), writef('var17 = ~w\n', [Val])
        %;
        %true),
        unify_vars(G1, G2, Vi, Vo, O).
solve_unify(G1, G2, V, V, _) :- %atom atom
        is_atom(G1, V, V1),
        is_atom(G2, V, V2),
        !,
        V1 == V2.
solve_unify(G1, G2, Vi, Vo, O) :- % term term, same functor and arity -> check args
        is_compound(G1, Vi, Val),
        is_compound(G2, Vi, Val2),
        Val =.. [Fun | A1],
        Val2 =.. [Fun | A2],
        length(A1, L),
        length(A2, L),
        !,
        solve_subunify(A1, A2, Vi, Vo, O).
solve_unify(G1, G2, Vi, Vo, O) :- % var atom/term
        \+is_var(G2),
        is_unbound(G1, Vi, C, F, _), % get any constraints
        !,
        %writef('a trying to unify var ~w with nonvar ~w, F = ~w\n', [G1, G2, F]),
        F \= 1, % var is bindable; else fail
        once(test_constraints(C, G2, Vi, V1)), % ensure constraints are satisfied.
        (O =:= 1 ->
                occurs_check(G1, G2, Vi)
        ;
                true
        ),
        %write('success\n'),
        update_var_value(G1, val(G2), V1, Vo).
solve_unify(G1, G2, Vi, Vo, O) :- % atom/term var
        \+is_var(G1),
        is_unbound(G2, Vi, C, F, _), % get any constraints
        !,
        %writef('b trying to unify var ~w with nonvar ~w, F = ~w\n', [G2, G1, F]),
        F \= 1, % var is bindable; else fail
        once(test_constraints(C, G1, Vi, V1)), % ensure constraints are satisfied.
        (O =:= 1 ->
                occurs_check(G2, G1, Vi)
        ;
                true
        ),
        %write('success\n'),
        update_var_value(G2, val(G1), V1, Vo).
solve_unify(G1, G2, Vi, Vo, O) :- % var atom/term
        \+is_var(G2),
        is_unbound(G1, Vi, C, F, _), % get any constraints
        !,
        F \= 1, % var is bindable; else fail
        var_value(G2, Vi, val(Gv)),
        once(test_constraints(C, Gv, Vi, V1)), % ensure constraints are satisfied.
        (O =:= 1 ->
                occurs_check(G1, G2, Vi)
        ;
                true
        ),
        update_var_value(G1, val(Gv), V1, Vo).
solve_unify(G1, G2, Vi, Vo, O) :- % atom/term var
        \+is_var(G1),
        is_unbound(G2, Vi, C, F, _), % get any constraints
        !,
        F \= 1, % var is bindable; else fail
        var_value(G1, Vi, val(Gv)),
        once(test_constraints(C, Gv, Vi, V1)), % ensure constraints are satisfied.
        (O =:= 1 ->
                occurs_check(G2, G1, Vi)
        ;
                true
        ),
        update_var_value(G2, val(Gv), V1, Vo).

%! solve_subunify(+Args1:list, +Args2:list, +VarsIn:compound, -VarsOut:compound, +OccursCheck:int)
% Given args for two compound terms, succeed if every matching pair unifies and
% the lists are the same length.
%
% @param Args1 A list of args to process.
% @param Args2 A list of args to process.
% @param VarsIn Input variable list.
% @param VarsOut Output variable list.
% @param OccursCheck 1 or 0 indicating whether or not to perform the occurs
%        check when unifying a variable with a structure.
solve_subunify([X | T], [Y | T2], Vi, Vo, O) :-
        solve_unify(X, Y, Vi, V1, O), % this pair is unifiable
        !,
        solve_subunify(T, T2, V1, Vo, O).
solve_subunify([], [], V, V, _) :-
        !.

%! solve_dnunify(+Goal1:compound, +Goal2:compound, +VarsIn:compound, -VarsOut:compound, -Flag:int)
% Solve a does-not-unify (\=(G1, G2)).
%
% @param Goal1 A goal to process.
% @param Goal2 A goal to process.
% @param VarsIn Input variable list.
% @param VarsOut Output variable list.
% @param Flag Used by recursive calls. 1 if constraints are set, 2 otherwise.
solve_dnunify(G1, G2, V, _, 1) :- % both unbound vars
        is_unbound(G1, V, _, _, _),
        is_unbound(G2, V, _, _, _),
        !,
        fatal_error('disunification expects at least one argument to be ground, got ~w and ~w', [G1, G2]).
solve_dnunify(G1, G2, Vi, Vo, 1) :- % G1 unbound
        is_unbound(G1, Vi, _, _, _),
        !,
        (is_ground(G2, Vi) -> % abort if G2 is non-ground
                add_var_constraint(G1, G2, Vi, Vo)
        ;
                format_term(G2, G22, _, Vi),
                fatal_error('disunification expects at least one argument to be ground, got ~w and ~w', [G1, G22])
        ).
solve_dnunify(G1, G2, Vi, Vo, 1) :- % G2 unbound
        is_unbound(G2, Vi, _, _, _),
        !,
        (is_ground(G1, Vi) -> % abort if G2 is non-ground
                add_var_constraint(G2, G1, Vi, Vo)
        ;
                format_term(G1, G12, _, Vi),
                fatal_error('disunification expects at least one argument to be ground, got ~w and ~w', [G12, G2])
        ).
solve_dnunify(G1, G2, V, V, 2) :- % atom atom
        is_atom(G1, V, Val),
        is_atom(G2, V, Val2),
        !,
        Val \= Val2.
solve_dnunify(G1, G2, Vi, Vo, F) :- % term term, same functor and arity -> check args
        is_compound(G1, Vi, Val),
        is_compound(G2, Vi, Val2),
        Val =.. [Fun | A1],
        Val2 =.. [Fun | A2],
        length(A1, L),
        length(A2, L),
        !,
        solve_subdnunify(A1, A2, Vi, Vo, F).
solve_dnunify(G1, G2, V, V, 2) :- % term term, different functor or arity -> succeed
        is_compound(G1, V, _),
        is_compound(G2, V, _),
        !.
solve_dnunify(_, _, V, V, 2) :- % bound to different types; automatically succeeds
        !.

%! solve_subdnunify(+Args1:list, +Args2:list, +VarsIn:compound, -VarsOut:compound, -Flag:int)
% Wrapper for solve_subdnunify2/5 so that we can check if at least one pair of
% args didn't unify.
%
% @param Args1 A list of args to process.
% @param Args2 A list of args to process.
% @param VarsIn Input variable list.
% @param VarsOut Output variable list.
% @param Flag Used by recursive calls.
solve_subdnunify(X, Y, Vi, Vo, F) :-
        once(solve_subdnunify2(X, Y, Vi, V1, F)),
        (
                (F = 1, Vo = V1) % succeed, keeping variable changes.
        ;
                (F = 2, Vo = Vi) % succeed, but discard variable changes. Two non-var args do not unify, so dropping changes doesn't change the outcome.
        ),
        !.

%! solve_subdnunify2(+Args1:list, +Args2:list, +VarsIn:compound, -VarsOut:compound, -Flag:int)
% Given args for two compound terms WITH THE SAME ARITIES, attempt to unify
% each pair. If a pair does not unify and one arg is a variable, call
% solve_dnunify/5 on the pair, set the flag to 1 and keep going. If a pair does
% not unify and neither arg is a variable, set output flag to 2 and return
% immediately.
%
% @param Args1 A list of args to process.
% @param Args2 A list of args to process.
% @param VarsIn Input variable list.
% @param VarsOut Output variable list.
% @param Flag Output flag.
solve_subdnunify2([X | _], [Y | _], Vi, Vo, Fo) :-
        solve_dnunify(X, Y, Vi, Vo, Fo).
solve_subdnunify2([X | T], [Y | T2], Vi, Vo, Fo) :-
        solve_unify(X, Y, Vi, V1, 0), % args unify; keep going
        !,
        solve_subdnunify2(T, T2, V1, Vo, Fo).
%solve_subdnunify2([], [], V, V, _) :-
%        !.

%! occurs_check(+Var:ground, Goal:compound, +Vars:compound) is det
% Perform the occurs check: fail if variable Var occurs in Goal, else succeed.
%
% @param Var The variable.
% @param Goal The goal.
% @param Vars The var struct to get variable IDs from.
occurs_check(X, _, _) :-
        \+is_var(X),
        !.
occurs_check(X, Y, V) :-
        is_var(Y),
        get_value_id(X, I, V),
        get_value_id(Y, I, V), % same ID; fail
        !,
        fail.
occurs_check(X, Y, V) :-
        is_var(Y),
        var_value(Y, V, val(Val)), % Y is bound
        !,
        occurs_check(X, Val, V).
occurs_check(_, Y, V) :-
        is_var(Y),
        is_unbound(Y, V, _, _, _), % Y is unbound, succeed
        !.
occurs_check(X, Y, V) :-
        Y =.. [_ | A],
        A \= [],
        !,
        occurs_check2(X, Y, V).
occurs_check(_, _, _) :- % if we haven't failed, succeed.
        !.

%! occurs_check2(+Var:ground, Goals:list, +Vars:compound) is det
% Perform occurs_check/3 on each member of Goals.
%
% @param Var The variable.
% @param Goals The goals.
% @param Vars The var struct to get variable IDs from.
occurs_check2(X, [Y | T], V) :-
        occurs_check(X, Y, V),
        !,
        occurs_check2(X, T, V).
occurs_check2(_, [], _) :-
        !.

%! solve_forall(+Var:ground, +Goal:compound, +VarsIn:compound, -VarsOut:compound, +CHSin:list, -CHSout:list, +CallStack:list, -EvenLoops:list, -Justification:compound, +InNMR:int)
% Solve a goal for all values of the variables Var. That is, the goal must
% succeed with Var unbound, or, if the goal succeeds with Var constrained, it
% must also succeed for each value it is constrained against.
%
% If a variable is already bound when entering the forall, ignore it and execute
% the goal normally. If constrained, ignore pre-existing constraints when
% checking all possible values.
%
% @param Var The variable to solve for.
% @param Goal The goal to solve.
% @param VarsIn Input var struct.
% @param VarsOut Output var struct.
% @param CHSin Input CHS.
% @param CHSout Output CHS.
% @param CallStack The list of ancestor calls. Used to ensure that positive
%        loops fail.
% @param EvenLoops List of even loops involving parent goal.
% @param Justification A struct of the form -(Goal, Constraints, List) where
%        list contains info on the rules used to satisfy Goal.
% @param InNMR 1 if the goal was first added after entering the NMR check, 0
%        otherwise.
solve_forall(V, G, Vi, Vo, Ci, Co, Cs, E, -(forall(Vj, Gj), C, Js), NMR) :-
        is_unbound(V, Vi, Old, _, _), % Variable must be unbound or constrained, else fail
        var_con(Val, Old, 1, -1), % variable CANNOT become a loop variable and flag must be stripped if present
        update_var_value(V, Val, Vi, V1), % set unbindable / unloopable
        %force(writef('forall solve_goals: ~w\n', [G])),
        format_term(V, Vj, Cv, Vi), % fill in variables, strip prefixes, etc.
        format_term(G, Gj, Cg, Vi), % fill in variables, strip prefixes, etc.
        append(Cv, Cg, C),
        solve_goals([G], V1, V2, Ci, C1, Cs, E1, J, NMR), % solve once here
        %fill_in_variable_values(G, Gp, [], _, V1),
        %force(writef('post forall solve_goals: ~w\n', [Gp])),
        %force(chs:print_chs(C1, V1, 2)),
        solve_forall2(V, Old, G, V2, V3, C1, C2, Cs, E2, Jt, NMR), % pass original variable values
        append(J, Jt, Js),
        var_con(Val2, Old, 1, -1), % unbound, unbindable, unloopable
        update_var_value(V, Val2, V3, V4),
        get_forall_goal(G, G2),
        G2 =.. [F | A],
        append(E1, E2, E),
        get_sub_vars(E, Ev, V4, V5),
        once(add_to_chs(F, A, 1, NMR, _, Ev, V5, Vo, C2, Co)), % add succeeding goal to CHS, but don't fail if already there
        if_debug(4, (
                format_term(G2, G3, _, Vo),
                writef('FORALL CHS marked success via forall for ~w\n', [G3])
                )).
        
%! solve_forall2(+Var:list, +OldValue, +Goal:compound, +VarsIn:compound, -VarsOut:compound, +CHSin:list, -CHSout:list, +CallStack:list, -EvenLoops:list, -Justification:list, +InNMR:int)
% Since solve_forall/8 has solved the goals once, the variable should have a
% value, be it bound, unbound or constrained. If bound, we simply fail. For
% constraints, we first check the original value and remove any pre-existing
% constraints, then call the goal for each ground value. If the variable is
% unbound, we simply succeed.
%
% @param Var The variable to solve for.
% @param OldValue The value of the variable before calling Goal in solve_forall/8.
% @param Goal The goal to solve.
% @param VarsIn Input var struct.
% @param VarsOut Output var struct.
% @param CHSin Input CHS.
% @param CHSout Output CHS.
% @param CallStack The list of ancestor calls. Used to ensure that positive
%        loops fail.
% @param EvenLoops List of even loops involving parent goal.
% @param Justification A list of structures representing the rules and goals
%        used to satisfy the current goal.
% @param InNMR 1 if the goal was first added after entering the NMR check, 0
%        otherwise.
solve_forall2(Var, _, _, V, V, C, C, _, [], [], _) :- % var is unbound; we're done
        is_unbound(Var, V, [], _, _),
        if_debug(4, writef('solve forall: var ~w is unbound\n', [Var])),
        !.
solve_forall2(Var, Ov, G, Vi, Vo, Ci, Co, Cs, E, J, NMR) :-
        is_unbound(Var, Vi, Val, _, _),
        Val = [_ | _],
        Ov \= [_ | _], % var is now constrained, but had no pre-existing constraints.
        !,
        if_debug(4, (
                format_term(Val, Val2, C, Vi),
                writef('solve forall: var ~w is constrained: ~w', [Var, Val2]),
                print_var_constraints(C),
                nl
                )),
        solve_forall3(Var, Val, G, Vi, Vo, Ci, Co, Cs, E, J, NMR).
solve_forall2(Var, Ov, G, Vi, Vo, Ci, Co, Cs, E, J, NMR) :-
        is_unbound(Var, Vi, Val, _, _),
        Val = [_ | _],
        Ov = [_ | _], % var is now constrained and had pre-existing constraints.
        !,
        list_diff(Val, Ov, Val2),
        if_debug(4, (
                format_term(Val2, Val3, C, Vi),
                writef('solve forall: var ~w is constrained: ~w', [Var, Val3]),
                print_var_constraints(C),
                nl
                )),
        solve_forall3(Var, Val2, G, Vi, Vo, Ci, Co, Cs, E, J, NMR).
solve_forall2(Var, _, _, V, _, _, _, _, _, _, _) :-
        var_value(Var, V, Val),
        if_debug(4, (
                format_term(Val, Val2, _, V),
                writef('solve forall FAILING: Var ~w is bound to ~w!\n', [Var, Val2])
                )),
        !,
        fail.

%! solve_forall3(+Var:ground, +Values:list, +Goals:list, +VarsIn:compound, -VarsOut:compound, +CHSin:list, -CHSout:list, +CallStack:list, -EvenLoops:list, -Justification:list, +InNMR:int) is nondet
% Since a variable from solve_forall2/9 has succeeded with constraints, call
% Goals once with the variable bound to each constrained value. If it succeeds
% for all of them, it succeeds for all possible values. Note that no changes to
% variable values will be returned.
%
% @param Var The variable to solve for.
% @param Values The list of values to solve for.
% @param Goals The goal to solve.
% @param VarsIn Input var struct.
% @param VarsOut Output var struct.
% @param CHSin Input CHS.
% @param CHSout Output CHS.
% @param CallStack The list of ancestor calls. Used to ensure that positive
%        loops fail.
% @param EvenLoops List of even loops involving parent goal.
% @param Justification A list of structures representing the rules and goals
%        used to satisfy the current goal.
% @param InNMR 1 if the goal was first added after entering the NMR check, 0
%        otherwise.
solve_forall3(Var, [X | T], G, Vi, Vo, Ci, Co, Cs, E, [-(G3, C, J1) | Jt], NMR) :-
        substitute(Var, X, G, G2), % substitute value for variable
        format_term(G2, G3, C, Vi),
        if_debug(4, (
                format_term(X, X2, _, Vi),
                writef('FORALL: calling ~w with ~w = ~w\n', [G3, Var, X2])
                )),
        %force(writef('forall3 solve_goals: ~w\n', [G2])),
        solve_goals([G2], Vi, V1, Ci, C1, Cs, E1, J1, NMR), % call with ground value
        %force(writef('back in forall3, T = ~w, chs = ~w\n', [T, C1])),
        solve_forall3(Var, T, G, V1, Vo, C1, Co, Cs, E2, Jt, NMR),
        append(E1, E2, E).
solve_forall3(_, [], _, V, V, C, C, _, [], [], _).

%! get_forall_goal(+GoalIn:compound, -GoalOut:compound) is det
% For a potentially nested forall, get the inner, non-forall goal.
%
% @param GoalIn Input goal.
% @param GoalOut Output goal.
get_forall_goal(Gi, Go) :-
        Gi = forall(_, G2),
        !,
        get_forall_goal(G2, Go).
get_forall_goal(Gi, Go) :-
        Gi = not(G2),
        !,
        G2 =.. [F | A], 
        negate_functor(F, Fn), % get dual functor
        Go =.. [Fn | A],
        !.
get_forall_goal(G, G) :-
        G \= forall(_, _),
        G \= not(_),
        !.

%! substitute(+Var:ground, +Value:ground, +GoalIn:compound, -GoalOut:compound) is det
% Substitute each occurrence of Var in GoalIn with Value.
%
% @param Var The variable to replace.
% @param Value The value to substitute.
% @param GoalIn Input goal.
% @param GoalOut Output goal.
substitute(X, Y, X, Y)  :- % match
        !.
substitute(X, Y, Gi, Go) :-
        Gi =.. [F | A], % compound term, check args
        substitute2(X, Y, A, Ao),
        Go =.. [F | Ao],
        !.
substitute(_, _, G, G) :- % not a match or a compound term; keep original
        !.

%! substitute2(+Var:ground, +Value:ground, +GoalsIn:compound, -GoalsOut:compound) is det
% For each goal in GoalsIn, substitute each occurrence of Var in with Value.
%
% @param Var The variable to replace.
% @param Value The value to substitute.
% @param GoalsIn Input goals.
% @param GoalsOut Output goals.
substitute2(X, Y, [G | T], [G2 | T2]) :-
        substitute(X, Y, G, G2),
        !,
        substitute2(X, Y, T, T2).
substitute2(_, _, [], []) :-
        !.


