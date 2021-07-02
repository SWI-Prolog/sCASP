:- module(program, [
                    defined_rule/3,
                    defined_query/2,
                    defined_predicates/1,
                    defined_nmr_check/1,
                    reserved_prefix/1,
                    has_prefix/2,
                    assert_program/1,
                    assert_rule/1,
                    assert_nmr_check/1,
                    destroy_program/0
               ]).

/** <module> Input program access

Allow access to input program rules and query by asserting them and exporting
the resulting dynamic predicates.

@author Kyle Marple
@version 20170628
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
:- use_module(common).
:- use_module(options).

%! defined_rule(+Head:ground, +FullHead:compound, -Body:list) is nondet
% Dynamic predicate for asserted rules.
%
% @param Head Head predicate has head/arity (no args).
% @param FullHead A predicate struct containing the head predicate with args.
% @param Body list of body goals.

%! defined_query(-Goals:list, -SolCount:int) is det
% Dynamic predicate for query.
%
% @param Goals List of goals in the query.
% @param SolCount The number of answer sets to compute.

%! defined_predicates(-Predicates:list) is det
% Dynamic predicate for the list of predicate symbols defined in the input
% program.
%
% @param Predicates List of predicate structs.

%! defined_nmr_check(+Subchecks:list) is det
% Dynamic predicate for the list of NMR sub-checks.
%
% @param Subchecks The list of subcheck goals.
:- dynamic
    defined_rule/3,
    defined_query/2,
    defined_predicates/1,
    defined_nmr_check/1.

%! program(?ProgramStruct:compound, ?Rules:list, ?OptimizationStatements:list, ?Query:compound) is det
% Convert a program structure into its components, or vice-versa.
%
% @param ProgramStruct Program structure.
% @param Rules List of rules.
% @param OptimizationStatements List of optimization statements.
% @param Query Query structure.
program(p(Rules, OptStmts, Query), Rules, OptStmts, Query).

%! query(?QueryStruct:compound, ?Query:list, ?NMR_Check:list, ?SolutionCount:int) is det
% Convert a query structure to its components, or vice-versa. NMR_Check will be
% unbound until after nmr_check:generate_nmr_check/0 has finished.
%
% @param QueryStruct Query structure.
% @param Query List of query goals.
% @param NMR_Check List of NMR check goals (heads of NMR sub-checks).
% @param SolutionCount Hard-coded solution count.
query(c(Q, Nmr_check, N), Q, Nmr_check, N).

%!  reserved_prefix(+Prefix:ground) is det
%
%   Define reserved prefixes for predicates   and  compound terms. These
%   take the form of a single  letter   followed  by an underscore. This
%   predicate just tests the letter. The dummy prefix (`o_`) is appended
%   to  predicates  and  compound  terms  that   either  begin  with  an
%   underscore (legal in ASP but not Prolog)  or with a reserved prefix.
%   It will be removed last before printing,   and at most one copy will
%   be removed, ensuring that user-defined   predicates  starting with a
%   reserved prefix won't be processed the   same  as internally created
%   ones.
%
%   @arg Prefix The letter portion of the prefix.

reserved_prefix('c'). % classical negation
reserved_prefix('n'). % dual rule prefix
reserved_prefix('d'). % dummy prefix

%! has_prefix(+Functor:atom, -Prefix:atom) is semidet.
%
%  Succeed if Functor begins  with  a   reserved  prefix,  returning the
%  character part of the (first) prefix.
%
%  @arg Functor The functor to test.
%  @arg Prefix The character of the (first) reserved prefix of the
%  functor.

has_prefix(F, C) :-
    sub_atom(F, 1, _, _, '_'),
    sub_atom(F, 0, 1, _, C),
    reserved_prefix(C). % the letter is a reserved prefix

%! assert_program(+Statements:list) is det
%
%  Get rules, initial query and called   predicates  and assert them for
%  easy access.  This fills the dynamic predicates
%
%    - defined_predicates(List) with a list of predicate identifiers,
%      atoms encoded as <name>_<arity>, e.g., `parent_2` for parent/2.
%    - defined_rule(Name, Head, Body) where Name is the functor name
%      of Head and Body represents the conjunction of the body as a
%      list. Variables are Prolog atoms that satisfy the Prolog variable
%      syntax.
%    - defined_query(List, Count)
%      List is like Body above, expressing the query and Count is the
%      number of answer sets to generate by default (based on the
%      `Count { Query }.` syntax.
%    - defined_nmr_check(List)
%      When defined, a list of one atom containing the rule name for the
%      NMR check.
%
%  @arg Statements List of rules and compute statements produced by DCG.

assert_program(Stmts) :-
    write_verbose(0, 'Converting program to internal format...\n'),
    format_program(Stmts, Program),
    get_predicates(Program, Predicates),
    assert_predicates(Predicates),
    assert_program_struct(Program),
    handle_classical_negations(Predicates, []),
    !.
assert_program(_) :-
    write_error('could not convert input program to internal format'),
    !,
    fail.

%! format_program(+Statements:list, -Program:compound) is det
% Convert the list of statements to a program structure containing a list of
% rules and a single query. Queries are generated from compute statements. Use
% the last compute statement encountered, or a default one if no other is found.
% The default will always succeed during execution, so the answer set returned
% will rely on the NMR check.
%
% @param Statements List of rules and compute statements produced by DCG.
% @param Program Program data struct.
format_program(X, P) :-
    X \= [],
    !,
    predicate(G, '_false_0', []),
    user_option(ascount, N), % default number of answer sets to compute
    query(Q2, [not(G)], _, N),
    sort_by_type(X, R, Q2, Q),
    program(P, R, _, Q).
format_program([], P) :-
    predicate(G, '_false_0', []),
    query(Q, [not(G)], _, 1),
    program(P, [], _, Q),
    !.

%! sort_by_type(+Statements:list, -Rules:list, +ComputeIn:compound, -ComputeOut:compound) is det
% Take a list of statements, return a list of rules and the last compute
% statement encountered. Compute statement will be formatted as a query.
%
% @param Statements List of rules and compute statements produced by DCG.
% @param Rules extracted from Statements.
% @param ComputeIn compute statement.
% @param ComputeOut compute statement. Only the final compute statement is kept.
sort_by_type([X | T], [X | R], Ci, Co) :-
    c_rule(X, _, _),
    !,
    sort_by_type(T, R, Ci, Co).
sort_by_type([X | T], R, _, Co) :-
    X = c(N, Q),
    query(C, Q, _, N),
    !,
    sort_by_type(T, R, C, Co).
sort_by_type([], [], C, C).

%! get_predicates(+Program:compound, -Predicates:list) is det
% Get a list of the predicate symbols used in the rules or query of the program.
% This includes predicates that are called but not defined. The internal-use
% predicate _false_0 should be included explictly, in case a hard-coded query
% overrode the default one.
%
% @param Program A program struct.
% @param Predicates A list of predicate symbols defined in the program.
get_predicates(P, Ps) :-
    program(P, R, _, Q),
    get_predicates2(R, ['_false_0'], Ps1), % ensure that _false_0 is present
    query(Q, Qs, _, _),
    get_predicates3(Qs, Ps1, Ps),
    !.

%! get_predicates2(+Rules:list, +PredicatesIn:list, -PredicatesOut:list) is det
% Get the predicates defined or called in a list of rules.
%
% @param Rules A list of rules.
% @param PredicatesIn Input list of predicates.
% @param PredicatesOut Output list of predicates.
get_predicates2([R | T], Psi, Pso) :-
    c_rule(R, H, B),
    predicate(H, F, _), % get the functor
    member(F, Psi), % already seen
    !,
    get_predicates3(B, Psi, Ps2),
    !,
    get_predicates2(T, Ps2, Pso).
get_predicates2([R | T], Psi, Pso) :-
    c_rule(R, H, B),
    predicate(H, F, _), % get the functor
    !,
    get_predicates3(B, [F | Psi], Ps2),
    !,
    get_predicates2(T, Ps2, Pso).
get_predicates2([], Ps, Ps) :-
    !.

%! get_predicates3(+Goals:list, +PredicatesIn:list, -PredicatesOut:list) is det
% Get the predicates called in a list of goals. Note that this includes only
% predicates structs as defined by common:predicate/3, not operators.
%
% @param Goals A list of goals.
% @param PredicatesIn Input list of predicates.
% @param PredicatesOut Output list of predicates.
get_predicates3([G | T], Psi, Pso) :-
    G = [_ | _], % list; skip
    !,
    get_predicates3(T, Psi, Pso).
get_predicates3([G | T], Psi, Pso) :-
    predicate(G, F, _), % get the functor
    member(F, Psi), % already seen
    !,
    get_predicates3(T, Psi, Pso).
get_predicates3([G | T], Psi, Pso) :-
    G = not(G2),
    get_predicates3([G2], Psi, Ps1),
    !,
    get_predicates3(T, Ps1, Pso).
get_predicates3([G | T], Psi, Pso) :-
    predicate(G, F, _), % get the functor
    !,
    get_predicates3(T, [F | Psi], Pso).
get_predicates3([_ | T], Psi, Pso) :-
    !, % skip non-predicates (expressions)
    get_predicates3(T, Psi, Pso).
get_predicates3([], Ps, Ps) :-
    !.

%!  handle_classical_negations(+Predicates:list, +Seen:list) is det
%
%   From a list of predicates, get those   that begin with a '-'. Assign
%   the required number of variables, then create a rule of the form
%
%	:- -x, x.
%
%   @arg Predicates The list of predicates in the program.
%   @arg Seen The classically negated predicates that have already
%   been seen.
%   @tbd If we can get variables from the original, we should.

handle_classical_negations([X | T], S) :-
    has_prefix(X, 'c'), % classically negated literal
    \+ memberchk(X, S), % unprocessed classical negation
    atom_concat(c_, Xn, X), % non-negated literal
    defined_predicates(P),
    memberchk(Xn, P), % only add constraint if non-negated literal is actually used.
    !,
    split_functor(X, _, N), % get arity
    var_list(N, 0, [], A), % get args,
    X2 =.. [X | A],
    Xn2 =.. [Xn | A],
    predicate(H, '_false_0', []), % dummy head for headless rules
    c_rule(R, H, [X2, Xn2]),
    assert_rule(R), % assert rule
    !,
    handle_classical_negations(T, [X | S]).
handle_classical_negations([_ | T], S) :-
    !, % not a classical negation, or it's already been seen
    handle_classical_negations(T, S).
handle_classical_negations([], _) :-
    !.


%! assert_program_struct(+Program:compound) is det
% Assert rules and query from program struct.
%
% @param Program A program struct.
assert_program_struct(P) :-
    program(P, R, _, Q),
    assert_rules(R),
    assert_query(Q),
    !.

%! assert_rules(+Rules:list) is det
% Assert each rule in a list.
%
% @param Rules A list of rules.
assert_rules([H | T]) :-
    assert_rule(H),
    !,
    assert_rules(T).
assert_rules([]).

%! assert_rule(+Rule:compound) is det
% Assert a program rule.
%
% @param Rule A rule struct.
assert_rule(R) :-
    c_rule(R, H2, B),
    predicate(H2, H, _), % get the head without args
    assertz(defined_rule(H, H2, B)),
    !.

%! assert_query(+Query:compound) is det
% Assert the initial query.
%
% @param Query A query struct.
assert_query(Q) :-
    query(Q, Qs, _, N),
    assertz(defined_query(Qs, N)),
    !.

%! assert_nmr_check(+NMR:list) is det
% Assert the NMR check.
%
% @param NMR The list of goals in the NMR check.
assert_nmr_check(NMR) :-
    c_rule(R, '_nmr_check_0', NMR),
    assert_rule(R),
    assertz(defined_nmr_check(['_nmr_check_0'])),
    !.

%! assert_predicates(+Predicates:list) is det.
% Assert the list of defined predicate symbols.
%
% @param Predicates A list of predicates.
assert_predicates(Ps) :-
    assertz(defined_predicates(Ps)),
    !.

%! destroy_program
% Remove all asserted predicates to allow multiple funs with different programs.
destroy_program :-
    once(retractall(defined_rule(_, _, _))),
    once(retractall(defined_query(_, _))),
    once(retractall(defined_predicates(_))),
    once(retractall(defined_nmr_check(_))).
