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

:- module(scasp_program,
          [ defined_rule/4,
            defined_query/2,
            defined_predicates/1,
            defined_nmr_check/1,
            defined_directive/1,
            reserved_prefix/1,
            has_prefix/2,
            replace_prefix/4,    % +FunctorIn,+OldPrefix,+NewPrefix,-Functor
            non_printable/1,     % +Name
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

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(debug)).

:- use_module(common).
:- use_module(variables).

%!  defined_rule(+Name:atom, +FullHead:compound, -Body:list, -Origin:compound) is nondet
%
%   Dynamic predicate for asserted rules.
%
%   @arg Head Head predicate has head/arity (no args).
%   @arg FullHead A predicate struct containing the head predicate with args.
%   @arg Body list of body goals.
%   @arg Origin A term describing the origin of this rule. One of
%   `clause(ClauseRef)` for rule derived from clauses, and `generated(Why)`
%   for compiler generated rules, where `Why` is one of `neg` for classical negation,
%   `nmr` for NMR checks, and `dual` for dual rules.

%!  defined_query(-Goals:list, -SolCount:int) is det
%
%   Dynamic predicate for query.
%
%   @arg Goals List of goals in the query.
%   @arg SolCount The number of answer sets to compute.

%!  defined_predicates(-Predicates:list) is det
%
%   Dynamic predicate for the list of   predicate symbols defined in the
%   input program.
%
%   @arg Predicates List of predicate structs.

%!  defined_nmr_check(+Subchecks:list) is det
%
%   Dynamic predicate for the list of NMR sub-checks.
%
%   @arg Subchecks The list of subcheck goals.

% These predicates are filled by assert_program/1 from the output of the
% parser.  scasp_load/1 realizes the entire compilation chain.
% Body terms contains variables as e.g. `'X'`.

:- thread_local
    defined_rule/4,		% Name, Head, Body, Origin
    defined_query/2,            % Body, Count
    defined_predicates/1,       % list(Name) (1 clause)
    defined_nmr_check/1,
    defined_directive/1.        % directive

%!  program(?ProgramStruct:compound, ?Rules:list, ?Directives:list,
%!          ?Query:compound) is det
%
%   Convert a program structure into its components, or vice-versa.
%
%   @arg ProgramStruct Program structure.
%   @arg Rules List of rules.
%   @arg Directives List of directives.
%   @arg Query Query structure.

program(p(Rules, Directives, Query), Rules, Directives, Query).

%!  query(?QueryStruct:compound, ?Query:list, ?NMR_Check:list,
%!        ?SolutionCount:int) is det
%
%   Convert  a  query  structure  to   its  components,  or  vice-versa.
%   NMR_Check will be unbound until after nmr_check:generate_nmr_check/0
%   has finished.
%
%   @arg QueryStruct Query structure.
%   @arg Query List of query goals.
%   @arg NMR_Check List of NMR check goals (heads of NMR sub-checks).
%   @arg SolutionCount Hard-coded solution count.

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

%!  has_prefix(+Functor:atom, -Prefix:atom) is semidet.
%
%   Succeed if Functor begins with  a   reserved  prefix,  returning the
%   character part of the (first) prefix.
%
%   @arg Functor The functor to test.
%   @arg Prefix The character of the (first) reserved prefix of the
%        functor.

has_prefix(F, C) :-
    sub_atom(F, 1, _, _, '_'),
    sub_atom(F, 0, 1, _, C),
    reserved_prefix(C). % the letter is a reserved prefix

%!  replace_prefix(+FunctorIn, +OldPrefix, +NewPrefix, -Functor)

replace_prefix(F0, P0, P, F) :-
    string_concat(P0, B, F0),
    atom_concat(P, B, F).

%!  non_printable(+Name) is semidet.
%
%   True if Name should not be printed. This   is true if it starts with
%   an underscore or has a normal prefix and then an underscore.

non_printable(Name) :-
    sub_atom(Name, 0, _, _, '_'),
    !.
non_printable(Name) :-
    sub_atom(Name, 1, _, _, '__'),
    !.


%!  assert_program(+Statements:list) is det
%
%   Get rules, initial query and called   predicates and assert them for
%   easy access. This fills the dynamic predicates
%
%    - defined_predicates(List) with a list of predicate identifiers,
%      atoms encoded as <name>_<arity>, e.g., `parent_2` for parent/2.
%    - defined_rule(Name, Head, Body, Origin) where Name is the functor name
%      of Head and Body represents the conjunction of the body as a
%      list, and Origin denotes the source of the rule's definition.
%    - defined_query(List, Count)
%      List is like Body above, expressing the query and Count is the
%      number of answer sets to generate by default (based on the
%      `Count { Query }.` syntax.
%    - defined_nmr_check(List)
%      When defined, a list of one atom containing the rule name for the
%      NMR check.
%
%  @arg Statements List of rules and compute statements produced by DCG.

:- det(assert_program/1).

assert_program(Stmts) :-
    debug(scasp(compile), 'Converting program to internal format...', []),
    format_program(Stmts, Program),
    get_predicates(Program, Predicates),
    assert_predicates(Predicates),
    assert_program_struct(Program),
    maplist(handle_classical_negation, Predicates).

%!  format_program(+Statements:list, -Program:compound) is det
%
%   Convert the list of statements to   a program structure containing a
%   list of rules and a single query. Queries are generated from compute
%   statements. Use the last compute statement encountered, or a default
%   one if no other is found.  The   default  will always succeed during
%   execution, so the answer set returned will rely on the NMR check.
%
%   @arg Statements List of rules and compute statements produced by DCG.
%   @arg Program Program data struct.

format_program([], P) :-
    !,                       % no program
    predicate(G, '_false_0', []),
    query(Q, [not(G)], _, 1),
    program(P, [], [], Q).
format_program(X, P) :-
    predicate(G, '_false_0', []),
    AScount = 1,
    query(Q2, [not(G)], _, AScount),
    sort_by_type(X, R, D, Q2, Q),
    program(P, R, D, Q).

%!  sort_by_type(+Statements:list, -Rules:list, -Directives:list,
%!               +ComputeIn:compound, -ComputeOut:compound) is det
%
%   Take a list of statements,  return  a   list  of  rules and the last
%   compute statement encountered. Compute statement   will be formatted
%   as a query.
%
%   @arg Statements List of rules and compute statements produced by DCG.
%   @arg Rules extracted from Statements. Each rule is a term Head-Body.
%   @arg Directives is a list of plain directive terms (without # or :-)
%   @arg ComputeIn compute statement. @arg ComputeOut compute statement.
%   Only the final compute statement is kept.

:- det(sort_by_type/5).

sort_by_type([clause(Ref, X)|T], [clause(Ref, X)|R], D, Ci, Co) :-
    c_rule(X, _, _),
    !,
    sort_by_type(T, R, D, Ci, Co).
sort_by_type([clause(_Ref, X)|T], R, D, _, Co) :-
    X = c(N, Q),
    query(C, Q, _, N),
    !,
    sort_by_type(T, R, D, C, Co).
sort_by_type([:-(Directive)|T], R, [Directive|D], C, Co) :-
    !,
    sort_by_type(T, R, D, C, Co).
sort_by_type([], [], [], C, C).

%!  get_predicates(+Program:compound, -Predicates:list) is det
%
%   Get a list of the predicate symbols used   in  the rules or query of
%   the program. This includes  predicates  that   are  called  but  not
%   defined. The internal-use predicate ``_false_0``  should be included
%   explictly, in case a hard-coded query   overrode the default one. We
%   add ``_false_0`` as head of the query   for  that purpose. This both
%   gets the query in the shape of   a  rule and ensures ``_false_0`` is
%   included.
%
%   @arg Program A program struct.
%   @arg Predicates A list of predicate symbols defined in the program.

:- det(rule_predicates/2).

get_predicates(P, Ps) :-
    program(P, R, _, Q),
    query(Q, Qs, _, _),
    rules_predicates(['_false_0'-Qs|R], Ps).

rules_predicates(Rules, Preds) :-
    maplist(rule_predicates, Rules, Preds0),
    append(Preds0, Preds1),
    list_to_set(Preds1, Preds).

rule_predicates(clause(_Ref, R), Preds) :-
    !,
    rule_predicates(R, Preds).
rule_predicates(R, Preds) :-
    c_rule(R, H, B),
    atom_predicate(H, F),
    convlist(atom_predicate, B, FB),
    list_to_set([F|FB], Preds).

atom_predicate(not(X), P) :-
    atom_predicate(X, P).
atom_predicate(X, F) :-
    predicate(X, F, _).

%!  handle_classical_negation(+Predicate:atom) is det
%
%   If Predicate is classically negated (in the source starts with '-').
%   Assign the required number of variables, then   create a rule of the
%   form
%
%	:- -x, x.
%
%   @arg Predicate is the name (atom) of a predicate

handle_classical_negation(X) :-
    has_prefix(X, 'c'), % classically negated literal
    atom_concat(c_, Xn, X), % non-negated literal
    defined_predicates(P),
    memberchk(Xn, P), % only add constraint if non-negated literal is actually used.
    !,
    split_functor(X, _, N), % get arity
    var_list(N, A), % get args,
    X2 =.. [X|A],
    Xn2 =.. [Xn|A],
    predicate(H, '_false_0', []), % dummy head for headless rules
    c_rule(R, H, [X2, Xn2]),
    assert_rule(neg(R)).
handle_classical_negation(_).

%!  assert_program_struct(+Program:compound) is det
%
%   Assert rules and query from program struct.
%
%   @arg Program A program struct.

assert_program_struct(P) :-
    program(P, R, D, Q),
    maplist(assert_rule, R),
    maplist(assert_directive, D),
    assert_query(Q).

%!  assert_rule(+Rule:compound) is det
%
%   Assert a program rule.
%
%   @arg Rule A rule struct.

:- det(assert_rule/1).

assert_rule(Rule) :-
    rule_origin(Rule, Origin, R),
    assert_rule_(R, Origin).

assert_rule_(Rule, Origin) :-
    c_rule(Rule, H2, B),
    predicate(H2, H, _), % get the head without args
    assertz(defined_rule(H, H2, B, Origin)).

rule_origin(clause(Ref, Rule), clause(Ref), Rule).
rule_origin(neg(Rule), generated(neg), Rule).
rule_origin(nmr(Rule), generated(nmr), Rule).
rule_origin(dual(Rule), generated(dual), Rule).


%!  assert_directive(+Directive) is det
%
%   Assert a directive
%
%   @arg Directive is a term table(Pred), show(Pred) or pred(Pred)

assert_directive(D) :-
    assertz(defined_directive(D)).

%!  assert_query(+Query:compound) is det
%
%   Assert the initial query.
%
%   @arg Query A query struct.

assert_query(Q) :-
    query(Q, Qs, _, N),
    assertz(defined_query(Qs, N)).

%!  assert_nmr_check(+NMR:list) is det
%
%   Assert the NMR check.
%
%   @arg NMR The list of goals in the NMR check.

assert_nmr_check(NMR) :-
    c_rule(R, '_nmr_check_0', NMR),
    assert_rule(nmr(R)),
    assertz(defined_nmr_check(['_nmr_check_0'])).

%!  assert_predicates(+Predicates:list) is det.
%
%   Assert the list of defined predicate symbols.
%
%   @arg Predicates A list of predicates.

assert_predicates(Ps) :-
    assertz(defined_predicates(Ps)).

%!  destroy_program
%
%   Remove all asserted predicates to allow multiple funs with different
%   programs.

destroy_program :-
    retractall(defined_rule(_, _, _, _)),
    retractall(defined_query(_, _)),
    retractall(defined_predicates(_)),
    retractall(defined_nmr_check(_)),
    retractall(defined_directive(_)).
