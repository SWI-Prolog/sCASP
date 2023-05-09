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

:- module(scasp_compile,
          [ scasp_load/2,          % :Sources, +Options
            scasp_compile/2,       % :Terms, +Options
            scasp_compile_query/3, % :Goal,-Query,+Options
            scasp_query/1,         % :Query
            scasp_query/3          % :Query, -Bindings, +Options
          ]).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(prolog_code)).
:- use_module(predicates).
:- use_module(common).

/** <module> s(ASP) Ungrounded Stable Models Solver

Read in a normal logic program. Compute dual rules and the NMR check. Execute
the modified program according to the stable model semantics and output the
results.

@author Kyle Marple
@version 20170127
@license BSD-3
*/

:- use_module(input).
:- use_module(program).
:- use_module(comp_duals).
:- use_module(nmr_check).
:- use_module(pr_rules).
:- use_module(variables).

:- meta_predicate
    scasp_load(:, +),
    scasp_compile(:, +),
    scasp_query(:),
    scasp_query(:, -, +),
    scasp_compile_query(:, -, +).

%!  scasp_load(:Sources, +Options)
%
%   Load the files from Sources.   Steps taken:
%
%     - Parse input and assert in dynamic predicates with
%       program.pl (defined_rule/4, etc,)
%     - Enrich the program in the same format (comp_duals/0,
%       generate_nmr_check/0).
%     - Transform into _pr_ rules (generate_pr_rules/1)
%     - Destroy the program dynamic predicates.
%
%   @arg Sources A list of paths of input files.

:- det(scasp_load/2).
scasp_load(M:Spec, Options) :-
    to_list(Spec, Sources),
    call_cleanup(
        scasp_load_guarded(M:Sources, Options),
        destroy_program).

to_list(List, List) :-
    is_list(List),
    !.
to_list(One, [One]).

scasp_load_guarded(M:Sources, Options) :-
    clean_pr_program(M),
    load_source_files(Sources),
    comp_duals,
    generate_nmr_check(M),
    generate_pr_rules(M:Sources, Options).

%!  scasp_compile(:Terms, +Options) is det.
%
%   Create an sCASP program from Terms.

scasp_compile(M:Terms, Options) :-
    call_cleanup(
        scasp_compile_guarded(M:Terms, Options),
        destroy_program).

scasp_compile_guarded(M:Terms, Options) :-
    clean_pr_program(M),
    scasp_load_terms(Terms, Options),
    comp_duals,
    generate_nmr_check(M),
    generate_pr_rules(M:_Sources, Options). % ignored anyway

%!  scasp_compile_query(:Goal, -Query, +Options) is det.

scasp_compile_query(M:Goal, M:Query, Options) :-
    conj_to_list(Goal, Q0),
    maplist(intern_negation, Q0, Q1),
    add_nmr(Q1, Query, Options),
    maplist(check_existence(M), Query).

conj_to_list(true, []) :-
    !.
conj_to_list(Conj, List) :-
    comma_list(Conj, List).

add_nmr(Q0, Q, Options) :-
    option(nmr(false), Options),
    Q = Q0.
add_nmr(Q0, Q, _Options) :-
    append(Q0, [o_nmr_check], Q).

check_existence(M, G) :-
    shown_predicate(M:G),
    !.
check_existence(_,G) :-
    prolog_builtin(G),
    !.
check_existence(_,G) :-
    clp_builtin(G),
    !.
check_existence(_,G) :-
    clp_interval(G),
    !.
check_existence(_,_ is _) :-
    !.
check_existence(_, G) :-
    scasp_pi(G, PI),
    existence_error(scasp_predicate, PI).

scasp_pi(not(G), PI) :-
    !,
    scasp_pi(G, PI).
scasp_pi(G, PI) :-
    pi_head(PI, G).

%!  scasp_query(:Query) is det.
%
%   True when Query is the  (last)  sCASP   query  that  is  part of the
%   program.
%
%   @error existence_error(scasp_query, Module)

scasp_query(M:_Query) :-
    M:pr_query([not(o_false)]), !,
    existence_error(scasp_query, M).
scasp_query(M:Query) :-
    M:pr_query(Query).

%!  scasp_query(:Query, -Bindings, +Options) is det.
%
%   True when Query is the s(CASP) query as a list that includes the NMR
%   check if required. Bindings is a list of `Name=Var` terms expressing
%   the names of the variables.

scasp_query(M:Query, Bindings, Options) :-
    scasp_query(M:Query0),
    process_query(M:Query0, _, M:Query, Bindings, Options),
    maplist(check_existence(M), Query).

process_query(M:Q, M:Query, M:TotalQuery, VarNames, Options) :-
    revar(Q, A, VarNames),
    (   is_list(A)
    ->  Query = A
    ;   comma_list(A, Query)
    ),
    (   option(nmr(false), Options)
    ->  TotalQuery = Query
    ;   append(Query, [o_nmr_check], TotalQuery)
    ).

