/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(scasp_compat,
          [ scasp/1,                    % :Query
            scasp/2,                    % :Query, +Options
            scasp_assert/1,             % :Clause
            scasp_assert/2,             % :Clause, +Pos
            scasp_retract/1,            % :Clause
            scasp_retractall/1,         % :Head
            scasp_trace/2,              % :Goal, +Ports
            uninterned_clause/2,        % +Clause0, -Clause
            legacy_tree/2,              % +Forest, -Tree
            sscasp_forest/2             % +Tree, -Forest
          ]).
:- use_module(library(sscasp), [op(_,_,_)]).
:- use_module(library(option), [select_option/3]).
:- use_module(library(terms), [mapsubterms/3]).

/** <module> The parts of library(scasp) that differ from library(sscasp)

Part of the compatibility layer `library(sscasp/compat)` loads for a
program written for `library(scasp)`: the predicates whose behaviour
differs.

  - scasp/1,2 return the justification in the shape of the old
    library: `query-Forest`, with the NMR check as `o_nmr_check`.
  - scasp_trace/2 runs the goal without tracing: sscasp has no
    tracer.
  - scasp_assert/1,2, scasp_retract/1 and scasp_retractall/1 accept
    the old interned name `'-p'(X)` for the classical negation
    `-p(X)`.  See uninterned_clause/2.

Everything else is imported from library(sscasp) directly.  Note that
a module that uses scasp/2 from here rather than from library(sscasp)
does not get sscasp's term expansion for `false :- Body`, `:- abducible`
etc.  That is fine for modules that only run queries.
*/

:- meta_predicate
    scasp(:),
    scasp(:, +),
    scasp_assert(:),
    scasp_assert(:, +),
    scasp_retract(:),
    scasp_retractall(:),
    scasp_trace(0, +).

%!  scasp(:Query) is nondet.
%!  scasp(:Query, +Options) is nondet.
%
%   As sscasp:scasp/2, but tree(Tree) returns the old shape.

scasp(Query) :-
    scasp(Query, []).

scasp(Query, Options0) :-
    select_option(tree(Tree), Options0, Options1),
    !,
    sscasp:scasp(Query, [tree(Forest)|Options1]),
    legacy_tree(Forest, Tree).
scasp(Query, Options) :-
    sscasp:scasp(Query, Options).

%!  legacy_tree(+Forest, -Tree) is det.
%!  sscasp_forest(+Tree, -Forest) is det.
%
%   Translate between the sscasp justification forest and the old
%   `query-Children` tree.

legacy_tree(Forest, query-Children) :-
    mapsubterms(legacy_node, Forest, Children).

legacy_node('$nmr', o_nmr_check).

sscasp_forest(query-Children, Forest) :-
    !,
    mapsubterms(sscasp_node, Children, Forest).
sscasp_forest(Forest0, Forest) :-
    mapsubterms(sscasp_node, Forest0, Forest).

sscasp_node(o_nmr_check, '$nmr').
sscasp_node(goal_origin(Goal, _), Goal).

		 /*******************************
		 *     INTERNED NEGATION	*
		 *******************************/

%!  scasp_assert(:Clause) is det.
%!  scasp_assert(:Clause, +Pos) is det.
%!  scasp_retract(:Clause) is nondet.
%!  scasp_retractall(:Head) is det.
%
%   As the sscasp versions, after uninterned_clause/2.

scasp_assert(M:Clause0) :-
    uninterned_clause(Clause0, Clause),
    sscasp:scasp_assert(M:Clause).
scasp_assert(M:Clause0, Pos) :-
    uninterned_clause(Clause0, Clause),
    sscasp:scasp_assert(M:Clause, Pos).
scasp_retract(M:Clause0) :-
    uninterned_clause(Clause0, Clause),
    sscasp:scasp_retract(M:Clause).
scasp_retractall(M:Head0) :-
    uninterned_clause(Head0, Head),
    sscasp:scasp_retractall(M:Head).

%!  uninterned_clause(+Clause0, -Clause) is det.
%
%   The old library interns the classical negation `-p(X)` as `'-p'(X)`
%   and programs written for it use that name to create clauses, e.g.
%   `'-p'(X) :- Body`.
%   sscasp does not intern: `-p(X)` is a term of -/1.  Map the interned
%   names in the head and the literals of the body.

uninterned_clause(Clause0, Clause), var(Clause0) =>
    Clause = Clause0.
uninterned_clause((Head0 :- Body0), Clause) =>
    Clause = (Head :- Body),
    uninterned_literal(Head0, Head),
    uninterned_body(Body0, Body).
uninterned_clause(Head0, Head) =>
    uninterned_literal(Head0, Head).

uninterned_body(Var, Body), var(Var) =>
    Body = Var.
uninterned_body((A0,B0), Body) =>
    Body = (A,B),
    uninterned_body(A0, A),
    uninterned_body(B0, B).
uninterned_body((A0;B0), Body) =>
    Body = (A;B),
    uninterned_body(A0, A),
    uninterned_body(B0, B).
uninterned_body((A0->B0), Body) =>
    Body = (A->B),
    uninterned_body(A0, A),
    uninterned_body(B0, B).
uninterned_body(not(A0), Body) =>
    Body = not(A),
    uninterned_body(A0, A).
uninterned_body(A0, A) =>
    uninterned_literal(A0, A).

uninterned_literal(Lit0, Lit),
    callable(Lit0),
    Lit0 =.. [Name0|Args],
    atom_concat(-, Name, Name0),
    sub_atom(Name, 0, 1, _, C),
    char_type(C, lower) =>
    Pos =.. [Name|Args],
    Lit = -Pos.
uninterned_literal(Lit0, Lit) =>
    Lit = Lit0.


%!  scasp_trace(:Goal, +Ports)
%
%   The old library traces the solver.  sscasp has no tracer, so this
%   just runs Goal.

scasp_trace(Goal, _Ports) :-
    print_message(warning, format("sscasp has no tracer; running ~p", [Goal])),
    call(Goal).
