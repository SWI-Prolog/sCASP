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

:- module(scasp_compat_dyncall,
          [ scasp_query_clauses/2       % :Query, -Clauses
          ]).
:- use_module(library(sscasp), []).
:- use_module(library(apply), [convlist/3]).

/** <module> library(scasp/dyncall) on top of library(sscasp)

For a program that walks the clauses a query reaches by clause
reference, as the old library allowed.
*/

:- meta_predicate
    scasp_query_clauses(:, -).

%!  scasp_query_clauses(:Query, -Clauses) is det.
%
%   The clauses Query reaches as `source(ClauseRef, Module:Clause)`, as
%   the old library returns them.  sscasp returns the clauses without
%   reference, so we find it back.  Clauses for which there is no
%   clause reference, such as global constraints, are omitted.

scasp_query_clauses(M:Query, Clauses) :-
    sscasp:scasp_query_clauses(M:Query, Terms),
    convlist(source_clause(M), Terms, Clauses).

source_clause(_, (:- _), _) :-
    !,
    fail.
source_clause(M, Term, source(Ref, M:Term)) :-
    (   Term = (Head :- Body)
    ->  true
    ;   Head = Term,
        Body = true
    ),
    callable(Head),
    catch(clause(M:Head, Body0, Ref), _, fail),
    unqualified(Body0, M, Body1),
    (Head :- Body) =@= (Head :- Body1),
    !.

%   The body of a multifile clause is qualified.

unqualified(M:Body0, M, Body) => Body = Body0.
unqualified(Body0, _, Body) => Body = Body0.
