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

:- module(scasp_compat_json,
          [ scasp_results_json/2        % +Results, -Dict
          ]).
:- use_module('../json', [answer_json/3, term_json/2]).
:- use_module('../render', [scasp_readable/2]).
:- use_module(scasp, [sscasp_forest/2]).
:- use_module(library(apply), [maplist/3, foldl/4]).
:- use_module(library(lists), [delete/3]).

/** <module> library(scasp/json) on top of library(sscasp)

The old library's one predicate, for a program that collects its
answers itself and wants them as a JSON document.  It takes the old
`Results` dict and writes the document library(sscasp) writes, which
differs from the old one as `sscasp/json.pl` says: the tree is a list,
a constraints entry is a list, `truth` is always a string.
*/

%!  scasp_results_json(+Results, -Dict) is det.
%
%   Results is a dict holding `query` and `answers`, and optionally
%   `cpu` and `inputs`, as for the old library.  Each answer is a dict
%   holding `answer` (its number), `bindings` (`Name = Value`), `model`
%   and optionally `query`, `tree` and `time` (a dict holding `cpu`).
%   The model and tree may be either library's: an old tree,
%   `query-Children` with `o_nmr_check`, is read as the forest it is.
%
%   The bindings, model and tree must be the answer's own, not copies,
%   or the constraints on its variables are not there to report.

:- det(scasp_results_json/2).

scasp_results_json(Results, Dict) :-
    _{ query: Query, answers: Answers } :< Results,
    query_json(Query, JQuery),
    maplist(old_answer_json(Query), Answers, JAnswers),
    Dict0 = scasp_result{ solver: sscasp,
                          query: JQuery,
                          answers: JAnswers
                        },
    foldl(copy_slot(Results), [cpu-time, inputs-inputs], Dict0, Dict).

copy_slot(From, Name-To, Dict0, Dict) :-
    (   get_dict(Name, From, Value)
    ->  put_dict(To, Dict0, Value, Dict)
    ;   Dict = Dict0
    ).

%   The query as a list of goals, without the NMR check the old solver
%   appends to it.

query_list(_:Query, List) =>
    query_list(Query, List).
query_list(Query, List), is_list(Query) =>
    delete(Query, o_nmr_check, List1),
    delete(List1, true, List).
query_list(Query, List) =>
    comma_list(Query, List0),
    query_list(List0, List).

query_json(Query, JQuery) :-
    query_list(Query, List),
    scasp_readable(List, Readable),
    maplist(term_json, Readable, JQuery).

old_answer_json(Query0, Answer, Dict) :-
    Query1 = Answer.get(query, Query0),
    query_list(Query1, List),
    comma_list(Query, List),
    get_dict(bindings, Answer, Bindings),
    get_dict(model, Answer, Model),
    (   get_dict(tree, Answer, Tree0)
    ->  unqualified(Tree0, Tree1),
        sscasp_forest(Tree1, Tree)
    ;   Tree = []
    ),
    answer_json(answer(Query, Model, Tree), Dict0,
                [variable_names(Bindings)]),
    (   get_dict(tree, Answer, _)
    ->  Dict1 = Dict0
    ;   del_dict(tree, Dict0, _, Dict1)
    ),
    get_dict(answer, Answer, N),
    put_dict(answer, Dict1, N, Dict2),
    (   get_dict(time, Answer, Time),
        get_dict(cpu, Time, CPU)
    ->  put_dict(time, Dict2, CPU, Dict)
    ;   Dict = Dict2
    ).

%   The old answers carry the tree module-qualified.  A tree itself is
%   a list or `Node-Children`, never `M:T`.

unqualified(M:Tree0, Tree), atom(M) =>
    Tree = Tree0.
unqualified(Tree0, Tree) =>
    Tree = Tree0.
