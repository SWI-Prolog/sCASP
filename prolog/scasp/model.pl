/*  Part of sCASP

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(scasp_model,
          [ canonical_model/2           % +RawModel,-Canonical
          ]).

/** <module> sCASP model handling


*/

%!  canonical_model(+RawModel, -Canonical)

canonical_model(Model, CanModel) :-
    flatten(Model, FlatModel),
    exclude(nonmodel_term, FlatModel, Model1),
    sort(Model1, Unique),
    maplist(raise_negation, Unique, Raised),
    sort_model(Raised, Sorted),
    simplify_model(Sorted, CanModel).

nonmodel_term(proved(_)).
nonmodel_term(chs(_)).
nonmodel_term(o_nmr_check).

raise_negation(not(TermIn), not(Term)) :-
    !,
    raise_negation(TermIn, Term).
raise_negation(TermIn, -Term) :-
    functor(TermIn, Name, _),
    atom_concat(-, Plain, Name),
    !,
    TermIn =.. [Name|Args],
    Term   =.. [Plain|Args].
raise_negation(Term, Term).

%!  sort_model(+ModelIn, -Sorted) is det.
%
%   Sort the model by literal, getting   affirming and denying knowledge
%   about a literal together.  If multiple terms about the same literal
%   appear they are ordered:
%
%     1. positive
%     2. not(positive)
%     3. -positive
%     4. not(-positive).

sort_model(ModelIn, Sorted) :-
    map_list_to_pairs(literal_key, ModelIn, Pairs),
    keysort(Pairs, KSorted),
    pairs_values(KSorted, Sorted).

literal_key(Term, Literal-Flags) :-
    literal_key(Term, Literal, 0, Flags).

literal_key(not(X), Key, F0, F) =>
    literal_key(X, Key, F0, F1),
    F is F1+0x1.
literal_key(-(X), Key, F0, F) =>
    literal_key(X, Key, F0, F1),
    F is F1+0x2.
literal_key(X, Key, F0, F) =>
    Key = X,
    F = F0.

%!  simplify_model(+ModelIn, -ModelOut) is det.
%
%   Remove model terms that are entailed by other model terms.

simplify_model([not(X0),-X1|T0], M), X0 == X1 =>
    M = [-X1|M0],
    simplify_model(T0, M0).
simplify_model([X0, not(-(X1))|T0], M), X0 == X1 =>
    M = [X0|M0],
    simplify_model(T0, M0).
simplify_model([H|T0], M) =>
    M = [H|M0],
    simplify_model(T0, M0).
simplify_model([], M) =>
    M = [].

