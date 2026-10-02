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

:- module(sscasp_compat, []).
:- use_module(library(apply), [partition/4]).

/** <module> Run a program written for library(scasp) on library(sscasp)

Load this before the program:

    :- use_module(library(sscasp/compat)).

From then on, for every file loaded from outside this checkout, the
program's imports of `library(scasp)` and its sub libraries are
rewritten to `library(sscasp)` and the modules in `compat/`, which give
the parts that differ the old behaviour:

  - `:- use_module(library(scasp))` imports library(sscasp), with
    scasp_trace/2 and the assert family from `compat/scasp.pl`;
  - `:- use_module(library(scasp), Imports)` imports each predicate from
    where it lives now: scasp/1,2 and the assert family from
    `compat/scasp.pl`, which returns the old justification shape and
    accepts the old interned negation, the rest from library(sscasp);
  - `:- use_module(library(scasp/Sub), ...)` imports `compat/Sub.pl`.

It also

  - makes `:- table scasp_prolog:_`, which tables the old library's
    bridge to Prolog for `:- prolog` goals, table sscasp's, sld_proof/3;
  - rewrites clauses that use the old interned negation `'-p'(X)` to
    `-p(X)`, in a module that imports scasp/2;
  - defines `::` and `pred` as operators, before the program is read.

The rewriting is in user:term_expansion/2 and so sees every file the
process loads.  Files inside this checkout are left alone, because the
old implementation and the test suite load `library(scasp)` meaning
that.
*/

:- op(950, xfx, user:(::)).
:- op(1150, fx, user:pred).

:- multifile
    user:term_expansion/2.
:- dynamic
    user:term_expansion/2.

%!  checkout(-Dir) is det.
%
%   The root of this checkout, with a trailing `/`.

:- dynamic checkout/1.

:- prolog_load_context(directory, Dir),             % .../prolog/sscasp
   file_directory_name(Dir, Prolog),
   file_directory_name(Prolog, Root),
   atom_concat(Root, /, Prefix),
   assertz(checkout(Prefix)).

%!  rewriting is semidet.
%
%   True when the file being loaded is a program's rather than one of
%   this checkout's.

rewriting :-
    prolog_load_context(source, File),
    checkout(Root),
    \+ sub_atom(File, 0, _, _, Root).

%!  compat(?PI) is nondet.
%
%   Predicates of library(scasp) that `compat/scasp.pl` provides rather
%   than library(sscasp).

compat(scasp/1).
compat(scasp/2).
compat(PI) :-
    assert_compat(PI).

assert_compat(scasp_assert/1).
assert_compat(scasp_assert/2).
assert_compat(scasp_retract/1).
assert_compat(scasp_retractall/1).

%!  import_directives(+Imports, -Directives) is det.
%
%   Split an import list of library(scasp) over `compat/scasp.pl` and
%   library(sscasp).

import_directives(Imports, Directives) :-
    partition(compat_pi, Imports, Compat, Plain),
    (   Compat == []
    ->  Directives = [(:- use_module(library(sscasp), Plain))]
    ;   Directives = [ (:- use_module(library(sscasp/compat/scasp), Compat)),
                       (:- use_module(library(sscasp), Plain))
                     ]
    ).

compat_pi(PI) :-
    ground(PI),
    compat(PI).

%   library(sscasp) exports the renderers that the old library left to
%   library(scasp/html), which `compat/html.pl` provides under the old
%   names; the assert family comes from `compat/scasp.pl` for the
%   interned negation, see uninterned_clause/2.  sscasp has no
%   scasp_trace/2.

user:term_expansion((:- use_module(library(scasp))),
                    [ (:- use_module(library(sscasp),
                                     except([ html_model//2,
                                              html_justification//2,
                                              html_query//2,
                                              html_atom//2
                                            | Asserts
                                            ]))),
                      (:- use_module(library(sscasp/compat/scasp),
                                     [scasp_trace/2|Asserts]))
                    ]) :-
    rewriting,
    findall(PI, assert_compat(PI), Asserts).
user:term_expansion((:- use_module(library(scasp), Imports)), Directives) :-
    rewriting,
    import_directives(Imports, Directives).
user:term_expansion((:- use_module(library(scasp/Sub))),
                    (:- use_module(library(sscasp/compat/Sub)))) :-
    rewriting.
user:term_expansion((:- use_module(library(scasp/Sub), Imports)),
                    (:- use_module(library(sscasp/compat/Sub), Imports))) :-
    rewriting.

%   The old library proves `:- prolog` goals by scasp_prolog:scasp_prolog/2,
%   and a program may table that to avoid proving the same Prolog goal
%   again; sscasp proves `:- sld` goals by sscasp_sld:sld_proof/3.  On one
%   large program the table was the difference between 16 and 2 seconds.

user:term_expansion((:- table scasp_prolog:_),
                    (:- table sscasp_sld:sld_proof/3)) :-
    rewriting.

%!  scasp_module(-Module) is semidet.
%
%   The module being loaded uses s(CASP): it has imported scasp/2.

scasp_module(M) :-
    prolog_load_context(module, M),
    predicate_property(M:scasp(_,_), imported_from(_)).

%   The old library interns `-p(X)` as `'-p'(X)`, and a program written
%   for it may use that name in clauses and declarations; see
%   uninterned_clause/2.  A clause of -/1 is declared discontiguous as
%   sscasp does, because this expansion takes the place of sscasp's.
%
%   `:- scasp_dynamic '-p'/1` is left alone.  It declares a thread-local
%   '-p'/1 that stays empty; the clauses go to 'scasp$neg'/1, which
%   sscasp declares thread-local along with it.

user:term_expansion((:- discontiguous Name/_), (:- discontiguous((-)/1))) :-
    rewriting,
    interned_neg(Name),
    scasp_module(_).
user:term_expansion(Clause0, Expanded) :-
    callable(Clause0),
    Clause0 \= (:- _),
    rewriting,
    scasp_module(M),
    scasp_compat:uninterned_clause(Clause0, Clause),
    Clause \== Clause0,
    (   clause_head(Clause, -(_)),
        \+ predicate_property(M:(-(_)), discontiguous)
    ->  Expanded = [(:- discontiguous((-)/1)), Clause]
    ;   Expanded = Clause
    ).

interned_neg(Name) :-
    atom(Name),
    atom_concat(-, Plain, Name),
    sub_atom(Plain, 0, 1, _, C),
    char_type(C, lower).

clause_head((Head :- _), H) => H = Head.
clause_head(Head, H) => H = Head.

%   uninterned_clause/2 has to be there before any program is read.

:- use_module(compat/scasp, []).
