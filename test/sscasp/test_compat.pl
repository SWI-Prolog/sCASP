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

:- module(test_compat,
          [ test_compat/0
          ]).
:- use_module(library(sscasp/compat)).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(filesex), [directory_file_path/3, delete_directory_and_contents/1]).

/** <module> library(sscasp/compat): a program written for library(scasp)

The compatibility layer only rewrites files from outside this checkout,
so each program here is written to a temporary directory and loaded from
there.  Loading this suite installs the rewriting for the rest of the
process, which is why `test_all.pl` loads it last.
*/

test_compat :-
    run_tests([ compat ]).

%!  old_program(+Name, +Text, -Module) is det.
%
%   Write Text as module Name in a fresh temporary directory and load it.

old_program(Name, Text, Name) :-
    tmp_file(compat, Dir),
    make_directory(Dir),
    file_name_extension(Name, pl, Base),
    directory_file_path(Dir, Base, File),
    setup_call_cleanup(open(File, write, Out),
                       ( format(Out, ":- module(~q, []).~n", [Name]),
                         format(Out, Text, [])
                       ),
                       close(Out)),
    load_files(File, [silent(true)]),
    delete_directory_and_contents(Dir).

:- begin_tests(compat).

%   A bare import gives library(sscasp)'s scasp/2, and the old HTML
%   renderer and the assert family from compat/.

test(a_bare_import) :-
    old_program(compat_bare,
                ":- use_module(library(scasp)).~n\c
                 q(a). q(b). r(b).~n\c
                 p(X) :- q(X), not r(X).~n", M),
    assertion(predicate_property(M:scasp(_,_), imported_from(sscasp_dyncall))),
    assertion(predicate_property(M:scasp_assert(_), imported_from(scasp_compat))),
    assertion(\+ predicate_property(M:html_model(_,_,_,_), imported_from(_))),
    findall(X, M:scasp(p(X), []), Xs),
    assertion(Xs == [a]).

%   An import list takes scasp/2 from compat/, which returns the
%   justification in the old shape.

test(an_import_list) :-
    old_program(compat_list,
                ":- use_module(library(scasp), [scasp/2]).~n\c
                 q(a).~n\c
                 p(X) :- q(X).~n", M),
    assertion(predicate_property(M:scasp(_,_), imported_from(scasp_compat))),
    once(M:scasp(p(X), [tree(Tree)])),
    assertion(X == a),
    assertion(Tree = query-_),
    assertion(sub_term(o_nmr_check, Tree)).

%   The old library interned -s(X) as '-s'(X); a clause written that way
%   is a clause of -/1.

test(the_interned_negation) :-
    old_program(compat_neg,
                ":- use_module(library(scasp)).~n\c
                 q(a).~n\c
                 '-s'(X) :- q(X).~n", M),
    assertion(clause(M:(-(s(_))), _)),
    findall(Y, M:scasp(-s(Y), []), Ys),
    assertion(Ys == [a]).

%   library(scasp/Sub) is compat/Sub, and `:: ` and `pred` are operators
%   before the program is read.

test(a_sub_library_and_the_operators) :-
    old_program(compat_sub,
                ":- use_module(library(scasp)).~n\c
                 :- use_module(library(scasp/output)).~n\c
                 :- use_module(library(scasp/html)).~n\c
                 :- describe p(_X) :: \"@(X) holds\".~n\c
                 p(a).~n", M),
    assertion(predicate_property(M:ovar_analyze_term(_), imported_from(scasp_compat_output))),
    assertion(predicate_property(M:html_model_term(_,_,_,_), imported_from(scasp_compat_html))),
    assertion(current_op(950, xfx, user:(::))).

:- end_tests(compat).
