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

:- module(scasp_compat_html,
          [ html_justification_tree//2, % :Tree, +Options
            html_model//2,              % :Model, +Options
            html_model_term//2,         % :Atom, +Options
            html_bindings//2,           % +Bindings, +Options
            html_term//2                % @Term, +Options
          ]).
:- use_module('../../sscasp', []).
:- use_module('../html', []).
:- use_module('../render', [scasp_readable/2]).
:- use_module(scasp, [sscasp_forest/2]).
:- use_module(library(http/html_write), [html//1]).
:- use_module(library(http/html_head), [html_resource/2]).
:- use_module('../annotations', [program_annotations/2]).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(lists), [member/2]).

/** <module> library(scasp/html) on top of library(sscasp)

What the old library's HTML renderer exported, on sscasp's: a program
written for `library(scasp/html)` gets this through
`library(sscasp/compat)`.
*/

%   The old library's resource.  sscasp's `sscasp.js` and `sscasp.css`
%   are copies of the old ones.

:- html_resource(scasp,
                 [ virtual(true),
                   requires([sscasp])
                 ]).

:- meta_predicate
    html_justification_tree(:, +, ?, ?),
    html_model(:, +, ?, ?),
    html_model_term(:, +, ?, ?).

html_justification_tree(M:Tree, Options0) -->
    { sscasp_forest(Tree, Forest0),
      scasp_readable(Forest0, Forest),
      annotated(M, Options0, Options)
    },
    sscasp_html:html_justification(Forest, Options).

html_model(M:Model0, Options0) -->
    { scasp_readable(Model0, Model),
      annotated(M, Options0, Options)
    },
    sscasp_html:html_model(Model, Options).

html_term(Term, Options) -->
    sscasp_html:html_term(Term, Options).

html_model_term(M:Atom0, Options0) -->
    { scasp_readable(Atom0, Atom),
      annotated(M, Options0, Options),
      term_string(Atom0, Title, [quoted(true)])
    },
    html([ span([class(human), title(Title)],
                \(sscasp_html:html_atom(Atom, Options))),
           span(class(machine),
                span(class([atom,pos]), \html_term(Atom, Options)))
         ]).

%!  annotated(+Module, +Options0, -Options) is det.
%
%   Add annotations(Annotations) for the `:- pred` descriptions of
%   Module and the modules it imports from.  The
%   old library finds these itself.  sscasp computes them from the
%   program of a query, so we compute them for the module and cache them
%   until a directive is added.

annotated(_, Options0, Options) :-
    memberchk(annotations(_), Options0),
    !,
    Options = Options0.
annotated(M, Options0, [annotations(Annotations)|Options0]) :-
    module_annotations(M, Annotations).

:- dynamic cached_annotations/3.

module_annotations(M, Annotations) :-
    findall(X, annotation_module(M, X), Modules),
    maplist(module_generation, Modules, Gens),
    (   cached_annotations(M, Gens, Annotations)
    ->  true
    ;   findall((:- D),
                ( member(X, Modules),
                  sscasp_dyncall:directive(X, D)
                ),
                Terms),
        program_annotations(Terms, Annotations),
        retractall(cached_annotations(M, _, _)),
        assertz(cached_annotations(M, Gens, Annotations))
    ).

annotation_module(M, M).
annotation_module(M, X) :-
    current_predicate(X:'scasp$directive'/1),
    X \== M,
    \+ predicate_property(X:'scasp$directive'(_), imported_from(_)),
    once(predicate_property(M:_, imported_from(X))).

module_generation(M, M-Gen) :-
    (   predicate_property(M:'scasp$directive'(_),
                           last_modified_generation(Gen))
    ->  true
    ;   Gen = 0
    ).

%!  html_bindings(+Bindings, +Options)//
%
%   The variable bindings as `Name = Value`, one per line.

html_bindings([], _Options) -->
    [].
html_bindings([Name=Value|T], Options) -->
    html(div(class('scasp-binding'),
             [ var(Name), ' = ', \html_term(Value, Options) ])),
    html_bindings(T, Options).
