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

:- module(scasp_compat_swish, []).
:- use_module(library(http/html_write), [print_html/1]).
:- use_module(library(lists), [selectchk/3]).
:- use_module(swish(lib/config), [swish_config/2]).
:- use_module('../render', [scasp_readable/3]).
:- use_module('../html', [html_model//2, html_justification//2]).
:- use_module('../annotations', [program_annotations/2]).

/** <module> library(scasp/swish) on top of library(sscasp)

Hook into SWISH to show the model and the justification of an s(CASP)
answer, as the old library's adapter does.  SWISH asks for two extra
variables, named by swish_config:config/2 below, and shows the HTML
they are bound to after the bindings; `runner.js` and `answer.js` in
SWISH do that part and are unchanged.

The answer is the one library(sscasp) leaves for the toplevel (see
`dyncall.pl:remember/4`), so this needs nothing from the query but that
it went through scasp/1,2 or a `?` shortcut.  The page needs
`sscasp.js` and `sscasp.css` (`web/`), which SWISH's
`config-available/scasp.pl` loads as a web plugin.
*/

:- multifile
    swish_config:config/2,
    swish_trace:post_context/1,
    swish_trace:post_context/3.

swish_config:config(scasp_model_var, '_swish__scasp_model').
swish_config:config(scasp_justification_var, '_swish__scasp_justification').

%!  swish_trace:post_context(+Dict) is semidet.
%
%   Bind the two variables SWISH added to the query to the model and the
%   justification as HTML strings.  The variables of the answer are
%   named as the query names them, so `X` in the bindings is `X` in the
%   model.  Fails, leaving the answer to SWISH, when the query was not
%   an s(CASP) one.

swish_trace:post_context(Dict) :-
    _{bindings:Bindings0} :< Dict,
    swish_config:config(scasp_model_var, ModelVar),
    swish_config:config(scasp_justification_var, JustificationVar),
    selectchk(ModelVar = HTMLModel, Bindings0, Bindings1),
    selectchk(JustificationVar = HTMLJustification, Bindings1, Bindings),
    nb_current(scasp_answer,
               scasp_answer(Model, Justification, Terms, _Options)),
    program_annotations(Terms, Annotations),
    scasp_readable(t(Model, Justification), t(RModel, RJustification),
                   [variable_names(Bindings)]),
    HTMLOptions = [annotations(Annotations)],
    html_string(html_model(RModel, HTMLOptions), HTMLModel),
    html_string(html_justification(RJustification, HTMLOptions),
                HTMLJustification).

%!  swish_trace:post_context(+Name, +Goal, -Var) is semidet.
%
%   Suppress the normal residual goals of an s(CASP) answer: its
%   constraints are in the model and the justification, written on the
%   variables they are about.

swish_trace:post_context(Name, _Goal, _) :-
    swish_config(residuals_var, Name),
    nb_current(scasp_answer, _),
    !.

:- meta_predicate
    html_string(//, -).

html_string(Goal, HTML) :-
    phrase(Goal, Tokens),
    with_output_to(string(HTML0), print_html(Tokens)),
    split_string(HTML0, "", "\n ", [HTML]).
