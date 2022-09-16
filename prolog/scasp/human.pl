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

:- module(scasp_just_human,
          [ human_justification_tree/2, % :Tree, +Options
            human_model/2,              % :Model, +Options
            human_query/2,              % :Query, +Options
            human_predicate/2,          % :Rule, +Options
            human_rule/2                % :Rule, +Options
          ]).
:- use_module(html).
:- use_module(html_text).
:- use_module(library(http/html_write)).
:- use_module(library(option)).

:- meta_predicate
    human_justification_tree(:, +),
    human_model(:, +),
    human_query(:, +),
    human_predicate(:, +),
    human_rule(:, +).

/** <module> Print s(CASP) output in human language

This module prints the s(CASP) justification,   model and query in human
language. It translates the s(CASP) data into   a list of tokens as used
by  the  SWI-Prolog  print_message/2   and    friends   and   then  uses
print_message_lines/3 to emit the tokens.

This module reuses the  human  output  from   html.pl.  It  does  so  by
modifying the DCG that produces the  HTML tokens. This transformation is
defined in html_text.pl.
*/


%!  human_justification_tree(:Tree, +Options) is det.
%
%   Print Tree to `current_output` in   _human_ representation. Normally
%   this is used together with ovar_analyze_term/1.
%
%   @see print_message/2.

:- det(human_justification_tree/2).

human_justification_tree(M:Tree, Options) :-
    phrase(human_output(M:Tree,
                        [ depth(0),
                          module(M)
                        | Options
                        ]), Tokens0),
    fixup_layout(Tokens0, Tokens),
    format(current_output, '~N', []),
    print_message_lines(current_output, '', Tokens).

%!  human_output(:FilterChildren, +Options)

human_output(Tree, Options) -->
    !,
    emit_as(\html_justification_tree(Tree, Options),
            plain).


		 /*******************************
		 *            MODEL		*
		 *******************************/

%!  human_model(:Model, +Options)

human_model(M:Model, Options) :-
    phrase(emit_model(Model,
                      [ module(M)
                      | Options
                      ]), Tokens0),
    fixup_layout(Tokens0, Tokens),
    print_message_lines(current_output, '', Tokens).

:- det(emit_model//2).

emit_model(Model, Options) -->
    emit_as(\html_model(Model, Options),
            plain).


		 /*******************************
		 *            QUERY		*
		 *******************************/

%!  human_query(:Query, +Options)

human_query(M:Query, Options) :-
    phrase(emit_query(M:Query,
                      [ module(M)
                      | Options
                      ]), Tokens0),
    fixup_layout(Tokens0, Tokens),
    print_message_lines(current_output, '', Tokens).

:- det(emit_query//2).

emit_query(Query, Options) -->
    emit_as(\html_query(Query, Options),
            plain).


		 /*******************************
		 *           PROGRAM		*
		 *******************************/

%!  human_predicate(Clauses, Options)
%
%

human_predicate(Clauses, Options) :-
    human_text(\html_predicate(Clauses, Options)).

%!  human_rule(:Rule, +Options)

human_rule(M:Rule, Options) :-
    phrase(emit_rule(Rule,
                     [ module(M)
                     | Options
                     ]), Tokens0),
    fixup_layout(Tokens0, Tokens),
    print_message_lines(current_output, '', Tokens).

:- det(emit_rule//2).

emit_rule(Rule, Options) -->
    emit_as(\html_rule(Rule, Options),
            plain).


:- html_meta
    human_text(html).

human_text(Rule) :-
    phrase(emit_as(Rule, plain), Tokens0),
    fixup_layout(Tokens0, Tokens),
    print_message_lines(current_output, '', Tokens).





		 /*******************************
		 *         INTEGRATION		*
		 *******************************/

:- multifile
    scasp_stack:justification_tree_hook/2,
    scasp_model:model_hook/2.

scasp_stack:justification_tree_hook(Tree, Options) :-
    option(human(true), Options),
    !,
    human_justification_tree(Tree, Options).
scasp_model:model_hook(Model, Options) :-
    option(human(true), Options),
    !,
    human_model(Model, Options).
