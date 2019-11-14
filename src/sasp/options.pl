:- module(options, [
                    user_option/1,
                    user_option/2,
                    set_default_options/0,
                    set_user_option/1,
                    set_user_option/2,
                    option_cleanup/0,
                    set_stack_sizes/0
                 ]).

/** <module> Assertion, retraction and testing of options.

Predicates related to managing options that alter runtime behavior.


@author Kyle Marple
@version 20170127
@license BSD-3
*/

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

:- use_module(config). % for default options

%! user_option(+Mode:atom) is det
% Asserted for options that are specified by the user via command-line
% arguments. Where applicable, these can override settings hard-coded into a
% program, such as the number of answer sets to compute. If not specified by the
% user, the hard-coded value would normally override the default.
%
% @param Mode The option specified by the user.

%! user_option(+Mode:atom, +Value:compound) is det
% Asserted for applicable options by set_user_option/2. Associates each option with
% a binary flag indicating whether or not the option is enabled. Only one fact
% should be asserted per Mode (ensured by set_user_option/2).
%
% @param Mode An atom identifying the option.
% @param Value The value for the option.
:- dynamic
    user_option/1,
    user_option/2.

%! set_default_options
% Get a list of all config:default_option/2 facts and set the options
% accordingly.
set_default_options :-
    findall(-(X, Y), default_option(X, Y), Defaults),
    set_default_options2(Defaults).

%! set_default_options2(+Options:list)
% For each option / flag pair, call set_user_option/2 after checking that no
% other entry for the same option exists.
%
% @param Options List of options to set.
set_default_options2([X | T]) :-
    X = -(A, B),
    \+user_option(A, _), % not overridden by user
    set_user_option(A, B),
    !,
    set_default_options2(T).
set_default_options2([X | T]) :-
    X = -(A, _),
    user_option(A, _), % overridden by user
    !,
    set_default_options2(T).
set_default_options2([]).

%! set_user_option(+Mode:atom) is det
% Assert user_option(Mode).
%
% @param Mode The option specified by the user.
set_user_option(Mode) :-
    assertz(user_option(Mode)).

%! set_user_option(+Mode:atom, +Value:compound)
% Assert an option with a given value.
%
% @param Mode The option to assert.
% @param Value The value for the option.
set_user_option(M, X) :-
    retractall(user_option(M, _)),
    assertz(user_option(M, X)),
    !.

%! option_cleanup
% Cleanup (retract) all user_option/1 and user_option/2 assertions.
option_cleanup :-
    retractall(user_option(_, _)),
    retractall(user_option(_)),
    !.

%! set_stack_sizes
% Increase the default stack sizes to the value indicated by
% config:stack_size/1. For 32-bit systems SWI should ignore this command. For
% 64-bit systems, a limit larger than the system allows will be automatically
% reduced to the maximum allowed size.
set_stack_sizes :-
    stack_size(_X),
    % set_prolog_stack(global, limit(X)),
    % set_prolog_stack(local, limit(X)),
    % set_prolog_stack(trail, limit(X)),
    !.
