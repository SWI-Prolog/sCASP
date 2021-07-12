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

:- module(interactive, [
                    help/0,
                    get_user_query/1,
                    get_user_response/0
                   ]).

/** <module> User interaction

Predicates related to interaction with the user, particularly when running in
interactive mode.

@author Kyle Marple
@version 20170128
@license BSD-3
*/

:- use_module(library(lists)).
:- use_module(io).
:- use_module(text_dcg).
:- use_module(tokenizer).

%! help
% Print usage information.
help :- !, fail.
help :-
    %        current_prolog_flag(argv, [Prog | _]),
    Prog = casp,
    format(user_error, 'Usage: ~w [options] InputFile(s)\n\n', [Prog]),
    format(user_error, 'c(ASP) computes stable models of ungrounded normal logic programs.\n', []),
    % for reference on line length (keep under 80 chars/line incl. newline):
    %                 '1                                                                          80->x'
    write(user_error, 'Command-line switches are case-sensitive!\n\n'),
    write(user_error, ' General Options:\n\n'),
    write(user_error, '  -h, -?, --help     Print this help message and terminate.\n'),
    write(user_error, '  -i, --interactive  Run in user / interactive mode.\n'),
    write(user_error, '  -a, --auto         Run in automatic mode (no user interaction).\n'),
    write(user_error, '  -sN                Compute N answer sets, where N >= 0. 0 for all.\n'),
    write(user_error, '                     Ignored unless running in automatic mode.\n'),
    write(user_error, '  -v, --verbose      Enable verbose progress messages.\n'),
    write(user_error, '  -vv, --veryverbose Enable very verbose progress messages.\n'),
    write(user_error, '  -j                 Print proof tree for each solution.\n'),
    write(user_error, '  -w                 Generate html file with proof tree for each solution.\n'),
    write(user_error, '  -g                 Generate the program transformation (+ duals and nmr_check)\n'),
    write(user_error, '                     formated with pr_rule/2 in a new file named <NAME_pr.pl>.\n'),
    write(user_error, '  -n                 Hide goals added to solution by global consistency checks.\n'),
    write(user_error, '  -la                Print a separate list of succeeding abducibles with each\n'),
    write(user_error, '                     CHS. List will only be printed if at least one abducible\n'),
    write(user_error, '                     has succeeded.\n').

%! get_user_query(-Query:list) is det
% Write a prompt, then get a query from the user, tokenize it, parse it and
% return the list of goals. If an invalid query is entered, prompt the user for
% another one.
%
% @param Query The list of goals entered by the user.
get_user_query(Q) :-
    write('?- '), % write initial prompt
    read_query(user_input, CharPairs),
    (tokenize(CharPairs, Toks) ->
            (parse_query(Toks, Q) ->
                    true % we're done
            ;
                    get_user_query(Q) % prompt user for another try
            )
    ;
            get_user_query(Q) % prompt user for another try
    ),
    !.

%!  get_user_response is det
%
%   A query has succeeded, so get input from the user to accept ('.') or
%   reject (';') it.

get_user_response :-
    write(' ? '),
    get_single_char(C),
    nl,
    !,
    get_user_response2(C).

%!  get_user_response2(+Char:char) is det
%
%   Process the user's response. For invalid   responses, print an error
%   and give them another chance.
%
%   @argx Char The one character response.

get_user_response2(C) :- % accept
    member(C, ['.', 'c', 'a', '\n']),
    !.
get_user_response2(C) :- % accept
    char_type(C, end_of_line),
    !.
get_user_response2(C) :- % redo
    member(C, [';', 'n', 'r', ' ', '\t']),
    !,
    fail.
get_user_response2(C) :- % help
    member(C, ['?', 'h']),
    !,
    format(';, n, r, space, tab:\treject\n'),
    format('., c, a, enter:\t\taccept\n'),
    format('?, h:\t\t\thelp\n'),
    format('Action? '),
    !,
    get_user_response.
get_user_response2(C) :-
    format('Unknown action: ~w (h for help)\n', [C]),
    format('Action? '),
    !,
    get_user_response.





