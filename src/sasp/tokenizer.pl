:- module(tokenizer, [tokenize/2]).

/** <module> Tokenizer for file parsing

Read a list of character and position pairs into a list of tokens and position
info. Note that the tokenizer is more general than the input file format, so
the presence of keywords, operators or tokens in this file should not bee seen
as implying support by the overall system.

Thanks to Feliks Kluzniak, who provided the algorithm on which the scanner is
based, and convinced me that DCGs could produce meaningful error messages.

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

:- use_module(library(lists)).
%:- use_module(library(writef)).
:- use_module(ciao_auxiliar).
:- use_module(common).

%! tokenize(+CharPairs:list, -Tokens:list)
% Convert a list of character/position pairs to a list of tokens and positions.
%
% @param CharPairs The list of character/position pairs.
% @param Tokens The list of tokens.
tokenize(CharPairs, Tokens) :-
    write_verbose(1, 'Tokenizing input...\n'),
    once(tokens(Tokens, 0, Errors, CharPairs, [])),
    Errors =:= 0,
    !.
tokenize(_, _) :-
    write(user_error, 'One or more errors occured during scanning!\n'),
    !,
    fail.

%! tokens(-Tokens:list, +ErrorsIn:int, -ErrorsOut:int, +CharsIn:list, -CharsOut:list)
% Parse the list of characters into a list of tokens, storing the position of
% the first character in the token.
%
% @param Tokens The list of tokens.
% @param ErrorsIn Input error count.
% @param ErrorsOut Output error count.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
tokens([Token | T], ErrorsIn, ErrorsOut) -->
    space_or_comment(ErrorsIn, E1),
    get_token(Token, E2),
    {E3 is E1 + E2},
    !,
    tokens(T, E3, ErrorsOut).
tokens([], ErrorsIn, ErrorsOut) -->
    space_or_comment(ErrorsIn, ErrorsOut),
    [].

%! space_or_comment(+ErrorsIn:int, -ErrorsOut:int, +CharsIn:list, -CharsOut:list)
% Strip out whitespace and comments. May be empty.
%
% @param ErrorsIn Input error count.
% @param ErrorsOut Output error count.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
space_or_comment(ErrorsIn, ErrorsOut) -->
    [(C, _)],
    {\+char_type(C, graph)},
    space_or_comment(ErrorsIn, ErrorsOut).
space_or_comment(ErrorsIn, ErrorsOut) -->
    start_comment(X),
    !,
    skip_comment(X, E1),
    {E2 is ErrorsIn + E1},
    space_or_comment(E2, ErrorsOut).
space_or_comment(Errors, Errors) -->
    [].

%! start_comment(+ID:atom, +CharsIn:list, -CharsOut:list)
% For comments, define start_comment and end_comment using a unique, matching
% id, such as pl or c.
%
% @param ID A unique identifier to link the start and end of a comment type.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
start_comment(pl) -->
    [('%', _)].
start_comment(c) -->
    [('/', _)], [('*', _)].

%! end_comment(+ID:atom, -ErrorCode:int, +CharsIn:list, -CharsOut:list)
% Match the end of a comment.
%
% @param ID A unique identifier to link the start and end of a comment type.
% @param ErrorCode 1 or 0 indicating if an error has occurred.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
end_comment(pl, 0) -->
    [('\n', _)].
end_comment(pl, 0, [], []). % if last line, don't require a terminal newline.
end_comment(c, 0) -->
    [('*', _)], [('/', _)].
end_comment(_, 1, [], []) :- % An error has occurred.
    eof_error.

%! skip_comment(+ID:atom, -ErrorCode:int, +CharsIn:list, -CharsOut:list)
% Given a comment id X with a defined end_comment(X), skip to
% the next occurrence of the comment end.
%
% @param ID A unique identifier to link the start and end of a comment type.
% @param ErrorCode 1 or 0 indicating if an error has occurred.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
skip_comment(X, ErrorCode) -->
    end_comment(X, ErrorCode),
    !.
skip_comment(X, ErrorCode) -->
    [_],
    skip_comment(X, ErrorCode).

%! get_token(-Token:compound, -ErrorCode:int, +CharsIn:list, -CharsOut:list)
% Get a token or handle an error if an invalid token is encountered. This is
% split from token/3 to allow cuts in token without breaking error-handling.
%
% @param Token The token returned.
% @param ErrorCode 0 if no errors, otherwise 1.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
get_token(T, 0) -->
    token(T),
    !.
get_token(_, 1) --> % an error has occurred, try to recover.
    [(C, Pos)],
    {char_type(C, graph), % don't match whitespace
    lex_error(C, Pos)},
    lex_recover,
    !.

%! token(-Token:compound, +CharsIn:list, -CharsOut:list)
% Token definitions. Place any multi-character tokens above the bottom two
% cases, which handle any other printable characters as well as errors.
%
% @param Token The token returned. A pair containing the token and its position
%        information.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
token((X, Pos)) -->
    identifier(Y2, Pos), % match [_]*[a-z][_A-Za-z0-9]+
    {c_name(Y, Y2)}, % get atom
    {check_type(Y, X)},
    !.
token((var(Y), Pos)) -->
    variable(Y2, Pos), % match [A-Z][_A-Za-z0-9]+
    {c_name(Y, Y2)}, % get atom
    !.
token((X, Pos)) -->
    number(X, Pos),
    !.
token(X) -->
    op_tok(X).
token(X) -->
    quoted_string(X),
    !.
token((C, Pos)) --> % any other visible characters that aren't letters or digits
    [(C, Pos)],
    {char_type(C, punct)}.

%! op_tok(-Token:compound, +CharsIn:list, -CharsOut:list)
% Token for a multi-character operator that won't be matched as an id.
%
% @param Token The token returned.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
op_tok((':-', Pos)) -->
    [(':', Pos)], [('-', _)].
op_tok((=:=, Pos)) -->
    [('=', Pos)], [(':', _)], [('=', _)].
op_tok((=\=, Pos)) -->
    [('=', Pos)], [('\\', _)], [('=', _)].
op_tok((=.., Pos)) -->
    [('=', Pos)], [('.', _)], [('.', _)].        
op_tok((\=, Pos)) -->
    [('\\', Pos)], [('=', _)].
op_tok((=<, Pos)) -->
    [('=', Pos)], [('<', _)].
op_tok((<<, Pos)) -->
    [('<', Pos)], [('<', _)].
op_tok((>=, Pos)) -->
    [('>', Pos)], [('=', _)].
op_tok((>>, Pos)) -->
    [('>', Pos)], [('>', _)].
op_tok(('-->', Pos)) -->
    [('-', Pos)], [('-', _)], [('>', _)].
op_tok(('->', Pos)) -->
    [('-', Pos)], [('>', _)].
op_tok(('**', Pos)) -->
    [('*', Pos)], [('*', _)].
op_tok((/\, Pos)) -->
    [('/', Pos)], [('\\', _)].
op_tok((//, Pos)) -->
    [('/', Pos)], [('/', _)].
op_tok((@>=, Pos)) -->
    [('@', Pos)], [('>', _)], [('=', _)].
op_tok((@=<, Pos)) -->
    [('@', Pos)], [('=', _)], [('<', _)].
op_tok((@<, Pos)) -->
    [('@', Pos)], [('<', _)].
op_tok((@>, Pos)) -->
    [('@', Pos)], [('>', _)].
op_tok(('?-', Pos)) -->
    [('?', Pos)], [('-', _)].
op_tok((\/, Pos)) -->
    [('\\', Pos)], [('/', _)].
%% clp(fd)
op_tok((#=, Pos)) -->
    [('#', Pos)], [('=', _)].
op_tok((#\=, Pos)) -->
    [('#', Pos)], [('\\', _)], [('=', _)].
op_tok((#>, Pos)) -->
    [('#', Pos)], [('>', _)].
op_tok((#<, Pos)) -->
    [('#', Pos)], [('<', _)].
op_tok((#>=, Pos)) -->
    [('#', Pos)], [('>', _)], [('=', _)].
op_tok((#=<, Pos)) -->
    [('#', Pos)], [('=', _)], [('<', _)].
op_tok(('..', Pos)) -->
    [('.', Pos)], [('.', _)].
%% clp(q/r)
op_tok((.=., Pos)) -->
    [('.', Pos)], [('=', _)], [('.', _)].
op_tok((.<>., Pos)) -->
    [('.', Pos)], [('<', _)], [('>', _)], [('.', _)].
op_tok((.<., Pos)) -->
    [('.', Pos)], [('<', _)], [('.', _)].
op_tok((.>., Pos)) -->
    [('.', Pos)], [('>', _)], [('.', _)].
op_tok((.>=., Pos)) -->
    [('.', Pos)], [('>', _)], [('=', _)], [('.', _)].
op_tok((.=<., Pos)) -->
    [('.', Pos)], [('=', _)], [('<', _)], [('.', _)].


%! quoted_string(-Token:compound, +CharsIn:list, -CharsOut:list)
% A string in single quotes. When looking for terminal quote, ignore those
% escaped by a backslash.
%
% @param Token The token returned.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
quoted_string((str(X), Pos)) -->
    [('\'', Pos)],
    !,
    [(Y, _)],
    quoted_string2(X2, Y),
    {c_name(X, ['\'', Y | X2])}.

%! quoted_string(-String:compound, +LastChar:compound, +CharsIn:list, -CharsOut:list)
% Get all characters until an un-escaped single quote is encountered.
%
% @param String The string returned.
% @param LastChar The previous character read, to handle escaped quotes.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
quoted_string2([X | T], _) -->
    [('\\', _)], % escaped character
    [(X2, _)],
    {escape(X2, X)},
    !,
    quoted_string2(T, X).
quoted_string2([X | T], _) -->
    [(X, _)],
    {X \= '\''}, % non-quote
    !,
    quoted_string2(T, X).
quoted_string2(['\''], _) -->
    [('\'', _)], % un-escaped quote
    !.

%! escape(+CharIn:compound, -CharOut:compound)
% Handle escape characters in quotes.
%
% @param CharIn The input character.
% @param CharOut The output character.
escape('n', '\n').
escape('l', '\n').
escape('r', '\r').
escape('t', '\t').
escape('\\', '\\').
escape('%', '%').
escape('\'', '\'').

%! check_type(+TokenIn:compound, -TokenOut:compound)
% Just make sure keywords aren't treated as normal identifiers.
%
% @param TokenIn Input token.
% @param TokenOut Output token.
check_type(X, X) :-
    keyword(X),
    !.
check_type(X, builtin(X)) :-
    builtin(X).
check_type(X, id(X)).

%! keyword(+Token:atom)
% Keyword and aggregate definitions.
%
% @param Token The keyword.
keyword(not).
keyword(is).
keyword(compute).
keyword(include).
keyword(table).
keyword(show).
keyword(abducible).
keyword('_abducible').
keyword(hide).
keyword('_false').
keyword('_nmr_check').
% clpfd
keyword(in).


%! builtin(+Token:atom)
% Predicates that will be executed by Prolog
%
% @param Token The predicate name.
builtin(write).
builtin(writef).
builtin(nl).
% clpfd
builtin(labeling).
builtin(label).

%! number(-Token:atom, -Position:compound, +CharsIn:list, -CharsOut:list)
% An integer or floating point token. Can be positive or negative.
%
% @param Token The token returned.
% @param Position The position info for the first character in the token.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
number(X, Pos) -->
    [(C, Pos)],
    {char_type(C, digit)},
    !,
    digits(Cs),
    number2(X, [C | Cs]).
number(X, Pos) -->
    [('-', Pos), (C, _)],
    {char_type(C, digit)},
    !,
    digits(Cs),
    number2(X, ['-', C | Cs]).

%! number2(-Token:atom, +CharsIn:list, -CharsOut:list)
% Integer or floating point token.
%
% @param Token The token returned.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
number2(float(X), Y) -->
    [('.', _), (C, _)],
    {char_type(C, digit)},
    !, % floating point
    digits(Cs),
    {append(Y, ['.', C | Cs], Cs3)},
    {c_name(X, Cs3)}.
number2(rat(X), Y) -->
    [('/', _), (C, _)],
    {char_type(C, digit)},
    !, % rational '/'
    digits(Cs),
    {append(Y, ['/', C | Cs], Cs3)},
    {c_name(X, Cs3)}.
number2(int(X), Y) -->
    [], % integer
    !,
    {c_name(X, Y)}.

%! digits(-Digits:list, +CharsIn:list, -CharsOut:list)
% Should only be called from number/4 and number2/4. Get digit characters.
%
% @param Digits The list of digits in the integer token.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
digits([C | T]) -->
    [(C, _)],
    {char_type(C, digit)},
    !,
    digits(T).
digits([]) -->
    [].

%! identifier(-AtomChars:list, -Pos:compound, +CharsIn:list, -CharsOut:list)
% Detect and save tokens matching [_]*[a-z][_A-Za-z0-9]+. Returns a list of
% characters, not an atom.
%
% @param AtomChars List of characters in a token.
% @param Pos The position info for the first character in the token.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
identifier(X, Pos, Cin, Cout) :-
    Cin = [(_, Pos) | _], % Instantiate Pos.
    leading_underscores(X1, Cin, C1),
    C1 = [(C, _) | C2],
    char_type(C, lower),
    csyms(Xt, C2, Cout),
    append(X1, [C | Xt], X).

%! variable(-AtomChars:list, -Pos:compound, +CharsIn:list, -CharsOut:list)
% Detect and save tokens matching [A-Z][_A-Za-z0-9]+. Returns a list of
% characters, not an atom.
%
% @param AtomChars List of characters in a token.
% @param Pos The position info for the first character in the token.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
variable(X, Pos, Cin, Cout) :-
    Cin = [(C, Pos) | C2],
    char_type(C, upper),
    csyms(Xt, C2, Cout),
    X = [C | Xt].

%! leading_underscores(-Underscores:list, +CharsIn:list, -CharsOut:list)
% Get any leading underscores. May be empty.
%
% @param Underscores The list of underscores at the start of a token.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
leading_underscores(['_' | T], [('_', _) | T2], T3) :-
    leading_underscores(T, T2, T3).
leading_underscores([], [X | T], [X | T]) :-
    X \= ('_', _).

%! csyms(-AtomChars:list, +CharsIn:list, -CharsOut:list)
% Get C symbols in an identifier. May be empty.
%
% @param AtomChars The list of characters that will be part of the current
%        token.
% @param CharsIn Input character list.
% @param CharsOut Output character list.
csyms([C | T], [(C, _) | T2], T3) :-
    char_type(C, csym),
    !,
    csyms(T, T2, T3).
csyms([], [C | T2], [C | T2]) :-
    C = (C2, _),
    \+char_type(C2, csym).

%! lex_error(+Char:char, +Position:compound)
% Print error message for failure during tokenization.
%
% @param Char The character which triggered the error.
% @param Position The position info for the character.
lex_error(Char, (Line, Col)) :-
    swritef(Msg, 'ERROR: ~w:~w: Illegal character: \"~w\"\n', [Line, Col, Char]),
    write(user_error, Msg).

%! eof_error
% Print error for unexpected end of file.
eof_error :-
    swritef(Msg, 'ERROR: Unexpected end of file!\n'),
    write(user_error, Msg).

%! lex_recover(+CharsIn:list, -CharsOut:list)
% Skip characters until whitespace is encountered. The idea is to produce as
% many useful error messages as possible, so we want to keep going if possible.
%
% @param CharsIn Input character list.
% @param CharsOut Output character list.
lex_recover -->
    [(C, _)],
    {char_type(C, graph)},
    !,
    lex_recover.
lex_recover -->
    [].





