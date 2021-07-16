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

:- module(tokenizer,
          [ tokenize/2
          ]).

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

:- use_module(library(lists)).
:- use_module(common).

%!  tokenize(+CharPairs:list, -Tokens:list) is semidet.
%
%   Convert a list of character/position pairs to   a list of tokens and
%   positions.
%
%   @arg CharPairs The list of character/position pairs.
%   @arg Tokens The list of tokens.

tokenize(CharPairs, Tokens) :-
    write_verbose(1, 'Tokenizing input...\n'),
    (   phrase(tokens(Tokens, 0, Errors), CharPairs)
    ->  (   Errors =:= 0
        ->  true
        ;   print_message(error, sasp(illegal_tokens(Errors))),
            fail
        )
    ;   print_message(error, sasp(illegal_tokens)),
        fail
    ).

%!  tokens(-Tokens:list, +ErrorsIn:int, -ErrorsOut:int)//
%
%   Parse the list of characters into  a   list  of  tokens, storing the
%   position of the first character in the token.
%
%   @arg Tokens The list of tokens.
%   @arg ErrorsIn Input error count.
%   @arg ErrorsOut Output error count.

tokens([Token | T], ErrorsIn, ErrorsOut) -->
    space_or_comment(ErrorsIn, E1),
    get_token(Token, E2),
    {E3 is E1 + E2},
    !,
    tokens(T, E3, ErrorsOut).
tokens([], ErrorsIn, ErrorsOut) -->
    space_or_comment(ErrorsIn, ErrorsOut).

%!  space_or_comment(+ErrorsIn:int, -ErrorsOut:int)//
%
%   Strip out whitespace and comments. May be empty.
%
%   @arg ErrorsIn Input error count.
%   @arg ErrorsOut Output error count.

space_or_comment(ErrorsIn, ErrorsOut) -->
    [(C, _)],
    { char_type(C, space) },
    !,
    space_or_comment(ErrorsIn, ErrorsOut).
space_or_comment(ErrorsIn, ErrorsOut) -->
    start_comment(X),
    !,
    skip_comment(X, E1),
    {E2 is ErrorsIn + E1},
    space_or_comment(E2, ErrorsOut).
space_or_comment(Errors, Errors) -->
    [].

%!  start_comment(+ID:atom)//
%
%   For comments, define start_comment and   end_comment using a unique,
%   matching id, such as pl or c.
%
%   @arg ID A unique identifier to link the start and end of a comment type.

start_comment(pl) -->
    [('%', _)], !.
start_comment(c) -->
    [('/', _)], [('*', _)].

%!  end_comment(+ID:atom, -ErrorCode:int)//
%
%   Match the end of a comment.
%
%   @arg ID A unique identifier to link the start and end of a comment type.
%   @arg ErrorCode 1 or 0 indicating if an error has occurred.

end_comment(pl, 0) -->
    [('\n', _)], !.
end_comment(pl, 0) -->
    eof.
end_comment(c, 0) -->
    [('*', _)], [('/', _)],
    !.
end_comment(c, 1) -->
    eof,
    {eof_error}.

eof([], []).

%!  skip_comment(+ID:atom, -ErrorCode:int)//
%
%   Given a comment id X with a defined end_comment(X), skip to the next
%   occurrence of the comment end.
%
%   @arg ID A unique identifier to link the start and end of a comment type.
%   @arg ErrorCode 1 or 0 indicating if an error has occurred.

skip_comment(X, ErrorCode) -->
    end_comment(X, ErrorCode),
    !.
skip_comment(X, ErrorCode) -->
    [_],
    skip_comment(X, ErrorCode).

%!  get_token(-Token:compound, -ErrorCode:int)//
%
%   Get a token or handle an error   if an invalid token is encountered.
%   This is split from token/3 to allow   cuts in token without breaking
%   error-handling.
%
%   @arg Token The token returned.
%   @arg ErrorCode 0 if no errors, otherwise 1.

get_token(T, 0) -->
    token(T),
    !.
get_token(_, 1) --> % an error has occurred, try to recover.
    [(C, Pos)],
    { \+ char_type(C, space),
      lex_error(C, Pos)
    },
    lex_recover,
    !.

%!  token(-Token:compound)//
%
%   Token definitions. Place any multi-character tokens above the bottom
%   two cases, which handle any other   printable  characters as well as
%   errors.
%
%   @arg Token The token returned. A pair containing the token and its position
%        information.

token((X, Pos)) -->
    identifier(Y2, Pos), % match [_]*[a-z][_A-Za-z0-9]+
    {atom_chars(Y, Y2)}, % get atom
    {check_type(Y, X)},
    !.
token((var(Y), Pos)) -->
    variable(Y2, Pos), % match [A-Z][_A-Za-z0-9]+
    {atom_chars(Y, Y2)}, % get atom
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

%!  op_tok(-Token:compound)//
%
%   Token for a multi-character operator that won't be matched as an id.
%
%   @arg Token The token returned as a pair(Atom, Pos)

term_expansion((op_tok(Tok) --> expand),
               (op_tok((Tok,Pos)) --> [(C0,Pos)|Body])) :-
    atom_chars(Tok, [C0|T]),
    maplist(c_pos, T, Body).

c_pos(C, (C,_)).

op_tok(:-)    --> expand.
op_tok(=:=)   --> expand.
op_tok(=\=)   --> expand.
op_tok(=..)   --> expand.
op_tok(\=)    --> expand.
op_tok(=<)    --> expand.
op_tok(<<)    --> expand.
op_tok(>=)    --> expand.
op_tok(>>)    --> expand.
op_tok(-->)   --> expand.
op_tok(->)    --> expand.
op_tok(**)    --> expand.
op_tok(/\)    --> expand.
op_tok(//)    --> expand.
op_tok(@>=)   --> expand.
op_tok(@=<)   --> expand.
op_tok(@<)    --> expand.
op_tok(@>)    --> expand.
op_tok(?-)    --> expand.
op_tok(\/)    --> expand.
% Default constraints
op_tok(#=<)   --> expand.
op_tok(#=)    --> expand.
op_tok(#<>)   --> expand.
op_tok(#<)    --> expand.
op_tok(#>=)   --> expand.
op_tok(#>)    --> expand.
% clpq/r)
op_tok(.=.)   --> expand.
op_tok(.<>.)  --> expand.
op_tok(.<.)   --> expand.
op_tok(.>.)   --> expand.
op_tok(.>=.)  --> expand.
op_tok(.=<.)  --> expand.
% operator for human output
op_tok(::)    --> expand.


%!  quoted_string(-Token:compound)//
%
%   A string in single quotes. When   looking for terminal quote, ignore
%   those escaped by a backslash.
%
%   @arg Token The token returned.

quoted_string((str(X), Pos)) -->
    [('\'', Pos)],
    !,
    [(Y, _)],
    quoted_string2(X2, Y),
    {atom_chars(X, ['\'', Y | X2])}.

%!  quoted_string(-String:compound, +LastChar:compound)//
%
%   Get all characters until an un-escaped single quote is encountered.
%
%   @arg String The string returned.
%   @arg LastChar The previous character read, to handle escaped quotes.

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

%!  escape(+CharIn:compound, -CharOut:compound)
%
%   Handle escape characters in quotes.
%
%   @arg CharIn The input character.
%   @arg CharOut The output character.
escape('n', '\n').
escape('l', '\n').
escape('r', '\r').
escape('t', '\t').
escape('\\', '\\').
escape('%', '%').
escape('\'', '\'').

%!  check_type(+TokenIn:compound, -TokenOut:compound)
%
%   Just make sure keywords aren't treated as normal identifiers.
%
%   @arg TokenIn Input token.
%   @arg TokenOut Output token.
check_type(X, X) :-
    keyword(X),
    !.
check_type(X, builtin(X)) :-
    builtin(X).
check_type(X, id(X)).

%!  keyword(+Token:atom)
%
%   Keyword and aggregate definitions.
%
%   @arg Token The keyword.

keyword(not).
keyword(is).
keyword(compute).
keyword(include).
keyword(table).
keyword(show).
keyword(pred).
keyword(abducible).
keyword('_abducible').
keyword(hide).
keyword('_false').
keyword('_nmr_check').


%!  builtin(+Token:atom)
%
%   Predicates that will be executed by Prolog
%
%   @arg Token The predicate name.

builtin(write).
builtin(writef).
builtin(nl).

%!  number(-Token:atom, -Position:compound)//
%
%   An integer or floating point token. Can be positive or negative.
%
%   @arg Token The token returned.
%   @arg Position The position info for the first character in the token.

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

%!  number2(-Token:atom)//
%
%   Integer or floating point token.
%
%   @arg Token The token returned.

number2(float(X), Y) -->
    c(.), c(C),
    {char_type(C, digit)},
    !, % floating point
    digits(Cs),
    {append(Y, ['.', C | Cs], Cs3)},
    {number_chars(X, Cs3)}.
number2(rat(X), Y) -->
    [('/', _), (C, _)],
    {char_type(C, digit)},
    !, % rational '/'
    digits(Cs),
    {append(Y, ['/', C | Cs], Cs3)},
    {number_chars(X, Cs3)}.
number2(int(X), Y) -->
    [], % integer
    !,
    {number_chars(X, Y)}.

%!  digits(-Digits:list)//
%
%   Should only be  called  from  number/4   and  number2/4.  Get  digit
%   characters.
%
%   @arg Digits The list of digits in the integer token.

digits([C|T]) -->
    c(C),
    {char_type(C, digit)},
    !,
    digits(T).
digits([]) -->
    [].

%!  identifier(-AtomChars:list, -Pos:compound)//
%
%   Detect and save tokens matching   [_]*[a-z][_A-Za-z0-9]+.  Returns a
%   list of characters, not an atom.
%
%   @arg AtomChars List of characters in a token.
%   @arg Pos The position info for the first character in the token.

identifier(Chars, Pos) -->
    pos(Pos),
    leading_underscores(Chars, [C|CSyms]),
    c(C),
    { char_type(C, lower) },
    csyms(CSyms).

%!  variable(-AtomChars:list, -Pos:compound)//
%
%   Detect and save tokens matching   [A-Z][_A-Za-z0-9]+. Returns a list
%   of characters, not an atom.
%
%   @arg AtomChars List of characters in a token.
%   @arg Pos The position info for the first character in the token.

variable(X, Pos) -->
    pos(Pos),
    c(C),
    { char_type(C, upper),
      X = [C | Xt]
    },
    csyms(Xt).

%!  leading_underscores(-Underscores:list, ?Tail)//
%
%   Get any leading underscores. May be empty.
%
%   @arg Underscores The list of underscores at the start of a token.

leading_underscores(['_'|T0], T) -->
    c('_'),
    !,
    leading_underscores(T0, T).
leading_underscores(T, T) -->
    [].

%!  csyms(-AtomChars:list)//
%
%   Get C symbols in an identifier. May be empty.
%
%   @arg AtomChars The list of characters that will be part of the current
%        token.

csyms([C|T]) -->
    c(C),
    { char_type(C, csym) },
    !,
    csyms(T).
csyms([]) -->
    [].

%!  lex_error(+Char:char, +Position:compound)
%
%   Print error message for failure during tokenization.
%
%   @arg Char The character which triggered the error.
%   @arg Position The position info for the character.

lex_error(Char, Pos) :-
    print_message(error, sasp(syntax_error(lexical(Char, Pos)))).

%!  eof_error
%
%   Print error for unexpected end of file.

eof_error :-
    print_message(error, sasp(syntax_error(unexpected_eof))).

%!  lex_recover// is det.
%
%   Skip characters until whitespace is  encountered.   The  idea  is to
%   produce as many useful error messages  as   possible,  so we want to
%   keep going if possible.

lex_recover -->
    c(C),
    { \+ char_type(C, space)},
    !,
    lex_recover.
lex_recover -->
    [].

pos(Pos, Chars, Chars) :-
    Chars = [(_,Pos)|_].

c(C) -->
    [(C,_Pos)].
