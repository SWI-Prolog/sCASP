:- module(io,
          [ load_source_files/1,
            read_query/2,
            (pred)/1,
            show/1,
            asp_table/1
          ]).

/** <module> Handle opening and closing files and directing output

Input and output handling. Handles the opening and closing of files and
provides a wrapper for the tokenizer to ensure that files are properly closed
even if an error occurs.

JW: A program is parsed by load_source_files/5.  This emits a list of

  - c(N, Query)
    Created from `N { Query }.` or `?- Query.` (where N = 1).
    sort_by_type/4 selects the _last_ query as c(Q,Nmr_check,N)
  - Literal-Body
    Where Body is a list of literals that expresses the conjunction.




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

:-set_prolog_flag(multi_arity_warnings,off).

:- use_module(library(lists)).
:- use_module(ciao_auxiliar).
:- use_module(common).
%:- use_module(debug).
:- use_module(program).
:- use_module(text_dcg).
:- use_module(tokenizer).

%! load_source_files(+Files:list) is det
% Given a list of source files, read, tokenize and parse them, merging their
% output into a single list of statements. Next, call program:assert_program/1
% to process the statements.
%
% @param Files The list of files to load.
load_source_files(Fs) :-
    once(load_source_files(Fs, [], S, 0, Errs)),
    Errs = 0,
    assert_program(S),
    %write_program,
    !.
load_source_files(_) :-
    write(user_error, 'One or more errors occurred while loading input files!\n'),
    !,
    fail.

%! load_source_files(+Files:list, +StmtsIn:list, -StmtsOut:list, +ErrorsIn:int, -ErrorsOut:int) is det
% Given a list of source files, read, tokenize and parse them, merging their
% output into a single list of statements. If a file fails to parse, keep going
% to get any other error messages, failing at the end.
%
% @param Files The list of files to load.
% @param StmtsIn Input list of statements.
% @param StmtsOut The list of statements parsed.
% @param ErrorsIn Input error count.
% @param ErrorsOut Output error count.
load_source_files([X | T], Si, So, Ei, Eo) :-
    absolute_file_name(X, X2),
    write_verbose(0, 'Loading file ~w...\n', [X2]),
    once(input(X2, CharPairs)),
    once(tokenize(CharPairs, Toks)),
    %writef('got tokens: ~w\n\n', [Toks]),
    once(parse_program(Toks, S, D, E)),
    E2 is Ei + E,
    %writef('got statements ~w\n', [S]),
    %writef('got directives ~w\n', [D]),
    append(Si, S, S2),
    once(process_directives(D, X2, S2, S3, T, T2)),
    %writef('loaded file ~w\n', [X]),
    !,
    load_source_files(T2, S3, So, E2, Eo).
load_source_files([], S, S, E, E) :-
    !.

%! process_directives(+Directives:list, +CurFile:ground, +StmtsIn:list, -StmtsOut:list, +FilesIn:list, -FilesOut:list) is det
% Process directives from a file.
%
% @param Directives The list of directives.
% @param CurFile The current file, for resolving relative file paths in include
%        directives.
% @param StmtsIn Input list of statements.
% @param StmtsOut Output list of statements.
% @param FilesIn Input list of files.
% @param FilesOut Output list of files.
process_directives([include(X) | T], C, Si, So, Fsi, [X2 | Fso]) :-
    catch(absolute_file_name(X, X2,
                             [ relative_to(C),
                               extensions([pl,'']),
                               access(read)
                             ]),
          E, (print_message(error, E), fail)),
    !, % include directive
    process_directives(T, C, Si, So, Fsi, Fso).
:- dynamic (asp_table)/1, show/1, (pred)/1.
process_directives([table(X) | T], C, Si, So, Fsi, Fso) :-
    assertz(asp_table(X)),
    !, % include directive
    process_directives(T, C, Si, So, Fsi, Fso).
process_directives([show(X) | T], C, Si, So, Fsi, Fso) :-
    assertz(show(X)),
    !, % include directive
    process_directives(T, C, Si, So, Fsi, Fso).
process_directives([pred(X) | T], C, Si, So, Fsi, Fso) :-
    assertz(pred(X)),
    !, % include directive
    process_directives(T, C, Si, So, Fsi, Fso).
process_directives([abducible(X) | T], C, Si, So, Fsi, Fso) :-
    X =.. [F | A],
    X2 = abducible_1(X),
    X2 =.. [F2 | A2],
    atom_chars(F, Fc),
    atom_chars(Fn, ['_' | Fc]), % user predicates with an underscore will have a dummy prefix, so this is guaranteed to be unused.
    Xn =.. [Fn | A],
    atom_chars(F2, Fc2),
    atom_chars(Fn2, ['_' | Fc2]), % user predicates with an underscore will have a dummy prefix, so this is guaranteed to be unused.
    Xn2 =.. [Fn2 | A2],
    c_rule(R1, X, [not(Xn), X2]), % set abducible(X) true iff X succeeds via this rule.
    c_rule(R2, Xn, [not(X)]), % A simple even loop
    c_rule(R3, X2, [not(Xn2)]), % rule to allow abducible(X) to be true or false.
    c_rule(R4, Xn2, [not(X2)]), % A simple even loop
    append(Si, [R1, R2, R3, R4], S2), % add to statements
    !, % abducible directive
    process_directives(T, C, S2, So, Fsi, Fso).
process_directives([c(X, Y) | T], C, Si, So, Fsi, Fso) :-
    append(Si, [c(X, Y)], S2), % Compute directive. Treat as a statement.
    !, % include directive
    process_directives(T, C, S2, So, Fsi, Fso).
process_directives([X | _], _, _, _, _, _) :-
    write_error('Could not process directive: ~w\n', [X]),
    !,
    fail.
process_directives([], _, S, S, F, F) :-
    !.

%! input(?Source:filepath, -CharPairs:list)
% Read the entire program into a list, then store each character with its
% position. Ensure that input file is closed properly even if reading fails.
% Wrapper for input2/3.
%
% @param Source Input file, if given. If no input file is supplied (Source is
%        unbound), an error message will be printed and the call will fail.
% @param CharPairs List of character-position pairs.
input(Source, CharPairs) :-
    write_verbose(1, 'Reading file...\n'),
    once(open_input(Source, Sread)),
    input2(Sread, Source, CharPairs),
    !.

%! input2(+Stream:stream, +Source:filepath, -CharPairs:list)
% Read the entire file into a list, then store each character with its position.
% Ensure that input file is closed properly even if reading fails.
%
% @param Stream Input stream from input/2.
% @param Source Input source. Stored with token position info.
% @param CharPairs List of character-position pairs.
input2(current_input, Source, CharPairs) :-
    read_file(current_input, Chars),
    add_positions(Chars, Source, CharPairs),
    !.
input2(Sread, Source, CharPairs) :-
    Sread \= current_input,
    read_file(Sread, Chars),
    add_positions(Chars, Source, CharPairs),
    close(Sread),
    !.
input2(Sread, _, _) :- % close the file even if tokenizer fails
    Sread \= current_input,
    close(Sread),
    !,
    fail.

%! open_input(+File:filepath, -Stream:stream)
% If reading from a file, open it and return the stream. Otherwise, return the
% current input stream.
%
% @param File Input file path. Will print an error and fail if unbound.
% @param Stream Stream to use for input.
open_input(File, Stream) :-
    nonvar(File),
    % prolog_to_os_filename(File2, File),
    % access_file(File2, read),
    % open_file(File2, Stream, read),
    (
        open(File,read,Stream)->
        !
    ;
        write_error('file \'~w\' does not exist or cannot be open for reading', [File]),
        !,
        fail
    ).
open_input(File, current_input) :-
    var(File),
    !.
% open_input(File, _) :-
%         nonvar(File),
%         \+exists_file(File),
%         write_error('file \'~w\' does not exist', [File]),
%         !,
%         fail.
% open_input(File, _) :-
%         nonvar(File),
%         \+access_file(File, read),
%         write_error('cannot open file \'~w\' for reading', [File]),
%         !,
%         fail.
open_input(_, _) :-
    write(user_error, 'One or more errors occured while accessing input!\n'),
    !,
    fail.

%! open_file(+File:filepath, -Stream:stream, +Mode:atom)
% Open a file for return a stream for it. This predicate just ensures the
% =eof_action= option is used.
%
% @param File The path of the file to open.
% @param Stream The stream returned.
% @param Mode One of: =read=, =write=, =append= or =update=.
open_file(File, Stream, Mode) :-
    open(File, Mode, Stream, [eof_action(eof_code)]),
    !.

%! read_file(+Input:stream, -Chars:list)
% Read the stream Input into a list of characters.
%
% @param Input Input stream.
% @param Chars The list of characters read from the file.
read_file(Input, Chars) :-
    write_verbose(1, 'Reading input...\n'),
    get_char(Input, Firstchar),
    read_file2(Input, Firstchar, Chars).

%! read_file2(+Input:stream, +FirstChar:char, -Chars:list)
% Read the entire file into a list of characters.
%
% @param Input Input stream.
% @param FirstChar The previous character read from the file.
% @param Chars The list of characters read from the file.
read_file2(_, Char, []) :-
    Char = end_of_file,
    !.
read_file2(Input, Char, [Char | Chars]) :-
    catch(get_char(Input, Char2),_,Chars=[]),
    read_file2(Input, Char2, Chars).

%! read_query(+Input:stream, -CharsOut:list) is det
% Read a user-entered query and add the position info expected by the tokenizer.
%
% @param Input Input stream.
% @param CharsOut The list of characters read from the file.
read_query(Input, Chars) :-
    read_query2(Input, Chars1),
    add_positions(Chars1, Input, Chars).

%! read_query2(+Input:stream, -CharsOut:list)
% Read a user-entered query. Basically, read lines of characters until the last
% non-whitespace character is a period.
%
% @param Input Input stream.
% @param CharsOut The list of characters read from the file.
read_query2(Input, Chars) :-
    read_query3(Input, NWS, Chars1),
    (NWS = '.' -> % period, we're done
            Chars = Chars1
    ; % else keep going
            write('   '), % indent line for input
            read_query2(Input, Chars2),
            append(Chars1, Chars2, Chars)
    ).

%! read_query3(+Input:stream, -LastNWS:char, -Chars:list)
% Read the stream Input until a newline or EOF is encountered.
%
% @param Input Input stream.
% @param LastNWS Last non-whitespace character read.
% @param Chars The list of characters read from the file.
read_query3(Input, NWSo, Chars) :-
    write_verbose(1, 'Reading input...\n'),
    get_char(Input, Firstchar),
    (char_type(Firstchar, space) -> % first char is whitespace
            NWSi = '0' % use a dummy char, so long as it isn't a period.
    ;
            NWSi = Firstchar
    ),
    read_query4(Input, NWSi, NWSo, Firstchar, Chars).

%! read_query4(+Input:stream, +LastNWSin:char, -LastNWSout:char, +FirstChar:char, -Chars:list)
% Read a line into a list of characters. Store the last non-whitespace
% character.
%
% @param Input Input stream.
% @param LastNWSin Input last non-whitespace character read.
% @param LastNWSout Output last non-whitespace character read.
% @param FirstChar The previous character read from the file.
% @param Chars The list of characters read from the file.
read_query4(_, NWS, NWS, Char, []) :-
    Char = end_of_file,
    !.
read_query4(_, NWS, NWS, Char, [Char]) :-
    char_type(Char, newline),
    !.
read_query4(Input, NWSi, NWSo, Char, [Char | Chars]) :-
    char_type(Char, space), % whitespace
    !,
    get_char(Input, Char2),
    read_query4(Input, NWSi, NWSo, Char2, Chars).
read_query4(Input, _, NWSo, Char, [Char | Chars]) :-
    !, % Char is non-whitespace
    get_char(Input, Char2),
    read_query4(Input, Char, NWSo, Char2, Chars).

%! add_positions(+CharsIn:list, +Source:filepath, -CharsOut:list)
% Store line and character position with each character. Remove whitespace while
% we're at it.
%
% @param CharsIn List of characters from the input file.
% @param Source Input source. Stored with token position info.
% @param CharsOut List of character-position pairs, with whitepace removed.
add_positions(Cin, Source, Cout) :-
    write_verbose(1, 'Storing character positions...\n'),
    add_positions2(Cin, Cout, Source, 1, 1).

%! add_positions2(+CharsIn:list, -CharsOut:list, +Source:filepath, +Line:int, +Col:int)
% Add position information to characters.
%
% @param CharsIn List of characters from the input file.
% @param CharsOut List of character-position pairs, with whitepace removed.
% @param Source Input source. Stored with token position info.
% @param Line The current line in the input file, determined by counting
%        newlines.
% @param Col The character position on the current row of the input file.
add_positions2([C | T], [C2 | T2], Source, Line, Col) :-
    C = '\n',
    !,
    C2 = (C, (Source, Line, Col)),
    Line2 is Line + 1,
    add_positions2(T, T2, Source, Line2, 1).
add_positions2([C | T], [C2 | T2], Source, Line, Col) :-
    C2 = (C, (Source, Line, Col)),
    Col2 is Col + 1,
    add_positions2(T, T2, Source, Line, Col2).
add_positions2([], [], _, _, _).




