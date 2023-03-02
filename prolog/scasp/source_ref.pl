:- module(scasp_source_reference,
          [ scasp_source_reference_file_line/3,   % +Ref, -File, -Line
            assert_scasp_source_reference/3,      % +File, +Pos, -Ref
            scasp_source_reference/3,             % +Ref, -File, -Pos
            scasp_dynamic_clause_position/2       % +Ref, -Pos
          ]).

/** <module> s(CASP) source references

*/

scasp_source_reference_file_line(Ref, File, Line) :-
    blob(Ref, clause),
    !,
    (   clause_file_line(Ref, File, Line)
    ->  true
    ;   File = none, Line = 0
    ).
scasp_source_reference_file_line(Ref, File, Line) :-
    scasp_source_reference(Ref, File, Pos),
    !,
    (   File == (-)
    ->  Line = Pos
    ;   stream_position_data(line_count, Pos, Line)
    ).

clause_file_line(ClauseRef, dynamic, Line) :-
    scasp_dynamic_clause_position(ClauseRef, Pos),
    !,
    stream_position_data(line_count, Pos, Line).
clause_file_line(ClauseRef, File, Line) :-
    clause_property(ClauseRef, file(File)),
    clause_property(ClauseRef, line_count(Line)).

:- dynamic scasp_source_reference/3.

assert_scasp_source_reference(File, Pos, Ref) :-
    scasp_source_reference(Ref, File, Pos), !.
assert_scasp_source_reference(File, Pos, Ref) :-
    (   scasp_source_reference(Ref0, _, _)
    ->  Ref is Ref0 + 1
    ;   Ref is 1
    ),
    asserta(scasp_source_reference(Ref, File, Pos)).


%!  scasp_dynamic_clause_position(+Ref, -Pos) is semidet.
%
%   True when Pos is the stream position is which the source code for
%   the dynamic clause referenced by Ref was read.

:- thread_local scasp_dynamic_clause_position/2.
