:- module(sscasp_source,
          [ read_program/2,             % +File, -Source

            op(1200, fx,  #),
            op(950,  xfx, ::),
            op(350,  fx,  [include, compute, abducible]),
            op(1150, fx,  [table, show])
          ]).
:- use_module(ops).
:- use_module(library(lists), [append/3, member/2]).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(error), [existence_error/2]).

/** <module> Reading an s(CASP) source file

A program written for the `scasp` executable, read into the value
`compile.pl` takes:

    source{ terms:     [...],   % clauses, constraints and annotations
            query:     Goal,    % the last `?- ...` in the file, or `none`
            templates: [...],   % `#pred p(X) :: '...'` — for a renderer
            shown:     [...],   % `#show p/1`
            abducible: [...],   % `#abducible p(X)`
            files:     [...] }  % what was read, includes last

## The legacy operators live here

`#` is a prefix operator at 1200 and `pred`, `show`, `include` and the rest
are prefix operators under it, so `#pred p(X) :: 'text'.` reads as
`#(pred(::(p(X), 'text')))`.  Those operators are declared in **this module**
and the reader is pointed at it, rather than being added to `ops.pl`: they are
the syntax of files that already exist, not of the language going forward.

They are exported, so a caller that wants to read what came back — a
renderer looking at `templates`, or a test — can write it down; but nothing
gets them by asking for `ops.pl`.

## `5/2` is a number

A term `Integer/Integer` is read as a **rational**, so `holdsAt(level(5/2), T)`
asks about the level 5r2 and not about a compound that happens to look like
one.  s(CASP) programs write rationals that way — `X #> 1/3 + 1/4` — and
inside `{}` CLP(Q) evaluates them, but as a plain argument nothing would, and
the number would never meet the constraint store as a number.

The old implementation does the same, in `variables.pl:special_atom/2`, while
walking the terms it reads.  Doing it here keeps it in the one place that
turns text into terms.

`p/1` is left alone, since `p` is not an integer, so `#show p/1` and the
predicate indicators in a signature are unaffected.

Keeping them apart is what lets `pred` mean two things without ambiguity.
`#pred p(X) :: 'text'` is a natural-language template and arrives wrapped in
`#/1`; `:- pred p(day)` is an argument-domain signature and arrives wrapped in
`:-/1`.  Decision 6a proposes renaming the first to `#describe`, which would
retire the clash rather than manage it; until then the wrapper tells them
apart.
*/

:- op(1200, fx,  #).
:- op(950,  xfx, ::).
:- op(350,  fx,  [include, compute, abducible]).
:- op(1150, fx,  [table, show]).

%!  read_program(+File, -Source) is det.
%
%   Read File and everything it includes.

read_program(File, Source) :-
    read_terms(File, [], Read0, Files, []),
    maplist(rationals, Read0, Read),
    partition_terms(Read, Source0),
    Source = Source0.put(files, Files).

%!  rationals(+Term, -Rationalised) is det.
%
%   Every `Integer/Integer` in Term becomes the rational it denotes.

rationals(Var, Var) :-
    var(Var),
    !.
rationals(A/B, Rational) :-
    integer(A),
    integer(B),
    B =\= 0,
    !,
    Rational is A rdiv B.
rationals(Term, Rationalised) :-
    compound(Term),
    !,
    Term =.. [Name|Args],
    maplist(rationals, Args, Rationalised0),
    Rationalised =.. [Name|Rationalised0].
rationals(Term, Term).

%!  read_terms(+File, +Seen, -Terms, -Files, ?FilesTail) is det.
%
%   The terms of File with those of its includes spliced in where the
%   `#include` was, which is where the author put them.  A file already
%   read is not read again: `bec_theory.incl` is included by several
%   programs and reading it twice would duplicate every clause.

read_terms(File, Seen, Terms, Files, FilesTail) :-
    absolute_file_name(File, Path, [access(read), file_type(prolog)]),
    (   memberchk(Path, Seen)
    ->  Terms = [], Files = FilesTail
    ;   Files = [Path|MoreFiles],
        setup_call_cleanup(
            open(Path, read, Stream),
            read_stream(Stream, Path, [Path|Seen], Terms, MoreFiles, FilesTail),
            close(Stream))
    ).

read_stream(Stream, Path, Seen, Terms, Files, FilesTail) :-
    read_term(Stream, Term, [module(sscasp_source)]),
    (   Term == end_of_file
    ->  Terms = [], Files = FilesTail
    ;   include_directive(Term, Included)
    ->  file_directory_name(Path, Dir),
        atomic_list_concat([Dir, /, Included], Include),
        read_terms(Include, Seen, From, Files, MoreFiles),
        append(From, Rest, Terms),
        read_stream(Stream, Path, Seen, Rest, MoreFiles, FilesTail)
    ;   Terms = [Term|Rest],
        read_stream(Stream, Path, Seen, Rest, Files, FilesTail)
    ).

include_directive(#(include(File)), File).

%!  partition_terms(+Read, -Source) is det.
%
%   Sort what was read into the program and the things that are about
%   the program.  The **last** query wins, which is what the `scasp`
%   executable answers.

partition_terms(Read, Source) :-
    partition_terms(Read, Terms, Templates, Shown, Abducible, none, Query),
    Source = source{ terms:     Terms,
                     query:     Query,
                     templates: Templates,
                     shown:     Shown,
                     abducible: Abducible }.

partition_terms([], [], [], [], [], Query, Query).
partition_terms([Term|Read], Terms, Templates, Shown, Abducible, Q0, Query) :-
    (   Term = (?-(Goal))
    ->  Terms = Terms1, Templates = Templates1, Shown = Shown1,
        Abducible = Abducible1, Q1 = Goal
    ;   Term = #(pred(Template))
    ->  Templates = [Template|Templates1], Terms = Terms1, Shown = Shown1,
        Abducible = Abducible1, Q1 = Q0
    ;   Term = #(show(What))
    ->  Shown = [What|Shown1], Terms = Terms1, Templates = Templates1,
        Abducible = Abducible1, Q1 = Q0
    ;   Term = #(abducible(What))
    ->  Abducible = [What|Abducible1], Terms = [(:- abducible(What))|Terms1],
        Templates = Templates1, Shown = Shown1, Q1 = Q0
    ;   Term = #(Other)
    ->  existence_error(scasp_directive, Other)
    ;   Terms = [Term|Terms1], Templates = Templates1, Shown = Shown1,
        Abducible = Abducible1, Q1 = Q0
    ),
    partition_terms(Read, Terms1, Templates1, Shown1, Abducible1, Q1, Query).
