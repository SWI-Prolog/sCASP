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

:- module(sscasp_source,
          [ read_program/2,             % +FileOrFiles, -Source

            op(1200, fx,  #),
            op(950,  xfx, ::),
            op(1150, fx,  [include, compute, abducible]),
            op(1150, fx,  [table, show])
          ]).
:- use_module(ops).
:- use_module(describe, [deprecated_description/1]).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(error), [existence_error/2, instantiation_error/1]).

/** <module> Reading an s(CASP) source file

A program written for the `scasp` executable, read into the value
`compile.pl` takes:

    source{ terms:     [...],   % clauses, constraints and annotations
            query:     Goal,    % the last `?- ...` in the file, or `none`
            bindings:  [...],   % its variable names, as the reader saw them
            templates: [...],   % `#pred p(X) :: '...'` — for a renderer
            shown:     [...],   % `#show p/1`, also in terms as `:- show(p/1)`
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

## A template is a description

`pred` means two things, and what tells them apart is the `::`, not the
prefix: `#pred p(X) :: 'text'` is a natural-language template, while
`:- pred p(day)` and `#pred p(day)` are an argument-domain signature.  That
is how a Prolog file reads them too (`sscasp.pl:template/3`).

Decision 6a proposed renaming the first to `#describe`, and `:- describe` is
that: one directive for the thing, spelled the way the rest of the language
is spelled.  `#pred p(X) :: 'text'` is still read — it is what the existing
programs say — and becomes `:- describe p(X) :: 'text'`, so the annotation
table has it whichever way the program wrote it.  It is also still reported
in `templates`, for a caller that wants to know what the *file* said.

A description names its variables by name, and a name is the one thing a read
term does not carry.  So the two directives that want one are read with
`variable_names/1` and handed the bindings; see `named_term/3`.
*/

:- op(1200, fx,  #).
:- op(950,  xfx, ::).
:- op(1150, fx,  [include, compute, abducible]).
:- op(1150, fx,  [table, show]).

%!  read_program(+FileOrFiles, -Source) is det.
%
%   Read File and everything it includes, or each of a list of files in
%   turn as one program — which is what the old implementation's
%   `scasp a.pl b.pl` means: the terms in order, and the **last** query.
%   A file included by two of them is read once, as it is when two
%   includes of one file name it.
%
%   @arg Source is the `source{...}` dict described in the module header.

:- det(read_program/2).
read_program(Spec, Source) :-
    (   is_list(Spec)
    ->  Files0 = Spec
    ;   Files0 = [Spec]
    ),
    phrase(read_files(Files0, [], Files, []), Read0),
    maplist(rationals, Read0, Read),
    partition_terms(Read, Source0),
    Source = Source0.put(files, Files).

read_files([], _, Files, Files) -->
    [].
read_files([File|More], Seen0, Files, FilesTail) -->
    read_terms(File, Seen0, Seen, Files, Files1),
    read_files(More, Seen, Files1, FilesTail).

%!  rationals(@Term, -Rationalised) is det.
%
%   Every `Integer/Integer` in Term becomes the rational it denotes.

rationals(Var, Rationalised), var(Var) =>
    Rationalised = Var.
rationals(A/B, Rational), integer(A), integer(B), B =\= 0 =>
    Rational is A rdiv B.
rationals(Term, Rationalised), compound(Term) =>
    Term =.. [Name|Args],
    maplist(rationals, Args, Rationalised0),
    Rationalised =.. [Name|Rationalised0].
rationals(Term, Rationalised) =>
    Rationalised = Term.

%!  read_terms(+File, +Seen0, -Seen, -Files, ?FilesTail)// is det.
%
%   The terms of File with those of its includes spliced in where the
%   `#include` was, which is where the author put them.  A file already
%   read is not read again: `bec_theory.incl` is included by several
%   programs and reading it twice would duplicate every clause.
%
%   That means *any* file read so far, not only the ones still being
%   read: two files that both include a third would otherwise each
%   splice in its clauses.  A file is recorded before it is read, so an
%   include cycle stops too.
%
%   @arg Seen0 is the list of absolute paths of the files read so far,
%       those still being read included; Seen adds the ones read here.
%   @arg Files is the absolute paths of the files read, each once, as a
%       difference list with FilesTail.

%   Written out rather than as a rule: the stream has to be closed
%   whatever happens to the phrase that reads it.

:- det(read_terms//5).
read_terms(File, Seen0, Seen, Files, FilesTail, Terms, Tail) :-
    absolute_file_name(File, Path, [access(read), file_type(prolog)]),
    (   memberchk(Path, Seen0)
    ->  Terms = Tail, Files = FilesTail, Seen = Seen0
    ;   Files = [Path|MoreFiles],
        setup_call_cleanup(
            open(Path, read, Stream),
            phrase(read_stream(Stream, Path, [Path|Seen0], Seen,
                               MoreFiles, FilesTail),
                   Terms, Tail),
            close(Stream))
    ).

read_stream(Stream, Path, Seen0, Seen, Files, FilesTail) -->
    { read_term(Stream, Term0, [ module(sscasp_source),
                                 variable_names(Bindings)
                               ]),
      named_term(Term0, Bindings, Term)
    },
    (   { Term == end_of_file }
    ->  { Files = FilesTail, Seen = Seen0 }
    ;   { include_directive(Term, Included) }
    ->  { file_directory_name(Path, Dir),
          atomic_list_concat([Dir, /, Included], Include)
        },
        read_terms(Include, Seen0, Seen1, Files, MoreFiles),
        read_stream(Stream, Path, Seen1, Seen, MoreFiles, FilesTail)
    ;   [Term],
        read_stream(Stream, Path, Seen0, Seen, Files, FilesTail)
    ).

%!  named_term(@Read, +Bindings, -Term) is det.
%
%   A description says which variable a placeholder means *by name* —
%   `:- describe flies(X) :: "@(X) can fly"` — and a name is the one
%   thing a read term does not carry.  So the two directives that want
%   one are handed the reader's bindings and everything else is left
%   alone.
%
%   @arg Bindings is the reader's `Name = Var` list (variable_names/1).
%   @arg Term is Read, or with Bindings added as a last argument for
%       `:- describe(Spec, Bindings)`, `:- pred(Spec, Bindings)` (a
%       description only), `#(pred(Spec, Bindings))` and
%       `?-(Goal, Bindings)`.

:- det(named_term/3).
named_term(Term, _, Named), var(Term) =>
    Named = Term.
named_term((:- describe Spec), Bindings, Named) =>
    Named = (:- describe(Spec, Bindings)).
named_term((:- pred Spec), Bindings, Named), nonvar(Spec), Spec = (_ :: _) =>
    deprecated_description(Spec),
    Named = (:- pred(Spec, Bindings)).
named_term(#(pred(Spec)), Bindings, Named), nonvar(Spec), Spec = (_ :: _) =>
    Named = #(pred(Spec, Bindings)).

%   The query keeps its variable names too, for the same reason and for
%   a plainer one: an answer is reported as `X = tweety`, and `X` is
%   what the *reader* called it.

named_term((?- Goal), Bindings, Named) =>
    Named = ?-(Goal, Bindings).
named_term(Term, _, Named) =>
    Named = Term.

include_directive(#(include(File)), File).

%!  partition_terms(@Read, -Source) is det.
%
%   Sort what was read into the program and the things that are about
%   the program.  The **last** query wins, which is what the `scasp`
%   executable answers.
%
%   @arg Read is the list of terms of all files, as named_term/3 left
%       them.
%   @arg Source is the `source{...}` dict of the module header, without
%       the `files` key.

:- det(partition_terms/2).
partition_terms(Read, Source) :-
    partition_terms(Read, Terms, Templates, Shown, Abducible,
                    none-[], Query-Bindings),
    Source = source{ terms:     Terms,
                     query:     Query,
                     bindings:  Bindings,
                     templates: Templates,
                     shown:     Shown,
                     abducible: Abducible }.

partition_terms([], [], [], [], [], Query, Query).
partition_terms([Term|Read], Terms, Templates, Shown, Abducible, Q0, Query) :-
    (   var(Term)                       % `X.` would unify with the query
    ->  instantiation_error(Term)
    ;   Term = ?-(Goal, Named)
    ->  Terms = Terms1, Templates = Templates1, Shown = Shown1,
        Abducible = Abducible1, Q1 = Goal-Named
    ;   Term = (?-(Goal))
    ->  Terms = Terms1, Templates = Templates1, Shown = Shown1,
        Abducible = Abducible1, Q1 = Goal-[]
    ;   Term = #(pred(Template, Bindings))
    ->  Templates = [Template|Templates1],
        Terms = [(:- describe(Template, Bindings))|Terms1], Shown = Shown1,
        Abducible = Abducible1, Q1 = Q0
    ;   Term = #(pred(Template)),
        Template = (_ :: _)
    ->  Templates = [Template|Templates1],
        Terms = [(:- describe(Template, []))|Terms1], Shown = Shown1,
        Abducible = Abducible1, Q1 = Q0
    ;   Term = #(pred(Signature))       % a signature, as :- pred is
    ->  Terms = [(:- pred(Signature))|Terms1], Templates = Templates1,
        Shown = Shown1, Abducible = Abducible1, Q1 = Q0
    ;   Term = #(show(What))
    ->  Shown = [What|Shown1], Terms = [(:- show(What))|Terms1],
        Templates = Templates1, Abducible = Abducible1, Q1 = Q0
    ;   Term = #(abducible(What))
    ->  Abducible = [What|Abducible1], Terms = [(:- abducible(What))|Terms1],
        Templates = Templates1, Shown = Shown1, Q1 = Q0
    ;   Term = #(Other)
    ->  existence_error(scasp_directive, Other)
    ;   Terms = [Term|Terms1], Templates = Templates1, Shown = Shown1,
        Abducible = Abducible1, Q1 = Q0
    ),
    partition_terms(Read, Terms1, Templates1, Shown1, Abducible1, Q1, Query).
