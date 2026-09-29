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

:- module(sscasp_mangle,
          [ mangle/2,                   % +Spec, -Mangled
            demangle/2,                 % +Mangled, -Spec
            mangled/1,                  % @Name
            literal_kind/3,             % @Literal, -Kind, -Head
            kind_literal/3,             % ?Kind, ?Head, ?Literal
            kind_truth/2                % ?Kind, ?Truth
          ]).
:- use_module(library(error), [permission_error/3, type_error/2, must_be/2]).
:- use_module(library(apply), [maplist/3]).

/** <module> Functor name mangling

The compiled program lands in **one** module and may draw clauses from
several, so the functor has to say which one a predicate came from.  The
solver works on functor names rather than `M:Term` because carrying the
qualifier was measured as the bottleneck, and that does not change by
compiling to predicates: a single module still needs `m1:p/1` and `m2:p/1` to
be different predicates.

**Always four fields:**

    Module $ Kind $ Name $ Arity

```
m$plain$flies$1            the user predicate flies/1
m$dual$flies$1             not flies
m$dual.2$flies$1           the dual of its second clause
m$dual.2.inner$flies$1     that clause's innermost dual, body variables added
m$neg$cold$1               -cold/1
m$dualneg$cold$1           not -cold
m$nmr$$0                   the NMR check
m$dualchk.1$$0             its first sub-check
```

`Name` is **the user predicate the thing derives from**, which is what makes a
mangled name worth reading in a message, and `Arity` is that predicate's
arity.  Note that this is deliberately *not* the generated predicate's own
arity — the innermost dual above has arity 2, not 1 — so anything wanting the
real arity should ask `functor/3` rather than take it from the name.

`Name` is **empty** when the predicate derives from no user predicate, which is
the case for the program-level ones: the NMR check and its sub-checks.  A
predicate must have a name, so an empty field is unambiguous, and for those
`Arity` is the generated predicate's own, which is 0: a sub-check's variables
are body-only variables its dual quantifies (`nmr.pl`).

One field count rather than two, so demangling is a single clause.

## Why this is unambiguous

`$` is **reserved**: a module or predicate name containing one is refused,
here and at the point a declaration is read.  `atomic_list_concat/3` splits on
every occurrence, so a name that slipped through makes `demangle/2` *fail*
rather than decode to something else.  A detectable failure mode is the whole
point; the scheme this replaces glued fields with `_` and needed an escape
prefix (`o_`) because it could not tell a prefix from a name beginning with
one, nor an arity from a name ending in `_1`.

The `Kind` field is the exception that costs nothing: it is **entirely
generated** and never holds user data, so it can carry sub-structure on a
separator of its own without that separator needing to be reserved.  `dual(2)`
is written `dual.2`, `dual(2,inner)` is `dual.2.inner`.
*/

%!  mangle(+Spec, -Mangled) is det.
%
%   Spec is `pred(Module, Kind, Name, Arity)` for a predicate deriving
%   from a user predicate, or `prog(Module, Kind, Arity)` for one that
%   does not.  Kind is an atom or a compound whose arguments are atoms or
%   integers.

:- det(mangle/2).

mangle(pred(Module, Kind, Name, Arity), Mangled) =>
    (   Name == ''
    ->  type_error(predicate_name, Name)
    ;   true
    ),
    mangle_fields(Module, Kind, Name, Arity, Mangled).
mangle(prog(Module, Kind, Arity), Mangled) =>
    mangle_fields(Module, Kind, '', Arity, Mangled).
mangle(Spec, _) =>
    type_error(mangle_spec, Spec).

mangle_fields(Module, Kind, Name, Arity, Mangled) :-
    reserved_free(Module),
    reserved_free(Name),
    must_be(nonneg, Arity),
    kind_atom(Kind, KindAtom),
    atomic_list_concat([Module, KindAtom, Name, Arity], $, Mangled).

%!  demangle(+Mangled, -Spec) is semidet.
%
%   The inverse.  Fails for a name that is not mangled, and for one whose
%   fields do not come back — which is what a stray `$` in a user name
%   would look like, and why it is better to fail than to guess.

demangle(Mangled, Spec) :-
    atom(Mangled),
    atomic_list_concat(Fields, $, Mangled),
    demangle_fields(Fields, Spec).

demangle_fields([Module, KindAtom, Name, ArityAtom], Spec) :-
    atom_number(ArityAtom, Arity),
    integer(Arity),
    kind_atom(Kind, KindAtom),
    (   Name == ''
    ->  Spec = prog(Module, Kind, Arity)
    ;   Spec = pred(Module, Kind, Name, Arity)
    ).

%!  mangled(@Name) is semidet.
%
%   True when Name is a mangled functor.  For deciding whether to report a
%   name as it stands or to demangle it first.

mangled(Name) :-
    demangle(Name, _).

		 /*******************************
		 *            KINDS		*
		 *******************************/

%!  kind_atom(+Kind, -Atom) is det.
%!  kind_atom(-Kind, +Atom) is det.
%
%   `dual` is `dual`, `dual(2)` is `dual.2`, `dual(2,inner)` is
%   `dual.2.inner`.  A component that reads as an integer comes back as
%   one, which is safe because the field never holds user data: the
%   components a compiler writes there are clause indices and tags.

kind_atom(Kind, Atom), var(Kind) =>
    atomic_list_concat(Parts, '.', Atom),
    maplist(kind_component, Parts, Components),
    (   Components = [Single]
    ->  Kind = Single
    ;   Components = [Functor|Args],
        Kind =.. [Functor|Args]
    ).
kind_atom(Kind, Atom), atom(Kind) =>
    Atom = Kind.
kind_atom(Kind, Atom), compound(Kind) =>
    Kind =.. Parts,
    atomic_list_concat(Parts, '.', Atom).
kind_atom(Kind, _) =>
    type_error(mangle_kind, Kind).

kind_component(Part, Component) :-
    (   atom_number(Part, Number),
        integer(Number)
    ->  Component = Number
    ;   Component = Part
    ).

%!  reserved_free(+Name) is det.
%
%   `$` separates the fields, so no field may contain one.

reserved_free(Name) :-
    must_be(atom, Name),
    (   sub_atom(Name, _, _, _, $)
    ->  permission_error(use, reserved_character($), Name)
    ;   true
    ).


		 /*******************************
		 *        THE FOUR KINDS	*
		 *******************************/

%!  literal_kind(@Literal, -Kind, -Head) is det.
%
%   Which of the four a literal is, in this module's vocabulary, so that
%   the literal a renderer holds and the predicate that proved it are
%   named alike.
%
%   @arg Kind is `plain`, `dual`, `neg` or `dualneg`.
%   @arg Head is Literal without its `not` and `-`.

:- det(literal_kind/3).

literal_kind(not(-(Head0)), Kind, Head) => Kind = dualneg, Head = Head0.
literal_kind(not(Head0),    Kind, Head) => Kind = dual,    Head = Head0.
literal_kind(-(Head0),      Kind, Head) => Kind = neg,     Head = Head0.
literal_kind(Head0,         Kind, Head) => Kind = plain,   Head = Head0.

%!  kind_literal(?Kind, ?Head, ?Literal) is semidet.
%
%   The other direction: the literal of a Kind about Head.  Fails for a
%   kind that is not one of the four — an abducible's halves, the
%   helpers — which have no literal of their own.

kind_literal(plain,   Head, Head).
kind_literal(neg,     Head, -(Head)).
kind_literal(dual,    Head, not(Head)).
kind_literal(dualneg, Head, not(-(Head))).

%!  kind_truth(?Kind, ?Truth) is nondet.
%
%   What a literal of Kind claims: `true`, `false` (known not to hold),
%   `unlikely` (no evidence that it holds) or `likely` (no evidence that
%   it does not).  The renderers colour by it and the JSON reports it.

kind_truth(plain,   true).
kind_truth(neg,     false).
kind_truth(dual,    unlikely).
kind_truth(dualneg, likely).
