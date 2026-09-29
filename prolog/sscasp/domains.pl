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

:- module(sscasp_domains,
          [ annotation/2,               % @Directive, -Annotation
            domain_clauses/2,           % +Annotation, -Clauses
            domain_elements/2,          % +Kind, -Elements
            domain_goal/3,              % +Name, @Arg, -Goal

            op(1150, fx, domain),
            op(1150, fx, pred),
            op(200,  xfx, ..),
            op(700,  xfx, [#=, #<>, #<, #>, #=<, #>=])
          ]).
:- use_module(ops).
:- use_module(library(error), [type_error/2, domain_error/2, permission_error/3,
                               must_be/2]).
:- use_module(library(lists), [numlist/3]).
:- use_module(library(apply), [maplist/3]).

/** <module> Domain annotations

A **domain is a unary predicate**.  That is the whole of the idea, and it is
ASP's: a modern answer set program binds a variable with a domain predicate in
the body -- `p(X) :- dom(X), q(X).` -- and relies on that for a finite
grounding.  `#domain` was lparse and gringo-3 syntax and has been removed.

s(CASP) does not ground, but it needs the same information for a different
reason.  Negating a rule with a body-only variable produces `forall(V, ...)`,
and the quantifier can only be decided when the answers cover V's domain.
Given a finite domain it expands to a conjunction; given none it is one of the
undecidable cases.  So annotations are what move a program from the second
class into the first (`doc/design-next.md` decisions 5 and 6a).

They are **optional**.  A program can always write the guard by hand -- a
domain being an ordinary predicate, `p(X) :- shape(X), ...` needs nothing
declared -- and the declaration only saves repeating it and lets the compiler
put the guard into the *dual*, which is the part a program cannot reach.

## Declaring

A domain that already exists as a predicate needs no declaration; name it in a
signature and it is used:

    :- pred wrap(shape).
    shape(f(a)).  shape(f(b)).  shape(g(a)).  shape(b).

For the two tedious cases there is sugar:

    :- domain day  = [mon, tue, wed, thu, fri, sat, sun].
    :- domain hour = 0..23.

The first expands to seven facts.  The second expands to a CLP(Q) region
rather than twenty-four facts, which is the better answer: `not busy(H)` over
a region comes back as one constrained answer instead of one per value, and it
scales to a range nobody would enumerate.

### Why a list, and not `;`

`:- domain day = mon ; tue ; wed.` parses — `domain` is a prefix operator at
1150 and `;` is 1100, so the term is `domain((day = mon ; tue ; wed))` — but
a disjunction reads as a choice between definitions rather than a set of
values, so domain_spec/2 refuses it (`domain_definition`).  A list has no
such reading.
*/

%!  annotation(@Directive, -Annotation) is semidet.
%
%   True when Directive is a domain annotation.  Fails -- rather than
%   raising -- for any other directive, so a caller can use it as the
%   test in a chain.  Raises only when Directive *is* one of ours and is
%   malformed, since at that point saying nothing would hide a typo.
%
%   Annotation is one of:
%
%     - `domain(Name, Kind)` with Kind `enum(Elements)` or `range(Low,High)`
%     - `signature(Name/Arity, Domains)`, Domains a list of domain names
%
%   @arg Directive is the body of a `:- Directive` term, such as
%       `domain(day = [mon, tue])` or `pred(meeting(day, hour))`.
%   @arg Annotation holds `any` in Domains for a variable argument.

annotation(domain(Spec), Annotation) =>
    domain_spec(Spec, Annotation).
%   Not `pred(_ :: _)`, which is a *description* and `describe.pl`'s.
%   The two share a directive because the old implementation spelled a
%   description that way; here `:- pred` is a signature, and a signature
%   is the one that changes what is proved.

annotation(pred(_ :: _), _) =>
    fail.
annotation(pred(Head), Annotation) =>
    signature(Head, Annotation).
annotation(_, _) =>
    fail.

%!  domain_spec(+Spec, -Annotation) is det.
%
%   @arg Spec is the argument of `:- domain`, `Name = [...]` or
%       `Name = Low..High`.
%   @arg Annotation is `domain(Name, Kind)`, as annotation/2.

:- det(domain_spec/2).

domain_spec(Name = Body, Annotation) =>
    Annotation = domain(Name, Kind),
    domain_name(Name),
    body_kind(Body, Name, Kind).
domain_spec(Spec, _) =>
    domain_error(domain_definition, Spec).

body_kind(Range, Name, Kind), Range = Low..High =>
    Kind = range(Low, High),
    (   integer(Low), integer(High)
    ->  true
    ;   type_error(integer_range, Name = Range)
    ),
    (   Low =< High
    ->  true
    ;   domain_error(non_empty_range, Name = Range)
    ).
body_kind(List, Name, Kind), is_list(List) =>
    Kind = enum(List),
    (   List == []
    ->  domain_error(non_empty_domain, Name)
    ;   true
    ),
    maplist(ground_element(Name), List, _).
body_kind(Body, Name, _) =>
    domain_error(domain_body, Name = Body).

ground_element(Name, Element, Element) :-
    (   ground(Element)
    ->  true
    ;   type_error(ground_domain_element, Name = Element)
    ).

%!  signature(@Head, -Annotation) is det.
%
%   `:- pred meeting(day, hour).` binds each argument to a domain.  `_`
%   leaves an argument unconstrained, so a signature need not name a
%   domain for every position.
%
%   @arg Annotation is `signature(Name/Arity, Domains)`, with `any` in
%       Domains where the argument is a variable.

:- det(signature/2).

signature(Head, Annotation), compound(Head) =>
    Annotation = signature(Name/Arity, Domains),
    functor(Head, Name, Arity),
    domain_name(Name),
    Head =.. [_|Args],
    maplist(signature_argument(Name/Arity), Args, Domains).
signature(Head, _) =>
    type_error(predicate_signature, Head).

signature_argument(_, Arg, Domain), var(Arg) =>
    Domain = any.
signature_argument(_, Arg, Domain), atom(Arg) =>
    domain_name(Arg),
    Domain = Arg.
signature_argument(PI, Arg, _) =>
    type_error(domain_name, PI-Arg).

%!  domain_name(+Name) is det.
%
%   `$` is the separator of the mangling scheme (`doc/design-next.md`
%   decision 2), which is unambiguous only because no name contains one.
%   Rejected here, where the message can still say which declaration.

domain_name(Name) :-
    must_be(atom, Name),
    (   sub_atom(Name, _, _, _, $)
    ->  permission_error(use, reserved_character($), Name)
    ;   true
    ).

		 /*******************************
		 *          EXPANSION		*
		 *******************************/

%!  domain_clauses(+Annotation, -Clauses) is det.
%
%   The clauses defining the domain predicate.  An enumeration becomes
%   facts; a range becomes a single clause carrying a CLP(Q) region,
%   which is what keeps `not p(X)` over it to one constrained answer.
%
%   A signature defines nothing -- it names domains rather than creating
%   them -- so it yields no clauses.

:- det(domain_clauses/2).

domain_clauses(domain(Name, enum(Elements)), Clauses) =>
    maplist(domain_fact(Name), Elements, Clauses).
domain_clauses(domain(Name, range(Low, High)), Clauses) =>
    Head =.. [Name, X],
    Clauses = [(Head :- X #>= Low, X #=< High)].
domain_clauses(signature(_, _), Clauses) =>
    Clauses = [].

domain_fact(Name, Element, Fact) :-
    Fact =.. [Name, Element].

%!  domain_elements(+Kind, -Elements) is semidet.
%
%   The elements of a finite domain, for the case where `forall` is to be
%   expanded into a conjunction.  Both kinds are finite; a range is
%   listed only when asked, since the point of writing `0..23` rather
%   than twenty-four facts is to *avoid* enumerating it.

domain_elements(enum(Elements0), Elements) =>
    Elements = Elements0.
domain_elements(range(Low, High), Elements) =>
    numlist(Low, High, Elements).

%!  domain_goal(+Name, @Arg, -Goal) is det.
%
%   The goal that guards Arg with the domain.  This is what the compiler
%   puts into a dual so that the quantifier over Arg has something finite
%   to range over.

domain_goal(Name, Arg, Goal) :-
    Goal =.. [Name, Arg].
