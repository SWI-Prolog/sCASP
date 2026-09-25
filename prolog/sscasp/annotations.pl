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

:- module(sscasp_annotations,
          [ program_annotations/2,      % +Terms, -Annotations
            annotation/2,               % +Directive, -Annotation
            argument_domain/4,          % +Annotations, +PI, +ArgNo, -Domain
            domain_kind/3,              % +Annotations, +Name, -Kind
            description/3,              % +Annotations, +Literal, -Description
            described/3,                % +Annotations, +Literal, -Text
            described_actions/3,        % +Annotations, +Literal, -Actions
            annotation_clauses/2        % +Annotations, -Clauses
          ]).
:- use_module(ops).
:- use_module(domains,  [domain_clauses/2]).
:- use_module(domains,  [annotation/2 as domain_annotation]).
:- use_module(describe, [annotation/2 as description_annotation,
                         description_actions/3,
                         description_text/3]).
:- use_module(library(error), [existence_error/2, permission_error/3]).
:- use_module(library(lists), [member/2, nth1/3, append/3]).
:- use_module(library(apply), [foldl/4, maplist/3]).
:- use_module(library(pairs), [pairs_keys/2]).

/** <module> The annotation table

Collects the domain annotations of a program, checks them against each other
and against the program, and answers the question the dual generator needs:
*what is the domain of argument N of this predicate?*

The table is a **value**, not a store.  `program_annotations/2` takes the
program's terms and returns a dict; nothing is asserted.  That is deliberate:
the compiler this replaces keeps its program in thread-local dynamic
predicates with an unconditional `destroy_program/0`, so a nested compile
destroys the outer one's state — a live hazard, since a query compiles per
call.  A value has no such failure mode and can be tested without setting up
five dynamic predicates first.

    annotations{ domains:    #{day: enum([mon,tue]), hour: range(0,23)},
                 signatures: #{meeting: [2-[day,hour]]} }

`signatures` is keyed by name with a list of `Arity-Domains`, so `p/1` and
`p/2` can each have one.

## What is checked

A domain is a unary predicate, so there are two ways for one to exist: the
program defines it, or `:- domain` declares it.  Exactly one, never both —

  - a declared domain the program also defines is a redefinition;
  - a domain named in a signature that is neither declared nor defined does
    not exist;
  - two declarations of the same domain, or two signatures for the same
    predicate, are redefinitions;
  - a signature for a predicate the program does not define is an error
    rather than a warning, because the whole program is in hand.
*/

%!  program_annotations(+Terms, -Annotations) is det.
%
%   Annotations is the table for the program given as a list of terms —
%   clauses and directives as read.  Directives that are not annotations
%   are ignored here; `:- Body` is a global constraint and somebody
%   else's business.

program_annotations(Terms, Annotations) :-
    foldl(collect, Terms,
          annotations{domains: #{}, signatures: #{}, descriptions: []},
          Collected),
    program_predicates(Terms, Defined),
    check_annotations(Collected, Defined),
    Annotations = Collected.

collect(Term, In, Out) :-
    (   Term = (:- Directive),
        annotation(Directive, Annotation)
    ->  add_annotation(Annotation, In, Out)
    ;   Out = In
    ).

add_annotation(domain(Name, Kind), In, Out) :-
    (   get_dict(Name, In.domains, _)
    ->  permission_error(redefine, domain, Name)
    ;   put_dict(Name, In.domains, Kind, Domains),
        Out = In.put(domains, Domains)
    ).
add_annotation(description(Kind, PI, Literal, Format), In, Out) :-
    Description = description(Kind, PI, Literal, Format),
    Out = In.put(descriptions, [Description|In.descriptions]).
add_annotation(signature(Name/Arity, Domains), In, Out) :-
    (   get_dict(Name, In.signatures, Arities)
    ->  true
    ;   Arities = []
    ),
    (   memberchk(Arity-_, Arities)
    ->  permission_error(redefine, signature, Name/Arity)
    ;   put_dict(Name, In.signatures, [Arity-Domains|Arities], Signatures),
        Out = In.put(signatures, Signatures)
    ).

%!  annotation(+Directive, -Annotation) is semidet.
%
%   True when Directive is an annotation of any kind — a domain, a
%   signature or a description.  One entry point, because a caller
%   deciding "is this an annotation or a global constraint?" should not
%   have to know how many kinds there are.

annotation(Directive, Annotation) :-
    (   domain_annotation(Directive, Annotation)
    ->  true
    ;   description_annotation(Directive, Annotation)
    ).

%!  description(+Annotations, +Literal, -Description) is nondet.
%
%   A description that applies to Literal, most specific first — a
%   program may describe `flies(tweety)` as well as `flies(X)`, and the
%   one naming the individual says more.  "Most specific" is *later
%   declaration first*, which is the order a reader would expect: a
%   description written after another is the more particular case.

description(Annotations, Literal, Description) :-
    member(Description, Annotations.descriptions),
    Description = description(_, _, Described, _),
    \+ \+ Described = Literal.

%!  described(+Annotations, +Literal, -Text) is semidet.
%
%   Literal said in words, by the first description that fits.  Fails
%   when nothing describes it, which is the ordinary case — a program
%   need not describe anything.

described(Annotations, Literal, Text) :-
    description(Annotations, Literal, Description),
    description_text(Description, Literal, Text),
    !.

%!  described_actions(+Annotations, +Literal, -Actions) is semidet.
%
%   The same, taken apart: a list of `text/1` and `@(Value:Type)`, or
%   `html(Spec)`.  This is what a renderer wants — `html.pl` emits a
%   variable, a constrained variable or "the <type> <value>" where
%   `described/3` would have printed one.

described_actions(Annotations, Literal, Actions) :-
    description(Annotations, Literal, Description),
    description_actions(Description, Literal, Actions),
    !.

%!  program_predicates(+Terms, -Defined) is det.
%
%   The predicate indicators the program defines.  A global constraint
%   defines nothing, and neither does an annotation.

program_predicates(Terms, Defined) :-
    findall(PI, (member(T, Terms), defines(T, PI)), PIs),
    sort(PIs, Defined).

defines((:- _), _) :- !, fail.
defines((Head :- _), PI) :- !, head_pi(Head, PI).
defines(Fact, PI) :- head_pi(Fact, PI).

head_pi(Head, Name/Arity) :-
    callable(Head),
    functor(Head, Name, Arity).

		 /*******************************
		 *         VALIDATION		*
		 *******************************/

check_annotations(A, Defined) :-
    dict_pairs(A.domains, _, DomainPairs),
    pairs_keys(DomainPairs, Declared),
    maplist(check_not_also_defined(Defined), Declared),
    dict_pairs(A.signatures, _, SignaturePairs),
    maplist(check_signature(Declared, Defined), SignaturePairs).

%   A declared domain the program also defines: two definitions of one
%   predicate, and no way to say which was meant.

check_not_also_defined(Defined, Name) :-
    (   memberchk(Name/1, Defined)
    ->  permission_error(redefine, domain, Name)
    ;   true
    ).

check_signature(Declared, Defined, Name-Arities) :-
    maplist(check_signature_arity(Declared, Defined, Name), Arities).

check_signature_arity(Declared, Defined, Name, Arity-Domains) :-
    (   memberchk(Name/Arity, Defined)
    ->  true
    ;   existence_error(procedure, Name/Arity)
    ),
    maplist(check_domain_exists(Declared, Defined), Domains).

check_domain_exists(_, _, any) :- !.
check_domain_exists(Declared, Defined, Name) :-
    (   memberchk(Name, Declared)
    ->  true
    ;   memberchk(Name/1, Defined)
    ->  true
    ;   existence_error(domain, Name)
    ).

		 /*******************************
		 *          QUERYING		*
		 *******************************/

%!  argument_domain(+Annotations, +PI, +ArgNo, -Domain) is semidet.
%
%   Domain is the domain of the ArgNo-th argument of PI.  Fails when the
%   predicate has no signature, or when the position was left `_`, so a
%   caller can read "no domain here" as failure rather than testing for
%   `any`.

argument_domain(Annotations, Name/Arity, ArgNo, Domain) :-
    get_dict(Name, Annotations.signatures, Arities),
    memberchk(Arity-Domains, Arities),
    nth1(ArgNo, Domains, Domain0),
    Domain0 \== any,
    Domain = Domain0.

%!  domain_kind(+Annotations, +Name, -Kind) is semidet.
%
%   Kind is how a *declared* domain was written — `enum(Elements)` or
%   `range(Low,High)`.  Fails for a domain the program defines as an
%   ordinary predicate, which has no declared kind and whose extension is
%   whatever its clauses say.

domain_kind(Annotations, Name, Kind) :-
    get_dict(Name, Annotations.domains, Kind).

%!  annotation_clauses(+Annotations, -Clauses) is det.
%
%   The clauses the declarations bring into being: the facts of each
%   enumeration and the guard of each range.  A signature contributes
%   none — it names domains rather than creating them.

annotation_clauses(Annotations, Clauses) :-
    dict_pairs(Annotations.domains, _, Pairs),
    foldl(domain_pair_clauses, Pairs, [], Clauses).

domain_pair_clauses(Name-Kind, In, Out) :-
    domain_clauses(domain(Name, Kind), Clauses),
    append(In, Clauses, Out).
