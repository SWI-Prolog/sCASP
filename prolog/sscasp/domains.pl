:- module(sscasp_domains,
          [ annotation/2,               % +Directive, -Annotation
            domain_clauses/2,           % +Annotation, -Clauses
            domain_elements/2,          % +Kind, -Elements
            domain_goal/3,              % +Name, ?Arg, -Goal

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

`:- domain day = mon ; tue ; wed.` parses, and parses **wrongly**: `domain` is
a prefix operator at 1150 and `;` is 1100, so the term is
`;(domain(day=mon), ;(tue,wed))` -- not a `domain/1` term at all, and nothing
would notice.  A list has no such reading.
*/

%!  annotation(+Directive, -Annotation) is semidet.
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

annotation(Var, _) :-
    var(Var),
    !,
    fail.
annotation(domain(Spec), Annotation) :-
    !,
    domain_spec(Spec, Annotation).
annotation(pred(Head), Annotation) :-
    !,
    signature(Head, Annotation).

%!  domain_spec(+Spec, -Annotation) is det.

domain_spec(Spec, _) :-
    \+ Spec = (_ = _),
    !,
    domain_error(domain_definition, Spec).
domain_spec(Name = Body, domain(Name, Kind)) :-
    domain_name(Name),
    domain_kind(Body, Name, Kind).

domain_kind(Low..High, Name, range(Low, High)) :-
    !,
    (   integer(Low), integer(High)
    ->  true
    ;   type_error(integer_range, Name = Low..High)
    ),
    (   Low =< High
    ->  true
    ;   domain_error(non_empty_range, Name = Low..High)
    ).
domain_kind(List, Name, enum(List)) :-
    is_list(List),
    !,
    (   List == []
    ->  domain_error(non_empty_domain, Name)
    ;   true
    ),
    maplist(ground_element(Name), List, _).
domain_kind(Body, Name, _) :-
    domain_error(domain_body, Name = Body).

ground_element(Name, Element, Element) :-
    (   ground(Element)
    ->  true
    ;   type_error(ground_domain_element, Name = Element)
    ).

%!  signature(+Head, -Annotation) is det.
%
%   `:- pred meeting(day, hour).` binds each argument to a domain.  `_`
%   leaves an argument unconstrained, so a signature need not name a
%   domain for every position.

signature(Head, _) :-
    \+ compound(Head),
    !,
    type_error(predicate_signature, Head).
signature(Head, signature(Name/Arity, Domains)) :-
    functor(Head, Name, Arity),
    domain_name(Name),
    Head =.. [_|Args],
    maplist(signature_argument(Name/Arity), Args, Domains).

signature_argument(_, Arg, any) :-
    var(Arg),
    !.
signature_argument(PI, Arg, _) :-
    \+ atom(Arg),
    !,
    type_error(domain_name, PI-Arg).
signature_argument(_, Arg, Arg) :-
    domain_name(Arg).

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

domain_clauses(domain(Name, enum(Elements)), Clauses) :-
    !,
    maplist(domain_fact(Name), Elements, Clauses).
domain_clauses(domain(Name, range(Low, High)), [Clause]) :-
    !,
    Head =.. [Name, X],
    Clause = (Head :- X #>= Low, X #=< High).
domain_clauses(signature(_, _), []).

domain_fact(Name, Element, Fact) :-
    Fact =.. [Name, Element].

%!  domain_elements(+Kind, -Elements) is semidet.
%
%   The elements of a finite domain, for the case where `forall` is to be
%   expanded into a conjunction.  Both kinds are finite; a range is
%   listed only when asked, since the point of writing `0..23` rather
%   than twenty-four facts is to *avoid* enumerating it.

domain_elements(enum(Elements), Elements).
domain_elements(range(Low, High), Elements) :-
    numlist(Low, High, Elements).

%!  domain_goal(+Name, ?Arg, -Goal) is det.
%
%   The goal that guards Arg with the domain.  This is what the compiler
%   puts into a dual so that the quantifier over Arg has something finite
%   to range over.

domain_goal(Name, Arg, Goal) :-
    Goal =.. [Name, Arg].
