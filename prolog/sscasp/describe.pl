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

:- module(sscasp_describe,
          [ annotation/2,               % @Directive, -Annotation
            deprecated_description/1,   % @Specification
            description_actions/3,      % @Description, @Literal, -Actions
            description_text/3          % @Description, @Literal, -Text
          ]).
:- use_module(mangle, [literal_kind/3]).
:- use_module(ops).
:- use_module(library(lists), [member/2, append/3]).
:- use_module(library(apply), [maplist/3, foldl/5]).
:- use_module(library(error), [domain_error/2, existence_error/2]).
:- use_module(library(dcg/basics), [string//1]).

/** <module> Saying a literal in words

    :- describe flies(X) :: "@(X) can fly".
    :- describe not flies(X) :: "@(X) cannot fly".
    :- describe -flies(X) :: "@(X) is known not to fly".

A description is for a *reader*: it says how to put a literal into a sentence,
so that a model or a justification can be shown to someone who did not write
the program.  It has no effect on what is proved.

## The template

A string or atom with **placeholders** naming the variables of the literal:

| `@(X)` | the value of X |
| `{{X}}` | the same, for a template that has to live inside something using `@` |
| `@(X:Type)`, `{{X:Type}}` | the value, with a type for a renderer that wants one |

The name is the variable's own name in the directive, which is why this is a
directive and not a term: `prolog_load_context/2` and `read_term/3` know what
the reader called it, and nothing downstream does.

A program given as a **term list** has no reader to ask, so there the variable
says its own name: `describe(flies('$VAR'('X')) :: "@(X) can fly")`.  That is
what a test writes, and it is the only place it should be needed.

## Singletons

A variable of a description occurs once in the directive and the rest of its
occurrences are inside a string, so the Prolog reader calls it a singleton and
says so.  Underscore it:

    :- describe flies(_X) :: "@(X) can fly".

The leading underscore is ignored when a placeholder is resolved, so `@(X)`
and `@(_X)` both find it.  (A program read by `source.pl` — an s(CASP) file
rather than a Prolog one — is not checked for singletons and needs none of
this.)

## `:- pred` for this is deprecated

`:- pred p(X) :: "..."` is the old implementation's spelling and still works,
with a warning.  It is deprecated because in *this* implementation `:- pred`
already means something else and something useful — `:- pred wrap(shape)` is
a **signature**, saying which domain an argument ranges over, which is what
lets the dual generator bound a quantified variable.  One directive cannot
usefully be both, and the signature is the one that changes what is proved.
*/

%!  annotation(@Directive, -Annotation) is semidet.
%
%   True when Directive is a description.  Fails for anything else, so a
%   caller can use it as the test in a chain; raises when the directive
%   *is* one of ours and is malformed.
%
%   Annotation is `description(Kind, PI, Literal, Actions)` — the
%   literal as written, with `Kind` and `PI` naming what it describes and
%   `Actions` the template taken apart by template_actions/3, over that
%   literal's own variables.
%
%   @arg Directive is `describe(Spec)` or `pred(Spec)`, Spec being
%       `Literal :: Template`, or either with a second argument holding
%       the reader's `Name=Var` bindings.
%   @arg Annotation is built by description/3: Kind is `plain`, `dual`,
%       `neg` or `dualneg`, and PI is `Name/Arity`.

annotation(describe(Spec), Annotation) =>
    description(Spec, [], Annotation).
annotation(describe(Spec, Bindings), Annotation) =>
    description(Spec, Bindings, Annotation).
annotation(pred(Spec), Annotation), Spec = (_ :: _) =>
    description(Spec, [], Annotation).
annotation(pred(Spec, Bindings), Annotation), Spec = (_ :: _) =>
    description(Spec, Bindings, Annotation).
annotation(_, _) =>
    fail.

%!  deprecated_description(@Specification) is det.
%
%   Say that `:- pred p(X) :: "..."` should be `:- describe`.
%
%   Called where the program is **read** — `dyncall.pl:pred/1` for a
%   Prolog file and `source.pl:named_term/3` for an s(CASP) one — and not
%   from `annotation/2`, which is asked once per *compile*: a program
%   queried in a loop would otherwise repeat the warning once a query.
%
%   @arg Specification is the `Literal :: Template` after `:- pred`.

:- det(deprecated_description/1).

deprecated_description(Spec) :-
    print_message(warning, scasp(deprecated_pred_description(Spec))).

%!  description(@Spec, @Bindings, -Annotation) is det.
%
%   @arg Spec is `Literal :: Template`.
%   @arg Bindings is the reader's `Name=Var` list, or `[]` where the
%       names are written into Spec as `'$VAR'(Name)`.
%   @arg Annotation is as annotation/2 describes it.

:- det(description/3).

description(Literal0 :: Template0, Bindings, Annotation) =>
    Annotation = description(Kind, PI, Literal, Actions),
    unmark(Literal0 - Template0, Literal - Template, Marked),
    literal_kind(Literal, Kind, Head),
    (   callable(Head)
    ->  functor(Head, Name, Arity),
        PI = Name/Arity
    ;   domain_error(described_literal, Literal)
    ),
    named(Bindings, Named0),
    append(Named0, Marked, Named),
    template_actions(Template, Named, Actions).
description(Spec, _, _) =>
    domain_error(description, Spec).

named([], Named) =>
    Named = [].
named([Name = Variable|Bindings], Named) =>
    Named = [Name-Variable|Named1],
    named(Bindings, Named1).

%!  unmark(@Term, -Normalised, -Named) is det.
%
%   Replace each `'$VAR'(Name)` by a variable, one per name, and say
%   which.  A program written as a term list has no reader to ask what
%   its variables are called, so it writes the name into the term — and
%   what comes out here has to be an ordinary term, or it would unify
%   with nothing.
%
%   @arg Named is a list of `Name-Variable`, one pair per name.

:- det(unmark/3).

unmark(Term, Normalised, Named) :-
    unmark(Term, Normalised, [], Named).

unmark(Term, Normalised, Named0, Named), var(Term) =>
    Normalised = Term,
    Named = Named0.
unmark('$VAR'(Name), Normalised, Named0, Named) =>
    (   member(Other-Variable, Named0),
        Other == Name
    ->  Normalised = Variable,
        Named = Named0
    ;   Named = [Name-Normalised|Named0]
    ).
unmark(Term, Normalised, Named0, Named), compound(Term) =>
    compound_name_arguments(Term, Functor, Arguments0),
    foldl(unmark, Arguments0, Arguments, Named0, Named),
    compound_name_arguments(Normalised, Functor, Arguments).
unmark(Term, Normalised, Named0, Named) =>
    Normalised = Term,
    Named = Named0.

		 /*******************************
		 *        THE TEMPLATE		*
		 *******************************/

%!  template_actions(@Template, @Named, -Actions) is det.
%
%   Actions is the template taken apart: a list of `text(Atom)` and
%   `@(Variable:Type)`, in order, where each `@` is a placeholder
%   resolved against the literal's own variables.  Resolved here rather
%   than kept as text, because this is the one moment at which the names
%   are known.
%
%   A list rather than a `format/2` pair because **two** renderers read
%   it and they want different things from a placeholder: the text one
%   prints the value, and the HTML one emits a variable, a constrained
%   variable or "the <type> <value>" and needs the pieces to do it.
%   `description_text/3` is the text one, written here because it is
%   three lines.
%
%   A template may instead be `html(Spec)`, which is passed to `html//1`
%   whole.  There is nothing to take apart, but the variables of the
%   literal are marked, each wrapped as `'$value'(Var)`: once the
%   template is unified with a literal, a value is a term like any other,
%   and `b(X)` with X = `f(a)` would read as the element `<f>a</f>`
%   inside `<b>` rather than as the value `f(a)`.  `\Goal` is left
%   alone: a template that says how to render something says it that
%   way, and the marking must not reach inside.
%
%   @arg Template is a string or atom, or `html(Spec)`.
%   @arg Named is a list of `Name-Variable`, the literal's variables by
%       the names the reader gave them.

:- det(template_actions/3).

template_actions(html(Spec), _, Actions) =>
    Actions = html(Marked),
    mark_values(Spec, Marked).
template_actions(Template, Named, Actions) =>
    (   text_codes(Template, Codes)
    ->  true
    ;   domain_error(description_template, Template)
    ),
    phrase(template(Raw, Named), Codes),
    merge_text(Raw, Actions).

mark_values(Var, Marked), var(Var) =>
    Marked = '$value'(Var).
mark_values(Spec, Marked), Spec = \_ =>
    Marked = Spec.
mark_values(Compound, Marked), compound(Compound) =>
    compound_name_arguments(Compound, Name, Args0),
    maplist(mark_values, Args0, Args),
    compound_name_arguments(Marked, Name, Args).
mark_values(Atomic, Marked) =>
    Marked = Atomic.

text_codes(Text, Codes), string(Text) =>
    string_codes(Text, Codes).
text_codes(Text, Codes), atom(Text) =>
    atom_codes(Text, Codes).
text_codes(_, _) =>
    fail.

template([@(Variable:Type)|Actions], Named) -->
    placeholder(Name, Type),
    !,
    { resolve(Name, Named, Variable) },
    template(Actions, Named).
template([code(Code)|Actions], Named) -->
    [Code],
    !,
    template(Actions, Named).
template([], _) -->
    [].

%!  merge_text(@Raw, -Actions) is det.
%
%   One `text/1` per *run* of ordinary characters rather than one per
%   character.  Done afterwards rather than by looking ahead in the
%   grammar, because "up to the next placeholder" is exactly the
%   lookahead a DCG has no way to say.
%
%   @arg Raw is a list of `code(Code)`, one per character, and
%       `@(Variable:Type)` placeholders.

merge_text([], Actions) =>
    Actions = [].
merge_text([code(Code)|Raw0], Actions) =>
    Actions = [text(Text)|Actions1],
    run(Raw0, Raw, Codes),
    atom_codes(Text, [Code|Codes]),
    merge_text(Raw, Actions1).
merge_text([Action|Raw], Actions) =>
    Actions = [Action|Actions1],
    merge_text(Raw, Actions1).

run([code(Code)|Raw0], Raw, Codes) =>
    Codes = [Code|Codes1],
    run(Raw0, Raw, Codes1).
run(Raw0, Raw, Codes) =>
    Raw = Raw0,
    Codes = [].

placeholder(Name, Type) -->
    "@(",
    !,
    placeholder_name(Codes, Type, ")"),
    { atom_codes(Name, Codes) }.
placeholder(Name, Type) -->
    "{{",
    !,
    placeholder_name(Codes, Type, "}}"),
    { atom_codes(Name, Codes) }.

placeholder_name(Codes, Type, End) -->
    string(Codes0),
    (   ":"
    ->  string(TypeCodes),
        End,
        { atom_codes(Type, TypeCodes) }
    ;   End,
        { Type = '' }
    ),
    !,
    { Codes = Codes0 }.

%!  resolve(+Name, @Named, -Variable) is det.
%
%   A placeholder names a variable of the literal.  The leading
%   underscore is ignored on either side, so that `@(X)` finds `_X` —
%   see *Singletons* above.
%
%   @arg Named is the `Name-Variable` list of template_actions/3.
%
%   @error existence_error(description_variable, Name)

:- det(resolve/3).

resolve(Name, Named, Variable) :-
    (   member(Other-Found, Named),
        same_name(Other, Name)
    ->  Variable = Found
    ;   existence_error(description_variable, Name)
    ).

same_name(A, B), A == B =>
    true.
same_name(A, B) =>
    bare_name(A, Bare),
    bare_name(B, Bare).

bare_name(Name, Bare) :-
    (   atom_concat('_', Bare0, Name)
    ->  Bare = Bare0
    ;   Bare = Name
    ).

		 /*******************************
		 *          RENDERING		*
		 *******************************/

%!  description_text(@Description, @Literal, -Text) is semidet.
%
%   Text is Literal said in words, for a caller that wants a string and
%   nothing else.  Fails for an `html(Spec)` template, which has no text
%   form — `html.pl` is what reads one.
%
%   @arg Description is a `description/4` term, as annotation/2 makes.
%   @arg Text is an atom, with each value written by `~p`.

description_text(Description, Query, Text) :-
    description_actions(Description, Query, Actions),
    is_list(Actions),
    phrase(action_text(Actions), Fragments),
    atomic_list_concat(Fragments, Text).

action_text([]) ==> [].
action_text([text(Fragment)|Actions]) ==>
    [Fragment],
    action_text(Actions).
action_text([@(Value:_Type)|Actions]) ==>
    { format(atom(Fragment), '~p', [Value]) },
    [Fragment],
    action_text(Actions).

%!  description_actions(@Description, @Literal, -Actions) is semidet.
%
%   The description's actions with its literal's variables bound by
%   Literal — which is what puts the values where the placeholders were,
%   and what makes a description with a *bound* argument apply only where
%   it fits, so that
%
%       :- describe flies(tweety) :: "the famous one flies".
%
%   says something the general description would not.
%
%   Fails where the description is not at least as general as Literal,
%   so a caller may try several.  The copy is one for the literal and the
%   actions together, so a variable the template names is the same
%   variable the literal bound.
%
%   Literal is never bound: a description of `flies(tweety)` says nothing
%   about `flies(X)`, and applying it would bind X to tweety in the
%   caller's term.
%
%   @arg Description is a `description/4` term, as annotation/2 makes.

description_actions(description(_, _, Literal, Actions0), Query, Actions) =>
    copy_term(Literal-Actions0, Copy-Actions),
    subsumes_term(Copy, Query),
    Copy = Query.

		 /*******************************
		 *          MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(scasp(deprecated_pred_description(Spec))) -->
    [ 's(CASP): `:- pred ~p` is deprecated for a description.'-[Spec], nl,
      'Write `:- describe ~p` instead.'-[Spec], nl,
      'Here `:- pred p(dom)` is a signature -- it says which domain an'-[], nl,
      'argument ranges over, and that does change what is proved.'-[]
    ].
