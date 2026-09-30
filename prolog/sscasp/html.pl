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

:- module(sscasp_html,
          [ html_justification//2,      % +Justification, +Options
            html_model//2,              % +Model, +Options
            html_query//2,              % +Query, +Options
            html_atom//2                % +Literal, +Options
          ]).
:- use_module(ops).
:- use_module(lang,        [connector/2]).
:- use_module(html_text,   [emit//1, emitting_as/1]).
:- use_module(annotations, [described_actions/3, described_tree/4]).
:- use_module(library(http/html_write)).
:- use_module(library(http/term_html),  [term//2]).
:- use_module(library(http/html_head),  [html_resource/2]).
:- use_module(library(dcg/high_order),  [sequence//2, sequence//3]).
:- use_module(library(apply),  [convlist/3]).
:- use_module(library(option), [option/2, option/3, select_option/4]).

/** <module> An answer described once

The model and the justification as HTML — and, through `html_text.pl`, as
plain text, which is the same description read differently rather than a
second renderer.  `emit//1` is `html//1` in one mode and message-line tokens
in the other; nothing below says which.

## What it is given

Terms that `render.pl` has already made readable: a variable is
`'$VAR'(Name)` and a constrained one is `Name | {Constraints}`.  That is a
real simplification over the old implementation, where this layer reads the
constraint stores itself — here the stores have been read once, for the
model and the justification together, which is also what makes a variable
they share carry one name.

## Three ways to say a literal

  1. **A description**, `:- describe flies(X) :: "@(X) can fly"`, which the
     program wrote.  This is the principled one: a program that says what
     its predicates mean gets output in its own words.
  2. **The vocabulary in `lang.pl`**, for the things every program has —
     `not`, classical negation, the comparisons, "holds for".
  3. **The term itself**, which is what `show(machine)` asks for and what a
     literal nothing describes falls back on.

`show(Format)` chooses: `human`, `machine`, or `both` (the default), which
emits both and lets a stylesheet show one.  In text there is no stylesheet,
so `html_text.pl` drops the machine half — see `is_machine/1` there.

## Options

  - annotations(+Annotations)
    The table from `annotations.pl`, for descriptions.  Without it every
    literal takes route 2 or 3.
  - show(human|machine|both)
    Default `both`.
  - nmr(Boolean)
    Justify the NMR check as well.  Default `true`; `false` says only that
    the global constraints hold.

## Not ported

A description may say a node together with part of its subtree, `:- describe
Literal - Pattern :: Template` (see `describe.pl`): node//2 asks
`annotations.pl:described_tree/4` first.  The old implementation's condition
on a spec is not supported.  Neither is the program listing
(`html_program//1`) or the source references, which need a `goal_origin/2`
the new solver does not produce.
*/

:- multifile user:file_search_path/2.

%   The stylesheet and the script are **copies** of the old
%   implementation's, so that this tree loads nothing out of the other
%   one.  The DOM below is deliberately the DOM they were written for,
%   which is why the class names still say `scasp-`.
%
%   Named `sscasp.css` and `sscasp.js` rather than copied under their old
%   names, because `js` and `css` are aliases both trees register: with
%   two search paths and one file name, which copy `js('scasp.js')`
%   found would depend on the order the clauses were loaded in.  The
%   resource is `sscasp` for the same reason — two `html_resource/2`
%   declarations of one identity are one resource, whichever was meant.

user:file_search_path(js,  library(sscasp/web/js)).
user:file_search_path(css, library(sscasp/web/css)).

:- html_resource(sscasp,
                 [ virtual(true),
                   requires([ jquery,
                              js('sscasp.js'),
                              css('sscasp.css')
                            ]),
                   ordered(true)
                 ]).

		 /*******************************
		 *       THE JUSTIFICATION	*
		 *******************************/

%!  html_justification(+Justification, +Options)// is det.
%
%   Justification is what `scasp/4` returns: a list of `Literal-Children`
%   with the NMR check under a node of its own.
%
%   @arg Justification has been through scasp_readable/3; the NMR check
%       is the root `'$nmr'-Children`, and a Literal may be wrapped in
%       `chs/1` or `proved/1`.

:- det(html_justification//2).

html_justification(Justification, Options) ==>
    emit(div(class('scasp-justification'),
             ul(class('scasp-justification'),
                \nodes(Justification, Options)))).

nodes([], _) ==>
    [].
nodes([Node], Options) ==>
    node(Node, Options).
nodes([Node|Nodes], Options) ==>
    node(Node, [connect(and)|Options]),
    nodes(Nodes, Options).

%   The NMR check is a question of its own — "does this answer violate a
%   constraint?" — and the answer to it is almost always "no" said at
%   length.  So it is one node, and `nmr(false)` keeps it closed.

node('$nmr'-[], Options) ==>
    leaf('$nmr', Options).
node('$nmr'-_, Options), option(nmr(false), Options) ==>
    leaf('$nmr', Options).
node('$nmr'-Children, Options) ==>
    branch('$nmr', Children, ['scasp-global-constraints'], Options).
node(Node, Options), tree_described(Node, Options, Actions, Children) ==>
    described_node(Node, Actions, Children, Options).
node(Literal-[], Options) ==>
    leaf(Literal, Options).
node(Literal-Children, Options) ==>
    branch(Literal, Children, [], Options).

%   A description of the subtree (`:- describe Literal - Pattern ::
%   Template`) says the node and replaces the children it matched.  An
%   empty description of a node left without children hides the node.

tree_described(Node, Options, Actions, Children) :-
    option(annotations(Annotations), Options),
    described_tree(Annotations, Node, Actions, Children).

described_node(_, html([]), [], _) ==>
    [].
described_node(Literal-_, Actions, [], Options) ==>
    leaf(Literal, [described(Actions)|Options]).
described_node(Literal-_, Actions, Children, Options) ==>
    branch(Literal, Children, [], [described(Actions)|Options]).

leaf(Literal, Options) ==>
    emit(li(div(class(node),
                [ \tree_atom(Literal, Options),
                  \connect(Options)
                ]))).

branch(Literal, Children, ExtraClasses, Options) ==>
    { deeper(Options, Deeper) },
    emit(li(class([collapsable|ExtraClasses]),
            [ div(class([node, 'collapsable-header']),
                  [ \tree_atom(Literal, Options),
                    \connective(implies, Options)
                  ]),
              ul(class('collapsable-content'),
                 \nodes(Children, Deeper))
            ])).

deeper(Options0, Options) :-
    select_option(connect(_), Options0, Options1, _),
    select_option(described(_), Options1, Options, _).

connect(Options), option(connect(Connector), Options) ==>
    connective(Connector, Options).
connect(_) ==>
    [].

		 /*******************************
		 *          THE MODEL		*
		 *******************************/

%!  html_model(+Model, +Options)// is det.
%
%   Model is the list of literals `scasp/3` returns.
%
%   @arg Options are the module's, plus `class(Class)` for an extra CSS
%       class on the `ul` holding the model.

:- det(html_model//2).

html_model(Model, Options) ==>
    { option(class(Class), Options)
    ->  Classes = [Class]
    ;   Classes = []
    },
    emit(ul(class(['scasp-model'|Classes]),
            \sequence(model_item(Options), Model))).

model_item(Options, Literal) ==>
    emit(li(class('scasp-atom'), \model_term(Literal, Options))).

model_term(Literal, Options), option(show(human), Options) ==>
    html_atom(Literal, Options).
model_term(Literal, Options), option(show(machine), Options) ==>
    machine_atom(Literal, Options).
model_term(Literal, Options) ==>
    emit([ span(class(human),   \html_atom(Literal, Options)),
           span(class(machine), \machine_atom(Literal, Options))
         ]).

		 /*******************************
		 *          THE QUERY		*
		 *******************************/

%!  html_query(+Query, +Options)// is det.
%
%   The question, as a conjunction of literals.

:- det(html_query//2).

html_query(Query, Options) ==>
    { phrase(conjuncts(Query), Literals) },
    emit(div(class('scasp-query'),
             [ 'I would like to know if'-[],
               \sequence(query_literal(Options), Literals),
               \connective(?, Options)
             ])).

query_literal(Options, Literal) ==>
    emit(div(class('scasp-query-literal'), \html_atom(Literal, Options))).

conjuncts((A, B)) ==>
    conjuncts(A),
    conjuncts(B).
conjuncts(Literal) ==>
    [Literal].

		 /*******************************
		 *          A LITERAL		*
		 *******************************/

%   In a tree a literal is shown both ways, with the machine form as the
%   `title` of the human one so that hovering says what was actually
%   proved.

tree_atom(Literal, Options), option(show(machine), Options) ==>
    emit(span(class('scasp-atom'), \machine_atom(Literal, Options))).
tree_atom(Literal, Options), option(show(human), Options) ==>
    emit(span(class('scasp-atom'), \html_atom(Literal, Options))).
tree_atom(Literal, Options) ==>
    { machine_text(Literal, Text) },
    emit(span(class('scasp-atom'),
              [ span([class(human), title(Text)], \html_atom(Literal, Options)),
                span(class(machine), \machine_atom(Literal, Options))
              ])).

%!  html_atom(+Literal, +Options)// is det.
%
%   A literal in words.  A description if the program wrote one, and
%   `lang.pl`'s vocabulary otherwise.
%
%   @arg Literal is a readable literal, possibly wrapped in `chs/1` or
%       `proved/1`, or the atom `'$nmr'` for the NMR check's node.

:- det(html_atom//2).

html_atom(Literal, Options), description_of(Literal, Options, Actions) ==>
    { classes(Options, Classes) },
    emit(span(class(Classes), \actions(Actions, Options))).
html_atom(chs(Literal), Options) ==>
    utter(chs, Literal, [class(chs)|Options]).
html_atom(proved(Literal), Options) ==>
    { connector(proved, Text) },
    html_atom(Literal, [class(proved)|Options]),
    emit([', ', Text]).
html_atom(not(-(Literal)), Options) ==>
    utter(may, Literal, [class(not), class(neg)|Options]).
html_atom(not(Literal), Options) ==>
    utter(not, Literal, [class(not)|Options]).
html_atom(-(Literal), Options) ==>
    utter(-, Literal, [class(neg)|Options]).
html_atom('$nmr', _Options) ==>
    { connector(global_constraints_hold, Text) },
    emit(Text).
html_atom(Comparison, Options),
    compound(Comparison),
    compound_name_arity(Comparison, _, 2),
    connector(Comparison, Text) ==>
    { arg(1, Comparison, Left),
      arg(2, Comparison, Right),
      classes(Options, Classes)
    },
    emit(span(class([arithmetic|Classes]),
              [ \html_term(Left, Options),
                &(nbsp), Text, &(nbsp),
                \html_term(Right, Options)
              ])).
html_atom(Literal, Options) ==>
    holds(Literal, Options).

%   A description is looked up on the *whole* literal — `not flies(X)`
%   reads its own description and not `flies(X)`'s, because the stored
%   literal keeps its `not` and `-` and is matched by subsumption.
%   `chs/1` and `proved/1` are about the proof rather than about the
%   literal, so they are peeled off above and described underneath.

description_of(_, Options, Actions),
        option(described(Actions0), Options) =>
    Actions = Actions0.
description_of(Literal, Options, Actions) =>
    option(annotations(Annotations), Options),
    described_actions(Annotations, Literal, Actions).

utter(Meaning, Literal, Options) ==>
    { connector(Meaning, Text) },
    emit([Text, ' ']),
    html_atom(Literal, Options).

holds(Literal, Options), atom(Literal) ==>
    { classes(Options, Classes),
      connector(holds, Text)
    },
    emit([span(class(Classes), Literal), Text]).
holds(Literal, Options), compound(Literal), connector(holds_for, Text) ==>
    { classes(Options, Classes),
      compound_name_arguments(Literal, Name, Args)
    },
    emit([span(class(Classes), Name), Text]),
    list(Args, Options).
holds(Literal, Options) ==>
    html_term(Literal, Options).

classes(Options, [atom|Classes]) :-
    convlist(class_option, Options, Classes0),
    (   Classes0 == []
    ->  Classes = [pos]
    ;   Classes = Classes0
    ).

class_option(class(Class), Class).

		 /*******************************
		 *        A DESCRIPTION		*
		 *******************************/

%!  actions(+Actions, +Options)// is det.
%
%   What `describe.pl` made of the template: `text/1` for a run of
%   characters, `@(Value:Type)` for a placeholder, or `html(Spec)` for a
%   template that was written as HTML in the first place.
%
%   @arg Actions is either `html(Spec)` as a whole or a list of
%       `text(Text)` and `@(Value:Type)`, as described_actions/3
%       returns them.

actions(html(Spec0), Options) ==>
    { html_values(Spec0, Options, Spec) },
    emit(Spec).
actions([], _) ==>
    [].
actions([Action|Actions], Options) ==>
    action(Action, Options),
    actions(Actions, Options).

action(text(Text), _Options) ==>
    emit(Text).
action(Term, Options) ==>
    html_term(Term, Options).

%!  html_values(@Spec0, +Options, -Spec) is det.
%
%   Replace each value of the literal in an HTML template by a call that
%   renders it.  `describe.pl:template_actions/3` wraps every variable of
%   the template as `'$value'(Var)`, so a value is found by its wrapper
%   whatever it is bound to by now — `f(a)` as well as a named variable.
%   Without this a template writing `b(X)` hands `html//1` a value where
%   it expects an element: a variable came out as `<| $VAR="A">`, and a
%   compound as markup.
%
%   `\Goal` is left alone: a template that wants to say how to render
%   something says it that way, and this must not reach inside.

html_values('$value'(Value), Options, Out) =>
    Out = \html_term(Value, Options).
html_values(Spec, _, Out), Spec = \_ =>
    Out = Spec.
html_values(Compound, Options, Out), compound(Compound) =>
    compound_name_arguments(Compound, Name, Args0),
    html_values_list(Args0, Options, Args),
    compound_name_arguments(Out, Name, Args).
html_values(Atomic, _, Out) =>
    Out = Atomic.

html_values_list([], _, Out) =>
    Out = [].
html_values_list([H0|T0], Options, Out) =>
    Out = [H|T],
    html_values(H0, Options, H),
    html_values_list(T0, Options, T).

		 /*******************************
		 *           A TERM		*
		 *******************************/

%!  html_term(@Term, +Options)// is det.
%
%   A value: a variable, a variable with what is known about it, or a
%   plain term.
%
%   @arg Term is `'$VAR'(Name)`, `'| '('$VAR'(Name), {Constraints})`,
%       a description placeholder `@(Value:Type)`, or any other term.

%   The `var/1` first, and it is not defensive: an unbound term is
%   "anything", and must be printed as such.  Under `==>` no other
%   clause would match it and it would fall through to the last one,
%   printing an internal `_G123`; under `-->`, unified rather than
%   matched, `@(Value:Type)` would bind it to a fresh `@(_:_)` whose
%   value is unbound again, and `html_term//2` would call itself for
%   ever.
%   A query is where one arrives — its variables are not named until an
%   answer names them.

:- det(html_term//2).

html_term(Var, Options), var(Var) ==>
    variable('_', Options).
html_term(@(Value:Type), Options) ==>
    typed(Value, Type, Options).
html_term('$VAR'(Name), Options) ==>
    variable(Name, Options).
html_term('| '(Var, Constraints), Options) ==>
    constrained(Var, Constraints, Options).
html_term(Term, Options) ==>
    plain_term(Term, Options).

%   A value reads "the bird tweety"; a variable is not one, and reads
%   "X, a bird", or "a bird" where it has no name — the old
%   implementation's `typed_var//3`.

typed(Value, '', Options) ==>
    html_term(Value, Options).
typed(Value, Type, _Options), var(Value) ==>
    emit(['a ~w'-[Type]]).
typed('$VAR'('_'), Type, _Options) ==>
    emit(['a ~w'-[Type]]).
typed('$VAR'(Name), Type, Options) ==>
    variable(Name, Options),
    emit([', a ~w'-[Type]]).
typed(Value, Type, Options) ==>
    emit(['the ~w '-[Type]]),
    html_term(Value, Options).

variable('_', _Options) ==>
    emit(anything).
variable(Name, _Options) ==>
    emit(var(Name)).

%   A constrained variable says what is known about it instead of saying
%   its name: `A | {A>=3}` reads "A is greater than or equal to 3".

constrained('$VAR'(Name), {Constraints}, Options) ==>
    { comma_list(Constraints, List) },
    emit(span(class(constrained),
              \sequence(one_constraint(Name, Options), and_also, List))).
constrained(Var, Constraints, Options) ==>
    plain_term('| '(Var, Constraints), Options).

and_also ==>
    { connector(and, Text) },
    emit([' ', Text, ' ']).

%   `A ∉ [a, b]` is "A is not a or b" — one statement rather than the
%   conjunction it stands for, which is why the store keeps it as a set.

one_constraint(Name, Options, '∉'(Var, Values)), Var == '$VAR'(Name) ==>
    (   { Values = [One] }
    ->  { connector(neq, Text) },
        emit([var(Name), ' ', Text, ' ']),
        html_term(One, Options)
    ;   { connector(not_in, Text) },
        emit([var(Name), ' ', Text, ' ']),
        list(Values, [last_connector(or)|Options])
    ).
one_constraint(Name, Options, Constraint),
    compound(Constraint),
    compound_name_arity(Constraint, _, 2),
    connector(Constraint, Text),
    arg(1, Constraint, Left),
    arg(2, Constraint, Right),
    other_side(Name, Left, Right, Other) ==>
    emit([var(Name), ' ', Text, ' ']),
    html_term(Other, Options).
one_constraint(_Name, Options, Constraint) ==>
    plain_term(Constraint, Options).

%   `A >= 3` and `3 =< A` say the same about A.  Only the first shape is
%   turned around, because reading `A + B >= 3` as being about A alone
%   would be a lie: that one is written as a plain term.

other_side(Name, Left, Right, Other), Left == '$VAR'(Name) =>
    Other = Right.
other_side(Name, Left, Right, Other), Right == '$VAR'(Name) =>
    Other = Left.
other_side(_, _, _, _) =>
    fail.

%!  plain_term(@Term, +Options)// is det.
%
%   A term with nothing to say about it.  Written with **our** operator
%   table, which `term//2` has no way to be told about, so in HTML the
%   term becomes one span rather than a marked-up tree.  That is the
%   simplification this port takes; an operator printed as a functor
%   would be worse.

plain_term(Term, _Options),
    \+ needs_our_operators(Term),
    \+ emitting_as(plain) ==>
    emit(\term(Term, [numbervars(true)])).
plain_term(Term, _Options) ==>
    { machine_text(Term, Text) },
    (   { emitting_as(plain) }
    ->  [ ansi(code, '~w', [Text]) ]
    ;   emit(span(class(term), Text))
    ).

needs_our_operators(Term) :-
    compound(Term),
    (   functor(Term, Name, _),
        our_operator(Name)
    ->  true
    ;   arg(_, Term, Arg),
        needs_our_operators(Arg)
    ).

our_operator(not).
our_operator(-).
our_operator('| ').
our_operator('∉').
our_operator(#>).
our_operator(#<).
our_operator(#>=).
our_operator(#=<).
our_operator(#=).
our_operator(#<>).

%!  list(@Elements, +Options)// is det.
%
%   A collection as "a, b and c".
%
%   @arg Options may hold `last_connector(Meaning)`, the connector/2
%       meaning put before the last element.  Default `and`.

list([One], Options) ==>
    html_term(One, Options).
list([Last1, Last], Options) ==>
    { option(last_connector(Meaning), Options, and),
      connector(Meaning, Text)
    },
    html_term(Last1, Options),
    emit(' ~w '-[Text]),
    html_term(Last, Options).
list([H|T], Options) ==>
    html_term(H, Options),
    emit(', '),
    list(T, Options).

		 /*******************************
		 *         THE MACHINE		*
		 *******************************/

%!  machine_atom(@Literal, +Options)// is det.
%
%   The literal as the program would write it.

machine_atom('$nmr', _Options) ==>
    emit(span(class([connector,nmr]), 'global constraints')).
machine_atom(not(Literal), Options) ==>
    emit([span(class([connector,not]), not), ' ']),
    machine_atom(Literal, [class(not)|Options]).
machine_atom(-(Literal), Options) ==>
    emit([span(class([connector,neg]), '¬'), ' ']),
    machine_atom(Literal, [class(neg)|Options]).
machine_atom(chs(Literal), Options) ==>
    emit([ span(class([connector,chs]), chs), '(',
           \machine_atom(Literal, [class(chs)|Options]), ')'
         ]).
machine_atom(proved(Literal), Options) ==>
    emit([ span(class([connector,proved]), proved), '(',
           \machine_atom(Literal, [class(proved)|Options]), ')'
         ]).
machine_atom(Literal, Options) ==>
    { classes(Options, Classes) },
    emit(span(class(Classes), \plain_term(Literal, Options))).

%!  machine_text(@Term, -Text) is det.
%
%   Term written with this language's operators, as a string.

machine_text(Term, Text) :-
    with_output_to(string(Text),
                   write_term(Term,
                              [ module(sscasp_ops),
                                numbervars(true),
                                quoted(true),
                                portray(true)
                              ])).

		 /*******************************
		 *         CONNECTORS		*
		 *******************************/

%!  connective(+Meaning, +Options)// is det.
%
%   A logical connector, said in words, as a symbol, or as both.
%
%   @arg Meaning is a key of `lang.pl:connector/2`, one of `and`,
%       `implies` and `?`: the ones with a symbol.

connective(Meaning, Options), option(show(human), Options) ==>
    { connector(Meaning, Text) },
    human_connector(Meaning, Text).
connective(Meaning, Options), option(show(machine), Options) ==>
    machine_connector(Meaning).
connective(Meaning, _Options) ==>
    { connector(Meaning, Text) },
    emit([ span(class(human),   \human_connector(Meaning, Text)),
           span(class(machine), \machine_connector(Meaning))
         ]).

human_connector(and,     Text) ==> emit([', ', Text]).
human_connector(implies, Text) ==> emit([', ', Text]).
human_connector(?,       Text) ==> emit(Text).

machine_connector(and)     ==> emit(',').
machine_connector(implies) ==> emit(' ←').
machine_connector(?)       ==> emit(?).

comma_list((Goal, Rest), Goals) =>
    Goals = [Goal|Goals1],
    comma_list(Rest, Goals1).
comma_list(Goal, Goals) =>
    Goals = [Goal].
