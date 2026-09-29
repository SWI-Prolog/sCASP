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

:- module(sscasp_html_text,
          [ emit_as//2,                 % :HTML, +Mode
            emitting_as/1,              % ?Mode
            emit//1,                    % :HTML
            emit_text/1,                % :HTML
            fixup_layout/2              % +Tokens, -Final
          ]).
:- use_module(library(dcg/high_order), [sequence//2]).
:- use_module(library(http/html_write)).
:- use_module(library(apply), [include/3, maplist/2, maplist/3]).
:- use_module(library(error), [must_be/2]).
:- use_module(library(lists), [append/3, member/2, flatten/2, last/2,
                                reverse/2]).
:- use_module(library(ansi_term), [ansi_format/3]).

/** <module> One renderer, two outputs

`html.pl` describes an answer *once*, as HTML.  This module lets the same
description come out as coloured plain text instead: `emit//1` behaves as
`html//1` inside `emit_as(Goal, html)` and produces `print_message_lines/3`
tokens inside `emit_as(Goal, plain)`.

That is the design of the old implementation's `html_text.pl` and this is a
port of it.  The alternative — two renderers — is what it exists to avoid,
and the reason is not economy but *drift*: the two would disagree about a
case neither author remembered, and the text one is what a test reads.

Two things follow from it and are worth knowing before changing either file:

  - **The translation is specific to the DOM `html.pl` emits.**  A `div` of
    class `scasp-justification` is a bulleted list here and a `ul` is an
    indent; a `span` of class `machine` is *dropped*.  Changing a class or
    nesting in `html.pl` usually means changing a clause here.
  - **The mode is read from the stack**, by `emitting_as/1` asking for the
    enclosing `emit_as/4` frame.  There is no argument to thread, which is
    what lets `html.pl` be written as ordinary `html//1` DCG rules with
    nothing about text in them.

Layout is two passes.  `emit//1` leaves instructions — `nl(N)`, `indent(N)`,
`bullet`, `ansi(Attrs)`, `pop_ansi` — in the token list, and
`fixup_layout/2` turns them into spaces, newlines and `ansi/3` tokens.  It
is two passes because indentation and colour are *state*, and there is no
argument to carry it in for the same reason the mode has none.
*/

:- html_meta
    emit_as(html, +, ?, ?),
    emit(html, ?, ?),
    emit_text(html).

%!  emit_as(:HTML, +Mode)// is det.
%
%   Run HTML with every `emit//1` under it behaving as `html//1`
%   (`Mode` = `html`) or emitting message-line tokens (`Mode` = `plain`).

emit_as(Goal, Mode) ==>
    { must_be(oneof([plain,html]), Mode) },
    html(Goal),
    { no_lco(Mode) }.

%   The frame has to still be there when `emitting_as/1` looks for it, so
%   the call above must not be the last one.

no_lco(_).

%!  emitting_as(?Mode) is semidet.
%
%   The mode of the enclosing `emit_as//2`, `plain` or `html`.

emitting_as(Mode) :-
    prolog_current_frame(Frame),
    prolog_frame_attribute(Frame, parent_goal, emit_as(_, Mode, _, _)).

%!  emit_text(:HTML) is det.
%
%   Write HTML to `current_output` as plain text, **not** ending with a
%   newline: whatever comes next — a toplevel's comma or full stop, the
%   caller's own `nl/0` — decides that, which is the contract of every
%   printer in `render.pl`.  print_message_lines/3 ends every line, the
%   last included, so the lines are written here.

:- det(emit_text/1).

emit_text(Goal) :-
    phrase(emit_as(Goal, plain), Tokens0),
    fixup_layout(Tokens0, Tokens1),
    trim_trailing_nl(Tokens1, Tokens),
    maplist(write_token, Tokens).

trim_trailing_nl(Tokens0, Tokens) :-
    reverse(Tokens0, Reversed0),
    drop_nl(Reversed0, Reversed),
    reverse(Reversed, Tokens).

drop_nl([nl|T0], T) =>
    drop_nl(T0, T).
drop_nl(T0, T) =>
    T = T0.

write_token(nl) =>
    nl.
write_token(ansi(Attrs, Format, Args)) =>
    ansi_format(Attrs, Format, Args).
write_token(Format-Args) =>
    format(Format, Args).
write_token(Format) =>
    format(Format, []).


%!  emit(:HTML)// is det.
%
%   Emit HTML, or the message-line tokens that say the same, depending
%   on the enclosing `emit_as//2`.

emit(M:Spec), emitting_as(plain) ==>
    emit(Spec, M).
emit(Spec) ==>
    html(Spec).

emit(Var, _), var(Var) ==>
    [ '~p'-[Var] ].
emit(List, M), is_list(List) ==>
    sequence(emit_r(M), List).
emit(\Goal, M), callable(Goal) ==>
    call(M:Goal).
emit(Element, _M), is_machine(Element) ==>
    [].
emit(var(Name), _M) ==>
    [ ansi(fg(magenta), '~w', [Name]) ].
emit(span(Content), M) ==>
    emit(Content, M).
emit(span(Attrs, Content), M) ==>
    (   classes_ansi(Attrs)
    ->  emit(Content, M),
        [pop_ansi]
    ;   emit(Content, M)
    ).
emit(div(Attrs, Content), M), has_class(Attrs, 'scasp-query-literal') ==>
    [indent(3)],
    emit(Content, M),
    [indent(-3), nl(1)].
emit(div(Attrs, Content), M) ==>
    (   classes_bullet(Attrs)
    ->  emit(Content, M),
        [bullet(pop)]
    ;   emit(Content, M)
    ),
    [nl(1)].
emit(div(Content), M) ==>
    emit(Content, M),
    [nl(1)].
emit(ul(_Attrs, LIs), M) ==>
    [indent(3)],
    emit(LIs, M),
    [indent(-3)].
emit(ul(LIs), M) ==>
    [indent(3)],
    emit(LIs, M),
    [indent(-3)].
emit(li(_Attrs, Content), M) ==>
    [bullet],
    emit(Content, M),
    [nl(1)].
emit(li(Content), M) ==>
    [bullet],
    emit(Content, M),
    [nl(1)].
emit(&(nbsp), _M) ==>
    [ ' '-[] ].
emit(Element, M),
    compound(Element),
    compound_name_arguments(Element, Name, Args),
    html_element(Name),
    last(Args, Content) ==>
    emit(Content, M).
emit(Line, _M), Line = _-_ ==>
    [ Line ].
emit(Atomic, _M) ==>
    [ '~w'-[Atomic] ].

emit_r(M, Spec) ==>
    emit(Spec, M).

%!  html_element(+Name) is semidet.
%
%   An element this has no layout of its own for: its content is
%   emitted and its markup dropped.  `html.pl` emits only elements with
%   a clause above, but an `html(Spec)` description template may say
%   anything, and `<b>tweety</b>` coming out as `b(tweety)` is not a
%   description.
%
%   A list rather than "any compound of arity one or two", because a
%   value of the literal is a compound too and emitting its argument
%   instead of it would be worse than the markup leaking.

html_element(b).      html_element(i).       html_element(em).
html_element(strong). html_element(code).    html_element(tt).
html_element(small).  html_element(sub).     html_element(sup).
html_element(u).      html_element(a).       html_element(p).
html_element(h1).     html_element(h2).      html_element(h3).
html_element(h4).     html_element(label).   html_element(q).

%!  is_machine(+Element) is semidet.
%
%   An element carrying class `machine` says the same thing in the
%   language the program was written in, beside the human form.  In HTML
%   they are both emitted and a stylesheet shows one; in text there is no
%   stylesheet, so the human one is what is left.

is_machine(Element) :-
    compound(Element),
    functor(Element, _, 2),
    arg(1, Element, Attrs),
    has_class(Attrs, machine).

		 /*******************************
		 *           CLASSES		*
		 *******************************/

classes_ansi(Attrs), attrs_ansi(Attrs, Ansi) ==>
    [ansi(Ansi)].
classes_ansi(_) ==>
    { fail }.

attrs_ansi(Attrs, Ansi) :-
    classes(Attrs, Classes),
    include(truth_class, Classes, TClasses),
    sort(TClasses, Sorted),
    classes_ansi_map(Sorted, Ansi).

classes_bullet(Attrs), has_class(Attrs, 'scasp-justification') ==>
    [ bullet('') ].
classes_bullet(_) ==>
    { fail }.

has_class(Attrs, Class) :-
    classes(Attrs, Classes),
    member(Class, Classes).

%   Matched, never unified: is_machine/1 asks this of any element of
%   arity two, and in a literal's value that may be `f(_, y)`.  Unifying
%   made the variable `class(_)`, whose unbound class then matched
%   `machine`, and the value was dropped as the machine form.  A class
%   is an atom, so anything else is not one.

classes(Attrs, Classes), is_list(Attrs) =>
    include(is_class, Attrs, ClassAttrs),
    maplist(arg(1), ClassAttrs, Classes0),
    flatten(Classes0, Classes1),
    include(atom, Classes1, Classes).
classes(class(Classes0), Classes) =>
    (   is_list(Classes0)
    ->  Classes1 = Classes0
    ;   Classes1 = [Classes0]
    ),
    include(atom, Classes1, Classes).
classes(_, _) =>
    fail.

is_class(class(_)) => true.
is_class(_) => fail.

truth_class(pos).
truth_class(not).
truth_class(neg).

%   The four shapes, coloured as `render.pl:style/2` colours them: what
%   holds bold, what is known not to hold bold red, what there is no
%   evidence for red, and `not -p` — no evidence against — plain.

classes_ansi_map([neg], [fg(red), bold]).
classes_ansi_map([not], [fg(red)]).
classes_ansi_map([neg, not], []).
classes_ansi_map([pos], [bold]).

		 /*******************************
		 *           LAYOUT		*
		 *******************************/

%!  fixup_layout(+Tokens, -Final) is det.
%
%   Turn the layout instructions `emit//1` left in the token list into
%   spaces, newlines and `ansi/3` tokens.  The state — indentation, the
%   bullet stack and the colour stack — lives here and nowhere else.
%
%   @arg Tokens are `Format-Args` tokens mixed with `nl(N)`,
%       `indent(N)`, `bullet`, `bullet(Bullet)` (push, or `pop`),
%       `ansi(Attrs)` and `pop_ansi`.
%   @arg Final is a token list for print_message_lines/3.

:- det(fixup_layout/2).

fixup_layout(Tokens, Final) :-
    fixup_layout(Tokens, Final,
                 #{ indent: 0,
                    ansi: [], ansi_stack: [],
                    bullet: ['•']
                  }).

fixup_layout([], Final, _) =>
    Final = [].
fixup_layout([nl(Lines0)|T0], Final, State) =>
    Indent0 = State.indent,
    join_blank_lines(T0, T1, Indent0, Indent, Lines0, Lines),
    skip_lines(Lines, Final, T),
    (   T1 == []
    ->  T = []
    ;   Indent > 0
    ->  format(atom(I), '~t~*|', [Indent]),
        T = [I|T2]
    ;   T = T2
    ),
    fixup_layout(T1, T2, State.put(indent, Indent)).
fixup_layout([indent(N)|T0], T, State) =>
    Indent is State.indent+N,
    indent(Indent, T, T1),
    fixup_layout(T0, T1, State.put(indent, Indent)).
fixup_layout([bullet|T0], T, State) =>
    [Bullet|_] = State.bullet,
    (   Bullet == ''
    ->  fixup_layout(T0, T, State)
    ;   T = ['~w '-[Bullet]|T1],
        fixup_layout(T0, T1, State)
    ).
fixup_layout([bullet(Bullet)|T0], T, State) =>
    Stack = State.bullet,
    (   Bullet == pop
    ->  Stack = [_|NewStack]
    ;   NewStack = [Bullet|Stack]
    ),
    fixup_layout(T0, T, State.put(bullet, NewStack)).
fixup_layout([ansi(Attrs)|T0], T, State) =>
    Old = State.ansi,
    Stack = State.ansi_stack,
    append(Attrs, Old, New),
    fixup_layout(T0, T, State.put(#{ansi: New, ansi_stack: [Old|Stack]})).
fixup_layout([pop_ansi|T0], T, State) =>
    [Old|Stack] = State.ansi_stack,
    fixup_layout(T0, T, State.put(#{ansi: Old, ansi_stack: Stack})).
fixup_layout([H0|T0], Final, State) =>
    Final = [H|T],
    fixup_element(H0, H, State.ansi),
    fixup_layout(T0, T, State).

fixup_element(E0, E, []) =>
    E = E0.
fixup_element(Fmt-Args, E, Ansi) =>
    E = ansi(Ansi, Fmt, Args).
fixup_element(E0, E, _) =>
    E = E0.

%!  join_blank_lines(+Tokens, -Rest, +Indent0, -Indent, +Lines0, -Lines)
%
%   A run of `nl/1` and `indent/1` tokens collapses to one break: the
%   widest gap asked for, at the indentation reached at the end of it.
%
%   @arg Tokens are what follows the `nl(Lines0)` that started the
%       run; Rest is what follows the run.
%   @arg Indent0 is the column before the run; Indent the one after.
%   @arg Lines is the most newlines any `nl/1` in the run asked for.

join_blank_lines([nl(N)|T0], T, I0, I, Lines0, Lines) =>
    Lines1 is max(N, Lines0),
    join_blank_lines(T0, T, I0, I, Lines1, Lines).
join_blank_lines([indent(N)|T0], T, I0, I, Lines0, Lines) =>
    I1 is I0+N,
    join_blank_lines(T0, T, I1, I, Lines0, Lines).
join_blank_lines(L0, L, I0, I, Lines0, Lines) =>
    L = L0,
    I = I0,
    Lines = Lines0.

indent(I, L0, L), I > 0 =>
    format(atom(Spaces), '~t~*|', [I]),
    L0 = [Spaces|L].
indent(_, L0, L) =>
    L0 = L.

skip_lines(N, L0, L), N > 0 =>
    L0 = [nl|L1],
    N1 is N-1,
    skip_lines(N1, L1, L).
skip_lines(_, L0, L) =>
    L0 = L.
