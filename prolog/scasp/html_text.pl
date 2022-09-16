/*  Part of sCASP

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(scasp_html_text,
          [ emit_as//2,
            emitting_as/1,
            emit//1,
            fixup_layout/2              % +Tokens,-Final
          ]).
:- use_module(library(dcg/high_order)).
:- use_module(library(http/html_write)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).

:- html_meta
    emit_as(html, +, ?, ?),
    emit(html, ?, ?).

/** <module> Switch between HTML and plain text output

Allow generating plain (colored) text from  html//1 compatible calls. We
do this in two steps, first creating  a   token  list and next deal with
state using fixup_layout/2. It would be  nicer   if  we  could avoid the
latter step, but we need  to  maintain   state  such  as indentation and
attributes and we cannot pass that around   in an additional argument as
there are calls that do not know about this transformation in between.

The translation is highly specific for the calls done in html.pl. If you
change CSS classes or the HTML  DOM   produced  you  likely have to make
changes here as well.

@tbd It might be better to maintain  a   stack  of parent DOM nodes in a
backtrackable global variable to deal with the state.
*/

%!  emit_as(+HTML, +Mode)//
%
%   Causes all emit//1 calls to either behave  as html//1 or convert the
%   commands to output for print_message_lines/3.

emit_as(Goal, Mode) -->
    { must_be(oneof([plain,html]), Mode) },
    html(Goal),
    {no_lco(Mode)}.

no_lco(_).

%!  emitting_as(-Mode)
%
%   Current emit mode.  One of `plain` or `html`

emitting_as(Mode) :-
    prolog_current_frame(F),
    prolog_frame_attribute(F, parent_goal, emit_as(_, Mode, _, _)).

%!  emit(:HTML)
%
%   Emits HTML or message line elements depending on emit_as//2.

emit(M:Spec) -->
    { emitting_as(plain)
    },
    !,
    emit(Spec, M).
emit(Spec) -->
    html(Spec).

emit(Var, _) -->
    { var(Var) },
    !,
    [ '~p'-[Var] ].
emit(List, M) -->
    { is_list(List) },
    !,
    sequence(emit_r(M), List).
emit(\Goal, M) -->
    { callable(Goal) },
    !,
    call(M:Goal).
emit(Element, _M) -->
    { is_machine(Element)
    },
    !.
emit(var(Name), _M) -->
    !,
    [ ansi(fg(magenta), '~w', [Name]) ].
emit(span(Content), M) -->
    !,
    emit(Content, M).
emit(span(Attrs, Content), M) -->
    !,
    (   classes_ansi(Attrs)
    ->  emit(Content, M),
        [pop_ansi]
    ;   emit(Content, M)
    ).
emit(div(Attrs, Content), M) -->
    { has_class(Attrs, 'scasp-query-literal') },
    !,
    [indent(3)],
    emit(Content, M),
    [indent(-3), nl(1)].
emit(div(Attrs, Content), M) -->
    !,
    classes_pre_lines(Attrs),
    (   classes_bullet(Attrs)
    ->  emit(Content, M),
        [bullet(pop)]
    ;   emit(Content, M)
    ),
    [nl(1)].
emit(div(Content), M) -->
    !,
    emit(Content, M),
    [nl(1)].
emit(ul(_Attrs, LIs), M) -->
    !,
    [indent(3)],
    emit(LIs, M),
    [indent(-3)].
emit(li(_Attrs, Content), M) -->
    !,
    [bullet],
    emit(Content, M),
    [nl(1)].
emit(li(Content), M) -->
    !,
    [bullet],
    emit(Content, M),
    [nl(1)].
emit(Fmt-Args, _M) -->
    !,
    [ Fmt-Args ].
emit(Atomic, _M) -->
    !,
    [ '~w'-[Atomic] ].

emit_r(M, Spec) -->
    emit(Spec, M).

is_machine(Element) :-
    compound(Element),
    functor(Element, _, 2),
    arg(1, Element, Attrs),
    has_class(Attrs, machine).

classes_ansi(Attrs) -->
    { classes(Attrs, Classes),
      include(truth_class, Classes, TClasses),
      sort(TClasses, TClassesS),
      classes_ansi_map(TClassesS, Ansi)
    },
    !,
    [ansi(Ansi)].

classes_bullet(Attrs) -->
    { has_class(Attrs, 'scasp-justification') },
    !,
    [ bullet('') ].

classes_pre_lines(Attrs) -->
    { has_class(Attrs, 'scasp-predicate') },
    !,
    [ nl(1) ].
classes_pre_lines(_) -->
    [].

has_class(Attrs, Class) :-
    classes(Attrs, Classes),
    memberchk(Class, Classes).

classes(Attrs, Classes) :-
    (   is_list(Attrs)
    ->  include(is_class, Attrs, ClassAttrs),
        maplist(arg(1), ClassAttrs, Classes0),
        flatten(Classes0, Classes)
    ;   Attrs = class(Classes0),
        (   is_list(Classes0)
        ->  Classes = Classes0
        ;   Classes = [Classes0]
        )
    ).

is_class(class(_)).

truth_class(pos).
truth_class(not).
truth_class(neg).

classes_ansi_map([neg],     [fg(red), bold]).
%classes_ansi_map([neg,not], []).
classes_ansi_map([not],     [fg(red)]).
classes_ansi_map([pos],     [bold]).

%!  fixup_layout(+Tokens, -Final)
%
%   Fixup layout instructions in the token list.

:- det(fixup_layout/2).

fixup_layout(Tokens, Final) :-
    fixup_layout(Tokens, Final,
                 #{ indent:0,
                    ansi:[], ansi_stack:[],
                    bullet:['\u2022']
                  }).

fixup_layout([], [], _).
fixup_layout([nl(Lines0)|T0], Final, State) :-
    !,
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
fixup_layout([indent(N)|T0], T, State) :-
    !,
    Indent is State.indent+N,
    indent(Indent, T, T1),
    fixup_layout(T0, T1, State.put(indent, Indent)).
fixup_layout([bullet|T0], T, State) :-
    !,
    [Bullet|_] = State.bullet,
    (   Bullet == ''
    ->  fixup_layout(T0, T, State)
    ;   T = ['~w '-[Bullet]|T1],
        fixup_layout(T0, T1, State)
    ).
fixup_layout([bullet(Bullet)|T0], T, State) :-
    !,
    Stack = State.bullet,
    (   Bullet == pop
    ->  Stack = [_|NewStack]
    ;   NewStack = [Bullet|Stack]
    ),
    fixup_layout(T0, T, State.put(bullet, NewStack)).
fixup_layout([ansi(Attrs)|T0], T, State) :-
    !,
    Old = State.ansi,
    Stack = State.ansi_stack,
    append(Attrs, Old, New),
    fixup_layout(T0, T, State.put(#{ansi:New, ansi_stack:[Old|Stack]})).
fixup_layout([pop_ansi|T0], T, State) :-
    !,
    [Old|Stack] = State.ansi_stack,
    fixup_layout(T0, T, State.put(#{ansi:Old, ansi_stack:Stack})).
fixup_layout([H0|T0], [H|T], State) :-
    fixup_element(H0, H, State.ansi),
    fixup_layout(T0, T, State).

fixup_element(E, E, []) :-
    !.
fixup_element(Fmt-Args, ansi(Ansi, Fmt, Args), Ansi) :-
    !.
fixup_element(E, E, _).

%!  join_blank_lines(+Tokens, -RestTokens,
%!                   +Indent0, -Indent, +Lines0, -Lines) is det.
%
%   Deal with a sequence of nl(N) and indent(Incr) tokens, computing the
%   next relevant indentation and the number of newlines to insert.

join_blank_lines([nl(N)|T0], T, I0, I, Lines0, Lines) :-
    !,
    Lines1 is max(N, Lines0),
    join_blank_lines(T0, T, I0, I, Lines1, Lines).
join_blank_lines([indent(N)|T0], T, I0, I, Lines0, Lines) :-
    !,
    I1 is I0+N,
    join_blank_lines(T0, T, I1, I, Lines0, Lines).
join_blank_lines(L, L, I, I, Lines, Lines).

indent(I, [Spaces|T], T) :-
    I > 0,
    !,
    format(atom(Spaces), '~t~*|', [I]).
indent(_, L, L).

skip_lines(N, [nl|L0], L) :-
    succ(N1, N),
    !,
    skip_lines(N1, L0, L).
skip_lines(_, L, L).

