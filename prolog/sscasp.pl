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

:- module(sscasp,
          [ scasp/1,                   % :Query
            scasp/2,                   % :Query, +Options
            scasp_query_clauses/2,     % :Query, -Terms
            scasp_query_clauses/3,     % :Query, -Terms, +Options
            scasp_directive/1,         % :Directive
            scasp_show/2,              % :Query, +What
            scasp_json/2,              % :Query, -Dict
            scasp_json/3,              % :Query, -Dict, +Options
            scasp_human/1,             % :Query
            scasp_human/2,             % :Query, +Options
            (scasp_dynamic)/1,         % :Spec
            scasp_assert/1,            % :Clause
            scasp_assert/2,            % :Clause, +Position
            scasp_retract/1,           % :Clause
            scasp_retractall/1,        % :Head
            scasp_abolish/1,           % :PredicateIndicator
            (sld)/1,                    % :PredicateIndicator
            (describe)/1,               % :Description
            (pred)/1,                   % :Signature or :Description
            (domain)/1,                 % :Definition
            (#=)/2, (#<>)/2,            % ?A, ?B: the comparisons,
            (#<)/2, (#>)/2,             % for Prolog's sake
            (#=<)/2, (#>=)/2,

            (?)/1,                      % :Query
            (??)/1,                     % :Query
            (?--)/1,                    % :Query
            (?+-)/1,                    % :Query
            (?-+)/1,                    % :Query
            (?++)/1,                    % :Query
            (??+-)/1,                   % :Query
            (??-+)/1,                   % :Query
            (??++)/1,                   % :Query

            scasp_readable/2,          % +Term, -Readable
            scasp_readable/3,          % +Term, -Readable, +Options
            portray_model/1,            % +Model
            portray_model/2,            % +Model, +Options
            portray_justification/1,    % +Justification
            portray_justification/2,    % +Justification, +Options
            html_model//2,              % +Model, +Options
            html_justification//2,      % +Justification, +Options
            html_query//2,              % +Query, +Options
            html_atom//2,               % +Literal, +Options

            op(900,  fy,  not),
            op(700,  xfx, [#=, #<>, #<, #>, #=<, #>=]),
            op(700,  xfx, '∉'),
            op(700,  xfx, '∈'),
            op(700,  xfx, '| '),
            op(1150, fx,  domain),
            op(1150, fx,  pred),
            op(1150, fx,  abducible),   % `#abducible p` and `:- abducible p`
            op(1150, fx,  show),        % `#show p`
            op(1150, fx,  sld),
            op(1150, fx,  describe),
            op(1150, fx,  scasp_dynamic),
            op(1200, fx,  #),           % the legacy `#pred` spelling
            op(950,  xfx, ::),
            op(200,  xfx, ..),

            op(1150, fx,  ?),           % the query shortcuts
            op(1150, fx,  ??),
            op(1150, fx,  ?--),
            op(1150, fx,  ?+-),
            op(1150, fx,  ?-+),
            op(1150, fx,  ?++),
            op(1150, fx,  ??+-),
            op(1150, fx,  ??-+),
            op(1150, fx,  ??++)
          ]).
:- use_module(sscasp/ops).
:- reexport(sscasp/dyncall,
            [ scasp/1, scasp/2,
              scasp_query_clauses/2, scasp_query_clauses/3,
              scasp_directive/1,
              scasp_show/2,
              scasp_json/2, scasp_json/3,
              (scasp_dynamic)/1,
              scasp_assert/1, scasp_assert/2,
              scasp_retract/1, scasp_retractall/1, scasp_abolish/1,
              (sld)/1,
              (describe)/1,
              (pred)/1,
              (domain)/1,
              (#=)/2, (#<>)/2, (#<)/2, (#>)/2, (#=<)/2, (#>=)/2
            ]).
:- reexport(sscasp/html,
            [ html_model//2, html_justification//2,
              html_query//2, html_atom//2
            ]).
:- use_module(sscasp/dyncall, [constraint_clauses/2]).
:- use_module(sscasp/annotations, [program_annotations/2]).
:- use_module(library(ansi_term), [ansi_format/3]).
:- use_module(library(option), [option/2, merge_options/3]).
:- use_module(library(apply), [exclude/3]).
:- use_module(library(lists), [member/2]).
:- reexport(sscasp/render,
            [ scasp_readable/2, scasp_readable/3,
              portray_model/1, portray_model/2,
              portray_justification/1, portray_justification/2
            ]).

%   With the imports and before the first clause, because a
%   meta-predicate mode is part of the module's **signature**: a reader —
%   or a tool — taking the interface from the head of the file stops at
%   the first clause, and a declaration below that point changes how a
%   caller must pass an argument without saying so where the interface is
%   stated.
%
%   `:` rather than `0`, for the reason `sscasp/dyncall.pl` gives: a
%   query is a term to take apart, and expanding it as a goal where it is
%   written turns `-p(X)` into something else.

:- meta_predicate
    ?(:),
    ??(:),
    ?--(:),
    ?+-(:),
    ?-+(:),
    ?++(:),
    ??+-(:),
    ??-+(:),
    ??++(:),
    scasp_human(:),
    scasp_human(:, +),
    solve_and_show(:, +).

/** <module> s(CASP) from Prolog

Load this, write the program as ordinary Prolog clauses, and ask:

    :- use_module(library(sscasp)).

    bird(tweety).
    bird(polly).
    penguin(polly).
    flies(X) :- bird(X), not penguin(X).

    ?- ? flies(X).
    Model:
        bird(tweety)
        flies(tweety)
        not penguin(tweety)
    X = tweety.

`scasp/2` is the predicate; the `?`-prefixed operators are the same thing
with the printing chosen for you, for use at a toplevel.  `scasp/2` collects
the clauses the query reaches — see `sscasp/dyncall.pl` for what that means
and for how a program says the things Prolog has no syntax for.

## The shortcuts

Each asks for what its name says — the `+` and `-` are the **model** and the
**justification**, in that order — and the *toplevel* shows it, as part of
the answer:

| `?-- Q` | bindings only |
| `?+- Q`, `? Q` | and the model |
| `?-+ Q` | and the justification |
| `?++ Q`, `?? Q` | both |
| `??+- Q`, `??-+ Q`, `??++ Q` | the same, said in words |

They are ordinary nondeterministic goals: `;` at the toplevel asks for the
next answer, and shows it the same way.

**They show nothing outside a toplevel.**  The model reaches the terminal
through `residual_goals//0`, which is the toplevel's hook for adding to an
answer, and that is deliberate: the model then comes *after* the bindings,
and the toplevel names the variables of the answer and of the model
together — `¬ ab(_A)` and `_A` being anything but john or sam are one
variable, and a model printed on its own could not say so.  To print from
a program, use `scasp_human/1,2`, or `scasp/2` with `portray_model/2` and
`portray_justification/2`.
*/

%!  user:term_expansion(+Term, -Expanded) is semidet.
%
%   A global constraint is a rule with no head.  `:- Body.` in a Prolog
%   file is a directive to run at load time, and `false/0` is a built-in
%   that cannot be given clauses, so `false :- Body.` becomes a clause of
%   `'scasp$constraint'/1` in the same module instead — see
%   `sscasp/dyncall.pl:constraint_clauses/2`.
%
%   In `user` because that is where SWI looks: an expansion exported from
%   a library is not consulted for the module that imported it.  The old
%   implementation puts its own there for the same reason
%   (`scasp/dyncall.pl`, which maps `false :- Body` onto a clause of
%   `-/0`).

:- multifile user:term_expansion/2.

%!  claimed(?Term, ?Module, -Expanded) is nondet.
%
%   The spellings **both** libraries read, and what *this* one expands
%   each to in Module: a directive recording it, or for a global
%   constraint the clauses holding it.  Everything else about the spellings
%   is the same, which is why they are a table rather than separate
%   expansions.
%
%   | `:- pred Spec` | a signature here, a description in the old tree, whose expansion **raises** on this spelling rather than declining it |
%   | `:- scasp_dynamic Spec` | declares `p/N` and `-/1`; the old tree declares the interned `'-p'/N`, which this one never puts a clause in |
%   | `#pred Spec` | the spelling s(CASP) programs are written in — 84 in `test/le_programs/` alone |
%   | `#abducible Spec`, `:- abducible Spec` | an abducible; the old tree calls its own `abducible/1` |
%   | `#show Spec`, `:- show Spec` | what the model shows |
%   | `false :- Body` | a global constraint; the old tree makes it a clause of `-/0` |
%
%   Qualified with `sscasp_dyncall` because what the guard establishes is
%   that the file imported `scasp/2`, not that it imported these:
%   `use_module(library(sscasp), [scasp/2])` is a file we claim and in
%   which a bare `pred/1` would not resolve.

claimed((:- pred Spec),          M, (:- sscasp_dyncall:pred(M:Spec))) :-
    nonvar(Spec).
claimed((:- scasp_dynamic Spec), M, (:- sscasp_dyncall:scasp_dynamic(M:Spec))) :-
    nonvar(Spec).
claimed(#(pred(Spec)),           M, (:- sscasp_dyncall:Goal)) :-
    nonvar(Spec),
    template(Spec, M, Goal).
claimed(#(abducible(Spec)),      M, (:- sscasp_dyncall:scasp_directive(M:abducible(Spec)))) :-
    nonvar(Spec).
claimed((:- abducible Spec),     M, (:- sscasp_dyncall:scasp_directive(M:abducible(Spec)))) :-
    nonvar(Spec).
claimed(#(show(Spec)),           M, (:- sscasp_dyncall:scasp_directive(M:show(Spec)))) :-
    nonvar(Spec).
claimed((:- show Spec),          M, (:- sscasp_dyncall:scasp_directive(M:show(Spec)))) :-
    nonvar(Spec).
claimed((false :- Body),         _, Clauses) :-
    constraint_clauses(Body, Clauses).

%!  template(@Spec, ?Module, -Goal) is det.
%
%   `#pred p(X) :: 'text'` is a description and `#pred p(dom)` a
%   signature.  The description goes to `describe/1` rather than
%   `pred/1`, so it does **not** warn:
%   `source.pl:partition_terms/7` makes the same choice, because the
%   warning is for `:- pred X :: "..."` written where `:- describe` was
%   available and `#pred` is what the generator emits.

:- det(template/3).

template(Spec, M, Goal), Spec = (_ :: _) =>
    Goal = describe(M:Spec).
template(Spec, M, Goal) =>
    Goal = pred(M:Spec).

%!  ours(+Module) is semidet.
%
%   Module is being compiled against **this** library rather than the old
%   one.  Anything installed in `user` is consulted for every file in the
%   process, so it has to ask which library the file it is firing on
%   imported — `prolog_load_context(module, M)` for the file, and then
%   this.  `scasp/dyncall.pl:ours/0` asks the mirror-image question, so
%   the two never both fire and load order decides nothing.
%
%   `scasp/2` is the marker rather than the directive being claimed,
%   because it is what a file imports the library *for*: a file that has
%   `pred/1` has it by way of `scasp/2`, and a file that has neither is
%   not speaking to either library.  The property survives the re-export
%   through `library(sscasp)`, which is why it can be asked of the
%   importing module rather than of the library.

ours(Module) :-
    predicate_property(Module:scasp(_, _), imported_from(sscasp_dyncall)).

%   One expansion for all of them, and it is the table plus the guard.
%
%   Term first, because it discriminates and the other two are asked of
%   every term in every file otherwise.  `Module` is unbound while the
%   table builds the goal around it and
%   `prolog_load_context/2` binds it after.
%
%   Below both, because a clause of `user:term_expansion/2` is live from
%   the moment it is read: every term after it in *this* file goes
%   through it, and one placed above would call a predicate that does not
%   exist yet.

user:term_expansion(Term, Expanded) :-
    claimed(Term, Module, Expanded),
    prolog_load_context(module, Module),
    ours(Module).

?    Query :- scasp(Query, [show_model(true)]).
??   Query :- scasp(Query, [show_model(true), show_justification(true)]).
?--  Query :- scasp(Query, []).
?+-  Query :- scasp(Query, [show_model(true)]).
?-+  Query :- scasp(Query, [show_justification(true)]).
?++  Query :- scasp(Query, [show_model(true), show_justification(true)]).
??+- Query :- scasp(Query, [show_model(human)]).
??-+ Query :- scasp(Query, [show_justification(human)]).
??++ Query :- scasp(Query, [show_model(human), show_justification(human)]).

		 /*******************************
		 *         THE TOPLEVEL		*
		 *******************************/

%!  scasp_residuals// is det.
%
%   Add the model and the justification to the toplevel's answer.  They
%   are emitted as *goals* the toplevel then portrays, which is how the
%   old implementation does it (`scasp/embed.pl`) and why the variables
%   come out named: the toplevel names the whole answer — bindings,
%   model and the constraints still on the answer's variables — in one
%   go, so a variable two of them share is one name.
%
%   Nothing is emitted unless the query asked, which is what separates
%   `? Q` from `scasp(Q)`.

:- residual_goals(scasp_residuals).

scasp_residuals,
    nb_current(scasp_answer,
               scasp_answer(Model, Justification, Terms, Options)) ==>
    shown(show_model(Model), Terms, Options),
    shown(show_justification(Justification), Terms, Options).
scasp_residuals ==>
    [].

shown(What, Terms, Options),
    compound_name_arguments(What, Name, [Value]),
    Spec =.. [Name, How],
    option(Spec, Options),
    How \== false ==>
    { how(How, Terms, ShowOptions),
      Goal =.. [Name, Value, ShowOptions]
    },
    [ Goal ].
shown(_, _, _) ==>
    [].

%   `human` is the one that needs the program: a description is the
%   program's own and the table is built from the terms that were run.

how(human, Terms, ShowOptions) =>
    ShowOptions = [readable(true), human(true), annotations(Annotations)],
    program_annotations(Terms, Annotations).
how(_, _, ShowOptions) =>
    ShowOptions = [readable(true), layout(columns)].

user:portray(show_model(Model, Options)) :-
    ansi_format(comment, '% s(CASP) model~n', []),
    portray_model(Model, Options).
user:portray(show_justification(Justification, Options)) :-
    ansi_format(comment, '% s(CASP) justification~n', []),
    portray_justification(Justification, Options).

%!  scasp_human(:Query) is nondet.
%!  scasp_human(:Query, +Options) is nondet.
%
%   The same, said in words: each literal by its `:- describe` template
%   where the program wrote one, and by `sscasp/lang.pl`'s vocabulary
%   where it did not.  This is the old implementation's `--human`, and
%   it goes through the same HTML description — see `sscasp/html.pl`.
%
%   Unlike the `?`-shortcuts this **prints**, so it is the one to use
%   from a program.  Options are `scasp/2`'s, plus `show_model(Boolean)`
%   and `show_justification(Boolean)`, both `true` here.

scasp_human(Query) :-
    scasp_human(Query, []).

scasp_human(Query, Options) :-
    merge_options(Options, [show_model(true), show_justification(true)], All),
    solve_and_show(Query, [human(true)|All]).

%!  solve_and_show(:Query, +Options) is nondet.
%
%   The model and the justification are made readable **together**, and
%   with the query: a variable shared between them has to get one name,
%   or the three printed terms say different things by the same letter.
%
%   @arg Options are scasp_human/2's, plus `human(Boolean)` to say the
%       answer in words.

solve_and_show(Query, Options) :-
    exclude(shown_here, Options, SolveOptions),
    scasp(Query, [model(Model), tree(Justification), program(Terms)
                 |SolveOptions]),
    \+ \+ show(Query, Model, Justification, Terms, Options).

%   What this prints is not handed on to scasp/2, which would leave it
%   for scasp_residuals//0 and the toplevel would print it again.

shown_here(show_model(_)).
shown_here(show_justification(_)).

show(Query, Model, Justification, Terms, Options) :-
    scasp_readable(answer(Query, Model, Justification),
                    answer(_, Readable, Because)),
    said_how(Terms, Options, How),
    (   option(show_model(true), Options)
    ->  format("Model:~n"),
        portray_model(Readable, How),
        nl
    ;   true
    ),
    (   option(show_justification(true), Options)
    ->  format("Justification:~n"),
        portray_justification(Because, How),
        nl
    ;   true
    ).

%   The annotation table is built only where something will read it: it
%   is the program's descriptions, and the machine form says the literal
%   itself whatever the program called it.

said_how(Terms, Options, How) :-
    (   option(human(true), Options)
    ->  program_annotations(Terms, Annotations),
        How = [readable(true), human(true), annotations(Annotations)]
    ;   How = [readable(true)]
    ).


		 /*******************************
		 *            SANDBOX		*
		 *******************************/

%   What SWISH may run.  The query shortcuts and `scasp/1,2` are safe
%   because the solver runs nothing of the program's but its logic: the
%   collected clauses are compiled, not called, and what the compiled
%   program calls is `runtime.pl` and limited arithmetic.  A qualified
%   goal `lists:append(...)` does reach another module — the collector
%   follows it and compiles *that* module's clauses — which reads code
%   rather than running it.  The one way out to Prolog is `:- sld`, and
%   the directive that declares one is not on this list.
%
%   The option `module(M)` is refused: it asserts the compiled program
%   into M and adds `sscasp_runtime` to M's imports for good, which in
%   `module(lists)` is exactly what a sandbox is there to stop.  So an
%   option list is accepted only as a proper list without it.
%
%   Three flags rather than the old library's seven, because the rest do
%   not exist here: `scasp_forall` selects among four algorithms and this
%   tree has one, dynamic consistency checking is `sscasp_dcc` rather than
%   `scasp_dcc`, `scasp_plain_dual` is unimplemented, and
%   `scasp_unknown` and `scasp_lang` have no reader yet.  A flag listed
%   before something reads it would be a promise about behaviour that is
%   not there.

:- multifile
    sandbox:safe_meta/2,
    sandbox:safe_prolog_flag/2,
    sandbox:safe_directive/1.

sandbox:safe_meta(sscasp:(?    _), []).
sandbox:safe_meta(sscasp:(??   _), []).
sandbox:safe_meta(sscasp:(?--  _), []).
sandbox:safe_meta(sscasp:(?+-  _), []).
sandbox:safe_meta(sscasp:(?-+  _), []).
sandbox:safe_meta(sscasp:(?++  _), []).
sandbox:safe_meta(sscasp:(??+- _), []).
sandbox:safe_meta(sscasp:(??-+ _), []).
sandbox:safe_meta(sscasp:(??++ _), []).
sandbox:safe_meta(sscasp:scasp_human(_), []).
sandbox:safe_meta(sscasp:scasp_human(_, Options), []) :-
    safe_options(Options).

sandbox:safe_meta(sscasp_dyncall:scasp(_), []).
sandbox:safe_meta(sscasp_dyncall:scasp(_, Options), []) :-
    safe_options(Options).
sandbox:safe_meta(sscasp_dyncall:scasp_show(_, _), []).
sandbox:safe_meta(sscasp_dyncall:scasp_json(_, _), []).
sandbox:safe_meta(sscasp_dyncall:scasp_json(_, _, Options), []) :-
    safe_options(Options).
sandbox:safe_meta(sscasp_dyncall:scasp_query_clauses(_, _), []).
sandbox:safe_meta(sscasp_dyncall:scasp_query_clauses(_, _, Options), []) :-
    safe_options(Options).

sandbox:safe_directive(sscasp_dyncall:Goal) :-
    safe_declaration(Goal).
sandbox:safe_directive(M:Goal) :-
    prolog_load_context(module, M),
    callable(Goal),
    predicate_property(M:Goal, imported_from(sscasp_dyncall)),
    safe_declaration_in(Goal, M).

safe_declaration(Goal) :-
    callable(Goal),
    Goal =.. [Name, Module:Arg],
    prolog_load_context(module, Module),
    Goal0 =.. [Name, Arg],
    safe_declaration_in(Goal0, Module).

safe_declaration_in(scasp_directive(Directive), _) :-
    nonvar(Directive),
    \+ Directive = sld(_).
safe_declaration_in(describe(_), _).
safe_declaration_in(pred(_), _).
safe_declaration_in(domain(_), _).
safe_declaration_in(scasp_dynamic(_), _).

safe_options(Options) :-
    is_list(Options),
    \+ ( member(Option, Options),
          ( var(Option) ; Option = module(_) )
        ).

%   Not the program-changing half: `scasp_assert/1` writes clauses into
%   the calling module, which is exactly what a sandbox is there to stop.
%
%   The **directives** are another matter: each writes only into the
%   module being loaded — a `'scasp$directive'/1` clause, or `dynamic`
%   for `:- scasp_dynamic` — which is what a sandboxed program is allowed
%   to do to itself.  They reach the sandbox in two shapes: expanded by
%   claimed/3 to `sscasp_dyncall:Goal`, which it takes for a cross-module
%   directive, and called as they stand, `M:describe(Spec)`, where the
%   work happens in code it would then walk and refuse.  Either is
%   allowed when it names the module being loaded — except `:- sld`,
%   whose whole point is to run Prolog the sandbox has not checked.


sandbox:safe_prolog_flag(scasp_compile_nmr, _).
sandbox:safe_prolog_flag(scasp_compile_olon, _).
sandbox:safe_prolog_flag(sscasp_dcc, _).
