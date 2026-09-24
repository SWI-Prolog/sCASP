:- module(sscasp,
          [ sscasp/1,                   % :Query
            sscasp/2,                   % :Query, +Options
            sscasp_query_clauses/2,     % :Query, -Terms
            sscasp_query_clauses/3,     % :Query, -Terms, +Options
            sscasp_directive/1,         % :Directive
            sscasp_show/2,              % :Query, +What
            sscasp_json/2,              % :Query, -Dict
            sscasp_json/3,              % :Query, -Dict, +Options
            sscasp_human/1,             % :Query
            sscasp_human/2,             % :Query, +Options
            (sld)/1,                    % :PredicateIndicator
            (describe)/1,               % :Description
            (pred)/1,                   % :Signature or :Description
            (domain)/1,                 % :Definition

            (?)/1,                      % :Query
            (??)/1,                     % :Query
            (?--)/1,                    % :Query
            (?+-)/1,                    % :Query
            (?-+)/1,                    % :Query
            (?++)/1,                    % :Query
            (??+-)/1,                   % :Query
            (??-+)/1,                   % :Query
            (??++)/1,                   % :Query

            sscasp_readable/2,          % +Term, -Readable
            sscasp_readable/3,          % +Term, -Readable, +Options
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
            op(1150, fx,  sld),
            op(1150, fx,  describe),
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
            [ sscasp/1, sscasp/2,
              sscasp_query_clauses/2, sscasp_query_clauses/3,
              sscasp_directive/1,
              sscasp_show/2,
              sscasp_json/2, sscasp_json/3,
              (sld)/1,
              (describe)/1,
              (pred)/1,
              (domain)/1
            ]).
:- use_module(sscasp/dyncall, [sscasp_constraint/2]).
:- reexport(sscasp/html,
            [ html_model//2, html_justification//2,
              html_query//2, html_atom//2
            ]).
:- use_module(sscasp/annotations, [program_annotations/2]).
:- use_module(library(ansi_term), [ansi_format/3]).
:- use_module(library(option), [option/2, option/3, merge_options/3]).
:- reexport(sscasp/render,
            [ sscasp_readable/2, sscasp_readable/3,
              portray_model/1, portray_model/2,
              portray_justification/1, portray_justification/2
            ]).

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

`sscasp/2` is the predicate; the `?`-prefixed operators are the same thing
with the printing chosen for you, for use at a toplevel.  `sscasp/2` collects
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
a program, use `sscasp_human/1,2`, or `sscasp/2` with `portray_model/2` and
`portray_justification/2`.
*/

%!  user:term_expansion(+Term, -Expanded) is semidet.
%
%   A global constraint is a rule with no head.  `:- Body.` in a Prolog
%   file is a directive to run at load time, and `false/0` is a built-in
%   that cannot be given clauses, so `false :- Body.` is **recorded**
%   rather than compiled, and the clause disappears.
%
%   In `user` because that is where SWI looks: an expansion exported from
%   a library is not consulted for the module that imported it.  The old
%   implementation puts its own there for the same reason
%   (`scasp/dyncall.pl`, which maps `false :- Body` onto a clause of
%   `-/0`).

:- multifile user:term_expansion/2.

%   The body is qualified because the clause lives in `user`, where the
%   name would not resolve.

user:term_expansion((false :- Body), []) :-
    prolog_load_context(module, Module),
    sscasp_dyncall:sscasp_constraint(Module, Body).

%!  claimed_directive(+Term, -Expanded) is semidet.
%
%   `:- pred` is a directive **both** libraries claim, and the old one
%   expands it unconditionally (`scasp/dyncall.pl`, `user:term_expansion((:-
%   pred(SpecIn)), ...)`).  So in a process holding both — which is every
%   run of the test suite, and the point of the two trees having different
%   module names — a `:- pred p(dom)` written for this library reached
%   *that* expansion, which knows only the old `p(X) :: "..."` spelling and
%   raises on anything else.  Our `pred/1` was never called.
%
%   This claims it back, for a file that imported us: the expansion fires
%   only where `pred/1` resolves to ours, so a file using the old library
%   is left alone.  It is `asserta`-ed rather than written as a clause
%   because two `user:term_expansion/2` clauses are tried in **load
%   order**, and which library was loaded first is not something a program
%   should be able to feel.

claim_directives :-
    (   clause(user:term_expansion(_, _), sscasp:claimed_directive(_, _))
    ->  true
    ;   asserta((user:term_expansion(Term, Expanded) :-
                    sscasp:claimed_directive(Term, Expanded)))
    ).

claimed_directive((:- pred Spec), (:- sscasp:pred(Module:Spec))) :-
    nonvar(Spec),
    prolog_load_context(module, Module),
    ours(Module, pred(_)).

ours(Module, Head) :-
    predicate_property(Module:Head, imported_from(sscasp_dyncall)).

:- initialization(claim_directives, now).

%   `:` rather than `0`, for the reason `sscasp/dyncall.pl` gives: the
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
    sscasp_human(:),
    sscasp_human(:, +),
    solve_and_show(:, +).

?    Query :- sscasp(Query, [show_model(true)]).
??   Query :- sscasp(Query, [show_model(true), show_justification(true)]).
?--  Query :- sscasp(Query, []).
?+-  Query :- sscasp(Query, [show_model(true)]).
?-+  Query :- sscasp(Query, [show_justification(true)]).
?++  Query :- sscasp(Query, [show_model(true), show_justification(true)]).
??+- Query :- sscasp(Query, [show_model(human)]).
??-+ Query :- sscasp(Query, [show_justification(human)]).
??++ Query :- sscasp(Query, [show_model(human), show_justification(human)]).

		 /*******************************
		 *         THE TOPLEVEL		*
		 *******************************/

%!  sscasp_residuals// is det.
%
%   Add the model and the justification to the toplevel's answer.  They
%   are emitted as *goals* the toplevel then portrays, which is how the
%   old implementation does it (`scasp/embed.pl`) and why the variables
%   come out named: the toplevel names the whole answer — bindings,
%   model and the constraints still on the answer's variables — in one
%   go, so a variable two of them share is one name.
%
%   Nothing is emitted unless the query asked, which is what separates
%   `? Q` from `sscasp(Q)`.

:- residual_goals(sscasp_residuals).

sscasp_residuals -->
    { nb_current(sscasp_answer,
                 sscasp_answer(Model, Justification, Terms, Options))
    },
    !,
    shown(show_model(Model), Terms, Options),
    shown(show_justification(Justification), Terms, Options).
sscasp_residuals -->
    [].

shown(What, Terms, Options) -->
    { compound_name_arguments(What, Name, [Value]),
      Spec =.. [Name, How],
      option(Spec, Options),
      How \== false,
      !,
      how(How, Terms, ShowOptions),
      Goal =.. [Name, Value, ShowOptions]
    },
    [ Goal ].
shown(_, _, _) -->
    [].

%   `human` is the one that needs the program: a description is the
%   program's own and the table is built from the terms that were run.

how(human, Terms, [readable(true), human(true), annotations(Annotations)]) :-
    !,
    program_annotations(Terms, Annotations).
how(_, _, [readable(true), layout(columns)]).

user:portray(show_model(Model, Options)) :-
    ansi_format(comment, '% s(CASP) model~n', []),
    portray_model(Model, Options).
user:portray(show_justification(Justification, Options)) :-
    ansi_format(comment, '% s(CASP) justification~n', []),
    portray_justification(Justification, Options).

%!  sscasp_human(:Query) is nondet.
%!  sscasp_human(:Query, +Options) is nondet.
%
%   The same, said in words: each literal by its `:- describe` template
%   where the program wrote one, and by `sscasp/lang.pl`'s vocabulary
%   where it did not.  This is the old implementation's `--human`, and
%   it goes through the same HTML description — see `sscasp/html.pl`.
%
%   Unlike the `?`-shortcuts this **prints**, so it is the one to use
%   from a program.  Options are `sscasp/2`'s, plus `show_model(Boolean)`
%   and `show_justification(Boolean)`, both `true` here.

sscasp_human(Query) :-
    sscasp_human(Query, []).

sscasp_human(Query, Options) :-
    merge_options(Options, [show_model(true), show_justification(true)], All),
    solve_and_show(Query, [human(true)|All]).

%!  solve_and_show(:Query, +Model, +Justification) is nondet.
%
%   The model and the justification are made readable **together**, and
%   with the query: a variable shared between them has to get one name,
%   or the three printed terms say different things by the same letter.

solve_and_show(Query, Options) :-
    sscasp(Query, [model(Model), tree(Justification), program(Terms)|Options]),
    \+ \+ show(Query, Model, Justification, Terms, Options).

show(Query, Model, Justification, Terms, Options) :-
    sscasp_readable(answer(Query, Model, Justification),
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
