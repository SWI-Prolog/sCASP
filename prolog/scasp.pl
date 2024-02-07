:- module(scasp,
          [ scasp/2,                    % :Goal, +Options
            (?)/1,                      % :Query
            (??)/1,                     % :Query
            (?--)/1,                    % :Query
            (?+-)/1,                    % :Query
            (?-+)/1,                    % :Query
            (?++)/1,                    % :Query
            (?+++)/1,                    % :Query
            (??+-)/1,                   % :Query
            (??-+)/1,                   % :Query
            (??++)/1,                   % :Query

            scasp_show/2,               % :Query, +What
            scasp_trace/2,              % :Goal, +Condition

            (scasp_dynamic)/1,          % :Spec
            scasp_assert/1,             % :Clause
            scasp_assert/2,             % :Clause, +Pos
            scasp_retract/1,            % :Clause
            scasp_retractall/1,         % :Head
            scasp_abolish/1,            % :Name/Arity
            (#)/1,                      % :Directive
            (#)/2,                      % :Directive, +Pos
            (pred)/1,                   % :Templates
            (show)/1,                   % :Atoms
            (abducible)/1,              % :Heads
            (abducible)/2,              % :Heads, +Pos

            begin_scasp/1,              % +Unit
            begin_scasp/2,              % +Unit, +Exports
            end_scasp/0,
            scasp_listing/2,            % +Unit, +Options
            scasp_model/1,              % :Model
            scasp_stack/1,              % -Stack
            scasp_justification/2,      % -Tree, +Options
            (not)/1,                    % :Query
            (-)/1,                      % :Query

            (#=)/2,
            (#<>)/2,
            (#<)/2,
            (#>)/2,
            (#=<)/2,
            (#>=)/2,
            '\u2209'/2,                 % Inequality

            op(900,  fy, not),
            op(700, xfx, '\u2209'),     % not element of
            op(1150, fx, ??),           % same as ?++
            op(1150, fx, ?),            % same as ?+-
            op(1150, fx, ?--),          % bindings only
            op(1150, fx, ?+-),          % bindings + model
            op(1150, fx, ?-+),          % bindings + tree
            op(1150, fx, ?++),          % bindings + model + tree
            op(1150, fx, ?+++),         % bindings + model + tree
            op(1150, fx, ??+-),         % Human versions of the above
            op(1150, fx, ??-+),
            op(1150, fx, ??++),
            op(950, xfx, ::),           % pred not x :: "...".
            op(1200, fx, #),
            op(1150, fx, pred),
            op(1150, fx, show),
            op(1150, fx, abducible),
            op(1150, fx, scasp_dynamic),
            op(700, xfx, #=),
            op(700, xfx, #<>),
            op(700, xfx, #<),
            op(700, xfx, #>),
            op(700, xfx, #=<),
            op(700, xfx, #>=)
          ]).

/** <module> Using s(CASP) from Prolog

While library(scasp/main) is used  to   build  the ``scasp`` executable,
this library (library(scasp)) is used  to   embed  or dynamically create
s(CASP) programs in Prolog and query them from Prolog.
*/

%:- set_prolog_flag(optimise, true).

:- use_module(scasp/embed).
:- use_module(scasp/dyncall).
:- use_module(scasp/messages).
:- use_module(scasp/verbose).

:- meta_predicate
    ?(:),
    ??(:),
    ?--(:),
    ?+-(:),
    ?-+(:),
    ?++(:),
    ?+++(:),
    ??+-(:),
    ??-+(:),
    ??++(:).

%!  ?--(:Query).
%!  ?+-(:Query).
%!  ?-+(:Query).
%!  ?++(:Query).
%!  ?+++(:Query).
%!  ??+-(:Query).
%!  ??-+(:Query).
%!  ??++(:Query).
%
%   Shortcuts for scasp/1 that control printing   the  model and/or tree
%   and the format. The +/- control whether   the  model and/or tree are
%   printed (in that order). The ?? versions print the human version.

?   Q :- scasp_and_show(Q, unicode, false, []).
??  Q :- scasp_and_show(Q, unicode, unicode, []).

?--  Q :- scasp_and_show(Q, false, false, []).
?-+  Q :- scasp_and_show(Q, false, unicode, []).
?+-  Q :- scasp_and_show(Q, unicode, false, []).
?++  Q :- scasp_and_show(Q, unicode, unicode, []).
?+++ Q :- scasp_and_show(Q, unicode, [unicode(true), long(true)], []).
??-+ Q :- scasp_and_show(Q, false, human, []).
??+- Q :- scasp_and_show(Q, human, false, []).
??++ Q :- scasp_and_show(Q, human, human, []).

scasp_and_show(Q, Model, Tree, Options) :-
    scasp_mode(M0, T0),
    setup_call_cleanup(
        set_scasp_mode(Model, Tree),
        (   scasp(Q, Options)
        ;   false                       % make always nondet.
        ),
        set_scasp_mode(M0, T0)).

scasp_mode(M, T) :-
    current_prolog_flag(scasp_show_model, M),
    current_prolog_flag(scasp_show_justification, T).

set_scasp_mode(M, T) :-
    set_prolog_flag(scasp_show_model, M),
    set_prolog_flag(scasp_show_justification, T).



		 /*******************************
		 *            SANDBOX		*
		 *******************************/

:- multifile
    sandbox:safe_meta_predicate/1,
    sandbox:safe_prolog_flag/2.

sandbox:safe_meta(scasp:(? _), []).
sandbox:safe_meta(scasp:(?? _), []).
sandbox:safe_meta(scasp:(?-- _), []).
sandbox:safe_meta(scasp:(?+- _), []).
sandbox:safe_meta(scasp:(?-+ _), []).
sandbox:safe_meta(scasp:(?++ _), []).
sandbox:safe_meta(scasp:(??+- _), []).
sandbox:safe_meta(scasp:(??-+ _), []).
sandbox:safe_meta(scasp:(??++ _), []).
sandbox:safe_meta(scasp_dyncall:(scasp_show(_,_)), []).

sandbox:safe_prolog_flag(scasp_lang, _).
sandbox:safe_prolog_flag(scasp_unknown, _).
sandbox:safe_prolog_flag(scasp_plain_dual, _).
sandbox:safe_prolog_flag(scasp_compile_olon, _).
sandbox:safe_prolog_flag(scasp_compile_nmr, _).
sandbox:safe_prolog_flag(scasp_forall, _).
sandbox:safe_prolog_flag(scasp_dcc, _).

