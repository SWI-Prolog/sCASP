:- module(scasp,
          [ scasp/1,                    % :Goal
            scasp/2,                    % :Goal, +Options
            (?)/1,                      % :Query
            (??)/1,                     % :Query

            scasp_show/2,               % :Query,+What

            (scasp_dynamic)/1,          % :Spec
            scasp_assert/1,             % :Clause
            scasp_retract/1,            % :Clause
            scasp_retractall/1,         % :Head
            scasp_abolish/1,            % :Name/Arity
            (#)/1,                      % :Directive
            (pred)/1,                   % :Templates
            (show)/1,                   % :Atoms
            (abducible)/1,              % :Heads

            begin_scasp/1,              % +Unit
            begin_scasp/2,              % +Unit, +Exports
            end_scasp/0,
            scasp_listing/2,            % +Unit, +Options
            scasp_model/1,              % :Model
            scasp_stack/1,              % -Stack
            scasp_justification/2,      % -Tree, +Options
            (not)/1,                    % :Query
            (-)/1,                      % :Query

            op(900,  fy, not),
            op(700, xfx, '\u2209'),     % not element of
            op(1150, fx, ??),
            op(1150, fx, ?),
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

:- meta_predicate
    ?(:),
    ??(:).

%!  ??(:Query)
%
%   Shorcut for scasp/1 that prints the justification.

?? Q :-
    current_prolog_flag(scasp_show_justification, Old),
    setup_call_cleanup(
        set_prolog_flag(scasp_show_justification, true),
        scasp(Q),
        set_prolog_flag(scasp_show_justification, Old)).

%!  ?(:Query)
%
%   Shorcut for scasp/1 that only prints the model.

? Q :-
    current_prolog_flag(scasp_show_justification, Old),
    setup_call_cleanup(
        set_prolog_flag(scasp_show_justification, false),
        scasp(Q),
        set_prolog_flag(scasp_show_justification, Old)).



