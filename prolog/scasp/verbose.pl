:- module(scasp_verbose,
          [ verbose/1,                  % :Goal
            scasp_warning/1,            % +Term
            scasp_warning/2,            % +When, +Term
            scasp_trace_event/2,              % +When, +Term
            scasp_info/2,		% +When, +Term
            scasp_trace/2,              % +Goal, +Ports
            scasp_trace_goal/2,         % +Goal, :Call
            print_goal/1,               % +Goal
            print_check_calls_calling/2 % ?Goal, ?StackIn
          ]).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(clpqr/dump)).
:- use_module(library(terms)).

:- use_module(clp/disequality).
:- use_module(clp/clpq).
:- use_module(modules).

:- meta_predicate
    verbose(0),
    scasp_trace_goal(:, 0).

/** <module> Print goal and stack in Ciao compatible format

This module prints the goal  and  stack  in   as  close  as  we can Ciao
compatible format such tha we can compare   the traces created by

    scasp -v program.pl
*/

:- create_prolog_flag(scasp_verbose,        false, []).
:- create_prolog_flag(scasp_warnings,       false, []).
:- create_prolog_flag(scasp_warn_pos_loops, false, []).
:- create_prolog_flag(scasp_trace_failures, false, []).

verbose(Goal) :-
    current_prolog_flag(scasp_verbose, true),
    !,
    with_output_to(user_error, call(Goal)).
verbose(_).

%!  scasp_warning(+Term) is det.
%
%   Emit a warning through print_message/2.

scasp_warning(Term) :-
    current_prolog_flag(scasp_warnings, true),
    !,
    print_message(warning, scasp(Term)).
scasp_warning(_).

%!  scasp_warning(+When, +Term) is det.
%
%   Emit a warning through print_message/2.

scasp_warning(When, Term) :-
    current_prolog_flag(When, true),
    !,
    print_message(warning, scasp(Term)).
scasp_warning(_, _).

%!  scasp_trace_event(+When, +Term) is det.
%
%   Emit a debug messages through print_message/2.

scasp_trace_event(When, Term) :-
    current_prolog_flag(When, true),
    !,
    print_message(debug, scasp(Term)).
scasp_trace_event(_, _).

%!  scasp_info(+When, +Term) is det.
%
%   Emit an informational through print_message/2.

scasp_info(When, Term) :-
    current_prolog_flag(When, true),
    !,
    print_message(informational, scasp(Term)).
scasp_info(_, _).

%!  print_check_calls_calling(?Goal, ?StackIn)
%
%   Auxiliar predicate to print StackIn the current stack and Goal. This
%   predicate is executed when the flag `check_calls` is _on_. NOTE: use
%   check_calls/0 to activate the flag

:- det(print_check_calls_calling/2).

print_check_calls_calling(Goal, I) :-
    fail,                               % TBD: New Ciao -v mode
    !,
    identation(I, 0, Ident),
    format('(~d) ~@~n', [Ident, print_goal(Goal)]).
print_check_calls_calling(Goal, I) :-
    reverse(I,RI),
    format('\n--------------------- Calling: ~@ -------------',
           [print_goal(Goal)]),
    print_check_stack(RI,4), !,
    nl.

identation([],Id,Id).
identation([[]|I],Id1,Id) :- !,
    Id2 is Id1 - 1,
    identation(I,Id2,Id).
identation([_|I],Id1,Id) :- !,
    Id2 is Id1 + 1,
    identation(I,Id2,Id).


%!  print_check_stack(A, B)
%
%   simple output of the stack to run faster during verboser

print_check_stack([],_).
print_check_stack([[]|As],I) :- !,
    I1 is I - 4,
    print_check_stack(As,I1).
print_check_stack([A|As],I) :-
    nl, tab(I),
    print_goal(A),
    I1 is I + 4,
    print_check_stack(As,I1).

:- multifile user:portray/1.

user:portray('G'(Goal)) :-
    print_goal(Goal).

%!  print_goal(+Goal)
%
%   Print an sCASP goal. The first clause   does  the actual work at the
%   moment to emit the goal as closely as we can to the Ciao output such
%   that we can compare traces created   using  ``scasp -v``. The second
%   uses default notation for constraints.

print_goal(goal_origin(Goal, _)) :- !,
    print_goal(Goal).
print_goal(Goal) :- !,
    ciao_goal(Goal, Ciao),
    print(Ciao).

ciao_goal(Goal, Ciao) :-
    strip_goal_origin(Goal, Goal1),
    copy_term(Goal1, Ciao),
    term_attvars(Ciao, AttVars),
    maplist(ciao_constraints, AttVars, Constraints),
    maplist(del_attrs, AttVars),
    maplist(ciao_attvar, AttVars, Constraints).

strip_goal_origin(StackIn, StackInCiao) :-
    mapsubterms(strip_goal_origin_, StackIn, StackInCiao).

strip_goal_origin_(goal_origin(Goal, _Origin), Goal).

:- use_module(library(clpqr/dump), [dump/3]).

ciao_constraints(Var, Constraints) :-
    (   is_clpq_var(Var),
        dump([Var], [NV], Constraints0),
        Constraints0 \== []
    ->  Constraints = NV-Constraints0
    ;   get_neg_var(Var, List),
        List \== []
    ->  Constraints = neg(_NV, List)
    ;   Constraints = []
    ).

:- op(700, xfx, user:'~').
:- op(700, xfx, ~).

ciao_attvar(_, []) :- !.
ciao_attvar({NV~Constraints}, NV-Constraints) :- !.
ciao_attvar({'\u2209'(Var, List)}, neg(Var, List)).


		 /*******************************
		 *        TRACING GOALS		*
		 *******************************/

:- dynamic
    scasp_tracing_/3.                   % Goal, PortMask, MinTime

scasp_trace_goal(Goal, Wrapped) :-
    scasp_tracing(Goal, Mask, MinTime),
    !,
    statistics(cputime, T0),
    (   if_tracing(Mask, call, MinTime, 0, Goal),
        call(Wrapped),
        deterministic(Det),
        statistics(cputime, T1),
        T is T1-T0,
        if_tracing(Mask, exit, MinTime, T, Goal),
        (   Det == true
        ->  !
        ;   (   true
            ;   if_tracing(Mask, redo, MinTime, T, Goal),
                fail
            )
        )
    ;   statistics(cputime, T1),
        T is T1-T0,
        if_tracing(Mask, fail, MinTime, T, Goal),
        fail
    ).
scasp_trace_goal(_Goal, Wrapped) :-
    call(Wrapped).

if_tracing(Mask, Port, MinTime, Time, Goal) :-
    port(Port, PortMask),
    (   PortMask /\ Mask =\= 0,
        Time >= MinTime
    ->  print_message(debug, scasp(trace(Port, Time, Goal)))
    ;   true
    ).

scasp_tracing(Goal, Mask, MinTime) :-
    scasp_tracing_(Tracing, Mask, MinTime),
    subsumes_term(Tracing, Goal),
    !.

scasp_trace(Goal, Ports) :-
    clause(scasp_tracing_(Tracing, Mask0, MinTime0), true, Ref),
    !,
    Tracing =@= Goal,
    !,
    update_mask(Ports, Mask0, Mask, MinTime0, MinTime),
    transaction(( erase(Ref),
                  asserta(scasp_tracing_(Tracing, Mask, MinTime)))).
scasp_trace(Goal, Ports) :-
    update_mask(Ports, 0, Mask, 0, MinTime),
    asserta(scasp_tracing_(Goal, Mask, MinTime)).


update_mask([], M0, M, MT, MT) => M = M0.
update_mask([H|T], M0, M, MT0, MT) => update_mask(H, M0, M1, MT0, MT1), update_mask(T, M1, M, MT1, MT).
update_mask(+Port, M0, M, MT0, MT), port(Port, Extra) => M is M0 \/ Extra, MT = MT0.
update_mask(-Port, M0, M, MT0, MT), port(Port, Extra) => M is M0 /\ \Extra, MT = MT0.
update_mask(Port, M0, M, MT0, MT),  port(Port, Extra) => M is M0 \/ Extra, MT = MT0.
update_mask(Time, M0, M, _,   MT),  number(Time)      => M is M0, MT is Time/1000.0.

port(call, 0x01).
port(redo, 0x02).
port(exit, 0x04).
port(fail, 0x08).
port(all,  0x0f).


		 /*******************************
		 *            MESSAGES		*
		 *******************************/
:- multifile
    prolog:message//1.

prolog:message(scasp(Msg)) -->
    scasp_message(Msg).

scasp_message(trace(Port, Time, M:Goal)) -->
    { unqualify_model_term(M, Goal, UserGoal) },
    [ ansi(port(Port), '~w: ', [Port]) ],
    time(Port, Time),
    [ ansi(code, '~@', [scasp_verbose:print_goal(UserGoal)]) ].

time(call, _) -->
    !.
time(_Port, Time) -->
    { Ms is Time*1000.0 },
    [ '+~3fms '-[Ms] ].
