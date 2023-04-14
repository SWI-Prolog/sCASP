:- module(scasp_verbose,
          [ verbose/1,                  % :Goal
            scasp_warning/1,            % +Term
            scasp_warning/2,            % +When, +Term
            scasp_trace/2,              % +When, +Term
            scasp_info/2,		% +When, +Term
            print_goal/1,               % +Goal
            print_check_calls_calling/2 % ?Goal, ?StackIn
          ]).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(clpqr/dump)).

:- use_module(clp/disequality).
:- use_module(clp/clpq).
:- use_module(library(terms)).

:- meta_predicate
    verbose(0).

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

%!  scasp_trace(+When, +Term) is det.
%
%   Emit a debug messages through print_message/2.

scasp_trace(When, Term) :-
    current_prolog_flag(When, true),
    !,
    print_message(debug, scasp(Term)).
scasp_trace(_, _).

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
