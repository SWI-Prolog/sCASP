:- module(scasp_verbose,
          [ verbose/1,                  % :Goal
            scasp_warning/1,            % +Term
            scasp_warning/2,            % +When, +Term
            scasp_trace_event/2,              % +When, +Term
            scasp_info/2,		% +When, +Term
            scasp_trace/2,              % :Goal, +Ports
            scasp_tracing/0,
            scasp_trace_goal/2,         % +Goal, :Call
            print_goal/1,               % +Goal
            print_check_calls_calling/2 % ?Goal, ?StackIn
          ]).
:- use_module(library(apply), [maplist/3, maplist/2]).
:- use_module(library(lists), [reverse/2]).
:- use_module(library(terms), [mapsubterms/3]).
:- use_module(library(clpqr/dump), [dump/3]).

:- use_module(clp/disequality, [get_neg_var/2]).
:- use_module(clp/clpq, [is_clpq_var/1]).
:- use_module(modules, [qualify_body/3, unqualify_model_term/3]).

:- meta_predicate
    verbose(0),
    scasp_trace(:, +),
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
    scasp_tracing_/4.                   % Goal, PortMask, MinTime, Seen

scasp_trace_goal(Goal, Wrapped) :-
    scasp_tracing(Goal, Mask, MinTime, RecordSeen),
    !,
    (   RecordSeen == true
    ->  seen_goal(Wrapped, Seen)
    ;   true
    ),
    parents(Parents),
    b_setval(scasp_trace_stack, [Goal|Parents]),
    length(Parents, Len),
    Depth is Len+1,
    statistics(cputime, T0),
    State = state(T0, 0),
    Trace = #{ mask: Mask,
               min_time: MinTime,
               depth: Depth,
               goal: Goal,
               seen: Seen
             },
    (   if_tracing(Trace, call, 0, 0),
        call(Wrapped),
        deterministic(Det),
        cpu_since(State, T),
        inc_answers(State, NAnswers),
        b_setval(scasp_trace_stack, Parents),
        if_tracing(Trace, exit, T, NAnswers),
        (   Det == true
        ->  !
        ;   (   true
            ;   if_tracing(Trace, redo, 0, NAnswers),
                statistics(cputime, R0),
                nb_setarg(1, State, R0),
                fail
            )
        )
    ;   cpu_since(State, T),
        arg(2, State, NAnswers),
        if_tracing(Trace, fail, T, NAnswers),
        fail
    ).
scasp_trace_goal(_Goal, Wrapped) :-
    call(Wrapped).

parents(Stack) :-
    nb_current(scasp_trace_stack, Stack), !.
parents([]).

cpu_since(state(T0, _), T) :-
    statistics(cputime, T1),
    T is T1-T0.

inc_answers(State, Answers) :-
    arg(2, State, Answers0),
    Answers1 is Answers0+1,
    nb_setarg(2, State, Answers1),
    Answers = Answers1.

if_tracing(Dict, Port, Time, Answers) :-
    _{ mask:Mask,
       min_time:MinTime
     } :< Dict,
    port(Port, PortMask),
    (   PortMask /\ Mask =\= 0,
        Time >= MinTime
    ->  Dict1 = Dict.put(_{port:Port, time:Time, answer:Answers}),
        print_message(debug, scasp(trace(Dict1)))
    ;   true
    ).

scasp_tracing(_M:Goal, Mask, MinTime, RecordSeen) :-
    scasp_tracing_(Tracing, Mask, MinTime, RecordSeen),
    subsumes_term(Tracing, Goal),
    !.

%!  scasp_trace(:Goal, +Ports) is det.
%
%   Enable tracing instances of the s(CASP) Goal.   Ports a single
%   port or a list of ports.   Each port may be prefixed with + or
%   - to enable or disable the port.  Valid ports are:
%
%     - call,redo,exit,fail
%       The normal Prolog ports
%     - a number
%       Show the port when the CPU time used to prove the goal
%       is at least the given number of milliseconds.  Setting
%       to `0` enables all ports
%     - seen
%       Record possibilities for caching/tabling.  This counts
%       the number of variants of the same goal, including the
%       current proof tree, we have seen.

scasp_trace(M:Goal, Ports) :-
    qualify(Goal, M, Goal1),
    scasp_trace_(Goal1, Ports).

scasp_trace_(Goal, Ports) :-
    clause(scasp_tracing_(Tracing, Mask0, MinTime0, RS0), true, Ref),
    Tracing =@= Goal,
    !,
    update_mask(Ports, Mask0, Mask, MinTime0, MinTime, RS0, RS),
    (   Mask =:= 0
    ->  erase(Ref)
    ;   transaction(( erase(Ref),
                      asserta(scasp_tracing_(Tracing, Mask, MinTime, RS))))
    ).
scasp_trace_(Goal, Ports) :-
    update_mask(Ports, 0, Mask, 0, MinTime, false, RS),
    (   Mask =:= 0
    ->  true
    ;   asserta(scasp_tracing_(Goal, Mask, MinTime, RS))
    ).

qualify(Var, _, QVar), var(Var) =>
    QVar = Var.
qualify(forall(Var,Goal), M, QForall) =>
    QForall = forall(Var, QGoal),
    qualify(Goal, M, QGoal).
qualify(not(Goal), M, QNot) =>
    QNot = not(QGoal),
    qualify(Goal, M, QGoal).
qualify(Goal, M, QGoal) =>
    qualify_body(Goal, M, QGoal).


update_mask([], M0, M, MT0, MT, RS0, RS) =>
    M = M0, MT = MT0, RS = RS0.
update_mask([H|T], M0, M, MT0, MT, RS0, RS) =>
    update_mask(H, M0, M1, MT0, MT1, RS0, RS1),
    update_mask(T, M1, M, MT1, MT, RS1, RS).
update_mask(+Port, M0, M, MT0, MT, RS0, RS), port(Port, Extra) =>
    M is M0 \/ Extra, MT = MT0, RS = RS0.
update_mask(-Port, M0, M, MT0, MT, RS0, RS), port(Port, Extra) =>
    M is M0 /\ \Extra, MT = MT0, RS = RS0.
update_mask(Port, M0, M, MT0, MT, RS0, RS),  port(Port, Extra) =>
    M is M0 \/ Extra, MT = MT0, RS = RS0.
update_mask(Time, M0, M, _,   MT, RS0, RS),  number(Time)      =>
    M = M0, MT is Time/1000.0, RS = RS0.
update_mask(seen, M0, M, MT0, MT, _RS0, RS) =>
    M = M0, MT = MT0, RS = true.
update_mask(-seen, M0, M, MT0, MT, _RS0, RS) =>
    M = M0, MT = MT0, RS = false.

port(call, 0x01).
port(redo, 0x02).
port(exit, 0x04).
port(fail, 0x08).
port(all,  0x0f).

seen_goal(_:solve_goal(Goal, M, _Parents,
                       ProvedIn, _ProvedOut,
                       _StackIn, _StackOut,
                       _Model),
          Seen) =>
    seen_goal_(t(Goal,M,ProvedIn), Seen).

seen_goal_(Goal, Seen) :-
    goal_sha1(Goal, SHA1),
    variant_trie(Trie),
    (   trie_lookup(Trie, SHA1, Seen)
    ->  Seen1 is Seen+1,
        trie_update(Trie, SHA1, Seen1)
    ;   trie_insert(Trie, SHA1, 1),
        Seen = 0
    ).

variant_trie(Trie) :-
    nb_current(scasp_variant_trie, Trie),
    !.
variant_trie(Trie) :-
    trie_new(Trie),
    nb_setval(scasp_variant_trie, Trie).

goal_sha1(Goal, SHA1) :-
    State = state(_),
    \+ \+ goal_sha1_(Goal, State),
    arg(1, State, SHA1).

goal_sha1_(Goal, State) :-
    term_attvars(Goal, AttVars),
    maplist(get_attrs, AttVars, Attrs),
    maplist(del_attrs, AttVars),
    variant_sha1(t(Goal,Attrs), SHA1),
    nb_setarg(1, State, SHA1).

scasp_tracing :-
    scasp_tracing_(_,_,_,_),
    !,
    print_message(information, scasp(tracing)),
    forall(scasp_tracing_(Goal, Mask, MinTime, RS),
           ( tracing(Goal, Mask, MinTime, RS, Tracing),
             print_message(information, scasp(Tracing)))).
scasp_tracing :-
    print_message(information, scasp(no_tracing)).


tracing(ScaspGoal, Mask, MinTime, RS,
        tracing(UserGoal, Ports, MinTime, RS)) :-
    unqualify_model_term(user, ScaspGoal, UserGoal),
    mask_to_ports(Mask, Ports).

mask_to_ports(0, []) :-
    !.
mask_to_ports(N, [H|T]) :-
    port(H, Mask),
    N /\ Mask =\= 0,
    !,
    N2 is N /\ \Mask,
    mask_to_ports(N2, T).


		 /*******************************
		 *            MESSAGES		*
		 *******************************/
:- multifile
    prolog:message//1.

prolog:message(scasp(Msg)) -->
    scasp_message(Msg).

:- discontiguous
    scasp_message//1.

scasp_message(trace(Data)) -->
    { _{ port: Port,
         goal: M:Goal,
         depth: Depth
       } :< Data,
      unqualify_model_term(M, Goal, UserGoal) },
    [ '[~D] '-[Depth] ],
    [ ansi(port(Port), '~w: ', [Port]) ],
    time(Port, Data),
    port_count(Port, Data),
    [ ansi(code, '~@', [scasp_verbose:print_goal(UserGoal)]) ].

time(call, _) -->
    !.
time(_Port, Data) -->
    { Ms is Data.time*1000.0 },
    [ '+~3fms '-[Ms] ].

port_count(call, Data) -->
    { Seen = Data.seen, nonvar(Seen), Seen > 0 },
    !,
    [ '#~D '-[Data.seen] ].
port_count(call, _Data) -->
    !.
port_count(_Port, Data) -->
    { Seen = Data.seen, nonvar(Seen), Seen > 0 },
    [ '#~D (called ~D times) '-[Data.answer, Seen] ].
port_count(_Port, Data) -->
    [ '#~D '-[Data.answer] ].

scasp_message(no_tracing) -->
    [ 'Not tracing any s(CASP) goals'-[] ].
scasp_message(tracing) -->
    [ 'Tracing the following s(CASP) goals:'-[], nl,nl ].
scasp_message(tracing(Goal, Ports, MinTime, Seen)) -->
    { numbervars(Goal, 0, _, [singletons(true)]) },
    [ ansi(code, '  ~p:', [Goal]), ' ~p'-[Ports] ],
    msg_min_time(MinTime),
    msg_seen(Seen).

msg_min_time(MinTime) -->
    { MinTime > 0,
      MS is MinTime*1000
    },
    !,
    [ ' >~1fms'-[MS] ].
msg_min_time(_) -->
    [].

msg_seen(false) -->
    [].
msg_seen(true) -->
    [ ' (count variant calls)' ].
