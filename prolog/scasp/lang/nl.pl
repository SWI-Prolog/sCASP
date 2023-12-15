:- module(casp_lang_nl,
          [ scasp_message//1
          ]).
:- use_module(library(dcg/high_order)).
:- use_module('../ops', [op(_,_,_)]).
:- use_module(library(lists), [reverse/2]).
:- use_module(library(prolog_code), [comma_list/2]).

:- multifile
    scasp_messages:scasp_lang_module/2.

scasp_messages:scasp_lang_module(nl, casp_lang_nl).


		 /*******************************
		 *            SCASP		*
		 *******************************/

scasp_message(version(Version, PrologVersion)) -->
    [ 'versie ~w op SWI-Prolog ~w'-[Version, PrologVersion] ].

% Usage messages

scasp_message(source_not_found(Source)) -->
    (   \+ { access_file(Source, exist) }
    ->  [ 'Invoer bestand '-[] ], code(Source), [ ' bestaat niet'-[] ]
    ;   [ 'Kan invoer bestand '-[] ], code(Source), [ ' niet lezen'-[] ]
    ).
scasp_message(no_input_files) -->
    [ 'Geen invoer gespecificeerd!' ].
scasp_message(no_query) -->
    [ 'Het programma bevat geen ?- Query.'-[] ].
scasp_message(undefined_operator(Op)) -->
    [ 'clp operator ~p is niet gedefinieerd'-[Op] ].
scasp_message(at_most_one_of([A,B])) -->
    ['Opties '], opt(A), [' and '], opt(B),
    [' gaan niet samen' ].
scasp_message(at_most_one_of(List)) -->
    [ 'Maximaal een van de opties '-[] ],
    options(List),
    [ ' kan gelijktijdig gebruikt worden.'-[] ].
scasp_message(opt_dcc_prev_forall) -->
    [ 'Optie --dcc kan alleen samen met --forall=prev' ].
scasp_message(opt_incompatible(Opt1, Opt2)) -->
    [ 'Optie ' ], opt(Opt1), [' gaat niet samen met '], opt(Opt2).

% Solver messages

scasp_message(failure_calling_negation(Goal)) -->
    [ 'Negatie van '-[] ], goal(Goal), [ ' faalt'-[] ].
scasp_message(co_failing_in_negated_loop(Goal, NegGoal)) -->
    [ 'Co-Failing in a negated loop due to a variant call'-[], nl,
      '(extension clp-disequality required).'-[]
    ],
    curr_prev_goals(Goal, NegGoal).
scasp_message(variant_loop(Goal, PrevGoal)) -->
    [ 'Failing in a positive loop due to a variant call (tabling required).'-[]
    ],
    curr_prev_goals(Goal, PrevGoal).
scasp_message(subsumed_loop(Goal, PrevGoal)) -->
    [ 'Failing in a positive loop due to a subsumed call under clp(q).'-[]
    ],
    curr_prev_goals(Goal, PrevGoal).
scasp_message(pos_loop(fail, Goal, PrevGoal)) -->
    [ 'Positive loop failing '-[] ],
    eq_goals(Goal, PrevGoal).

scasp_message(pos_loop(continue, Goal, PrevGoal)) -->
    [ 'Positive loop continuing '-[] ],
    eq_goals(Goal, PrevGoal).
scasp_message(trace_failure(Goal, Stack)) -->
    print_check_calls_calling(Goal, Stack),
    [ ansi(warning, 'FAILURE to prove the literal: ', []) ],
    goal(Goal).

scasp_message(dcc_call(Goal, Stack)) -->
    [ 'DCC of ' ], goal(Goal),
    [ ' in ' ], print_stack(Stack).
scasp_message(dcc_discard(Goal, BodyL)) -->
    { comma_list(Body, BodyL) },
    [ 'DCC discards '], goal(Goal),
    [ ' when checking nmr ~p'-[ dcc(Goal) :- Body ] ].

% Results

scasp_message(no_models(CPU)) -->
    [ 'Geen modellen (~3f seconden)'-[CPU] ].


% Justifications

scasp_message(and)       --> [ 'en' ].
scasp_message(or)        --> [ 'of' ].
scasp_message(not)       --> [ 'er is geen bewijs dat' ].
scasp_message(-)         --> [ 'het is niet het geval dat' ].
scasp_message(implies)   --> [ 'omdat' ].
scasp_message(?)         --> [ '?' ].
scasp_message(proved)    --> ['als hierboven aangetoond'].
scasp_message(chs)       --> ['het is aangenomen dat'].
scasp_message(assume)    --> ['we nemen aan dat'].
scasp_message(holds)     --> [' is waar'].
scasp_message(holds_for) --> [' is waar voor '].
scasp_message(not_in)    --> ['niet zijnde'].
scasp_message('\u2209'(_,_)) --> ['niet zijnde'].
scasp_message(neq)       --> ['ongelijk aan'].
scasp_message(_>_)       --> ['is groter dan'].
scasp_message(_>=_)      --> ['is groter dan of gelijk aan'].
scasp_message(_<_)       --> ['is kleiner dan'].
scasp_message(_=<_)      --> ['is kleiner dan of gelijk aan'].
scasp_message(_#=_)      --> ['gelijk aan'].
scasp_message(_#<>_)     --> ['ongelijk aan'].
scasp_message(_#>_)      --> ['groter dan'].
scasp_message(_#>=_)     --> ['groter dan of gelijk aan'].
scasp_message(_#<_)      --> ['kleiner dan'].
scasp_message(_#=<_)     --> ['kleiner dan of gelijk aan'].
scasp_message(global_constraints_hold) -->
    [ 'Aan alle globale restricties is voldaan' ].
scasp_message(global_constraint(N)) -->
    [ 'Aan de globale restrictie nummer ', N, ' is voldaan' ].
scasp_message(abducible) -->
    [ 'middels abductie concluderen we dat' ].
scasp_message(according_to) --> [ 'volgens' ].



		 /*******************************
		 *       GOALS AND STACKS	*
		 *******************************/

print_check_calls_calling(Goal, Stack) -->
    [ansi(bold, '~`-t Calling: ~@ ~`-t~72|', [scasp_verbose:print_goal(Goal)]), nl],
    print_stack(Stack).

%!  print_stack(+Stack)//
%
%   This is a DCG version of print_check_stack/2 from verbose.pl

print_stack(Stack) -->
    { reverse(Stack, RevStack) },
    print_stack(RevStack, 4).

print_stack([], _) -->
    [].
print_stack([[]|As],I) -->
    !,
    { I1 is I - 4 },
    print_stack(As, I1).
print_stack([A|As],I) -->
    ['~t~*|'-[I]], goal(A), [ nl ],
    { I1 is I + 4 },
    print_stack(As,I1).

eq_goals(Goal, PrevGoal) -->
    [ '(Goal '-[] ], goal(Goal), [ ' == '-[] ], goal(PrevGoal), [')'-[]].

curr_prev_goals(Goal, NegGoal) -->
    [ nl,
      '    Current call:  '-[] ], goal(Goal), [ nl,
      '    Previous call: '-[] ], goal(NegGoal).

goal(Goal) -->
    [ ansi(code, '~@', [scasp_verbose:print_goal(Goal)]) ].


		 /*******************************
		 *             UTIL		*
		 *******************************/

options(Values) -->
    sequence(opt, [', '-[]], Values).

opt(Name) -->
    { atom_length(Name, 1) },
    !,
    [ ansi(code, '-~w', [Name]) ].
opt(Name) -->
    [ ansi(code, '--~w', [Name]) ].

list(Values) -->
    sequence(code, [', '-[]], Values).

code(Value) -->
    [ ansi(code, '~w', [Value]) ].
