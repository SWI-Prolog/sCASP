:- module(casp_lang_en,
          [ scasp_message//1
          ]).
:- use_module(library(dcg/high_order)).

:- multifile
    scasp_messages:scasp_lang/2.

scasp_messages:scasp_lang(en, casp_lang_en).

:- multifile
    prolog:error_message//1.

prolog:error_message(existence_error(scasp_query, scasp_main)) -->
    [ 'sCASP: the program does not contain a query'-[] ].
prolog:error_message(existence_error(scasp_query, M)) -->
    [ 'sCASP: no query in module ~p'-[M] ].


		 /*******************************
		 *            CASP		*
		 *******************************/

scasp_message(version(Version)) -->
    [ 'version ~w'-[Version] ].

% Usage messages

scasp_message(source_not_found(Source)) -->
    (   \+ { access_file(Source, exist) }
    ->  [ 'Input file '-[] ], code(Source), [ ' does not exist'-[] ]
    ;   [ 'Cannot read input file '-[] ], code(Source)
    ).
scasp_message(no_input_files) -->
    [ 'No input file specified!' ].
scasp_message(no_query) -->
    [ 'the program does not contain ?- Query.'-[] ].
scasp_message(undefined_operator(Op)) -->
    [ 'clp operator ~p not defined'-[Op] ].
scasp_message(at_most_one_of([A,B])) -->
    ['Options '], opt(A), [' and '], opt(B),
    [' cannot be used together' ].
scasp_message(at_most_one_of(List)) -->
    [ 'At most one of the options '-[] ],
    options(List),
    [ ' is allowed.'-[] ].

% Solver messages

scasp_message(failure_calling_negation(Goal)) -->
    [ 'Failure calling negation of '-[] ], goal(Goal).
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
    [ 'No models (~3f seconds)'-[CPU] ].

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
      '    Current call:  ~@'-[] ], goal(Goal), [ nl,
      '    Previous call: ~@'-[] ], goal(NegGoal).

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
