:- module(scasp_load_compiled,
          [ read_compiled_source/1
          ]).
:- use_module(pr_rules).

/** <module> Load a precompiled program

@tbd: Not compatible with module handling.
*/

:- op(700, fx, [not, #]).
:- op(700, xfx, [(.\=.), (.=.)]).
:- op(700, xfx, [#=, #<>, #<, #>, #=<, #>=]).

read_compiled_source(M:S) :-
    clean_pr_program(M),
    read_compiled_files(M:S), !.

read_compiled_files([]).
read_compiled_files([F|Fs]) :-
    read_compiled_file(F),
    read_compiled_files(Fs).

read_compiled_file(F) :-
    open(F, read, ID),  % open a stream
    repeat,             % try again forever
    read(ID, X),        % read from the stream
    assert_clause(X),
    X == end_of_file,   % fail (backtrack) if not end of file
    close(ID).          % close the file

assert_clause(end_of_file) :- !.

%% TODO: Process the compiled files to add directives (e.g., #pred).
assert_clause('#'(_)) :- !.

assert_clause('?-'(Query)) :- !,
    conj_to_list(Query, LQuery),
    assert(pr_query(LQuery)).

assert_clause(':-'(Head,Body)) :- !,
    conj_to_list(Body,LBody),
    capture_minus(LBody,MBody),
    capture_minus([Head],[MHead]),
    assert_rule(MHead,MBody).
assert_clause(Fact) :-
    capture_minus([Fact],[MFact]),
    assert_rule(MFact,[]).

assert_rule(global_constraint,Body) :- !,
    assertz(pr_rule(o_nmr_check,Body)),
    assertz(pr_user_predicate(o_nmr_check/0)),
    assertz(pr_user_predicate(global_constraints/0)).
assert_rule(Head,Body) :-
    functor(Head, Name, La),
    (   pr_user_predicate(Name/La)
    ->  true
    ;   assertz(pr_user_predicate(Name/La))
    ),
    assertz(pr_rule(Head,Body)).

capture_minus([],[]).
capture_minus([-Neg|Bs],[MNeg|MBs]) :- !,
    Neg =.. [Name|Args],
    atom_concat('-',Name,MName),
    MNeg =.. [MName|Args],
    capture_minus(Bs,MBs).
capture_minus([not(-Neg)|Bs],[not(MNeg)|MBs]) :- !,
    Neg =.. [Name|Args],
    atom_concat('-',Name,MName),
    MNeg =.. [MName|Args],
    capture_minus(Bs,MBs).
capture_minus([B|Bs],[B|MBs]) :-
    capture_minus(Bs,MBs).

conj_to_list(true, []) :-
    !.
conj_to_list(Conj, List) :-
    comma_list(Conj, List).
