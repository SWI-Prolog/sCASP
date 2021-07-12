:- module(scasp_load_compiled,
          [ read_compiled_source/1
          ]).

% :- use_module(library(assertions/assrt_lib)).

:- use_module(sasp/output).
:- use_module(scasp_io).

:- op(700, fx, [not, #]).
:- op(700, xfx, [(.\=.), (.=.)]).
:- op(700, xfx, ['.=.' ,
                 '.<>.',
                 '.<.' ,
                 '.>.' ,
                 '.=<.',
                 '.>=.'
                 ]).
:- op(700, xfx, ['#=' ,
                 '#<>',
                 '#<' ,
                 '#>' ,
                 '#=<',
                 '#>='
                 ]).

read_compiled_source(S) :-
    retractall(pr_user_predicate(_)),
    retractall(pr_query(_)),
    retractall(pr_rule(_,_)),
    read_compiled_files(S), !.

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
    assert(pr_rule(o_nmr_check,Body)),
    assert(pr_user_predicate(o_nmr_check/0)),
    assert(pr_user_predicate(global_constraints/0)).
assert_rule(Head,Body) :-
    Head =.. [Name|Args],
    length(Args,La),
    (   pr_user_predicate(Name/La) ->
        true
    ;
        assert(pr_user_predicate(Name/La))
    ),
    assert(pr_rule(Head,Body)).


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
