:- module(diff,
          [ serialize/1,
            serialize_into/2,
            diff_terms/2
          ]).

diff_terms(T1, T2) :-
    serialize_into(File1, T1),
    serialize_into(File2, T2),
    thread_create(show_diff(File1, File2), _,
                  [ detached(true)
                  ]).

show_diff(File1, File2) :-
    setup_call_cleanup(
        process_create(path(meld),
                       [ file(File1), file(File2),
                         '-L', "Compare Prolog terms"
                       ],
                       [ process(PID)]),
        wait_or_kill(PID),
        call_cleanup(
            delete_file(File1),
            delete_file(File2))).

wait_or_kill(PID) :-
    catch(process_wait(PID, _), '$aborted',
          ( process_kill(PID),
            process_wait(PID, _)
          )).

serialize_into(File, Term) :-
    var(File),
    !,
    setup_call_cleanup(
        tmp_file_stream(File, Out, [extension(txt)]),
        serialize(Out, Term),
        close(Out)).

serialize_into(File, Term) :-
    setup_call_cleanup(
        open(File, write, Out),
        serialize(Out, Term),
        close(Out)).

serialize(Term) :-
    serialize(current_output, Term).

serialize(Out, Term) :-
    \+ \+ ( numbervars(Term, 0, _),
            serialize(Out, Term, 0)
          ).

serialize(Out, Term, Depth) :-
    Term == [],
    !,
    indent(Out, Depth),
    format(Out, '[]~n', []).
serialize(Out, Term, Depth) :-
    is_list(Term),
    !,
    length(Term, Length),
    indent(Out, Depth),
    format(Out, '[~d]~n', [Length]),
    Depth2 is Depth+1,
    forall(member(Arg, Term),
           serialize(Out, Arg, Depth2)).
serialize(Out, Term, Depth) :-
    improper_list(Term, List, Last),
    List \== [],
    !,
    length(List, Length),
    indent(Out, Depth),
    format(Out, '[~d|+]~n', [Length]),
    Depth2 is Depth+1,
    forall(member(Arg, List),
           serialize(Out, Arg, Depth2)),
    indent(Out, Depth2, '+ '),
    serialize(Out, Last, Depth2).
serialize(Out, Term, Depth) :-
    is_dict(Term, Tag),
    !,
    indent(Out, Depth),
    format(Out, '{~q}~n', [Tag]),
    Depth2 is Depth+1,
    forall(get_dict(Key, Term, Value),
           serialize_kv(Out, Key, Value, Depth2)).
serialize(Out, Term, Depth) :-
    compound(Term),
    !,
    compound_name_arity(Term, Name, Arity),
    indent(Out, Depth),
    format(Out, '~q/~d~n', [Name, Arity]),
    Depth2 is Depth+1,
    forall(arg(_I, Term, Arg),
           serialize(Out, Arg, Depth2)).
serialize(Out, Term, Depth) :-
    indent(Out, Depth),
    format(Out, '~p~n', [Term]).

serialize_kv(Out, Key, Value, Depth) :-
    indent(Out, Depth, ''),
    format(Out, '~q:~n', [Key]),
    Depth2 is Depth+1,
    serialize(Out, Value, Depth2).

indent(Out, Depth) :-
    indent(Out, Depth, '- ').

indent(Out, Depth, Prefix) :-
    Depth > 0,
    line_position(Out, 0),
    !,
    format(Out, '~t~*|~w', [Depth, Prefix]).
indent(_, _, _).

improper_list([H|T0], List, Last) =>
    List = [H|T],
    improper_list(T0, T, Last).
improper_list(Term, List, Last) =>
    List = [],
    Last = Term.
