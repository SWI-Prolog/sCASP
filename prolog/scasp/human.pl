:- module(scasp_just_human,
          [ human_justification_tree/1,
            human_justification_tree/2
          ]).
:- use_module(common).
:- use_module(clp/disequality).

:- meta_predicate
    human_justification_tree(:),
    human_justification_tree(:, +).

%!  human_justification_tree(:Tree) is det.
%!  human_justification_tree(:Tree, +Options) is det.
%
%   Print Tree through the message system in _human_ representation.
%
%   @see print_message/2.

human_justification_tree(Tree) :-
    human_justification_tree(Tree, []).

human_justification_tree(M:Tree, Options) :-
    \+ \+ ( analyse_variables(Tree),
            print_message(information,
                          scasp_justification(Tree,
                                              [ depth(0),
                                                module(M)
                                              | Options
                                              ]))
          ).

analyse_variables(Tree) :-
    term_singletons(Tree, Singletons),
    term_variables(Tree, AllVars),
    maplist(mark_singleton, Singletons),
    foldl(name_variable, AllVars, 0, _).

mark_singleton(Var) :-
    put_attr(Var, scasp_just_human, singleton).

name_variable(Var, N0, N) :-
    (   is_singleton(Var)
    ->  N = N0
    ;   L is N0 mod 26 + 0'A,
        N is N0 // 26,
        (   N == 0
        ->  char_code(Name, L)
        ;   format(atom(Name), '~c~d', [L, N])
        ),
        put_attr(Var, scasp_just_human, name(Name))
    ).

attr_unify_hook(_Attr, _Value).

is_singleton(Var) :-
    get_attr(Var, scasp_just_human, singleton).
var_name(Var, Name) :-
    get_attr(Var, scasp_just_human, name(Name)).


%!  human_output(+FilterChildren, +Options)

human_output(query-[Query,o_nmr_check-[]], Options) -->
    !,
    human_output(Query, Options),
    full_stop(Options).
human_output(query-Children, Options) -->
    !,
    human_output_children(Children, Options),
    full_stop(Options).
human_output(o_nmr_check-[], _Options) -->
    !.
human_output(Term-[], Options) -->
    indent(Options),
    emit_atom(Term, Options).
human_output(Term-Children, Options) -->
    indent(Options),
    emit_atom(Term, Options),
    connector(implies, Options),
    { incr_indent(Options, Options1) },
    human_output_children(Children, Options1).


human_output_children([A,B|Rs], Options) -->
    human_output(A, Options),
    connector(and, Options),
    human_output_children([B|Rs], Options).
human_output_children([A], Options) -->
    human_output(A, Options).

emit_atom(not(GlobalConstraint), _Options) -->
    { is_global_constraint(GlobalConstraint, N)
    },
    !,
    [ 'the global constraint number ~p holds'-[N] ].
emit_atom(not(Term), Options) -->
    !,
    connector(not, Options),
    emit_atom(Term, Options).
emit_atom(-Term, Options) -->
    !,
    connector(-, Options),
    emit_atom(Term, Options).
emit_atom(proved(Term), Options) -->
    !,
    emit_atom(Term, Options),
    [ ', justified above'-[] ].
emit_atom(chs(Term), Options) -->
    !,
    [ 'it is assumed that '-[] ],
    emit_atom(Term, Options).
emit_atom(M:Term, Options) -->
    { atom(M) },
    !,
    emit_atom(Term, [module(M)|Options]).
emit_atom(Term, Options) -->            % #pred Term::Template
    { option(module(M), Options),       % Used existing translation
      current_predicate(M:pr_pred_predicate/1),
      \+ predicate_property(M:pr_pred_predicate(_), imported_from(_)),
      M:pr_pred_predicate(::(Term,format(Fmt, Args))),
      !,
      parse_fmt(Fmt, Args, Actions)
    },
    emit_fmt_actions(Actions, Options).
emit_atom(o_nmr_check, _Options) -->
    !,
    [ 'The global constraints hold'-[] ].
emit_atom(Term, _Options) -->
    { atom(Term) },
    !,
    ['~p holds'-[Term] ].
emit_atom(Term, _Options) -->
    { Term =.. [Rule|Args] },
    ['~p holds (for '-[Rule] ],
    emit_args(Args, [last_connector(', and ')]),
    [')'-[]].

:- det(emit_term//2).

emit_term(Var, _Options) -->
    { var(Var) },
    !,
    [ 'X'-[] ].
emit_term(@(NegVar:''), Options) -->
    { get_neg_var(NegVar, List)
    },
    !,
    (   {List = [One]}
    ->  [ 'not '-[] ],
        emit_term(One, Options)
    ;   [ 'not in '-[] ],
        emit_args(List, [last_connector(', or ')|Options])
    ).
emit_term(@(NegVar:Type), Options) -->
    { get_neg_var(NegVar, List),
      is_singleton(NegVar)
    },
    !,
    (   {List = [One]}
    ->  [ 'a ~w other than '-[Type] ],
        emit_term(One, Options)
    ;   [ 'a ~w not in '-[Type] ],
        emit_args(List, [last_connector(', or ')|Options])
    ).
emit_term(@(Value:''), Options) -->
    !,
    emit_term(Value, Options).
emit_term(@(Value:Type), Options) -->
    [ 'the ~w '-[Type] ],
    !,
    emit_term(Value, Options).
emit_term(Term, _Options) -->
    { atomic(Term) },
    !,
    [ '~q'-[Term] ].
emit_term(Term, Options) -->
    { compound(Term),
      Term =.. [Name|Args]
    },
    !,
    [ '~q('-[Name] ],
    emit_args(Args, Options),
    [ ')'-[] ].

emit_args([L1,L], Options) -->
    { option(last_connector(Conn), Options) },
    !,
    emit_term(L1, Options),
    [ '~w'-[Conn] ],
    emit_term(L, Options).
emit_args([H|T], Options) -->
    emit_term(H, Options),
    (   {T==[]}
    ->  []
    ;   {option(connector(Conn), Options, ', ')},
        ['~w'-[Conn]],
        emit_args(T, Options)
    ).

emit_fmt_actions([], _) --> [].
emit_fmt_actions([H|T], Options) -->
    emit_fmt_action(H, Options),
    emit_fmt_actions(T, Options).

emit_fmt_action(text(S), _) -->
    [ '~w'-[S] ].
emit_fmt_action(Term, Options) -->
    emit_term(Term, Options).


%!  connector(+Meaning, +Options)//
%
%   Emit a logical connector.

connector(and, _Options) -->
    [ ', and'-[], nl ].
connector(not, _Options) -->
    [ 'there is no evidence that '-[] ].
connector(-, _Options) -->
    [ '-'-[] ].
connector(implies, _Options) -->
    [ ', because'-[], nl ].

full_stop(_Options) -->
    [ '.'-[] ].

indent(Options) -->
    { option(depth(D), Options),
      Spaces is D*3,
      format(string(S), '~t~*|', [Spaces])
    },
    [ '~w'-[S] ].

incr_indent(Options0, [depth(D)|Options1]) :-
    select_option(depth(D0), Options0, Options1),
    D is D0+1.

%!  parse_fmt(+Fmt, +Args, -Actions) is det.
%
%   Translate a human template and its arguments  into a list of actions
%   for our DCG. The template  allows   form  interpolating  a variable,
%   optionally with a type. The core translator   adds  ~p to the format
%   and a term @(Var:Type) or @(Var:'') to   the arguments. Actions is a
%   list of text(String) or @(Var:Type).

:- det(parse_fmt/3).

parse_fmt(Fmt, Args, Actions) :-
    format_spec(Fmt, Spec),
    fmt_actions(Spec, Args, Actions).

fmt_actions([], [], []).
fmt_actions([text(S)|T0], Args, [text(S)|T]) :-
    fmt_actions(T0, Args, T).
fmt_actions([escape(nothing, no_colon, p)|T0], [A0|Args], [A0|T]) :-
    fmt_actions(T0, Args, T).


:- multifile prolog:message//1.

prolog:message(scasp_justification(Tree, Options)) -->
    human_output(Tree, Options).
