:- module(scasp_just_human,
          [ human_justification_tree/1,
            human_justification_tree/2
          ]).
:- use_module(common).
:- use_module(output).
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

:- det(human_justification_tree/2).

human_justification_tree(Tree) :-
    human_justification_tree(Tree, []).

human_justification_tree(M:Tree, Options) :-
    \+ \+ ( ovar_analyze_term(Tree),
            print_message(information,
                          scasp_justification(Tree,
                                              [ depth(0),
                                                module(M)
                                              | Options
                                              ]))
          ).

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
      human_expression(M:Term, Actions)
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
      ovar_is_singleton(NegVar)
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
emit_term(Term, _Options) -->
    { var_number(Term, _) },
    !,
    [ '~p'-[Term] ].
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

:- multifile prolog:message//1.

prolog:message(scasp_justification(Tree, Options)) -->
    human_output(Tree, Options).

:- multifile
    scasp_stack:justification_tree_hook/2.

scasp_stack:justification_tree_hook(Tree, Options) :-
    option(format(human), Options),
    !,
    human_justification_tree(Tree, Options).
