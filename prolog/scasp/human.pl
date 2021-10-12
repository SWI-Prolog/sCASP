:- module(scasp_just_human,
          [ human_justification_tree/1, % :Tree
            human_justification_tree/2, % :Tree, +Options
            human_model/2,              % :Model, +Options
            human_query/2               % :Query, +Options
          ]).
:- use_module(common).
:- use_module(output).
:- use_module(library(dcg/high_order)).

:- meta_predicate
    human_justification_tree(:),
    human_justification_tree(:, +),
    human_model(:, +),
    human_query(:, +).

%!  human_justification_tree(:Tree) is det.
%!  human_justification_tree(:Tree, +Options) is det.
%
%   Print Tree through the message system in _human_ representation.
%   Normally this is used together with ovar_analyze_term/1.
%
%   @see print_message/2.

:- det(human_justification_tree/2).

human_justification_tree(Tree) :-
    human_justification_tree(Tree, []).

human_justification_tree(M:Tree, Options) :-
    print_message(information,
                  scasp_justification(Tree,
                                      [ depth(0),
                                        module(M)
                                      | Options
                                      ])).

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

:- det(scasp_term//2).

scasp_term(Var, Options) -->
    { var(Var) },
    !,
    var(Var, Options).
scasp_term(@(Var:''), Options) -->
    { var(Var) },
    !,
    var(Var, Options).
scasp_term(@(Var:Type), Options) -->
    { var(Var)
    },
    !,
    typed_var(Var, Type, Options).
scasp_term(@(Value:''), Options) -->
    !,
    scasp_term(Value, Options).
scasp_term(@(Value:Type), Options) -->
    [ 'the ~w '-[Type] ],
    !,
    scasp_term(Value, Options).
scasp_term(Term, _Options) -->
    { atomic(Term) },
    !,
    [ '~q'-[Term] ].
scasp_term(Term, _Options) -->
    { var_number(Term, _) },
    !,
    [ '~p'-[Term] ].
scasp_term('| '(Var, {Constraints}), Options) -->
    !,
    inlined_var(Var, Constraints, Options).
scasp_term(Term, Options) -->
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
    scasp_term(L1, Options),
    [ '~w'-[Conn] ],
    scasp_term(L, Options).
emit_args([H|T], Options) -->
    scasp_term(H, Options),
    (   {T==[]}
    ->  []
    ;   {option(connector(Conn), Options, ', ')},
        ['~w'-[Conn]],
        emit_args(T, Options)
    ).

%!  var(+Var, +Options)//
%
%   Handle a variable, optionally with   constraints and annotated using
%   ovar_analyze_term/2.

var(Var, Options) -->
    { copy_term(Var, Copy),
      inline_constraints(Copy, Options),
      nonvar(Copy),
      Copy = '| '(V, {Constraints})
    },
    !,
    inlined_var(V, Constraints, Options).
var(Var, _Options) -->
    { ovar_var_name(Var, Name)
    },
    !,
    [ '~w'-[Name] ].
var(_, _) -->
    [ 'anything'-[] ].


%!  inlined_var(+Var, +Constraint, +Options)//
%
%   Deal with constraints as represented after inline_constraints/2.

inlined_var(Var, Constraints, Options) -->
    { Constraints = '\u2209'(Var, List),
      Var == '$VAR'('_')
    },
    !,
    (   {List = [One]}
    ->  ['anything except for '-[] ],
        scasp_term(One, Options)
    ;   ['anything except for '-[] ],
        list(List, [last_connector(or)|Options])
    ).
inlined_var(Var, Constraints, Options) -->
    { Constraints = '\u2209'(Var, List),
      compound(Var),
      Var = '$VAR'(Name)
    },
    !,
    (   {List = [One]}
    ->  [ '~w other than '-[Name] ],
        scasp_term(One, Options)
    ;   [ '~w not '-[Name] ],
        list(List, [last_connector(or)|Options])
    ).
inlined_var(Var, Constraints, Options) -->
    { comma_list(Constraints, CLPQ)
    },
    clpq(Var, CLPQ, Options).

%!  clpq(@Var, +Constraints, +Options)//

clpq(Var, [Constraint|More], Options) -->
    { compound(Constraint),
      Constraint =.. [Op,A,B],
      Var == A,
      cmp_op(Op, Text),
      (   nonvar(Var),
          Var = '$VAR'(Name)
      ->  Id = Name
      ;   Id = number
      )
    },
    [ 'any ~w ~w '-[Id, Text] ],
    scasp_term(B, Options),
    (   {More == []}
    ->  []
    ;   [' and '-[] ],
        clpq_and(More, Var, Options)
    ).

clpq_and([Constraint|More], Var, Options) -->
    { compound(Constraint),
      Constraint =.. [Op,A,B],
      A == Var,
      cmp_op(Op, Text)
    },
    [ '~w '-[Text] ],
    scasp_term(B, Options),
    (   {More == []}
    ->  []
    ;   [' and '-[] ],
        clpq_and(More, Var, Options)
    ).

cmp_op(.>.,  'larger than').            % should be made canonical
cmp_op(.>=., 'larger than or equal to').
cmp_op(.<.,  'smaller than').
cmp_op(.=<., 'smaller than or equal to').
cmp_op(.=.,  'equal to').
cmp_op(.<>., 'not equal to').

cmp_op(#>,  'larger than').
cmp_op(#>=, 'larger than or equal to').
cmp_op(#<,  'smaller than').
cmp_op(#=<, 'smaller than or equal to').
cmp_op(#=,  'equal to').
cmp_op(#<>, 'not equal to').

typed_var(Var, Type, Options) -->
    { copy_term(Var, Copy),
      inline_constraints(Copy, Options),
      nonvar(Copy),
      Copy = '| '(V, {Constraints})
    },
    !,
    inlined_typed_var(V, Type, Constraints, Options).
typed_var(Var, Type, _Options) -->
    { ovar_var_name(Var, Name)
    },
    !,
    [ '~w, a ~w'-[Name, Type] ].
typed_var(_Var, Type, _Options) -->
    [ 'a ~w'-[Type] ].


inlined_typed_var(Var, Type, Constraints, Options) -->
    { Constraints = '\u2209'(Var, List),
      Var == '$VAR'('_')
    },
    !,
    [ 'any ~w except for '-[Type] ],
    (   {List = [One]}
    ->  scasp_term(One, Options)
    ;   list(List, [last_connector(or)|Options])
    ).
inlined_typed_var(Var, Type, Constraints, Options) -->
    { Constraints = '\u2209'(Var, List),
      nonvar(Var),
      Var = '$VAR'(Name)
    },
    !,
    (   {List = [One]}
    ->  ['~w, a ~w other than '-[Name, Type]],
        scasp_term(One, Options)
    ;   ['~w, a ~w not '-[Name, Type]],
        list(List, [last_connector(or)|Options])
    ).
inlined_typed_var(Var, Type, Constraints, Options) --> % TBD: include type in NLP
    { comma_list(Constraints, CLPQ)
    },
    clpq(Var, CLPQ, [type(Type)|Options]).

%!  list(+Elements, +Options) is det.
%
%   Emit a collection as "a, b, and c"

list([L1,L], Options) -->
    !,
    { option(last_connector(Conn), Options, 'and') },
    scasp_term(L1, Options),
    [ ', ~w '-[Conn] ],
    scasp_term(L, Options).
list([H|T], Options) -->
    scasp_term(H, Options),
    (   {T==[]}
    ->  []
    ;   [ ', '-[] ],
        list(T, Options)
    ).

%!  emit_fmt_actions(+Actions, +Options)//

emit_fmt_actions([], _) --> [].
emit_fmt_actions([H|T], Options) -->
    emit_fmt_action(H, Options),
    emit_fmt_actions(T, Options).

emit_fmt_action(text(S), _) -->
    [ '~w'-[S] ].
emit_fmt_action(Term, Options) -->
    scasp_term(Term, Options).


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

		 /*******************************
		 *            MODEL		*
		 *******************************/

%!  human_model(:Model, +Options)

human_model(M:Model, Options) :-
    print_message(information,
                  scasp_model(Model,
                              [ module(M)
                              | Options
                              ])).

:- det(emit_model//2).

emit_model(Model, Options) -->
    sequence(model_term(Options), [nl], Model).

model_term(Options, Term) -->
    [ '   \u2022 '-[] ],
    emit_atom(Term, Options).

		 /*******************************
		 *            QUERY		*
		 *******************************/

%!  human_query(:Query, +Options)

human_query(M:Query, Options) :-
    print_message(information,
                  scasp_query(Query,
                              [ module(M)
                              | Options
                              ])).

:- det(emit_model//2).

emit_query(Query, Options) -->
    { delete(Query, o_nmr_check, Query1) },
    [ 'I would like to know if'-[], nl ],
    sequence(query_term(Options), connector(and, Options), Query1).

query_term(Options, Term) -->
    [ '   '-[] ],
    emit_atom(Term, Options).






		 /*******************************
		 *         INTEGRATION		*
		 *******************************/

:- multifile
    prolog:message//1,
    user:message_hook/3.

prolog:message(scasp_justification(Tree, Options)) -->
    human_output(Tree, Options).
prolog:message(scasp_model(Model, Options)) -->
    emit_model(Model, Options).
prolog:message(scasp_query(Query, Options)) -->
    emit_query(Query, Options).

% Avoid coloring and prefix if we are not printing for the toplevel
user:message_hook(scasp_justification(_Tree, Options), _, Lines) :-
    option(as_comment(false), Options),
    print_message_lines(current_output, '', Lines).
user:message_hook(scasp_model(_Model, Options), _, Lines) :-
    option(as_comment(false), Options),
    print_message_lines(current_output, '', Lines).
user:message_hook(scasp_query(_Query, Options), _, Lines) :-
    option(as_comment(false), Options),
    print_message_lines(current_output, '', Lines).

:- multifile
    scasp_stack:justification_tree_hook/2,
    scasp_model:model_hook/2.

scasp_stack:justification_tree_hook(Tree, Options) :-
    option(human(true), Options),
    !,
    human_justification_tree(Tree, Options).
scasp_model:model_hook(Model, Options) :-
    option(human(true), Options),
    !,
    human_model(Model, Options).
