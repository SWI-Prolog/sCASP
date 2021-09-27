:- module(scasp_just_html,
          [ html_justification_tree//2
          ]).
:- use_module(common).
:- use_module(clp/disequality).
:- use_module(output).
:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).

:- meta_predicate
    html_justification_tree(:, +, ?, ?).

/** <module> Render s(CASP) justification as HTML
*/

%!  html_justification_tree(:Tree, +Options)// is det.
%
%   Convert the tree to HTML.
%
%   @see print_message/2.

:- det(html_justification_tree//2).

html_justification_tree(Tree, Options) -->
    { copy_term(Tree, M:Tree1),
      ovar_analyze_term(Tree1)
    },
    html(ul(class(tree),
            \justification_tree(Tree1,
                                [ depth(0),
                                  module(M)
                                | Options
                                ]))).

%!  justification_tree(+FilterChildren, +Options)//

justification_tree(query-[Query,o_nmr_check-[]], Options) -->
    !,
    justification_tree(Query, Options),
    full_stop(Options).
justification_tree(query-Children, Options) -->
    !,
    justification_tree_children(Children, Options),
    full_stop(Options).
justification_tree(o_nmr_check-[], _Options) -->
    !.
justification_tree(Term-[], Options) -->
    html(li(\atom(Term, Options))).
justification_tree(Term-Children, Options) -->
    { incr_indent(Options, Options1) },
    html(li([ \atom(Term, Options),
              \connector(implies, Options),
              ul(\justification_tree_children(Children, Options1))
            ])).

justification_tree_children([A,B|Rs], Options) -->
    justification_tree(A, Options),
    connector(and, Options),
    justification_tree_children([B|Rs], Options).
justification_tree_children([A], Options) -->
    justification_tree(A, Options).

atom(not(GlobalConstraint), Options) -->
    { is_global_constraint(GlobalConstraint, N)
    },
    !,
    utter(global_constraint(N), Options).
atom(not(Term), Options) -->
    !,
    connector(not, Options),
    atom(Term, Options).
atom(-Term, Options) -->
    !,
    connector(-, Options),
    atom(Term, Options).
atom(proved(Term), Options) -->
    !,
    utter(proved(Term), Options).
atom(chs(Term), Options) -->
    !,
    utter(chs(Term), Options).
atom(M:Term, Options) -->
    { atom(M) },
    !,
    atom(Term, [module(M)|Options]).
atom(Term, Options) -->            % #pred Term::Template
    { option(module(M), Options),       % Used existing translation
      human_expression(M:Term, Actions)
    },
    !,
    actions(Actions, Options).
atom(o_nmr_check, Options) -->
    !,
    utter(global_constraints_hold, Options).
atom(Term, Options) -->
    utter(holds(Term), Options).

%!  utter(+Exppression, +Options)

utter(global_constraints_hold, _Options) -->
    html('The global constraints hold').
utter(global_constraint(N), _Options) -->
    html('the global constraint number ~p holds'-[N]).
utter(proved(Atom), Options) -->
    atom(Atom, Options),
    html(', justified above').
utter(chs(Atom), Options) -->
    html('it is assumed that '),
    atom(Atom, Options).
utter(holds(Atom), Options) -->
    (   { atom(Atom) }
    ->  html([span(class(atom), Atom), ' holds'])
    ;   { Atom =.. [Name|Args] }
    ->  html([span(class(atom), Name), ' holds for']),
        list(Args, Options)
    ).

:- det(scasp_term//2).

scasp_term(Var, _Options) -->
    { var(Var) },
    !,
    html(i('X')).
scasp_term(@(NegVar:''), Options) -->
    { get_neg_var(NegVar, List)
    },
    !,
    (   {List = [One]}
    ->  html('not '),
        scasp_term(One, Options)
    ;   html('not in '),
        list(List, [last_connector(or)|Options])
    ).
scasp_term(@(NegVar:Type), Options) -->
    { get_neg_var(NegVar, List),
      ovar_is_singleton(NegVar)
    },
    !,
    (   {List = [One]}
    ->  html('a ~w other than '-[Type]),
        scasp_term(One, Options)
    ;   html('a ~w not in '-[Type]),
        list(List, [last_connector(or)|Options])
    ).
scasp_term(@(Value:''), Options) -->
    !,
    scasp_term(Value, Options).
scasp_term(@(Value:Type), Options) -->
    html('the ~w '-[Type]),
    !,
    scasp_term(Value, Options).
scasp_term(Term, _Options) -->
    { var_number(Term, _) },
    !,
    [ '~p'-[Term] ].
scasp_term(Term, Options) -->
    term(Term, Options).

%!  list(+Elements) is det.
%
%   Emit a collection as "a, b, and c"

list([L1,L], Options) -->
    !,
    { option(last_connector(Conn), Options, 'and') },
    scasp_term(L1, Options),
    html(', ~w'-[Conn]),
    scasp_term(L, Options).
list([H|T], Options) -->
    scasp_term(H, Options),
    (   {T==[]}
    ->  []
    ;   html(', '),
        list(T, Options)
    ).

actions([], _) --> [].
actions([H|T], Options) -->
    action(H, Options),
    actions(T, Options).

action(text(S), _) -->
    html(S).
action(Term, Options) -->
    scasp_term(Term, Options).

%!  connector(+Meaning, +Options)//
%
%   Emit a logical connector.

connector(and, _Options) -->
    html(', and').
connector(not, _Options) -->
    html('there is no evidence that ').
connector(-, _Options) -->
    html('-').
connector(implies, _Options) -->
    html(', because').

full_stop(_Options) -->
    html('.').

incr_indent(Options0, [depth(D)|Options1]) :-
    select_option(depth(D0), Options0, Options1),
    D is D0+1.
