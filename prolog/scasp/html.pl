:- module(scasp_just_html,
          [ html_justification_tree//2,		% +Tree, +Options
            html_model//2			% +Model, +Options
          ]).
:- use_module(common).
:- use_module(clp/disequality).
:- use_module(clp/clpq).
:- use_module(output).

:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_server_files)).
:- use_module(library(dcg/high_order)).

:- meta_predicate
    html_justification_tree(:, +, ?, ?).

:- multifile user:file_search_path/2.

user:file_search_path(js,  library(scasp/web/js)).
user:file_search_path(css, library(scasp/web/css)).

:- html_resource(scasp,
                 [ virtual(true),
                   requires([ jquery,
                              js('scasp.js'),
                              css('scasp.css')
                            ]),
                   ordered(true)
                 ]).

/** <module> Render s(CASP) justification as HTML
*/

%!  html_justification_tree(:Tree, +Options)// is det.
%
%   Convert the tree to HTML. The  caller should use ovar_analyze_term/1
%   on Tree to name variables and identify  singletons. This is not done
%   in this predicate as the user may or  may not wish to combin the the
%   variable analysis with the bindings and/or model.
%
%   @see print_message/2.

:- det(html_justification_tree//2).

html_justification_tree(M:Tree, Options) -->
    html(ul(class('scasp-justification'),
            \justification_tree(Tree,
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
    !,
    html(li([ div(class(node),
                  [ \tree_atom(Term, Options),
                    \connect(Options)
                  ])
            ])).
justification_tree(Term-Children, Options) -->
    { incr_indent(Options, Options1) },
    html(li( class(collapsable),
             [ div(class([node, 'collapsable-header']),
                  [ \tree_atom(Term, Options),
                    \connector(implies, Options)
                  ]),
              ul(class('collapsable-content'),
                 \justification_tree_children(Children, Options1))
            ])).

justification_tree_children([A,B|Rs], Options) -->
    justification_tree(A, [connect(and)|Options]),
    justification_tree_children([B|Rs], Options).
justification_tree_children([A], Options) -->
    justification_tree(A, Options).

connect(Options) -->
    { option(connect(Connector), Options) },
    !,
    connector(Connector, Options).
connect(_) -->
    [].

tree_atom(Atom, Options) -->
    { atom_classes(Atom, Classes),
      scasp_atom_string(Atom, String)
    },
    html(span([ class(['scasp-atom'|Classes]),
                title(String)
              ],
              \atom(Atom, Options))).

scasp_atom_string(Atom, String) :-
    with_output_to(string(String),
                   print_model_term(Atom, [])).

%!  html_model(+Model, +Options)// is det.
%
%   Emit the model as HTML terms.   We export the model as a dict with
%   nested model terms.

html_model(Model, Options) -->
    { (   option(class(Class), Options)
      ->  Classes = [Class]
      ;   Classes = []
      )
    },
    html(div(class(['scasp-model'|Classes]),
             \sequence(model_term_r(Options), Model))).

model_term_r(Options, Atom) -->
    { atom_classes(Atom, Classes),
      scasp_atom_string(Atom, String)
    },
    html(div([ class(['scasp-atom'|Classes]),
               title(String)
             ],
             \atom(Atom, Options))).


atom_classes(A, []) :-
    var(A),
    !.
atom_classes(not(A), ['scasp-not'|T]) :-
    !,
    atom_classes(A, T).
atom_classes(-(A), ['scasp-negation'|T]) :-
    !,
    atom_classes(A, T).
atom_classes(_, []).

%!  atom(+SCASPAtom, +Options)//
%
%   Emit an s(CASP) atom with annotations as   they  appear in the model
%   and justification.

atom(not(GlobalConstraint), Options) -->
    { is_global_constraint(GlobalConstraint, N)
    },
    !,
    utter(global_constraint(N), Options).
atom(not(Term), Options) -->
    !,
    utter(not(Term), Options).
atom(-Term, Options) -->
    !,
    utter(-(Term), Options).
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
utter(not(Atom), Options) -->
    html('there is no evidence that '),
    atom(Atom, Options).
utter(-(Atom), Options) -->
    html('it is not the case that '),
    atom(Atom, Options).
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
    ->  html([span(class(atom), Name), ' holds for ']),
        list(Args, Options)
    ).

:- det(scasp_term//2).

scasp_term(Var, Options) -->
    { var(Var) },
    !,
    var(Var, Options).
scasp_term(@(Var:''), Options) -->
    { var(Var)
    },
    !,
    var(Var, Options).
scasp_term(@(Var:Type), Options) -->
    { var(Var)
    },
    !,
    var(Var, Type, Options).
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

var(NegVar, Options) -->
    { get_neg_var(NegVar, List),
      ovar_is_singleton(NegVar)
    },
    !,
    (   {List = [One]}
    ->  html('anything except for '),
        scasp_term(One, Options)
    ;   html('anything except for '),
        list(List, [last_connector(or)|Options])
    ).
var(NegVar, Options) -->
    { get_neg_var(NegVar, List),
      ovar_var_name(NegVar, Name)
    },
    !,
    (   {List = [One]}
    ->  html([var(Name), ' other than ']),
        scasp_term(One, Options)
    ;   html([var(Name), ' not ']),
        list(List, [last_connector(or)|Options])
    ).
var(Var, Options) -->
    { is_clpq_var(Var),
      !,
      clpqr_dump_constraints([Var], [Var], Constraints)
    },
    clpq(Var, Constraints, Options).
var(Var, _Options) -->
    { ovar_var_name(Var, Name)
    },
    !,
    html(var(Name)).
var(_, _) -->
    html(anything).

%!  clpq(@Var, +Constraints, +Options)//

clpq(Var, [Constraint], Options) -->
    { compound(Constraint),
      Constraint =.. [Op,A,B],
      Var == A,
      cmp_op(Op, Text),
      (   ovar_var_name(Var, Name)
      ->  Id = var(Name)
      ;   Id = number
      )
    },
    html(['any ', Id, ' ', Text, ' ']),
    scasp_term(B, Options).

cmp_op(.>.,  'larger than').
cmp_op(.>=., 'larger than or equal to').
cmp_op(.<.,  'smaller than').
cmp_op(.=<., 'smaller than or equal to').
cmp_op(.=.,  'equal to').
cmp_op(.<>., 'not equal to').


%!  var(@Var, +Type, +Options)//

var(NegVar, Type, Options) -->
    { get_neg_var(NegVar, List),
      ovar_is_singleton(NegVar)
    },
    !,
    (   {List = [One]}
    ->  html(['any ', Type, ' except for ']),
        scasp_term(One, Options)
    ;   html(['any ', Type, ' except for ']),
        list(List, [last_connector(or)|Options])
    ).
var(NegVar, Type, Options) -->
    { get_neg_var(NegVar, List),
      ovar_var_name(NegVar, Name)
    },
    !,
    (   {List = [One]}
    ->  html([var(Name), ', a ', Type, ' other than ']),
        scasp_term(One, Options)
    ;   html([var(Name), ', a ', Type, ' not ']),
        list(List, [last_connector(or)|Options])
    ).
var(Var, _Type, Options) -->            % TBD: include type in NLP
    { is_clpq_var(Var),
      !,
      clpqr_dump_constraints([Var], [Var], Constraints)
    },
    clpq(Var, Constraints, Options).
var(Var, Type, _Options) -->
    { ovar_var_name(Var, Name)
    },
    !,
    html([var(Name), ', a ', Type]).
var(_, Type, _) -->
    html(['a ', Type]).

%!  list(+Elements) is det.
%
%   Emit a collection as "a, b, and c"

list([L1,L], Options) -->
    !,
    { option(last_connector(Conn), Options, 'and') },
    scasp_term(L1, Options),
    html(', ~w '-[Conn]),
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
    html('\u220e').

incr_indent(Options0, [depth(D)|Options2]) :-
    select_option(depth(D0), Options0, Options1),
    select_option(connect(_), Options1, Options2, _),
    D is D0+1.
