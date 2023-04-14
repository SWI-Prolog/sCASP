/*  Part of s(CASP)-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2021-2023, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(scasp_just_html,
          [ html_justification_tree//2,		% :Tree, +Options
            html_model//2,			% :Model, +Options
            html_model_term//2,			% :Atom, +Options
            html_bindings//2,                   % :Bindings, +Options
            html_program/1,                     % :Dict
            html_program//1,                    % :Dict
            html_query//2,                      % :Query, +Options
            html_predicate//2,                  % :Predicate, +Options
            html_rule//2,                       % :Rule, +Options
            html_term//2                        % +Term, +Options
          ]).
:- use_module(common).
:- use_module(output).
:- use_module(html_text).
:- use_module(messages).
:- use_module(source_ref).
:- use_module(listing).

:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module(library(http/html_head), [html_resource/2]).
:- if(exists_source(library(http/http_server_files))).
:- use_module(library(http/http_server_files), []).
:- endif.
:- use_module(library(dcg/high_order)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(prolog_code)).
:- use_module(library(apply)).

:- meta_predicate
    html_model(:, +, ?, ?),
    html_model_term(:, +, ?, ?),
    html_justification_tree(:, +, ?, ?),
    html_program(:),
    html_program(:, ?, ?),
    html_query(:, +, ?, ?),
    html_predicate(:, +, ?, ?),
    html_rule(:, +, ?, ?).

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
%   in this predicate as the user may or  may not wish to combine the
%   variable analysis with the bindings and/or model. Options processed:
%
%     - pred(Boolean)
%       When `false` (default `true`), ignore user pred/1 rules.
%     - justify_nmr(Boolean)
%       When `false` (default `true`), do not omit a justification for
%       the global constraints.
%     - show(Format)
%       One of `human`, `machine` or `both`.
%     - source(Boolean)
%       When `false` (default `true`), do not omit source locations.

:- det(html_justification_tree//2).

html_justification_tree(M:Tree, Options) -->
    emit(div(class('scasp-justification'),
             ul(class('scasp-justification'),
                \justification_tree(Tree,
                                    [ depth(0),
                                      module(M)
                                    | Options
                                    ])))).

%!  justification_tree(+Tree, +Options)//
%
%   Emit  HTML  for  Tree.  Tree  is  of   the  format  as  returned  by
%   justification_tree/3, a term of the   shape Atom-ListOfChildren. The
%   first clause deals with mapping subtrees  to human descriptions. The
%   remainder deals with  special  cases  where   there  are  no  global
%   constraints.  normal_justification_tree/2  deals  with  the  general
%   case.

justification_tree(Tree, Options) -->
    { \+ option(show(machine), Options),        % human or both
      option(pred(true), Options, true),
      option(module(M), Options),
      human_expression(M:Tree, Children, Actions)
    },
    !,
    (   {Children == [], Actions == html([])}
    ->  []
    ;   {Children == []}
    ->  emit(li([ div(class(node),
                      [ \human_atom(Tree, Actions, Options),
                        \connect(Options)
                      ])
                ]))
    ;   { incr_indent(Options, Options1),
          (   Tree == o_nmr_check
          ->  ExtraClasses = ['scasp-global-constraints']
          ;   ExtraClasses = []
          )
        },
        emit(li( class([collapsable|ExtraClasses]),
             [ div(class([node, 'collapsable-header']),
                   [ \human_atom(Tree, Actions, Options),
                     \connector(implies, Options)
                   ]),
               ul(class('collapsable-content'),
                  \justification_tree_children(Children, Options1))
            ]))
    ).
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
justification_tree(Tree, Options) -->
    normal_justification_tree(Tree, Options).

normal_justification_tree(goal_origin(Term, Origin)-[], Options) -->
    !,
    { scasp_source_reference_file_line(Origin, File, Line) },
    emit(li([ div(class(node),
                  [ \tree_atom(Term, Options),
                    \origin(File, Line, Options),
                    \connect(Options)
                  ])
            ])).
normal_justification_tree(Term-[], Options) -->
    !,
    emit(li([ div(class(node),
                  [ \tree_atom(Term, Options),
                    \connect(Options)
                  ])
            ])).
normal_justification_tree(o_nmr_check-_, Options) -->
    { option(justify_nmr(false), Options) },
    !.
normal_justification_tree(goal_origin(Term, Origin)-Children, Options) -->
    { incr_indent(Options, Options1),
      (   Term == o_nmr_check
      ->  ExtraClasses = ['scasp-global-constraints']
      ;   ExtraClasses = []
      ),
      scasp_source_reference_file_line(Origin, File, Line)
    },
    !,
    emit(li(class([collapsable|ExtraClasses]),
            [ div(class([node, 'collapsable-header']),
                  [ \tree_atom(Term, Options),
                    \connector(implies, [origin(File:Line)|Options]),
                    \origin(File, Line, Options)
                  ]),
              ul(class('collapsable-content'),
                 \justification_tree_children(Children, Options1))
            ])).
normal_justification_tree(Term-Children, Options) -->
    { incr_indent(Options, Options1),
      (   Term == o_nmr_check
      ->  ExtraClasses = ['scasp-global-constraints']
      ;   ExtraClasses = []
      )
    },
    emit(li(class([collapsable|ExtraClasses]),
            [ div(class([node, 'collapsable-header']),
                  [ \tree_atom(Term, Options),
                    \connector(implies, Options)
                  ]),
              ul(class('collapsable-content'),
                 \justification_tree_children(Children, Options1))
            ])).

justification_tree_children([A,B|Rs], Options) -->
    !,
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

%!  human_atom(+Tree, +Human, +Options)// is det.
%
%   Emits an atom handled by  a  pred/1   rule.  Human  is a sequence of
%   actions as produced by human_expression/3.

human_atom(_, Actions, Options) -->
    { option(show(human), Options),
      empty_actions(Actions)
    },
    !.
human_atom(_Atom-_Children, Actions, Options) -->
    { option(show(human), Options),
      !,
      css_classes(Options, Classes)
    },
    emit(span(class(['scasp-atom'|Classes]),
              \actions(Actions, Options))).
human_atom(Atom-_Children, _Actions, Options) -->
    { option(show(machine), Options),
      !
    },
    emit(span(class('scasp-atom'), \machine_atom(Atom, Options))).
human_atom(Atom-_Children, Actions, Options) -->
    { css_classes(Options, Classes),
      scasp_atom_string(Atom, String)
    },
    emit(span(class('scasp-atom'),
              [ span([class(human), title(String)],
                     span(class(Classes), \actions(Actions, Options))),
                span(class(machine), \machine_atom(Atom, Options))
              ])).

empty_actions('').
empty_actions([]).


tree_atom(Atom, Options) -->
    { option(show(machine), Options) },
    !,
    emit(span(class('scasp-atom'), \machine_atom(Atom, Options))).
tree_atom(Atom, Options) -->
    { option(show(human), Options),
      !
    },
    emit(span(class('scasp-atom'), \atom(Atom, Options))).
tree_atom(Atom, Options) -->
    { scasp_atom_string(Atom, String)
    },
    emit(span(class(['scasp-atom']),
              [ span([class(human), title(String)], \atom(Atom, Options)),
                span(class(machine), \machine_atom(Atom, Options))
              ])).

scasp_atom_string(goal_origin(Atom, _Origin), String) =>
    scasp_atom_string(Atom, String).
scasp_atom_string(Atom, String) =>
    with_output_to(string(String),
                   print_model_term_v(Atom, [])).

print_model_term_v(Atom, Options) :-
    \+ \+ ( inline_constraints(Atom, Options),
            print_model_term(Atom, Options)
          ).


%!  html_model(:Model, +Options)// is det.
%
%   Emit the model as HTML terms.   We export the model as a dict with
%   nested model terms.

html_model(M:Model, Options) -->
    { (   option(class(Class), Options)
      ->  Classes = [Class]
      ;   Classes = []
      ),
      Options1 = [module(M)|Options]
    },
    emit(ul(class(['scasp-model'|Classes]),
            \sequence(model_item_r(Options1), Model))).

model_item_r(Options, Atom) -->
    emit(li(class('scasp-atom'),
                  \model_term(Atom, Options))).

%!  html_model_term(:Atom, +Options)// is det.
%
%   Emit a model term.

html_model_term(M:Atom, Options) -->
    model_term(Atom, [module(M)|Options]).

model_term(Atom, Options) -->
    { option(show(human), Options),
      !
    },
    atom(Atom, Options).
model_term(Atom, Options) -->
    { option(show(machine), Options),
      !
    },
    machine_atom(Atom, Options).
model_term(Atom, Options) -->
    { scasp_atom_string(Atom, String)
    },
    emit([ span([class(human), title(String)], \atom(Atom, Options)),
           span(class(machine), \machine_atom(Atom, Options))
         ]).


%!  html_bindings(+Bindings, +Options)//
%
%   Print the variable bindings.

html_bindings([], _Options) -->
    [].
html_bindings([H|T], Options) -->
    (   {T==[]}
    ->  html_binding(H, [last(true)|Options])
    ;   html_binding(H, Options),
        html_bindings(T, Options)
    ).

html_binding(Name=Value, Options) -->
    emit(div(class('scasp-binding'),
             [ var(Name),
               ' = ',
               \html_term(Value, Options),
               \connect_binding(Options)
             ])).

connect_binding(Options) -->
    { option(last(true), Options) },
    !.
connect_binding(_Options) -->
    emit(',').

%!  html_program(:Dict)
%
%

html_program(Dict) :-
    phrase(html_program(Dict), Tokens),
    print_html(current_output, Tokens).

%!  html_program(:Dict)//
%
%   Emit the current program in human format using HTML.

html_program(M:Dict) -->
    { Dict1 = Dict.put(module, M)
    },
    html_program_section(query,       Dict1),
    html_program_section(user,        Dict1),
    html_program_section(duals,       Dict1),
    html_program_section(constraints, Dict1),
    html_program_section(dcc,         Dict1).

html_program_section(Section, Dict) -->
    { _{module:M, options:Options} :< Dict,
      Content = Dict.get(Section),
      Content \= [],
      scasp_code_section_title(Section, Default, Title),
      Opt =.. [Section,true],
      option(Opt, Options, Default)
    },
    !,
    html(h2(Title)),
    (   {Section == query}
    ->  {ovar_set_bindings(Dict.bindings)},
        html_query(M:Content, Options)
    ;   sequence(predicate_r(M:Options), Content)
    ).
html_program_section(_, _) -->
    [].

predicate_r(M:Options, Clauses) -->
    html_predicate(M:Clauses, Options).


%!  html_query(:Query, +Options)//
%
%   Emit the query. This rule accepts  the   query  both  in s(CASP) and
%   normal Prolog notation.

:- det(html_query//2).

html_query(M:Query, Options) -->
    { (   is_list(Query)
      ->  prolog_query(Query, Prolog)
      ;   Prolog = Query
      ),
      !,
      comma_list(Prolog, List0),
      clean_query(List0, List)
    },
    (   { option(show(human), Options) }
    ->  emit(div(class('scasp-query'),
                 [ div(class('scasp-query-title'), 'I would like to know if'),
                   \query_terms(List, [module(M)|Options])
                 ]))
    ;   { option(show(machine), Options) }
    ->  emit(div(class('scasp-query'),
                 [ span(class('scasp-query-title'), '?- '),
                   \term(Prolog, [numbervars(true)|Options])
                 ]))
    ;   emit(div(class('scasp-query'),
                  [ div(class(human),
                        [ div(class('scasp-query-title'),
                              'I would like to know if'),
                          \query_terms(List, [module(M)|Options])
                        ]),
                    div(class(machine),
                        [ '?- ', \term(Prolog, [numbervars(true)|Options])
                        ])
                  ]))
    ).
html_query(_, _) -->
    emit(div(class(comment), '% No query')).

prolog_query([not(o_false)], _) =>
    fail.
prolog_query(List, Query), is_list(List) =>
    clean_query(List, List1),
    (   List1 == []
    ->  Query = true
    ;   comma_list(Query, List1)
    ).

clean_query(Q0, Q) :-
    delete(Q0, o_nmr_check, Q1),
    delete(Q1, true, Q).

query_terms([], Options) -->
    query_term(true, Options).
query_terms([H1,H2|T], Options) -->
    !,
    query_term(H1, [connect(and)|Options]),
    query_terms([H2|T], Options).
query_terms([Last], Options) -->
    { option(end(End), Options, ?)
    },
    query_term(Last, [connect(End)|Options]).

query_term(Term, Options) -->
    emit(div(class('scasp-query-literal'),
             [ \atom(Term, Options),
               \connect(Options)
             ])).

%!  html_predicate(:Rules, Options)//

html_predicate(M:Clauses, Options) -->
    emit(div(class('scasp-predicate'),
             \sequence(html_rule_r(M, Options), Clauses))).

html_rule_r(M, Options, Clause) -->
    html_rule(M:Clause, Options).

%!  html_rule(:Rule, +Options)//

html_rule(Rule, Options) -->
    { ovar_analyze_term(Rule) },
    html_rule_(Rule, Options),
    { ovar_clean(Rule) }.

html_rule_(M:(Head0 :- Body), Options) -->
    !,
    { MOptions = [module(M)|Options],
      raise_negation(Head0, Head)
    },
    emit(div(class('scasp-rule'),
             [ div(class('scasp-head'),
                   [ \atom(Head, MOptions),
                     ', if'
                   ]),
               div(class('scasp-body'),
                   \html_body(Body, [end(.)|MOptions]))
             ])).
html_rule_(M:Head, Options) -->
    emit(div(class('scasp-rule'),
             div(class('scasp-head'),
                 [ \atom(Head, [module(M)|Options]),
                   \connector('.', Options)
                 ]))).

html_body(forall(X, not(Goal)), Options) -->
    !,
    emit(div(class('scasp-query-literal'),
             [ 'there exist no ', \html_term(X, Options),
               ' for which ', \atom(Goal, Options)
             ])).
html_body(Body, Options) -->
    { comma_list(Body, List0),
      maplist(raise_negation, List0, List)
    },
    query_terms(List, Options).



%!  atom(+SCASPAtom, +Options)//
%
%   Emit an s(CASP) atom with annotations as   they  appear in the model
%   and justification.

atom(Atom, Options) -->
    { option(pred(true), Options, true),
      option(module(DefM), Options),
      option(source_module(M), Options, DefM),
      human_expression(M:(Atom-[]), [], Actions),
      !,
      css_classes(Options, Classes)
    },
    emit(span(class(Classes), \actions(Actions, Options))).
atom(not(GlobalConstraint), Options) -->
    { is_global_constraint(GlobalConstraint, N)
    },
    !,
    utter(global_constraint(N), Options).
atom(not(Term), Options) -->
    !,
    utter(not(Term), [class(not)|Options]).
atom(-Term, Options) -->
    !,
    utter(-(Term), [class(neg)|Options]).
atom(proved(Term), Options) -->
    !,
    utter(proved(Term), [class(proved)|Options]).
atom(assume(Term), Options) -->
    !,
    utter(assume(Term), [class(assume)|Options]).
atom(chs(Term), Options) -->
    !,
    utter(chs(Term), [class(chs)|Options]).
atom(abduced(Term), Options) -->
    !,
    utter(abduced(Term), [class(abducible)|Options]).
atom(M:Term, Options) -->
    { atom(M) },
    !,
    atom(Term, [module(M)|Options]).
atom(o_nmr_check, Options) -->
    !,
    utter(global_constraints_hold, Options).
atom(is(Value,Expr), Options) -->
    !,
    { css_classes(Options, Classes)
    },
    emit(span(class([arithmetic|Classes]),
              [ \expr(Expr, Options), &(nbsp), is, &(nbsp), \html_term(Value, Options)
              ])).
atom(Comp, Options) -->
    { human_connector(Comp, Text),
      !,
      Comp =.. [_,Left,Right],
      css_classes(Options, Classes)
    },
    emit(span(class([arithmetic|Classes]),
              [ \html_term(Left, Options),
                &(nbsp), Text, &(nbsp),
                \html_term(Right, Options)
              ])).
atom(Term, Options) -->
    utter(holds(Term), Options).

%!  expr(+Term, +Options)// is det.
%
%   Render an expression.
%
%   @tbd Should deal with parenthesis  where   needed.  Possibly it is a
%   better option to use term//2 from  library(http/html_term) and add a
%   portray hook for that?

expr(Term, Options) -->
    { compound(Term),
      compound_name_arguments(Term, Op, [Left, Right])
    },
    !,
    emit(span([ \expr(Left, Options),
                &(nbsp), Op, &(nbsp),
                \expr(Right, Options)
              ])).
expr(Term, Options) -->
    { compound(Term),
      compound_name_arguments(Term, Op, [Arg])
    },
    !,
    emit(span([ Op,
                \expr(Arg, Options)
              ])).
expr(Simple, Options) -->
    html_term(Simple, Options).

%!  utter(+Expression, +Options)

utter(global_constraints_hold, _Options) -->
    { human_connector(global_constraints_hold, Text) },
    emit(Text).
utter(global_constraint(N), _Options) -->
    { human_connector(global_constraint(N), Text) },
    emit(Text).
utter(not(-(Atom)), Options) -->
    !,
    { human_connector(may, Text) },
    emit([Text, ' ']),
    atom(Atom, Options).
utter(not(Atom), Options) -->
    { human_connector(not, Text) },
    emit([Text, ' ']),
    atom(Atom, Options).
utter(-(Atom), Options) -->
    { human_connector(-, Text) },
    emit([Text, ' ']),
    atom(Atom, Options).
utter(proved(Atom), Options) -->
    { human_connector(proved, Text) },
    atom(Atom, Options),
    emit([', ', Text]).
utter(chs(Atom), Options) -->
    { human_connector(chs, Text) },
    emit([Text, ' ']),
    atom(Atom, Options).
utter(abduced(Atom), Options) -->
    { human_connector(abducible, Text) },
    emit([Text, ' ']),
    atom(Atom, Options).
utter(according_to(File, Line), _Options) -->
    { human_connector(according_to, Text) },
    emit(span(class('scasp-source-reference'),
              span(class(human), [' [', Text, ' ~w:~w]'-[File, Line]]))).
utter(assume(Atom), Options) -->
    { human_connector(assume, Text) },
    emit([Text, ' ']),
    atom(Atom, Options).
utter(holds(Atom), Options) -->
    { css_classes(Options, Classes) },
    (   { atom(Atom) }
    ->  { human_connector(holds, Text) },
        emit([span(class(Classes), Atom), Text])
    ;   { Atom =.. [Name|Args] }
    ->  { human_connector(holds_for, Text) },
        emit([span(class(Classes), Name), Text]),
        list(Args, Options)
    ).

css_classes(Options, [atom|Classes]) :-
    findall(Class, member(class(Class), Options), Classes0),
    (   Classes0 == []
    ->  Classes = [pos]
    ;   Classes = Classes0
    ).


:- det(html_term//2).

html_term(Var, Options) -->
    { var(Var) },
    !,
    var(Var, Options).
html_term(@(Var:''), Options) -->
    { var(Var)
    },
    !,
    var(Var, Options).
html_term(@(Var:Type), Options) -->
    { var(Var)
    },
    !,
    typed_var(Var, Type, Options).
html_term(@(Value:''), Options) -->
    !,
    html_term(Value, Options).
html_term(@(Value:Type), Options) -->
    emit('the ~w '-[Type]),
    !,
    html_term(Value, Options).
html_term(Term, _Options) -->
    { var_number(Term, _) },
    !,
    emit('~p'-[Term]).
html_term('| '(Var, {Constraints}), Options) -->
    !,
    inlined_var(Var, Constraints, Options).
html_term(Term, _Options) -->
    { emitting_as(plain) },
    !,
    [ ansi(code, '~p', [Term]) ].
html_term(Term, Options) -->
    term(Term, [numbervars(true)|Options]).

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
    emit(var(Name)).
var(_, _) -->
    emit(anything).

%!  inlined_var(+Var, +Constraint, +Options)//
%
%   Deal with constraints as represented after inline_constraints/2.

inlined_var(Var, Constraints, Options) -->
    { Constraints = '\u2209'(Var, List),
      Var == '$VAR'('_')
    },
    !,
    (   {List = [One]}
    ->  emit('anything except for '),
        html_term(One, Options)
    ;   emit('anything except for '),
        list(List, [last_connector(or)|Options])
    ).
inlined_var(Var, Constraints, Options) -->
    { Constraints = '\u2209'(Var, List),
      compound(Var),
      Var = '$VAR'(Name)
    },
    !,
    (   {List = [One]}
    ->  {human_connector(neq, Text)},
        emit([var(Name), ' ', Text, ' ']),
        html_term(One, Options)
    ;   {human_connector(not_in, Text)},
        emit([var(Name), ' ', Text, ' ']),
        list(List, [last_connector(or)|Options])
    ).
inlined_var(Var, Constraints, Options) -->
    { comma_list(Constraints, CLPQ)
    },
    clpq(Var, CLPQ, Options).

%!  clpq(@Var, +Constraints, +Options)//

clpq(Var, [Constraint|More], Options) -->
    { compound(Constraint),
      Constraint =.. [_,A,B],
      Var == A,
      human_connector(Constraint, Text),
      (   nonvar(Var),
          Var = '$VAR'(Name)
      ->  Id = var(Name)
      ;   Id = number
      )
    },
    emit(['any ', Id, ' ', Text, ' ']),
    html_term(B, Options),
    (   {More == []}
    ->  []
    ;   emit(' and '),
        clpq_and(More, Var, Options)
    ).

clpq_and([Constraint|More], Var, Options) -->
    { compound(Constraint),
      Constraint =.. [_,A,B],
      A == Var,
      human_connector(Constraint, Text)
    },
    emit([Text, ' ']),
    html_term(B, Options),
    (   {More == []}
    ->  []
    ;   emit(' and '),
        clpq_and(More, Var, Options)
    ).

%!  typed_var(@Var, +Type, +Options)//

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
    emit([var(Name), ', a ', Type]).
typed_var(_Var, Type, _Options) -->
    emit(['a ', Type]).


inlined_typed_var(Var, Type, Constraints, Options) -->
    { Constraints = '\u2209'(Var, List),
      Var == '$VAR'('_')
    },
    !,
    (   {List = [One]}
    ->  emit(['any ', Type, ' except for ']),
        html_term(One, Options)
    ;   emit(['any ', Type, ' except for ']),
        list(List, [last_connector(or)|Options])
    ).
inlined_typed_var(Var, Type, Constraints, Options) -->
    { Constraints = '\u2209'(Var, List),
      nonvar(Var),
      Var = '$VAR'(Name)
    },
    !,
    (   {List = [One]}
    ->  emit([var(Name), ', a ', Type, ' other than ']),
        html_term(One, Options)
    ;   emit([var(Name), ', a ', Type, ' not ']),
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
    { option(last_connector(Conn), Options, and),
      human_connector(Conn, Text)
    },
    html_term(L1, Options),
    emit(', ~w '-[Text]),
    html_term(L, Options).
list([H|T], Options) -->
    html_term(H, Options),
    (   {T==[]}
    ->  []
    ;   emit(', '),
        list(T, Options)
    ).

actions(html(HTML), _) -->
    !,
    emit(HTML).
actions([], _) --> [].
actions([H|T], Options) -->
    action(H, Options),
    actions(T, Options).

action(text(S), _) -->
    !,
    emit(S).
action(Term, Options) -->
    html_term(Term, Options).

%!  connector(+Meaning, +Options)//
%
%   Emit a logical connector.

connector(Meaning, Options) -->
    { option(show(human), Options),
      !,
      human_connector(Meaning, Text)
    },
    emit_human_connector(Meaning, Text, Options).
connector(Meaning, Options) -->
    { option(show(machine), Options)
    },
    !,
    emit_machine_connector(Meaning, Options).
connector(Meaning, Options) -->
    { human_connector(Meaning, Text)
    },
    emit([ span(class(human),   \emit_human_connector(Meaning, Text, Options)),
           span(class(machine), \emit_machine_connector(Meaning, Options))
         ]).

emit_human_connector(and, Text, _) --> emit([', ', Text]).
emit_human_connector(not, Text, _) --> emit([Text, ' ']).
emit_human_connector(-,   Text, _) --> emit([Text, ' ']).
emit_human_connector(?,   Text, _) --> emit(Text).
emit_human_connector(.,   Text, _) --> emit(Text).
emit_human_connector(implies, Text, Options) -->
    emit([', ', \origin_annotated(Text, Options)]).

emit_machine_connector(and, _) --> emit(',').
emit_machine_connector(not, _) --> emit('not ').
emit_machine_connector(-,   _) --> emit('\u00ac ').
emit_machine_connector(?,   _) --> emit(?).
emit_machine_connector(.,   _) --> emit(.).
emit_machine_connector(implies, Options) -->
    origin_annotated(' \u2190', Options).

human_connector(Term, Connector) :-
    phrase(scasp_justification_message(Term), List),
    (   List = [Connector]
    ->  true
    ;   Connector = List
    ).

full_stop(Options) -->
    { option(show(human), Options) },
    !.
full_stop(_Options) -->
    emit(span([class(machine), title('QED')], '\u220e')).

incr_indent(Options0, [depth(D)|Options2]) :-
    select_option(depth(D0), Options0, Options1),
    select_option(connect(_), Options1, Options2, _),
    D is D0+1.

		 /*******************************
		 *         MACHINE HTML		*
		 *******************************/

%!  machine_atom(+SCASPAtom, +Options)//
%
%   Emit an s(CASP) atom with annotations as   they  appear in the model
%   and justification.

machine_atom(goal_origin(Term, _Origin), Options) -->
    !,
    machine_atom(Term, Options).
machine_atom(not(Term), Options) -->
    !,
    emit([span(class([connector,not]), not), ' ']),
    machine_atom(Term, [class(not)|Options]).
machine_atom(-Term, Options) -->
    !,
    emit([span(class([connector,neg]), '\u00ac'), ' ']),
    machine_atom(Term, [class(neg)|Options]).
machine_atom(proved(Term), Options) -->
    !,
    emit([ span(class([connector,proved]), proved), '(',
           \machine_atom(Term, [class(proved)|Options]),
           ')'
         ]).
machine_atom(assume(Term), Options) -->
    !,
    emit([ span(class([connector,assume]), assume), '(',
           \machine_atom(Term, [class(assume)|Options]),
           ')'
         ]).
machine_atom(chs(Term), Options) -->
    !,
    emit([ span(class([connector,chs]), chs), '(',
           \machine_atom(Term, [class(chs)|Options]),
           ')'
         ]).
machine_atom(M:Term, Options) -->
    { atom(M) },
    !,
    emit(span(class(module), [M,:])),
    machine_atom(Term, [module(M)|Options]).
machine_atom(Term, Options) -->
    { css_classes(Options, Classes),
      merge_options(Options, [numbervars(true)], WOptions)
    },
    emit(span(class(Classes), \term(Term, WOptions))).

:- multifile
    term_html:portray//2.

term_html:portray(Term, Options) -->
    { nonvar(Term),
      Term = '| '(Var, Constraints)
    },
    term(Var, Options),
    emit(' | '),
    term(Constraints, Options).

origin(File, Line, Options) -->
    {    option(source(true), Options)   },
    !,
    utter(according_to(File, Line), Options).
origin(_, _, _) --> [].

origin_annotated(Content, Options) -->
    { option(origin(File:Line), Options)
    },
    !,
    emit(span([ class(scasp_origin),
                'data-file'(File),
                'data-line'(Line)
              ], Content)).
origin_annotated(Content, _) -->
    emit(Content).
