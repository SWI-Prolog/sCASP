:- module(scasp_output,
          [ print_model_term/2,
            inline_constraints/2,                % +Term, +Options
            connector/3,                         % +Semantics,-Conn,+Options
            print_connector/2,
            ovar_analyze_term/1,                 % +Term
            ovar_analyze_term/2,                 % +Term,+Options
            ovar_clean/1,                        % +Term
            ovar_is_singleton/1,                 % @Var
            ovar_var_name/2,                     % @Var, -Name
            ovar_set_name/2,                     % +Var, +Name
            ovar_set_bindings/1,                 % +Bindings
            human_expression/3                   % :Tree, -Children, -Actions
          ]).
:- use_module(library(ansi_term)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(prolog_code)).
:- use_module(library(prolog_format)).
:- use_module(library(terms)).

:- use_module(clp/disequality).
:- use_module(clp/clpq).

:- meta_predicate
    human_expression(:, -, -).


/** <module> Emit sCASP terms
*/

%!  print_model_term(+Term, +Options) is det.
%
%   Print a model element to the terminal.

:- det(print_model_term/2).

print_model_term(not(-Term), Options) =>
    print_connector(not, Options),
    print_connector(negation, Options),
    print_plain(Term, likely, Options).
print_model_term(-Term, Options) =>
    print_connector(negation, Options),
    print_plain(Term, false, Options).
print_model_term(not(Term), Options) =>
    print_connector(not, Options),
    print_plain(Term, unlikely, Options).
print_model_term(Term, Options) =>
    print_plain(Term, true, Options).

print_plain(M:Term, Trust, Options) :-
    atom(M),
    !,
    ansi_format(fg('#888'), '~q:', [M]),
    print_plain(Term, Trust, Options).
print_plain(Term, Trust, _Options) :-
    style(Trust, Style),
    ansi_format(Style, '~p', [Term]).

print_connector(Semantics, Options) :-
    connector(Semantics, Conn, Options),
    ansi_format(fg(cyan), '~w', [Conn]).

style(true,     [bold]).
style(false,    [bold, fg(red)]).
style(likely,   []).
style(unlikely, [fg(red)]).


%!  connector(+Semantics, -Conn, +Options) is det.
%
%   Get an ASCII or Unicode connector string with the claimed Semantics.

:- det(connector/3).

connector(Semantics, Conn, Options) :-
    option(format(Format), Options, unicode),
    connector_string(Semantics, Format, Conn),
    !.

connector_string(implies,  ascii, ':-').
connector_string(and,      ascii, ',').
connector_string(negation, ascii, '-').

connector_string(implies,  unicode, '\u2190').   % <-
connector_string(and,      unicode, ' \u2227').  % /\
connector_string(negation, unicode, '\u00ac ').  % -

connector_string(not,      _, 'not ').
connector_string(proved,   _, 'proved').
connector_string(assume,   _, 'assume').
connector_string(chs,      _, 'chs').


		 /*******************************
		 *          CONSTRAINTS		*
		 *******************************/

%!  inline_constraints(+Term, +Options) is det.
%
%   Get constraints on variables notated as  ``Var | {Constraints}`` and
%   use assigned variable names. Note  that   this  binds the attributed
%   variables in Term. This code is normally   used  on a copy or inside
%   double  negation  (``\+  \+   (  inline_constraints(Term,  Options),
%   ...)``).

inline_constraints(Term, Options) :-
    term_attvars(Term, AttVars),
    maplist(inline_constraint(Options), AttVars).

inline_constraint(_Options, Var) :-
    get_neg_var(Var, List),
    List \== [],
    !,
    var_name(Var, Name),
    del_attrs(Var),
    Var = '| '(Name, {'\u2209'(Name, List)}).
inline_constraint(_Options, Var) :-
    is_clpq_var(Var),
    clpqr_dump_constraints([Var], [Var], Constraints),
    Constraints \== [],
    !,
    var_name(Var, Name),
    sort(0, @>, Constraints, Sorted),
    comma_list(Term0, Sorted),
    del_attrs(Var),
    replace_var(Var, Name, Term0, Term),
    Var = '| '(Name, {Term}).
inline_constraint(_Options, Var) :-
    var_name(Var, Name),
    del_attrs(Var),
    Var = Name.

var_name(Var, Name) :-
    ovar_var_name(Var, Name0),
    !,
    Name = '$VAR'(Name0).
var_name(Var, Name) :-
    ovar_is_singleton(Var),
    !,
    Name = '$VAR'('_').
var_name(Var, Var).

%!  replace_var(+Var, +Name, +TermIn, -TermOut)

replace_var(Var, Name, TermIn, TermOut) :-
    Var == TermIn,
    !,
    TermOut = Name.
replace_var(Var, Name, TermIn, TermOut) :-
    compound(TermIn),
    !,
    same_functor(TermIn, TermOut, Arity),
    replace_var(1, Arity, Var, Name, TermIn, TermOut).
replace_var(_, _, Term, Term).

replace_var(I, Arity, Var, Name, TermIn, TermOut) :-
    I =< Arity,
    !,
    arg(I, TermIn, AIn),
    arg(I, TermOut, AOut),
    replace_var(Var, Name, AIn, AOut),
    I2 is I+1,
    replace_var(I2, Arity, Var, Name, TermIn, TermOut).
replace_var(_, _, _, _, _, _).





		 /*******************************
		 *        VARIABLE ANALYSIS	*
		 *******************************/

%!  ovar_analyze_term(+Term) is det.
%!  ovar_analyze_term(+Term, +Options) is det.
%
%   Analyze variables in an  output  term.   Adds  attributes  to  these
%   variables that indicate their status and make this available through
%   ovar_is_singleton/1 and ovar_var_name/1.

ovar_analyze_term(Tree) :-
    ovar_analyze_term(Tree, []).

ovar_analyze_term(Tree, Options) :-
    term_attvars(Tree, AttVars),
    convlist(ovar_var_name, AttVars, VarNames),
    term_singletons(Tree, Singletons),
    term_variables(Tree, AllVars),
    (   option(name_constraints(true), Options)
    ->  maplist(mark_singleton_no_attvar, Singletons)
    ;   maplist(mark_singleton, Singletons)
    ),
    foldl(name_variable(VarNames), AllVars, 0, _).

mark_singleton(Var) :-
    (   ovar_var_name(Var, _)
    ->  true
    ;   put_attr(Var, scasp_output, singleton)
    ).

mark_singleton_no_attvar(Var) :-
    (   attvar(Var)
    ->  true
    ;   put_attr(Var, scasp_output, singleton)
    ).

name_variable(Assigned, Var, N0, N) :-
    (   (   ovar_is_singleton(Var)
        ;   ovar_var_name(Var, _)
        )
    ->  N = N0
    ;   between(N0, 100000, N1),
        L is N1 mod 26 + 0'A,
        I is N1 // 26,
        (   I == 0
        ->  char_code(Name, L)
        ;   format(atom(Name), '~c~d', [L, I])
        ),
        \+ memberchk(Name, Assigned)
    ->  ovar_set_name(Var, Name),                % make sure it is unique
        N is N1+1
    ).

attr_unify_hook(_Attr, _Value).

%!  ovar_clean(+Term)
%
%   Delete all attributes added by ovar_analyze_term/1

ovar_clean(Term) :-
    term_attvars(Term, Attvars),
    maplist(del_var_info, Attvars).

del_var_info(V) :-
    del_attr(V, scasp_output).


%!  ovar_is_singleton(@Var) is semidet.
%
%   True when Var is a singleton variable

ovar_is_singleton(Var) :-
    get_attr(Var, scasp_output, singleton).

%!  ovar_set_name(+Var, +Name)
%
%   Set the name of Var to Name.

ovar_set_name(Var, Name) :-
    put_attr(Var, scasp_output, name(Name)).

%!  ovar_set_bindings(+Bindings) is det.
%
%   Given Bindings as a  list  of  `Name=Var`,   set  the  names  of the
%   variables.

ovar_set_bindings(Bindings) :-
    maplist(ovar_set_binding, Bindings).

ovar_set_binding(Name=Var) :-
    (   var(Var)
    ->  ovar_set_name(Var, Name)
    ;   true
    ).

%!  ovar_var_name(@Var, -Name) is semidet.
%
%   True when var is not a singleton and has the assigned name.  Names
%   are assigned as `A`, `B`, ... `A1`, `B1`, ...

ovar_var_name(Var, Name) :-
    get_attr(Var, scasp_output, name(Name)).


		 /*******************************
		 *         HUMAN (#PRED)	*
		 *******************************/

%!  human_expression(:Tree, -Children, -Actions) is semidet.
%
%   If there is a human print rule  for   Atom,  return a list of format
%   actions as Actions. Actions is currently  a list of text(String) and
%   `@(Var:Type)`, where `Type` can be the empty atom.

human_expression(Tree, Children, Actions) :-
    tree_atom_children(Tree, M, Atom, ChildrenIn),
    current_predicate(M:pr_pred_predicate/4),
    \+ predicate_property(M:pr_pred_predicate(_,_,_,_), imported_from(_)),
    human_utterance(Atom, ChildrenIn, M, Children, Human),
    (   Human = format(Fmt, Args)
    ->  parse_fmt(Fmt, Args, Actions)
    ;   Actions = Human                          % html(Terms)
    ).

tree_atom_children(M0:(Atom0-Children), M, Atom, Children) :-
    clean_atom(Atom0, M0, Atom, M).

clean_atom(goal_origin(Atom0, _), M0, Atom, M) =>
    clean_atom(Atom0, M0, Atom, M).
clean_atom(not(-Atom0), M0, Atom, M) =>
    Atom = not(-Atom1),
    strip_module(M0:Atom0, M, Atom1).
clean_atom(not(Atom0), M0, Atom, M) =>
    Atom = not(Atom1),
    strip_module(M0:Atom0, M, Atom1).
clean_atom(-(Atom0), M0, Atom, M) =>
    Atom = -(Atom1),
    strip_module(M0:Atom0, M, Atom1).
clean_atom(Atom0, M0, Atom, M) =>
    strip_module(M0:Atom0, M, Atom).

human_utterance(Atom, Children0, M, Children, Format) =>
    M:pr_pred_predicate(Atom, ChildSpec, Cond, Format),
    match_children(ChildSpec, M, Children0, Children),
    call(Cond),
    !.

match_children(*, _, Children0, Children) =>
    Children = Children0.
match_children(-, _, _, Children) =>
    Children = [].
match_children([H|T], M, Children0, Children) =>
    append(Pre, [Atom-C0|Post], Children0),
    match_node(H, M, Atom, C0, C),
    !,
    append([Pre,C,Post], Children2),
    match_children(T, M, Children2, Children).
match_children([], _, Children0, Children) =>
    Children = Children0.

match_node(Node-ChildSpec, M, Atom, C0, C) =>
    match_node(Node, M, Atom),
    match_children(ChildSpec, M, C0, C).
match_node(Node, M, Atom, C0, C) =>
    match_node(Node, M, Atom),
    C = C0.

%!  match_node(+Spec, +Module, +Atom) is semidet.
%
%   Succeed if Spec (from the `:-   pred` declaration) matches Atom from
%   the justification tree.

match_node(Node, M, goal_origin(Child, _)) :-
    !,
    match_node(Node, M, Child).
match_node(Node, _M, Node).
match_node(Node, _M, proved(Node)).
match_node(not(Node), M, not(M:Node)).
match_node(Node, M, proved(M:Node)).
match_node(Node, M, M:Node).

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

% portray rules for inlines constraints. Possibly we need something that
% alows us to specify that an operator always requires spaces?

user:portray('| '(Var, Constraints)) :-
    format('~p | ~p', [Var, Constraints]).
user:portray('\u2209'(Var, List)) :-
    format('~p \u2209 ~p', [Var, List]).
