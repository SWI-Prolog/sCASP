:- module(scasp_json,
          [ scasp_results_json/2        % +Results, -Dict
          ]).
:- use_module(output).
:- use_module(options).
:- use_module(clp/disequality).
:- use_module(clp/clpq).

/** <module> s(CASP) JSON I/O

*/

%!  scasp_results_json(+Results, -Dict) is det.
%
%

:- det(scasp_results_json/2).
scasp_results_json(Result, Dict) :-
    _{ query: Query,
       cpu: Time,
       answers: Answers,
       inputs: Inputs
     } :< Result,
    Dict = scasp_result{ solver: Version,
                         query: JQuery,
                         time: Time,
                         answers: JAnswers,
                         inputs: Inputs
                       },
    scasp_version(Version),
    query_json(Query, JQuery),
    maplist(answer_json, Answers, JAnswers).

:- meta_predicate query_json(:, -).
:- det(query_json/2).
query_json(_:Query, JQuery) :-
    delete(Query, o_nmr_check, Query1),
    delete(Query1, true, Query2),
    plain_term_json(Query2, JQuery).


%!  answer_json(+Answer, -Dict) is det.

:- det(answer_json/2).
answer_json(Answer, Dict),
    _{ answer:Counter,
       bindings:Bindings,
       model:Model,
       tree:_:Tree,
       time:Time
     } :< Answer =>
    Dict = scasp_answer{answer: Counter,
                        time: Time.cpu,
                        bindings: JBindings,
                        model: JModel,
                        tree: JTree,
                        constraints: Constraints},
    maplist(binding_json, Bindings, Pairs),
    dict_create(JBindings, #, Pairs),
    maplist(model_term_json, Model, JModel),
    tree_json(Tree, JTree),
    constraints_json(t(Bindings,Model,JTree), Constraints).
answer_json(Answer, Dict),
    _{ answer:Counter,
       bindings:Bindings,
       model:Model,
       time:Time
     } :< Answer =>
    Dict = scasp_answer{answer: Counter,
                        time: Time.cpu,
                        bindings: JBindings,
                        model: JModel,
                        constraints: Constraints},
    maplist(binding_json, Bindings, Pairs),
    dict_create(JBindings, #, Pairs),
    maplist(model_term_json, Model, JModel),
    constraints_json(t(Bindings,Model), Constraints).

binding_json(Name=Value, Name-JValue) :-
    model_term_json(Value, JValue).

tree_json(Root-Children, Dict) =>
    Dict = #{ node: JRoot,
              children: JChildren },
    node_json(Root, JRoot),
    maplist(tree_json, Children, JChildren).

node_json(chs(Node), Dict) =>
    model_term_json(Node, Dict0),
    Dict = Dict0.put(chs, true).
node_json(assume(Node), Dict) =>
    model_term_json(Node, Dict0),
    Dict = Dict0.put(assume, true).
node_json(proved(Node), Dict) =>
    model_term_json(Node, Dict0),
    Dict = Dict0.put(proved, true).
node_json(Node, Dict) =>
    model_term_json(Node, Dict).


%!  model_term_json(+ModelTerm, -Dict) is det.

:- det(model_term_json/2).
model_term_json(not(-Term), Dict) =>
    Dict = scasp_model_term{truth: likely,   value: TermJSON},
    module_term_json(Term, TermJSON).
model_term_json(-Term, Dict) =>
    Dict = scasp_model_term{truth: false,    value: TermJSON},
    module_term_json(Term, TermJSON).
model_term_json(not(Term), Dict) =>
    Dict = scasp_model_term{truth: unlikely, value: TermJSON},
    module_term_json(Term, TermJSON).
model_term_json(Term, Dict) =>
    Dict = scasp_model_term{truth: true,     value: TermJSON},
    module_term_json(Term, TermJSON).

module_term_json(M:Term, Dict) =>
    plain_term_json(Term, Dict0),
    Dict = Dict0.put(module, M).
module_term_json(Term, Dict) =>
    plain_term_json(Term, Dict).

%!  plain_term_json(+Term, -Dict) is det.

:- det(plain_term_json/2).
plain_term_json(Var, Dict), var(Var) =>
    (   ovar_var_name(Var, Name)
    ->  Dict = prolog{type: var,
                      name: Name}
    ;   ovar_is_singleton(Var)
    ->  Dict = prolog{type: var}
    ).
plain_term_json(Var, Dict), var_number(Var, Num) =>
    (   Num == '_'
    ->  Dict = prolog{type: var}
    ;   format(string(S), '~p', [Var]),
        Dict = prolog{type: var,
                      name: S}
    ).
plain_term_json(Atom, Dict), atom(Atom) =>
    Dict = atom{type:atom, value:Atom}.
plain_term_json(Num, Dict), rational(Num, N, D) =>
    (   current_prolog_flag(scasp_rational, float)
    ->  Value is float(Num),
        Dict = prolog{type:number, value:Value}
    ;   Dict = prolog{type:rational, numerator:N, denominator:D}
    ).
plain_term_json(Num, Dict), number(Num) =>
    Dict = prolog{type:number, value:Num}.
plain_term_json(List, JList), is_list(List) =>
    maplist(plain_term_json, List, JList).
plain_term_json(Compound, Dict), compound(Compound) =>
    compound_name_arguments(Compound, Name, Arguments),
    Dict = prolog{type:compound, functor:Name, args:JArgs},
    maplist(plain_term_json, Arguments, JArgs).

%!  constraints_json(+Term, -Dict) is det.

:- det(constraints_json/2).
constraints_json(Term, Dict) :-
    term_attvars(Term, Attvars),
    include(has_constraints, Attvars, CVars),
    copy_term(CVars, Copy),
    inline_constraints(Copy, []),
    maplist(constraint_json, Copy, Constraints),
    dict_create(Dict, #, Constraints).

has_constraints(Var) :-
    get_neg_var(Var, _List),
    !.
has_constraints(Var) :-
    is_clpq_var(Var).

constraint_json('| '(Var, {'\u2209'(Var, List)}), Pair) =>
    Pair = Name-constraint{ type: not_in, set: JList},
    var_name(Var, Name),
    maplist(plain_term_json, List, JList).
constraint_json('| '(Var, {Term}), Pair) =>
    Pair = Name-constraint{ type: clpq, constraints: Constraints},
    var_name(Var, Name),
    comma_list(Term, Constraints0),
    maplist(clpq_json(Var), Constraints0, Constraints).

var_name('$VAR'(Name0), Name) =>
    Name = Name0.
var_name(Var, Name), ovar_var_name(Var, Name0) =>
    Name = Name0.

clpq_json(Var, Term, Dict), Term =.. [Var, Op, Arg] =>
    Dict = constraint{type:  Op, value: JArg},
    plain_term_json(Arg, JArg).


:- multifile
    json:json_dict_pairs/2.

json:json_dict_pairs(Dict, Pairs) :-
    is_dict(Dict, Tag),
    order(Tag, Order),
    dict_keys(Dict, All),
    sort(Order, Ordered),
    ord_subtract(All, Ordered, Unordered),
    phrase(json_pairs(Order, Dict), Pairs, Pairs1),
    phrase(json_pairs(Unordered, Dict), Pairs1).

json_pairs([], _) -->
    [].
json_pairs([H|T], Dict) -->
    (   {get_dict(H, Dict, Value)}
    ->  [H-Value]
    ;   []
    ),
    json_pairs(T, Dict).

order(scasp_result,     [solver,inputs,query,time,answers]).
order(scasp_answer,     [answer,time,bindings,model,tree]).
order(scasp_model_term, [truth, value, chs, assume, proved]).
order(prolog,           [type,functor,numerator,denominator,name,value]).
order(constraint,       [type,set,constraints,value]).
