:- module(test_json,
          [ test_json/0
          ]).
:- use_module('../scasp_plunit', []).        % sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/json)).
:- use_module(library(sscasp/run), [scasp/4]).
:- use_module(library(plunit)).
:- use_module(library(lists), [member/2, memberchk/2]).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(json), [json_write_dict/3, atom_json_dict/3]).

/** <module> An answer as JSON

`html.pl` renders an answer for a person; `json.pl` reports it for a
program.  What is worth testing is that the *shape* of every term is said
rather than shown — a variable is `{"type":"var","name":"A"}` and not the
text `A` — and that the document a consumer gets back is the one it was
handed.

So most of these round-trip through `atom_json_dict/3`: writing the dict
and reading it back is what a consumer actually does, and it catches a
term that only *looks* like data.
*/

test_json :-
    run_tests([ json_terms,
                json_answers,
                json_constraints,
                json_document
              ]).

%!  answer(+Program, +Query, -Dict) is nondet.

answer(Program, Query, Dict) :-
    scasp(Program, Query, Model, Justification),
    answer_json(answer(Query, Model, Justification), Dict).

%!  round_trip(+Dict, -Read) is det.
%
%   What a consumer gets: the dict written as JSON and read back.

round_trip(Dict, Read) :-
    atom_json_dict(Atom, Dict, [as(atom)]),
    atom_json_dict(Atom, Read, [value_string_as(atom)]).

		 /*******************************
		 *          THE TERMS		*
		 *******************************/

:- begin_tests(json_terms).

test(an_atom) :-
    term_json(tweety, Dict),
    assertion(Dict.type == atom),
    assertion(Dict.value == tweety).

test(a_number) :-
    term_json(42, Dict),
    assertion(Dict.type == number),
    assertion(Dict.value == 42).

test(a_rational) :-
    term_json(5r2, Dict),
    assertion(Dict.type == rational),
    assertion(Dict.numerator == 5),
    assertion(Dict.denominator == 2).

%   An integer is a rational with denominator 1 and is a number, not a
%   rational: a consumer asking for `value` should get it.

test(an_integer_is_not_a_rational) :-
    term_json(3, Dict),
    assertion(Dict.type == number).

%   The destructuring is outside `assertion/1`, which runs its goal in a
%   double negation and lets no binding out.

test(a_compound_says_its_shape) :-
    term_json(f(a, 1), Dict),
    Dict.args = [A, N],
    assertion(Dict.type == compound),
    assertion(Dict.functor == f),
    assertion(A.value == a),
    assertion(N.value == 1).

test(a_list_is_a_list) :-
    term_json([a, b], Dicts),
    assertion(is_list(Dicts)),
    assertion(Dicts = [_, _]).

test(a_named_variable) :-
    term_json('$VAR'('A'), Dict),
    assertion(Dict.type == var),
    assertion(Dict.name == 'A').

%   A variable nothing is said about has no name to report, and saying
%   `"name":"_"` would invite a consumer to treat `_` as one.

test(an_anonymous_variable_has_no_name) :-
    term_json('$VAR'('_'), Dict),
    assertion(Dict.type == var),
    assertion(\+ get_dict(name, Dict, _)).

%   A constrained variable reports as the variable.  What the store says
%   is in the answer's `constraints`, once.

test(a_constrained_variable_is_just_the_variable) :-
    term_json('| '('$VAR'('A'), {'$VAR'('A') >= 3}), Dict),
    assertion(Dict.type == var),
    assertion(Dict.name == 'A').

:- end_tests(json_terms).

		 /*******************************
		 *         THE ANSWER		*
		 *******************************/

:- begin_tests(json_answers).

program([ bird(tweety), bird(polly), penguin(polly),
          (flies(X) :- bird(X), not penguin(X))
        ]).

test(the_model_is_four_truths, [forall(truth(Literal, Truth))]) :-
    model_json([Literal], [Dict]),
    assertion(Dict.truth == Truth).

truth(p,          "true").
truth(not p,      "unlikely").
truth(-p,         "false").
truth(not -p,     "likely").

%   Written as strings, all four.  The old implementation writes two of
%   them as JSON booleans and two as strings, and one field with two
%   types is a trap for a consumer switching on it.

test(and_every_truth_is_a_string, [forall(truth(Literal, _))]) :-
    model_json([Literal], Dicts),
    round_trip(#{model: Dicts}, Read),
    Model = Read.model,
    Model = [One],
    assertion(atom(One.truth)).

test(a_literal_keeps_its_arguments) :-
    program(P),
    once(answer(P, flies(X), Dict)),
    assertion(X == tweety),
    Model = Dict.model,
    member(Literal, Model),
    Value = Literal.value,
    Value.get(functor) == flies,
    !,
    Value.get(args) = [Arg],
    assertion(Arg.value == tweety).

test(the_tree_is_a_forest) :-
    program(P),
    once(answer(P, flies(_), Dict)),
    Tree = Dict.tree,
    assertion(is_list(Tree)),
    assertion(( member(Node, Tree),
                Root = Node.node,
                Value = Root.value,
                Value.get(value) == nmr
              )).

test(and_a_node_has_children) :-
    program(P),
    once(answer(P, flies(_), Dict)),
    Tree = Dict.tree,
    member(Node, Tree),
    Root = Node.node,
    Value = Root.value,
    Value.get(functor) == flies,
    !,
    assertion(Node.children = [_, _]).

%   `chs` and `proved` are flags on a node rather than truths: the
%   literal is what it is, and the flag says how it came to be known.

test(a_coinductive_node_is_flagged) :-
    once(answer([(a :- not b), (b :- not a)], a, Dict)),
    Tree = Dict.tree,
    assertion(flagged(Tree, chs)).

flagged(Nodes, Flag) :-
    member(Node, Nodes),
    (   get_dict(Flag, Node, true)
    ->  true
    ;   Children = Node.children,
        flagged(Children, Flag)
    ).

test(the_query_variables_are_bound) :-
    program(P),
    once(answer(P, flies(_), Dict)),
    Bindings = Dict.bindings,
    assertion(Bindings == #{}).

test(and_a_free_one_is_reported_by_name) :-
    once(answer([q(1), (r(X) :- not q(X))], r(_), Dict)),
    Bindings = Dict.bindings,
    get_dict('A', Bindings, Binding),
    assertion(Binding.type == var).

:- end_tests(json_answers).

		 /*******************************
		 *       THE CONSTRAINTS		*
		 *******************************/

/** The two stores come out under the variable's name, and a variable may
    be in both — which is why an entry is a *list*.  The old
    implementation has one dict per variable and assumes one kind.
*/

:- begin_tests(json_constraints).

test(a_dense_region) :-
    once(answer([(p(X) :- X #> 3, X #< 10)], not p(_), Dict)),
    Constraints = Dict.constraints,
    get_dict('A', Constraints, [C]),
    Value = C.value,
    assertion(C.type == (=<)),
    assertion(Value.get(value) == 3).

test(a_prohibited_value) :-
    once(answer([q(1), (r(X) :- not q(X))], r(_), Dict)),
    Constraints = Dict.constraints,
    get_dict('A', Constraints, [C]),
    C.set = [V],
    assertion(C.type == not_in),
    assertion(V.value == 1).

test(and_a_set_of_them) :-
    once(answer([q(1), q(2), (r(X) :- not q(X))], r(_), Dict)),
    Constraints = Dict.constraints,
    get_dict('A', Constraints, [C]),
    assertion(C.type == not_in),
    assertion(C.set = [_, _]).

%   Both stores on one variable, which is the ordinary shape: a `forall`
%   over a range leaves a region, and `consistent/3` leaves values.

test(both_stores_at_once) :-
    Program = [ (:- domain hour = 0..23),
                (:- pred q(hour)),
                q(9)
              ],
    once(( answer(Program, not q(_), Dict),
           get_dict('A', Dict.constraints, Constraints),
           length(Constraints, N),
           N >= 2
         )).

test(an_unconstrained_answer_has_none) :-
    once(answer([p(1)], p(_), Dict)),
    Constraints = Dict.constraints,
    assertion(Constraints == #{}).

:- end_tests(json_constraints).

		 /*******************************
		 *        THE DOCUMENT		*
		 *******************************/

:- begin_tests(json_document).

test(a_whole_run_is_one_document) :-
    findall(answer(flies(X), M, J),
            scasp([bird(tweety), bird(polly),
                    (flies(Y) :- bird(Y))], flies(X), M, J),
            Answers),
    answers_json(Answers, Dict),
    As = Dict.answers,
    assertion(As = [_, _]).

test(and_the_answers_are_numbered) :-
    findall(answer(p(X), M, J),
            scasp([p(1), p(2), p(3)], p(X), M, J),
            Answers),
    answers_json(Answers, Dict),
    As = Dict.answers,
    assertion(( member(A, As), A.answer == 1 )),
    assertion(( member(A, As), A.answer == 3 )).

%   It has to survive being written and read, which is what a consumer
%   does with it and what a term that only looks like data does not.

test(it_round_trips) :-
    once(answer([q(1), (r(X) :- not q(X))], r(_), Dict)),
    round_trip(Dict, Read),
    Model = Read.model,
    Constraints = Read.constraints,
    assertion(is_list(Model)),
    assertion(get_dict('A', Constraints, [_])).

test(and_writing_it_says_nothing_about_Prolog) :-
    once(answer([q(1), (r(X) :- not q(X))], r(_), Dict)),
    with_output_to(string(Text), json_write_dict(current_output, Dict, [])),
    assertion(\+ sub_string(Text, _, _, _, "$VAR")),
    assertion(\+ sub_string(Text, _, _, _, "| ")),
    assertion(\+ sub_string(Text, _, _, _, "_G")).

%   The keys come out in a reading order rather than in standard order of
%   terms, which would put `value` before `type`.

test(the_keys_are_ordered) :-
    term_json(f(a), Dict),
    with_output_to(string(Text), json_write_dict(current_output, Dict, [])),
    once(sub_string(Text, Type, _, _, "\"type\"")),
    once(sub_string(Text, Functor, _, _, "\"functor\"")),
    once(sub_string(Text, Args, _, _, "\"args\"")),
    assertion(Type < Functor),
    assertion(Functor < Args).

:- end_tests(json_document).
