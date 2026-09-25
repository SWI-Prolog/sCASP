/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(scasp_json,
          [ answer_json/2,              % +Answer, -Dict
            answer_json/3,              % +Answer, -Dict, +Options
            answers_json/2,             % +Answers, -Dict
            term_json/2,                % +Term, -Dict
            model_json/2,               % +Model, -Dicts
            justification_json/2        % +Justification, -Dicts
          ]).
:- use_module(ops).
:- use_module(render, [scasp_readable/3]).
:- use_module(library(lists), [member/2, memberchk/2, append/3]).
:- use_module(library(apply), [maplist/2, maplist/3]).
:- use_module(library(option), [option/3]).

/** <module> An answer as JSON

For a consumer that is a program rather than a person.  `html.pl` renders an
answer for reading; this reports it as **data**, with the shape of every
term said rather than shown:

    ?- scasp([(p(X) :- X #> 3)], p(V), M, J),
       answer_json(answer(p(V), M, J), Dict).

    Dict = scasp_answer{ bindings: #{'A': prolog{name:'A', type:var}},
                          constraints: #{'A': [constraint{type: #>, value:...}]},
                          model: [...], tree: [...] }.

## The variables are named once, and their constraints kept apart

`render.pl:scasp_readable/3` names the variables of the whole answer
together — query, model and justification — so a variable two of them share
is one name, which a consumer needs as much as a reader does.  It also
inlines the constraint stores, and here they come straight back out: a term
says `{"type":"var","name":"A"}` and the answer carries one `constraints`
entry per named variable.  Repeating the store at every occurrence of the
variable would say the same thing many times and leave a consumer to work
out that they were the same.

## What differs from the old implementation's `json.pl`

  - **`tree` is a list.**  The proof is a forest here — the query's
    literals at the top and the NMR check beside them — not one rooted
    tree.
  - **A variable may carry constraints of both kinds**, since `∉` and
    CLP(Q) are two stores, so a `constraints` entry is a *list*.  The old
    one is a single dict and assumes one kind per variable.
  - `module`, `source_file` and `source_line` do not appear: the solver
    is not module aware and does not keep source references.
  - `assume` and `abduced` do not appear as node flags.  Nothing is
    assumed but by the coinductive hypothesis, which is `chs`, and an
    abducible has no node of its own — the assumption is said by the
    literal being in the model.
*/

%!  answers_json(+Answers, -Dict) is det.
%
%   A whole run.  Answers is a list of `answer(Query, Model,
%   Justification)` in the order they came, or of the dicts
%   `answer_json/2` makes of them.

answers_json(Answers, Dict) :-
    maplist(as_dict, Answers, Dicts),
    number_answers(Dicts, 1, JAnswers),
    Dict = scasp_result{ answers: JAnswers }.

%   Takes either the answers or the dicts they make, so that a caller
%   that had to build each dict while its answer was still live — which
%   `dyncall.pl:scasp_json/3` does — can hand them straight over.

as_dict(Dict, Dict) :-
    is_dict(Dict),
    !.
as_dict(Answer, Dict) :-
    answer_json(Answer, Dict).

number_answers([], _, []).
number_answers([Answer|Answers], N, [Numbered|Rest]) :-
    Numbered = Answer.put(answer, N),
    N1 is N+1,
    number_answers(Answers, N1, Rest).

%!  answer_json(+Answer, -Dict) is det.
%
%   Answer is `answer(Query, Model, Justification)`.  The three are made
%   readable **together**, or a variable they share is named twice and
%   the two names mean different things.

answer_json(Answer, Dict) :-
    answer_json(Answer, Dict, []).

%!  answer_json(+Answer, -Dict, +Options) is det.
%
%   Options:
%
%     - variable_names(+Bindings)
%       Name the bindings as the *reader* named them — `X` rather than
%       the `A` a renderer would give it — which is what a caller that
%       read the query from a file has and what its user will recognise.
%       The bindings go through the same `scasp_readable/3` as the
%       answer, or they would name variables the answer no longer has.

answer_json(answer(Query, Model, Justification), Dict, Options) :-
    option(variable_names(Named), Options, []),
    scasp_readable(answer(Query, Model, Justification)-Named,
                    answer(RQuery, RModel, RJustification)-RNamed,
                    [constraints(true)]),
    bindings_json(RQuery, RNamed, Bindings),
    model_json(RModel, JModel),
    justification_json(RJustification, JTree),
    constraints_json(answer(RQuery, RModel, RJustification), Constraints),
    Dict = scasp_answer{ bindings:    Bindings,
                          model:       JModel,
                          tree:        JTree,
                          constraints: Constraints }.

%!  bindings_json(+Query, -Dict) is det.
%
%   What the query's own variables came back as, keyed by the name they
%   were given: the reader's names where the caller has them, and the
%   renderer's otherwise.  A query with no variables has none, which is
%   a fact about the query rather than a missing field.

bindings_json(_Query, Named, Dict) :-
    Named \== [],
    !,
    findall(Name-JValue,
            ( member(Name=Value, Named),
              term_json(Value, JValue)
            ),
            Pairs),
    dict_create(Dict, #, Pairs).
bindings_json(Query, _, Dict) :-
    named_variables(Query, Pairs0),
    keysort(Pairs0, Pairs1),
    dedup(Pairs1, Pairs),
    dict_create(Dict, #, Pairs).

named_variables(Term, Pairs) :-
    findall(Name-JValue,
            ( sub_term(Sub, Term),
              named(Sub, Name),
              term_json(Sub, JValue)
            ),
            Pairs).

named('$VAR'(Name), Name) :-
    Name \== '_'.
named('| '('$VAR'(Name), _), Name) :-
    Name \== '_'.

dedup([], []).
dedup([Key-Value|Pairs], [Key-Value|Rest]) :-
    exclude_key(Pairs, Key, More),
    dedup(More, Rest).

exclude_key([], _, []).
exclude_key([Key0-Value|Pairs], Key, Rest) :-
    (   Key0 == Key
    ->  Rest = More
    ;   Rest = [Key0-Value|More]
    ),
    exclude_key(Pairs, Key, More).

		 /*******************************
		 *          THE MODEL		*
		 *******************************/

%!  model_json(+Model, -Dicts) is det.
%
%   Four truths, which are the four shapes a literal has: it holds, it
%   is known not to, there is no evidence that it does, and there is no
%   evidence that it does not.  The words are the old implementation's.
%
%   Written as **strings**, where the old writes `true` and `false` as
%   JSON booleans and the other two as strings: one field with two types
%   is a trap for a consumer switching on it, and there is no old
%   consumer here to keep.

literal_json(not(-(Term)), Dict) =>
    Dict = scasp_model_term{truth: "likely",   value: JTerm},
    term_json(Term, JTerm).
literal_json(-(Term), Dict) =>
    Dict = scasp_model_term{truth: "false",    value: JTerm},
    term_json(Term, JTerm).
literal_json(not(Term), Dict) =>
    Dict = scasp_model_term{truth: "unlikely", value: JTerm},
    term_json(Term, JTerm).
literal_json(Term, Dict) =>
    Dict = scasp_model_term{truth: "true",     value: JTerm},
    term_json(Term, JTerm).

model_json(Model, Dicts) :-
    maplist(literal_json, Model, Dicts).

		 /*******************************
		 *      THE JUSTIFICATION	*
		 *******************************/

%!  justification_json(+Justification, -Dicts) is det.
%
%   A **forest**: the query's literals and, beside them, everything the
%   NMR check needed under one `nmr` node.

justification_json(Forest, Dicts) :-
    maplist(node_json, Forest, Dicts).

node_json(Literal-Children, Dict) :-
    root_json(Literal, JRoot),
    justification_json(Children, JChildren),
    Dict = JRoot.put(children, JChildren).

%   `chs/1` marks a literal established by the coinductive hypothesis and
%   `proved/1` one an earlier branch established.  They are flags rather
%   than truths: the literal is what it is, and the flag says how it came
%   to be known.

root_json(chs(Literal), Dict) =>
    root_json(Literal, Dict0),
    Dict = Dict0.put(chs, true).
root_json(proved(Literal), Dict) =>
    root_json(Literal, Dict0),
    Dict = Dict0.put(proved, true).
root_json(nmr, Dict) =>
    Dict = scasp_node{node: scasp_model_term{truth: "true",
                                               value: atom{type: atom,
                                                           value: nmr}}}.
root_json(Literal, Dict) =>
    literal_json(Literal, JLiteral),
    Dict = scasp_node{node: JLiteral}.

		 /*******************************
		 *           A TERM		*
		 *******************************/

%!  term_json(+Term, -Dict) is det.
%
%   The shape of a term, said rather than written.  A `Name | {Store}`
%   is reported as the variable alone: what the store says is in the
%   answer's `constraints`, once, rather than at every occurrence.

term_json('| '(Var, _Constraints), Dict) =>
    term_json(Var, Dict).
term_json('$VAR'('_'), Dict) =>
    Dict = prolog{type: var}.
term_json('$VAR'(Name), Dict) =>
    Dict = prolog{type: var, name: Name}.
term_json(Var, Dict), var(Var) =>
    Dict = prolog{type: var}.
term_json(Atom, Dict), atom(Atom) =>
    Dict = atom{type: atom, value: Atom}.
term_json(String, Dict), string(String) =>
    Dict = prolog{type: string, value: String}.
term_json(Number, Dict), rational(Number, N, D), D =\= 1 =>
    Dict = prolog{type: rational, numerator: N, denominator: D}.
term_json(Number, Dict), number(Number) =>
    Dict = prolog{type: number, value: Number}.
term_json(List, JList), is_list(List) =>
    maplist(term_json, List, JList).
term_json(Compound, Dict), compound(Compound) =>
    compound_name_arguments(Compound, Name, Arguments),
    Dict = prolog{type: compound, functor: Name, args: JArgs},
    maplist(term_json, Arguments, JArgs).

		 /*******************************
		 *        THE CONSTRAINTS	*
		 *******************************/

%!  constraints_json(+Answer, -Dict) is det.
%
%   One entry per named variable that carries a store, and a **list** of
%   constraints under it: the Herbrand store and CLP(Q) are two stores
%   and a variable may be in both.

constraints_json(Answer, Dict) :-
    findall(Name-JConstraints,
            ( sub_term('| '('$VAR'(Name), {Store}), Answer),
              Name \== '_',
              store_json(Store, JConstraints)
            ),
            Pairs0),
    keysort(Pairs0, Pairs1),
    dedup(Pairs1, Pairs),
    dict_create(Dict, #, Pairs).

store_json(Store, JConstraints) :-
    conjunction(Store, Constraints),
    maplist(constraint_json, Constraints, JConstraints).

conjunction((A, B), Constraints) :-
    !,
    conjunction(A, As),
    conjunction(B, Bs),
    append(As, Bs, Constraints).
conjunction(Constraint, [Constraint]).

%   `render.pl:readable_constraint/2` has already put these in the
%   source language, so what arrives is what a program would have
%   written.  A prohibited-value set of one reads as `\=`, and both say
%   the same thing, so both report as `not_in`.

constraint_json('∉'(_, Values), Dict) =>
    Dict = constraint{type: not_in, set: JValues},
    maplist(term_json, Values, JValues).
constraint_json(_ \= Value, Dict) =>
    Dict = constraint{type: not_in, set: [JValue]},
    term_json(Value, JValue).
constraint_json(Comparison, Dict),
    compound(Comparison),
    compound_name_arity(Comparison, Type, 2),
    arg(2, Comparison, Value) =>
    Dict = constraint{type: Type, value: JValue},
    term_json(Value, JValue).
constraint_json(Other, Dict) =>
    Dict = constraint{type: other, value: JOther},
    term_json(Other, JOther).

		 /*******************************
		 *         THE ORDER		*
		 *******************************/

%   `json_write_dict/3` writes a dict in standard order of terms unless
%   told otherwise, which puts `value` before `type` and makes the output
%   harder to read than it needs to be.  The old implementation does the
%   same, for the same reason.

:- multifile
    json:json_dict_pairs/2.

json:json_dict_pairs(Dict, Pairs) :-
    is_dict(Dict, Tag),
    key_order(Tag, Order),
    dict_pairs(Dict, Tag, All),
    findall(Key-Value,
            ( member(Key, Order),
              memberchk(Key-Value, All)
            ),
            Ordered),
    findall(Key-Value,
            ( member(Key-Value, All),
              \+ memberchk(Key, Order)
            ),
            Rest),
    append(Ordered, Rest, Pairs).

key_order(scasp_result,     [answers]).
key_order(scasp_answer,     [answer, bindings, model, tree, constraints]).
key_order(scasp_model_term, [truth, value]).
key_order(scasp_node,       [node, chs, proved, children]).
key_order(prolog,            [type, functor, numerator, denominator, name, value]).
key_order(atom,              [type, value]).
key_order(constraint,        [type, set, value]).
