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

:- module(sscasp_json,
          [ answer_json/2,              % @Answer, -Dict
            answer_json/3,              % @Answer, -Dict, +Options
            answers_json/2,             % @Answers, -Dict
            term_json/2,                % @Term, -Dict
            model_json/2                % @Model, -Dicts
          ]).
:- use_module(ops).
:- use_module(mangle, [literal_kind/3, kind_truth/2]).
:- use_module(render, [scasp_readable/3]).
:- use_module(library(lists), [append/3]).
:- use_module(library(occurs), [sub_term/2]).
:- use_module(library(apply), [maplist/3, convlist/3, exclude/3]).
:- use_module(library(option), [option/3]).

/** <module> An answer as JSON

For a consumer that is a program rather than a person.  `html.pl` renders an
answer for reading; this reports it as **data**, with the shape of every
term said rather than shown:

    ?- scasp([(p(X) :- X #> 3)], p(V), M, J),
       answer_json(answer(p(V), M, J), Dict).

    Dict = scasp_answer{ bindings: #{'A': prolog{name:'A', type:var}},
                          constraints: #{'A': [constraint{type: >, value:...}]},
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

%!  answers_json(@Answers, -Dict) is det.
%
%   A whole run.  Answers is a list of `answer(Query, Model,
%   Justification)` in the order they came, or of the dicts
%   `answer_json/2` makes of them.
%
%   @arg Dict is `scasp_result{answers: Dicts}`, each answer's dict with
%       an `answer` key added numbering it from 1.

:- det(answers_json/2).

answers_json(Answers, Dict) :-
    maplist(as_dict, Answers, Dicts),
    number_answers(Dicts, 1, JAnswers),
    Dict = scasp_result{ answers: JAnswers }.

%   Takes either the answers or the dicts they make, so that a caller
%   that had to build each dict while its answer was still live — which
%   `dyncall.pl:scasp_json/3` does — can hand them straight over.

as_dict(Dict0, Dict), is_dict(Dict0) =>
    Dict = Dict0.
as_dict(Answer, Dict) =>
    answer_json(Answer, Dict).

number_answers([], _, Numbered) =>
    Numbered = [].
number_answers([Answer|Answers], N, Numbered0) =>
    Numbered0 = [Numbered|Rest],
    Numbered = Answer.put(answer, N),
    N1 is N+1,
    number_answers(Answers, N1, Rest).

%!  answer_json(@Answer, -Dict) is det.
%
%   Answer is `answer(Query, Model, Justification)`.  The three are made
%   readable **together**, or a variable they share is named twice and
%   the two names mean different things.

:- det(answer_json/2).

answer_json(Answer, Dict) :-
    answer_json(Answer, Dict, []).

%!  answer_json(@Answer, -Dict, +Options) is det.
%
%   Options:
%
%     - variable_names(+Bindings)
%       Name the bindings as the *reader* named them — `X` rather than
%       the `A` a renderer would give it — which is what a caller that
%       read the query from a file has and what its user will recognise.
%       The bindings go through the same `scasp_readable/3` as the
%       answer, or they would name variables the answer no longer has.

:- det(answer_json/3).

answer_json(Answer, Dict, Options), Answer = answer(_, _, _) =>
    option(variable_names(Named), Options, []),
    scasp_readable(Answer-Named,
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

%!  bindings_json(@Query, @Named, -Dict) is det.
%
%   What the query's own variables came back as, keyed by the name they
%   were given: the reader's names where the caller has them, and the
%   renderer's otherwise.  A query with no variables has none, which is
%   a fact about the query rather than a missing field.
%
%   @arg Named is the `Name=Value` list of the option `variable_names/1`,
%       made readable with the answer, or `[]`.
%   @arg Dict maps each name to the term_json/2 dict of its value.

bindings_json(_Query, Named, Dict), Named \== [] =>
    maplist(binding_json, Named, Pairs),
    dict_create(Dict, #, Pairs).
bindings_json(Query, _, Dict) =>
    named_variables(Query, Pairs0),
    keysort(Pairs0, Pairs1),
    dedup(Pairs1, Pairs),
    dict_create(Dict, #, Pairs).

binding_json(Name=Value, Name-JValue) :-
    term_json(Value, JValue).

named_variables(Term, Pairs) :-
    findall(Name-JValue,
            ( sub_term(Sub, Term),
              named(Sub, Name),
              term_json(Sub, JValue)
            ),
            Pairs).

named('$VAR'(Name0), Name), Name0 \== '_' =>
    Name = Name0.
named('| '('$VAR'(Name0), _), Name), Name0 \== '_' =>
    Name = Name0.
named(_, _) =>
    fail.

dedup([], Deduped) =>
    Deduped = [].
dedup([Pair|Pairs], Deduped), Pair = Key-_ =>
    Deduped = [Pair|Rest],
    exclude_key(Pairs, Key, More),
    dedup(More, Rest).

exclude_key([], _, Rest) =>
    Rest = [].
exclude_key([Pair|Pairs], Key, Rest), Pair = Key0-_ =>
    (   Key0 == Key
    ->  Rest = More
    ;   Rest = [Pair|More]
    ),
    exclude_key(Pairs, Key, More).

		 /*******************************
		 *          THE MODEL		*
		 *******************************/

%!  model_json(@Model, -Dicts) is det.
%
%   Four truths, which are the four shapes a literal has: it holds, it
%   is known not to, there is no evidence that it does, and there is no
%   evidence that it does not.  The words are the old implementation's.
%
%   Written as **strings**, where the old writes `true` and `false` as
%   JSON booleans and the other two as strings: one field with two types
%   is a trap for a consumer switching on it, and there is no old
%   consumer here to keep.
%
%   @arg Model is a model that has been through scasp_readable/3.
%   @arg Dicts is a list of `scasp_model_term{truth, value}`, value
%       being the term_json/2 dict of the literal without its `not`/`-`.

literal_json(Literal, Dict) =>
    literal_kind(Literal, Kind, Term),
    kind_truth(Kind, Truth),
    atom_string(Truth, String),
    Dict = scasp_model_term{truth: String, value: JTerm},
    term_json(Term, JTerm).

:- det(model_json/2).

model_json(Model, Dicts) :-
    maplist(literal_json, Model, Dicts).

		 /*******************************
		 *      THE JUSTIFICATION	*
		 *******************************/

%!  justification_json(@Justification, -Dicts) is det.
%
%   A **forest**: the query's literals and, beside them, everything the
%   NMR check needed under one `nmr` node.
%
%   @arg Justification is a list of `Literal-Children` trees that has
%       been through scasp_readable/3.
%   @arg Dicts is a list of `scasp_node{node, children}`, `node` as in
%       model_json/2, with `chs: true` or `proved: true` added where the
%       Literal was wrapped in `chs/1` or `proved/1`.

:- det(justification_json/2).

justification_json(Forest, Dicts) :-
    maplist(node_json, Forest, Dicts).

node_json(Literal-Children, Dict) =>
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
root_json('$nmr', Dict) =>
    Dict = scasp_node{node: scasp_model_term{truth: "true",
                                               value: atom{type: atom,
                                                           value: '$nmr'}}}.
root_json(Literal, Dict) =>
    literal_json(Literal, JLiteral),
    Dict = scasp_node{node: JLiteral}.

		 /*******************************
		 *           A TERM		*
		 *******************************/

%!  term_json(@Term, -Dict) is det.
%
%   The shape of a term, said rather than written.  A `Name | {Store}`
%   is reported as the variable alone: what the store says is in the
%   answer's `constraints`, once, rather than at every occurrence.
%
%   @arg Term is a readable term, as scasp_readable/3 makes it.
%   @arg Dict is a `prolog{type, ...}` dict, or `atom{type, value}` for
%       an atom; a proper list becomes a *list* of such dicts.

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
%
%   @arg Answer is any readable term, here `answer(Query, Model,
%       Justification)`; each `'| '('$VAR'(Name), {Store})` in it counts.
%   @arg Dict maps each such Name to a list of `constraint{type, ...}`.

constraints_json(Answer, Dict) :-
    findall(Name-JConstraints,
            ( sub_term('| '('$VAR'(Name), {Store}), Answer),
              Name \== '_',
              store_json(Name, Store, JConstraints)
            ),
            Pairs0),
    keysort(Pairs0, Pairs1),
    dedup(Pairs1, Pairs),
    dict_create(Dict, #, Pairs).

store_json(Name, Store, JConstraints) :-
    phrase(conjuncts(Store), Constraints),
    maplist(constraint_json('$VAR'(Name)), Constraints, JConstraints).

conjuncts((A, B)) ==>
    conjuncts(A),
    conjuncts(B).
conjuncts(Constraint) ==>
    [Constraint].

%   `render.pl:readable_constraint/2` has already put these in the
%   source language, so what arrives is what a program would have
%   written.  A prohibited-value set of one reads as `\=`, and both say
%   the same thing, so both report as `not_in`.
%
%   Only a constraint between the variable it is listed under and a
%   **value** says something about that variable alone.  `A \= B`, or
%   `A - B > 0`, relates it to another one, and is reported whole as
%   `other` with both named — read as `A` against a value it would say
%   `A ∉ [A]`.

constraint_json(Var, '∉'(X, Values), Dict), X == Var, valued(Values) =>
    Dict = constraint{type: not_in, set: JValues},
    maplist(term_json, Values, JValues).
constraint_json(Var, X \= Value, Dict), X == Var, valued(Value) =>
    Dict = constraint{type: not_in, set: [JValue]},
    term_json(Value, JValue).
constraint_json(Var, Comparison, Dict),
    compound(Comparison),
    compound_name_arguments(Comparison, Type, [X, Value]),
    X == Var,
    valued(Value) =>
    Dict = constraint{type: Type, value: JValue},
    term_json(Value, JValue).
constraint_json(_, Other, Dict) =>
    Dict = constraint{type: other, value: JOther},
    term_json(Other, JOther).

%   No other variable in it.  The terms are readable, so a variable is
%   a `'$VAR'` term and ground/1 cannot tell.

valued(Term) :-
    \+ sub_term('$VAR'(_), Term).

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
    convlist(key_pair(All), Order, Ordered),
    exclude(ordered_pair(Order), All, Rest),
    append(Ordered, Rest, Pairs).

key_pair(Pairs, Key, Pair) :-
    Pair = Key-_,
    memberchk(Pair, Pairs).

ordered_pair(Order, Key-_) :-
    memberchk(Key, Order).

key_order(scasp_result,     [answers]).
key_order(scasp_answer,     [answer, bindings, model, tree, constraints]).
key_order(scasp_model_term, [truth, value]).
key_order(scasp_node,       [node, chs, proved, children]).
key_order(prolog,            [type, functor, numerator, denominator, name, value]).
key_order(atom,              [type, value]).
key_order(constraint,        [type, set, value]).
