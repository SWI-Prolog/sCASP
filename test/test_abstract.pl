:- module(test_abstract,
          [ test_abstract/0
          ]).
:- use_module(scasp_plunit, []).        % sets the library search path
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/abstract)).
:- use_module(library(plunit)).
:- use_module(library(lists), [member/2]).

/** <module> Head abstraction

`sscasp/abstract.pl` makes the unification implicit in a clause head explicit,
which is what lets a dual succeed where head unification would have failed.

These destructure the result and compare the parts, rather than matching a
whole term with `=@=`.  A pattern like `(p(_) :- _ = 0)` looks right and is
not: those two `_` are *distinct* variables, so it is not a variant of
`p(A) :- A = 0` and the test passes or fails for the wrong reason.
*/

test_abstract :-
    run_tests([ abstract_variables,
                abstract_values,
                abstract_unchanged,
                abstract_guards
              ]).

		 /*******************************
		 *         VARIABLES		*
		 *******************************/

:- begin_tests(abstract_variables).

test(a_repeated_variable_is_split) :-
    abstract_head(t(A, A), Abstracted),
    Abstracted = (t(H1, H2) :- Guard),
    assertion(H1 == A),
    assertion(H2 \== A),
    assertion(Guard == (A = H2)).

%   The first occurrence keeps the variable; only the later ones are
%   replaced.  It reads better, and a dual is read by people.

test(the_first_occurrence_keeps_its_variable) :-
    abstract_head((p(X, X, X) :- q(X)), (Head :- Body)),
    Head = p(A, B, C),
    assertion(A == X),
    assertion(B \== X),
    assertion(C \== X),
    assertion(B \== C),
    assertion(Body =@= (X = _, X = _, q(X))).

test(distinct_variables_are_left_alone) :-
    abstract_head(edge(X, Y), Abstracted),
    assertion(Abstracted =@= edge(_, _)),
    Abstracted = edge(A, B),
    assertion(A == X),
    assertion(B == Y).

:- end_tests(abstract_variables).

		 /*******************************
		 *          VALUES		*
		 *******************************/

:- begin_tests(abstract_values).

test(an_atom_argument) :-
    abstract_head(p(0), Abstracted),
    Abstracted = (p(V) :- Guard),
    assertion(var(V)),
    assertion(Guard == (V = 0)).

test(a_compound_argument) :-
    abstract_head((wrap(f(X)) :- item(X)), Abstracted),
    Abstracted = (wrap(V) :- Guard, Rest),
    assertion(Guard == (V = f(X))),
    assertion(Rest == item(X)).

%   One level, not recursive: the nested structure stays inside the
%   guard.  See the module comment for why.

test(a_nested_compound_is_not_decomposed) :-
    abstract_head(p(f(g(X))), Abstracted),
    Abstracted = (p(V) :- Guard),
    assertion(Guard == (V = f(g(X)))).

test(a_list_argument) :-
    abstract_head((len([H|T]) :- len(T)), Abstracted),
    Abstracted = (len(V) :- Guard, Rest),
    assertion(Guard == (V = [H|T])),
    assertion(Rest == len(T)).

test(several_arguments_at_once) :-
    abstract_head(p(0, X, X), Abstracted),
    Abstracted = (p(V, A, B) :- G1, G2),
    assertion(G1 == (V = 0)),
    assertion(A == X),
    assertion(G2 == (X = B)).

:- end_tests(abstract_values).

		 /*******************************
		 *         UNCHANGED		*
		 *******************************/

%   A head that is already abstract gets no guards, so the clause comes
%   back as it went in.

:- begin_tests(abstract_unchanged).

test(a_fact_with_distinct_variables) :-
    abstract_head(edge(X, Y), A),
    assertion(A == edge(X, Y)).
test(a_rule_with_distinct_variables) :-
    abstract_head((p(X) :- q(X)), A),
    assertion(A == (p(X) :- q(X))).
test(an_atom_head) :-
    abstract_head(p, A),
    assertion(A == p).
test(an_atom_head_with_a_body) :-
    abstract_head((p :- q), A),
    assertion(A == (p :- q)).

:- end_tests(abstract_unchanged).

		 /*******************************
		 *          GUARDS		*
		 *******************************/

%   The three-argument form hands back the guards separately, which is
%   what a dual generator needs: it has to put them somewhere other than
%   the front of a body.

:- begin_tests(abstract_guards).

test(guards_come_back_as_a_list) :-
    abstract_head((wrap(f(X)) :- item(X)), Head, Guards),
    assertion(Head =@= wrap(_)),
    assertion(Guards =@= [_ = f(X)]).

test(no_guards_for_an_abstract_head) :-
    abstract_head((p(X) :- q(X)), Head, Guards),
    assertion(Head == p(X)),
    assertion(Guards == []).

test(the_head_arguments_are_all_distinct_variables) :-
    abstract_head(p(f(a), Y, Y, 0), Head, _),
    Head =.. [_|Args],
    assertion(maplist_var(Args)),
    sort(0, @<, Args, Sorted),
    assertion(length(Sorted, 4)).

maplist_var([]).
maplist_var([A|T]) :- var(A), maplist_var(T).

test(the_guards_reconstruct_the_original_head) :-
    Original = p(f(a), Y, Y, 0),
    abstract_head(Original, Head, Guards),
    maplist(call, Guards),
    assertion(Head == Original).

:- end_tests(abstract_guards).
