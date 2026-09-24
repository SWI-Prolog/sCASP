:- module(test_api,
          [ test_api/0
          ]).
:- use_module(scasp_plunit, []).        % sets the library search path
:- use_module(library(sscasp)).
:- use_module(library(plunit)).
:- use_module(library(sscasp/annotations), [program_annotations/2, described/3,
                                          argument_domain/4]).
:- use_module(library(lists), [member/2, memberchk/2, subtract/3]).
:- use_module(library(clpq), [{}/1]).   % a rendered store is called, not only read

/** <module> s(CASP) from Prolog

`library(sscasp)` over ordinary Prolog clauses: what `sscasp/2` collects, what
comes back, and what it looks like printed.

The program under test is in this file, as a program using the library would
be.  Two things about that are worth knowing.

**`library(scasp)` may be loaded too.**  `test_all.pl` puts every suite in one
process, and both libraries install a `user:term_expansion/2` for
`false :- Body`; the first one declared wins.  `dyncall.pl:global_constraint/2`
reads what *either* leaves behind, so these tests pass whichever order the two
were loaded in — which is the point of it reading both.

**A `-p(X)` head is not written here.**  The old library's expansion rewrites
one into a predicate named `'-p'`, so what the file ends up holding depends on
load order in a way nothing can read back.  The classical-negation test
asserts its clauses instead, which no expansion touches.
*/

test_api :-
    run_tests([ api_answers,
                api_collection,
                api_rendering,
                api_sld,
                api_show,
                api_describe,
                api_human,
                api_hygiene
              ]).

		 /*******************************
		 *         THE PROGRAM		*
		 *******************************/

bird(tweety).
bird(polly).
penguin(polly).
flies(X) :- bird(X), not penguin(X).

%  a constrained answer
window(X) :- X #> 3, X #< 10.

%  global constraints: `false :- Body`, since `:- Body` is a Prolog
%  directive and `false/0` cannot be given clauses.  The two are written
%  together because with `library(scasp)` also loaded its expansion makes
%  them clauses of `-/0`, and clauses of one predicate apart in a file
%  are a warning.
colour(red).
colour(blue).
chosen(X) :- colour(X), not rejected(X).
rejected(X) :- colour(X), not chosen(X).

%  lonely/1 is reached by the second constraint and by nothing else, so
%  that constraint is one the query cannot reach and must not constrain it
lonely(1).

false :- chosen(red).
false :- lonely(1).

%  directives, for what Prolog has no syntax for
:- sscasp_directive(abducible(assumed_thing)).
perhaps :- assumed_thing.

:- sscasp_directive(domain(day = [mon, tue])).

%  saying a literal in words.  The variable is underscored because the
%  Prolog reader would otherwise call it a singleton -- it occurs once in
%  the directive and the rest of its occurrences are inside a string.
:- describe flies(_X) :: "@(X) can fly".
:- describe not flies(_X) :: "@(X) cannot fly".
:- describe flies(tweety) :: "the famous one flies".

%  a signature, and a description, for a predicate no query here reaches.
%  A signature is checked against the program's definitions, so collecting
%  one the query cannot reach would refuse a perfectly good program.
:- domain shade = [dark, light].
:- pred shading(shade).
:- describe shading(_X) :: "@(X) is the shading".

shading(dark).

%  proved by SLD resolution rather than by the solver
:- sld successor/2.
:- sld noisy/1 as opaque.

successor(X, Y) :- Y is X+1.
noisy(hello).

next(Y) :- successor(1, Y).
announce :- noisy(hello).
small(1).
small(2).
outside(X) :- colour_index(X), not small(X).
colour_index(1).
colour_index(3).

		 /*******************************
		 *          ANSWERING		*
		 *******************************/

:- begin_tests(api_answers).

test(a_query_answers_from_the_clauses_around_it, set(X == [tweety])) :-
    sscasp(flies(X)).

test(and_carries_a_model) :-
    sscasp(flies(tweety), [model(Model)]),
    assertion(memberchk(bird(tweety), Model)),
    assertion(memberchk(not penguin(tweety), Model)).

test(and_a_justification) :-
    sscasp(flies(tweety), [tree(Tree)]),
    assertion(memberchk(flies(tweety)-Children, Tree)),
    assertion(memberchk(bird(tweety)-[], Children)),
    assertion(memberchk(nmr-_, Tree)).

%   A global constraint whose call tree overlaps the query is applied;
%   one that does not is not.  `chosen(red)` is denied, so only blue is
%   left, and `lonely/1` says nothing about either.

test(an_overlapping_constraint_applies, set(X == [blue])) :-
    sscasp(chosen(X)).

test(and_one_that_does_not_overlap_is_ignored) :-
    assertion(\+ \+ sscasp(colour(red))).

%   `collect(Goal)` widens what is collected without widening the query.
%   With `lonely(1)` named, its constraint comes too and denies it.

test(collect_brings_in_what_the_query_cannot_reach) :-
    assertion(\+ \+ sscasp(colour(red))),
    assertion(\+ sscasp(colour(red), [collect(lonely(_))])).

%   An abducible may be assumed, so the query holds — and holds without
%   it too, which is what `not perhaps` having an answer says.

test(an_abducible_may_be_assumed) :-
    assertion(\+ \+ sscasp(perhaps)),
    assertion(\+ \+ sscasp(not perhaps)).

test(a_declared_domain_is_a_predicate, set(D == [mon, tue])) :-
    sscasp(day(D)).

%   A classically negated predicate is collected by the argument's name,
%   not by `-/1` — which holds the clauses of every one of them at once.
%   Asserted rather than written above; see the module comment.

test(classical_negation_is_collected,
     [ setup(assert_negation), cleanup(retract_negation) ]) :-
    assertion(\+ \+ sscasp(-dead(tweety))),
    assertion(\+ sscasp(-dead(polly))),
    sscasp(-dead(tweety), [model(Model)]),
    assertion(memberchk(alive(tweety), Model)).

assert_negation :-
    assertz(test_api:alive(tweety)),
    assertz(test_api:(-dead(X) :- alive(X))).

retract_negation :-
    retractall(test_api:alive(_)),
    retractall(test_api:(-dead(_))).

:- end_tests(api_answers).

		 /*******************************
		 *         COLLECTION		*
		 *******************************/

:- begin_tests(api_collection).

test(the_reachable_clauses_and_no_others) :-
    sscasp_query_clauses(flies(_), Terms),
    assertion(memberchk(bird(tweety), Terms)),
    assertion(memberchk(penguin(polly), Terms)),
    assertion(\+ memberchk(colour(_), Terms)).

test(a_global_constraint_comes_as_a_headless_rule) :-
    sscasp_query_clauses(chosen(_), Terms),
    assertion(memberchk((:- chosen(red)), Terms)).

%   `p` and `-p` are one predicate as far as consistency goes: the
%   compiler writes `:- p(X), -p(X)` for every classically negated
%   predicate, and a program deriving both has no answer set at all.  So
%   reaching one reaches the other, however far apart they are written.
%
%   Left out, this was unsound rather than untidy — the check was never
%   generated and the query answered anyway.

test(a_classically_negated_counterpart_comes_along,
     [ setup(assert_inconsistency), cleanup(retract_inconsistency) ]) :-
    sscasp_query_clauses(api_p, Terms),
    assertion(memberchk((-api_p :- api_q), Terms)).

test(and_its_body_is_collected_too,
     [ setup(assert_inconsistency), cleanup(retract_inconsistency) ]) :-
    sscasp_query_clauses(api_p, Terms),
    assertion(memberchk(api_q, Terms)).

test(so_the_program_has_no_answer,
     [ setup(assert_inconsistency), cleanup(retract_inconsistency) ]) :-
    assertion(\+ sscasp(api_p)).

%   And the other way: a query about the negation collects the positive.

test(and_the_other_way_round,
     [ setup(assert_inconsistency), cleanup(retract_inconsistency) ]) :-
    sscasp_query_clauses(-api_p, Terms),
    assertion(memberchk((api_p :- api_q), Terms)).

assert_inconsistency :-
    assertz(test_api:api_q),
    assertz(test_api:(api_p :- api_q)),
    assertz(test_api:(-api_p :- api_q)).

retract_inconsistency :-
    retractall(test_api:api_q),
    retractall(test_api:api_p),
    retractall(test_api:(-api_p)).

test(and_a_directive_as_one_too) :-
    sscasp_query_clauses(day(_), Terms),
    assertion(memberchk((:- domain day = [mon, tue]), Terms)).

%   Two modules defining the same predicate cannot both be in one
%   program: the solver mangles a single module name into every functor,
%   so one of them would be discarded without saying so.

test(two_modules_defining_one_predicate_are_refused,
     [ setup(assert_elsewhere), cleanup(retract_elsewhere),
       error(permission_error(collect, sscasp_predicate, _)) ]) :-
    sscasp_query_clauses(test_api:api_shared_head(_), _).

%   The names are deliberately unlikely: these three exist only between
%   `setup` and `cleanup`, so between tests they are undefined, and an
%   undefined predicate resolves to whatever a loaded library happens to
%   export under that name.  See the note in `CLAUDE.md`.

assert_elsewhere :-
    assertz(test_api:(api_shared_head(X) :-
                          sscasp_api_a:api_two_modules(X),
                          sscasp_api_b:api_two_modules(X))),
    assertz(sscasp_api_a:api_two_modules(1)),
    assertz(sscasp_api_b:api_two_modules(1)).

retract_elsewhere :-
    retractall(test_api:api_shared_head(_)),
    retractall(sscasp_api_a:api_two_modules(_)),
    retractall(sscasp_api_b:api_two_modules(_)).

:- end_tests(api_collection).

		 /*******************************
		 *         RENDERING		*
		 *******************************/

:- begin_tests(api_rendering).

%   A variable carrying nothing and occurring once is `_`; one occurring
%   twice gets a name; one carrying a constraint gets a name and the
%   constraint beside it, in the language the program was written in
%   rather than the store that holds it.

test(a_constraint_is_shown_on_the_variable_it_is_about) :-
    { X =< 3 },
    sscasp_readable(f(X), Readable),
    assertion(Readable = f('| '('$VAR'('A'), {'$VAR'('A') =< 3}))).

test(a_disequality_is_shown_as_the_program_wrote_it) :-
    dif(X, a),
    sscasp_readable(f(X), Readable),
    assertion(Readable = f('| '('$VAR'('A'), {'$VAR'('A') \= a}))).

test(a_singleton_is_not_named) :-
    sscasp_readable(f(_, Y, Y), Readable),
    assertion(Readable == f('$VAR'('_'), '$VAR'('A'), '$VAR'('A'))).

%   A constraint between two variables is shown once, on the first of
%   them: printing it again on the second would say the same thing twice.

test(a_shared_constraint_is_shown_once) :-
    dif(X, Y),
    sscasp_readable(f(X, Y), f(First, Second)),
    assertion(First = '| '('$VAR'('A'), _)),
    assertion(Second == '$VAR'('B')).

%   Reading a `dif/2` must not destroy it — `dif:attribute_goals//1`
%   deletes each or-node as it reports it, and only `copy_term/3` and the
%   toplevel undo that.

test(rendering_leaves_the_answer_alone) :-
    dif(X, a),
    sscasp_readable(f(X), _),
    assertion(\+ X = a),
    assertion(\+ \+ X = b).

%   The constrained answer, end to end.

test(a_constrained_answer_reads_back) :-
    findall(Readable,
            ( sscasp(not window(V)),
              sscasp_readable(V, Readable)
            ),
            Answers),
    assertion(Answers = [_, _]),
    assertion(memberchk('| '(_, {_ =< 3}), Answers)),
    assertion(memberchk('| '(_, {_ >= 10}), Answers)).

%   The model and the justification have to be made readable *together*,
%   or a variable they share is named twice and the two names mean
%   different things.

test(one_naming_for_the_model_and_the_tree) :-
    once(sscasp(not window(V), [model(Model), tree(Tree)])),
    sscasp_readable(v(V, Model, Tree), v(Named, ReadableModel, _)),
    assertion(memberchk(not window(Named), ReadableModel)).

:- end_tests(api_rendering).

		 /*******************************
		 *      SLD RESOLUTION		*
		 *******************************/

/** `:- sld p/2.`

The predicate is proved the way Prolog proves things, not the way the rest of
the program is.  Its clauses are not collected, it is not dualised, and the
justification shows what the interpreter found under it — or, for `as opaque`,
nothing.
*/

:- begin_tests(api_sld).

test(an_sld_goal_computes, set(Y == [2])) :-
    sscasp(next(Y)).

test(and_its_clauses_are_not_collected) :-
    sscasp_query_clauses(next(_), Terms),
    assertion(memberchk((:- sld _), Terms)),
    assertion(\+ memberchk((successor(_, _) :- _), Terms)).

%   The literal is in the model like any other proved one, and the
%   justification holds the derivation below it — `2 is 1+1` is the step
%   the interpreter took.

test(the_derivation_is_in_the_justification) :-
    sscasp(next(2), [model(Model), tree(Tree)]),
    assertion(memberchk(successor(1, 2), Model)),
    assertion(memberchk(next(2)-[successor(1, 2)-[(2 is 1+1)-[]]], Tree)).

%   `as opaque` runs it instead, so there is a node for the call and
%   nothing under it.  Which is the whole difference.

test(opaque_shows_the_call_and_no_more) :-
    sscasp(announce, [tree(Tree)]),
    assertion(memberchk(announce-[noisy(hello)-[]], Tree)).

%   Negating an SLD goal **constrains** rather than tests.  `\+ small(X)`
%   for an unbound X asks whether *some* X works and says nothing about
%   the others, so the solutions are collected and the variable kept away
%   from each — which is what the answer says.

test(negation_of_an_unbound_sld_goal_constrains) :-
    findall(X-Store,
            ( sscasp(not small(X)),
              copy_term(X, _, Store)
            ),
            Answers),
    assertion(Answers = [_]),
    Answers = [Value-_],
    assertion(\+ Value = 1),
    assertion(\+ Value = 2),
    assertion(\+ \+ Value = 3).

test(and_a_ground_one_is_simply_run) :-
    assertion(\+ \+ sscasp(not small(3))),
    assertion(\+ sscasp(not small(1))).

test(so_a_program_over_it_answers, set(X == [3])) :-
    sscasp(outside(X)).

:- end_tests(api_sld).

		 /*******************************
		 *          SHOWING		*
		 *******************************/

:- begin_tests(api_show).

test(the_program_a_query_would_run) :-
    with_output_to(string(Text), sscasp_show(flies(_), code)),
    assertion(sub_string(Text, _, _, _, "?- flies(")),
    assertion(sub_string(Text, _, _, _, "bird(tweety)")),
    assertion(sub_string(Text, _, _, _, "not penguin")),
    assertion(\+ sub_string(Text, _, _, _, "colour")).

%   The duals are what `not p` *tries*, with the helpers `dualise.pl`
%   generates inlined as a disjunction — which is the one thing a reader
%   of a dual wants and the reason this is not `portray_clause/1` over
%   the compiled terms.

test(and_what_not_p_tries) :-
    with_output_to(string(Text),
                   sscasp_show(flies(_), code(query(false), user(false),
                                              duals(true)))),
    assertion(sub_string(Text, _, _, _, "not flies(")),
    assertion(sub_string(Text, _, _, _, ";")),
    assertion(\+ sub_string(Text, _, _, _, "$dual")).

test(and_the_check_it_has_to_pass) :-
    with_output_to(string(Text),
                   sscasp_show(chosen(_), code(query(false), user(false),
                                               constraints(true)))),
    assertion(sub_string(Text, _, _, _, "check(1)")),
    assertion(sub_string(Text, _, _, _, "the global constraint")),
    assertion(sub_string(Text, _, _, _, "not chosen(red)")).

%   The one section that shows a directive, since the compiler turns
%   those into clauses and they stop being visible as themselves.

test(and_the_terms_as_collected) :-
    with_output_to(string(Text),
                   sscasp_show(day(_), code(query(false), user(false),
                                            source(true)))),
    assertion(sub_string(Text, _, _, _, ":- domain day")).

test(and_the_clauses_it_compiles_to) :-
    with_output_to(string(Text),
                   sscasp_show(flies(_), code(query(false), user(false),
                                              compiled(true)))),
    assertion(sub_string(Text, _, _, _, "$dual$flies$1")),
    assertion(\+ sub_string(Text, _, _, _, "bird(tweety).")).

test(anything_else_is_refused,
     [ error(domain_error(sscasp_show, tree)) ]) :-
    sscasp_show(flies(_), tree).

:- end_tests(api_show).

		 /*******************************
		 *         DESCRIBING		*
		 *******************************/

/** `:- describe Literal :: Template` says how to put a literal into a
    sentence.  It changes nothing about what is proved, so what is worth
    testing from here is that it *arrives* — and that a description or a
    signature for a predicate the query cannot reach does not.

    That second half is not tidiness.  A signature is checked against the
    program's definitions (`annotations.pl:check_signature_arity/4`), and
    a relevance-scoped program has only the definitions the query
    reaches, so collecting `:- pred shading(shade)` for a query about
    birds raised `existence_error(procedure, shading/1)` on a program
    that is perfectly well formed.
*/

:- begin_tests(api_describe).

annotations(Query, Annotations) :-
    sscasp_query_clauses(Query, Terms),
    program_annotations(Terms, Annotations).

test(a_description_travels_with_the_query) :-
    annotations(flies(_), A),
    assertion(described(A, flies(polly), 'polly can fly')),
    assertion(described(A, not flies(polly), 'polly cannot fly')).

test(the_later_description_is_the_more_specific_one) :-
    annotations(flies(_), A),
    assertion(described(A, flies(tweety), 'the famous one flies')).

test(a_signature_the_query_cannot_reach_is_left_behind) :-
    sscasp_query_clauses(flies(_), Terms),
    assertion(\+ memberchk((:- pred shading(shade)), Terms)),
    assertion(\+ member((:- describe(shading(_) :: _, _)), Terms)).

test(and_comes_along_when_it_can) :-
    annotations(shading(_), A),
    assertion(argument_domain(A, shading/1, 1, shade)),
    assertion(described(A, shading(dark), 'dark is the shading')).

%   A domain declares a predicate rather than describing one, so it has no
%   definition to be checked against and travels with every query.  It
%   costs two facts nothing calls.

test(a_domain_declaration_needs_no_definition) :-
    sscasp_query_clauses(flies(_), Terms),
    assertion(memberchk((:- domain shade = [dark, light]), Terms)).

test(a_description_changes_no_answer, all(X == [tweety])) :-
    sscasp(flies(X)).

:- end_tests(api_describe).

		 /*******************************
		 *           IN WORDS		*
		 *******************************/

/** Two routes to a printed answer, and they are different on purpose.

    `sscasp_human/1,2` **prints**, which is what a program wants.  The
    `?`-shortcuts do not: they leave the answer where the toplevel can
    find it and it is shown as part of the answer, after the bindings and
    named together with them.  So a test for the shortcuts asks what the
    toplevel would be handed, not what reached the terminal.
*/

:- begin_tests(api_human).

test(an_answer_in_words) :-
    with_output_to(string(Text), once(sscasp_human(flies(_)))),
    assertion(sub_string(Text, _, _, _, "the famous one flies")),
    assertion(sub_string(Text, _, _, _, "Justification")).

test(a_literal_with_no_description_falls_back) :-
    with_output_to(string(Text), once(sscasp_human(flies(_)))),
    assertion(sub_string(Text, _, _, _, "bird holds for tweety")).

test(the_model_alone) :-
    with_output_to(string(Text),
                   once(sscasp_human(flies(_), [show_justification(false)]))),
    assertion(sub_string(Text, _, _, _, "Model")),
    assertion(\+ sub_string(Text, _, _, _, "Justification")).

%   The shortcuts hand the toplevel a goal to portray.  Nothing is handed
%   over unless the query asked, which is what separates `? Q` from
%   `sscasp(Q)`.

test(the_toplevel_is_handed_the_model) :-
    once(( sscasp(flies(_), [show_model(true)]),
           phrase(sscasp:sscasp_residuals, Residuals)
         )),
    assertion(Residuals = [show_model(_, _)]).

test(and_the_justification_when_asked) :-
    once(( sscasp(flies(_), [show_model(true), show_justification(true)]),
           phrase(sscasp:sscasp_residuals, Residuals)
         )),
    assertion(Residuals = [show_model(_, _), show_justification(_, _)]).

test(and_nothing_when_it_did_not) :-
    once(( sscasp(flies(_), []),
           phrase(sscasp:sscasp_residuals, Residuals)
         )),
    assertion(Residuals == []).

test(and_it_portrays_as_the_machine_form) :-
    once(( sscasp(flies(_), [show_model(true)]),
           phrase(sscasp:sscasp_residuals, [Goal|_])
         )),
    with_output_to(string(Text), print(Goal)),
    assertion(sub_string(Text, _, _, _, "flies(tweety)")),
    assertion(\+ sub_string(Text, _, _, _, "can fly")).

test(and_in_words_when_that_was_asked) :-
    once(( sscasp(flies(_), [show_model(human)]),
           phrase(sscasp:sscasp_residuals, [Goal|_])
         )),
    with_output_to(string(Text), print(Goal)),
    assertion(sub_string(Text, _, _, _, "the famous one flies")).

:- end_tests(api_human).

		 /*******************************
		 *          HYGIENE		*
		 *******************************/

/** No test may rest on a predicate this file does not define

`sscasp/2` collects by **name**: a goal is looked up where the query's module
can see it, so a predicate the program leaves undefined resolves to whatever a
loaded library happens to export under that name.  That is not hypothetical.
An earlier version of this file declared `maybe` abducible and called it;
`maybe/0` is a predicate of `library(random)`, so the moment anything in the
process loaded that library the collector took its one clause —
`random(2) =:= 0` — and the query answered with a coin toss in the model.  The
test passed alone and failed in `make check`.

Two ways out, and `CLAUDE.md` records the rule:

  - define it, `p :- fail.` if it is meant to have no solutions;
  - declare it, if it is an abducible, a domain or an `:- sld`, which is what
    `dyncall.pl:declared/2` reads.

A name so unlikely that nothing else could own it is the third, for a
predicate that exists only between `setup` and `cleanup`.

This test walks what each query actually collects and checks it.
*/

:- begin_tests(api_hygiene).

%!  queried(?Goal) is nondet.
%
%   Every goal the tests above ask.  Kept here rather than derived,
%   because a test that derived them would pass by finding none.

queried(flies(_)).
queried(chosen(_)).
queried(colour(_)).
queried(day(_)).
queried(perhaps).
queried(next(_)).
queried(announce).
queried(small(_)).
queried(outside(_)).
queried(window(_)).

test(every_collected_predicate_is_this_module_s, [forall(queried(Goal))]) :-
    sscasp_query_clauses(test_api:Goal, Terms),
    forall(( member(Term, Terms),
             collected_head(Term, Head)
           ),
           assertion(ours(Head))).

collected_head((:- _), _) :- !, fail.
collected_head((Head :- _), Head) :- !.
collected_head(Head, Head).

%!  ours(+Head) is semidet.
%
%   True when this module defines the predicate, or a directive says what
%   it is.  `-p(X)` is a clause of `-/1`, so it is asked about under the
%   name it is stored by.

ours(-(Inner)) :-
    !,
    functor(Inner, Name, Arity),
    ( predicate_property(test_api:(-(_)), number_of_clauses(_)) -> true ; fail ),
    functor(Inner, Name, Arity).
ours(Head) :-
    functor(Head, Name, Arity),
    functor(General, Name, Arity),
    (   predicate_property(test_api:General, number_of_clauses(_)),
        \+ predicate_property(test_api:General, imported_from(_))
    ->  true
    ;   sscasp_dyncall:declared(test_api, General)
    ).

:- end_tests(api_hygiene).
