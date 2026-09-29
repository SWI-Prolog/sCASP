:- module(test_api,
          [ test_api/0
          ]).
:- use_module('../scasp_plunit', []).        % sets the library search path
:- use_module(library(sscasp)).
:- use_module(library(plunit)).
:- use_module(library(dif), [dif/2]).
:- use_module(library(modules), [in_temporary_module/3]).
:- use_module(library(sscasp/annotations), [program_annotations/2, described/3,
                                          argument_domain/4]).
:- use_module(library(lists), [member/2, memberchk/2, subtract/3]).
:- use_module(library(clpq), [{}/1]).   % a rendered store is called, not only read
:- use_module(library(debug), [assertion/1]).

/** <module> s(CASP) from Prolog

`library(sscasp)` over ordinary Prolog clauses: what `scasp/2` collects, what
comes back, and what it looks like printed.

The program under test is in this file, as a program using the library would
be.  Two things about that are worth knowing.

**`library(scasp)` may be loaded too.**  `test_all.pl` puts every suite in one
process, and both libraries install a `user:term_expansion/2` for
`false :- Body`; the first one declared wins.  `dyncall.pl:global_constraint/2`
reads what *either* leaves behind, so these tests pass whichever order the two
were loaded in — which is the point of it reading both.

**A `-p(X)` head is written below**, and `api_expansion` checks where it
landed.  Both libraries read that spelling and represent it differently —
`-/1` here, a predicate named `'-p'` there — and both install their
expansions in `user`, where they see every file in the process.  Each asks
whether the file being compiled imported *it* (`sscasp.pl:ours/1`,
`scasp/dyncall.pl:ours/0`); without that, which library had loaded first
would decide what this file holds.

The tests that assert their clauses at runtime pin something else: `assertz/1`
runs no expansion, so they are about the collector rather than the reader.
*/

test_api :-
    run_tests([ api_answers,
                api_collection,
                api_rendering,
                api_sld,
                api_show,
                api_describe,
                api_human,
                api_expansion,
                api_dynamic,
                api_sandbox,
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
:- scasp_directive(abducible(assumed_thing)).
:- dynamic assumed_thing/0.     % no clauses, on purpose: for check/0
perhaps :- assumed_thing.

:- scasp_directive(domain(day = [mon, tue])).

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
:- sld api_anything/1.
:- sld api_second_a/2.
:- sld api_shaped/1.
:- sld api_sign/2.
:- sld api_calls_nowhere/1.
:- sld (api_sld_one/1, api_sld_two/1) as opaque.
:- sld api_sld_three/1, api_sld_four/1.
api_sld_one(X) :- X is 0+1.
api_sld_two(X) :- X is 1+1.
api_sld_three(X) :- X is 2+1.
api_sld_four(X) :- X is 3+1.
api_sign(X, Y), X > 0 => Y = pos.
api_sign(_, Y) => Y = other.
api_calls_nowhere(X) :- api_nowhere(X).
api_anything(_).
api_second_a(_, a).
api_shaped(f(_)).
outside(X) :- colour_index(X), not small(X).
colour_index(1).
colour_index(3).

%  A classically negated head, written rather than asserted, so that it
%  goes through the reader.  Unguarded, the other library's expansion
%  interns it into `'-api_known_dead'/1` and the clause is lost to a name
%  nothing looks for.
-api_known_dead(X) :- api_buried(X).
%  a description of a classically negated predicate with no positive
%  clauses: the query reaches only -api_sinks/1
-api_sinks(X) :- api_floater(X).
:- describe -api_sinks(_X) :: "@(X) is known not to sink".
api_buried(yorick).
api_floater(cork).

%  the program `api_dynamic` changes.  Here rather than inside the unit
%  because plunit gives a unit a module of its own: a clause written
%  there is not one `test_api` has, and the tests assert into `test_api`.
%
%  `as shared` is `dynamic` rather than `thread_local`, because
%  `test_all.pl` runs units in parallel and a thread_local clause
%  asserted in one thread is invisible in another.  One test checks that
%  the *default* is the other way.
:- scasp_dynamic api_dyn_bird/1 as shared.
:- scasp_dynamic (api_dyn_penguin/1, api_dyn_p/0) as shared.

api_dyn_flies(X) :- api_dyn_bird(X), not api_dyn_penguin(X).

		 /*******************************
		 *          ANSWERING		*
		 *******************************/

:- begin_tests(api_answers).

test(a_query_answers_from_the_clauses_around_it, set(X == [tweety])) :-
    scasp(flies(X)).

test(and_carries_a_model) :-
    scasp(flies(tweety), [model(Model)]),
    assertion(memberchk(bird(tweety), Model)),
    assertion(memberchk(not penguin(tweety), Model)).

test(and_a_justification) :-
    scasp(flies(tweety), [tree(Tree)]),
    assertion(memberchk(flies(tweety)-Children, Tree)),
    assertion(memberchk(bird(tweety)-[], Children)),
    assertion(memberchk('$nmr'-_, Tree)).

%   `module(?Module)` keeps the compiled program for the profiler.

test(the_program_may_be_kept_in_a_module) :-
    scasp(flies(tweety), [module(M)]),
    assertion(atom(M)),
    assertion(current_module(M)).

%   A global constraint whose call tree overlaps the query is applied;
%   one that does not is not.  `chosen(red)` is denied, so only blue is
%   left, and `lonely/1` says nothing about either.

test(an_overlapping_constraint_applies, set(X == [blue])) :-
    scasp(chosen(X)).

test(and_one_that_does_not_overlap_is_ignored) :-
    assertion(\+ \+ scasp(colour(red))).

%   `collect(Goal)` widens what is collected without widening the query.
%   With `lonely(1)` named, its constraint comes too and denies it.

test(collect_brings_in_what_the_query_cannot_reach) :-
    assertion(\+ \+ scasp(colour(red))),
    assertion(\+ scasp(colour(red), [collect(lonely(_))])).

%   scasp_json/3 takes scasp/2's options, and has to pass them on.

test(scasp_json_passes_the_options_on) :-
    scasp_json(colour(red), Plain),
    scasp_json(colour(red), Collected, [collect(lonely(_))]),
    assertion(Plain.answers = [_]),
    assertion(Collected.answers == []).

%   An abducible may be assumed, so the query holds — and holds without
%   it too, which is what `not perhaps` having an answer says.

test(an_abducible_may_be_assumed) :-
    assertion(\+ \+ scasp(perhaps)),
    assertion(\+ \+ scasp(not perhaps)).

test(a_declared_domain_is_a_predicate, set(D == [mon, tue])) :-
    scasp(day(D)).

%   A classically negated predicate is collected by the argument's name,
%   not by `-/1` — which holds the clauses of every one of them at once.
%   Asserted rather than written above; see the module comment.

test(classical_negation_is_collected,
     [ setup(assert_negation), cleanup(retract_negation) ]) :-
    assertion(\+ \+ scasp(-dead(tweety))),
    assertion(\+ scasp(-dead(polly))),
    scasp(-dead(tweety), [model(Model)]),
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
    scasp_query_clauses(flies(_), Terms),
    assertion(memberchk(bird(tweety), Terms)),
    assertion(memberchk(penguin(polly), Terms)),
    assertion(\+ memberchk(colour(_), Terms)).

test(a_global_constraint_comes_as_a_headless_rule) :-
    scasp_query_clauses(chosen(_), Terms),
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
    scasp_query_clauses(api_p, Terms),
    assertion(memberchk((-api_p :- api_q), Terms)).

test(and_its_body_is_collected_too,
     [ setup(assert_inconsistency), cleanup(retract_inconsistency) ]) :-
    scasp_query_clauses(api_p, Terms),
    assertion(memberchk(api_q, Terms)).

test(so_the_program_has_no_answer,
     [ setup(assert_inconsistency), cleanup(retract_inconsistency) ]) :-
    assertion(\+ scasp(api_p)).

%   And the other way: a query about the negation collects the positive.

test(and_the_other_way_round,
     [ setup(assert_inconsistency), cleanup(retract_inconsistency) ]) :-
    scasp_query_clauses(-api_p, Terms),
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
    scasp_query_clauses(day(_), Terms),
    assertion(memberchk((:- domain day = [mon, tue]), Terms)).

%   Two modules defining the same predicate cannot both be in one
%   program: the solver mangles a single module name into every functor,
%   so one of them would be discarded without saying so.

test(two_modules_defining_one_predicate_are_refused,
     [ setup(assert_elsewhere), cleanup(retract_elsewhere),
       error(permission_error(collect, scasp_predicate, _)) ]) :-
    scasp_query_clauses(test_api:api_shared_head(_), _).

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
    scasp_readable(f(X), Readable),
    assertion(Readable = f('| '('$VAR'('A'), {'$VAR'('A') =< 3}))).

test(a_disequality_is_shown_as_the_program_wrote_it) :-
    dif(X, a),
    scasp_readable(f(X), Readable),
    assertion(Readable = f('| '('$VAR'('A'), {'$VAR'('A') \= a}))).

test(a_singleton_is_not_named) :-
    scasp_readable(f(_, Y, Y), Readable),
    assertion(Readable == f('$VAR'('_'), '$VAR'('A'), '$VAR'('A'))).

%   A constraint between two variables is shown once, on the first of
%   them: printing it again on the second would say the same thing twice.

test(a_shared_constraint_is_shown_once) :-
    dif(X, Y),
    scasp_readable(f(X, Y), f(First, Second)),
    assertion(First = '| '('$VAR'('A'), _)),
    assertion(Second == '$VAR'('B')).

%   Reading a `dif/2` must not destroy it — `dif:attribute_goals//1`
%   deletes each or-node as it reports it, and only `copy_term/3` and the
%   toplevel undo that.

test(rendering_leaves_the_answer_alone) :-
    dif(X, a),
    scasp_readable(f(X), _),
    assertion(X \= a),
    assertion(\+ X \= b).

%   The constrained answer, end to end.

test(a_constrained_answer_reads_back) :-
    findall(Readable,
            ( scasp(not window(V)),
              scasp_readable(V, Readable)
            ),
            Answers),
    assertion(Answers = [_, _]),
    assertion(memberchk('| '(_, {_ =< 3}), Answers)),
    assertion(memberchk('| '(_, {_ >= 10}), Answers)).

%   The model and the justification have to be made readable *together*,
%   or a variable they share is named twice and the two names mean
%   different things.

%   CLP(Q) reports its store as one goal.  Each constraint goes to a
%   variable it is about, and a variable mentioned in another's store is
%   named there, not inlined with its own.

test(each_constraint_goes_to_a_variable_it_is_about) :-
    {B > 0, A - B > 0},
    scasp_readable(q(A, B), q(RA, RB)),
    assertion(RA = '| '('$VAR'('A'), {_})),
    assertion(RB = '| '('$VAR'('B'), {'$VAR'('B') > 0})),
    RA = '| '(_, {StoreA}),
    assertion(StoreA \= (_, _)),
    assertion(\+ sub_term('| '(_, _), StoreA)).

test(one_naming_for_the_model_and_the_tree) :-
    once(scasp(not window(V), [model(Model), tree(Tree)])),
    scasp_readable(v(V, Model, Tree), v(Named, ReadableModel, _)),
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
    scasp(next(Y)).

test(and_its_clauses_are_not_collected) :-
    scasp_query_clauses(next(_), Terms),
    assertion(memberchk((:- sld _), Terms)),
    assertion(\+ memberchk((successor(_, _) :- _), Terms)).

%   The literal is in the model like any other proved one, and the
%   justification holds the derivation below it — `2 is 1+1` is the step
%   the interpreter took.

test(the_derivation_is_in_the_justification) :-
    scasp(next(2), [model(Model), tree(Tree)]),
    assertion(memberchk(successor(1, 2), Model)),
    assertion(memberchk(next(2)-[successor(1, 2)-[(2 is 1+1)-[]]], Tree)).

%   `as opaque` runs it instead, so there is a node for the call and
%   nothing under it.  Which is the whole difference.

test(opaque_shows_the_call_and_no_more) :-
    scasp(announce, [tree(Tree)]),
    assertion(memberchk(announce-[noisy(hello)-[]], Tree)).

%   Negating an SLD goal **constrains** rather than tests.  `\+ small(X)`
%   for an unbound X asks whether *some* X works and says nothing about
%   the others, so the solutions are collected and the variable kept away
%   from each — which is what the answer says.

test(negation_of_an_unbound_sld_goal_constrains) :-
    findall(X-Store,
            ( scasp(not small(X)),
              copy_term(X, _, Store)
            ),
            Answers),
    assertion(Answers = [_]),
    Answers = [Value-_],
    assertion(Value \= 1),
    assertion(Value \= 2),
    assertion(\+ Value \= 3).

test(and_a_ground_one_is_simply_run) :-
    assertion(\+ \+ scasp(not small(3))),
    assertion(\+ scasp(not small(1))).

test(so_a_program_over_it_answers, set(X == [3])) :-
    scasp(outside(X)).

%   A solution that leaves a variable free holds for every value there.
%   So `not api_anything(V)` has no answer at all, and `api_second_a/2`
%   excludes only its second argument.  Kept apart from the free
%   variable instead, `dif(V, _)` held by choosing the `_`.

test(a_free_solution_leaves_nothing_to_negate) :-
    assertion(\+ scasp(not api_anything(_))),
    assertion(\+ scasp(not api_anything(3))).

test(and_constrains_only_the_positions_it_binds) :-
    findall(X-Y, scasp(not api_second_a(X, Y)), Answers),
    assertion(Answers = [_]),
    Answers = [X-Y],
    assertion(var(X)),
    assertion(\+ Y = a),
    assertion(\+ \+ Y = b).

%   A solution that fixes a *shape* excludes every instance of it, which
%   no disequality can say.  Refused rather than answered wrongly.

%   An SSU rule reaches the interpreter as `(Guard, !, Body)`, since
%   clause/2 drops the `=>`.  The guard is part of the derivation, and a
%   guard that fails passes the call on to the next rule.

test(an_ssu_rule_is_interpreted) :-
    scasp(api_sign(1, Y), [tree(Tree)]),
    assertion(Y == pos),
    assertion(memberchk(api_sign(1, pos)-[(1 > 0)-[], (pos = pos)-[]], Tree)),
    scasp(api_sign(-1, Z)),
    assertion(Z == other).

%   A predicate the interpreted clauses call and nobody defines is an
%   error, as it is under `as opaque` — not a failure that reads like an
%   answer.

test(an_undefined_callee_is_an_error,
     [ error(existence_error(procedure, _:api_nowhere/1)) ]) :-
    scasp(api_calls_nowhere(1)).

%   One `:- sld` may declare several, and `as` covers them all when they
%   are parenthesised.  Opaque shows the call and nothing under it;
%   justified shows the `3 is 2+1` the interpreter took.

test(a_declaration_may_list_several) :-
    forall(member(G, [api_sld_one(_), api_sld_two(_),
                      api_sld_three(_), api_sld_four(_)]),
           ( scasp_query_clauses(G, Terms),
             functor(G, Name, 1),
             assertion(\+ ( member((H :- _), Terms),
                             functor(H, Name, 1) ))
           )),
    scasp(api_sld_two(2), [tree(Opaque)]),
    assertion(memberchk(api_sld_two(2)-[], Opaque)),
    scasp(api_sld_three(3), [tree(Justified)]),
    assertion(memberchk(api_sld_three(3)-[_|_], Justified)).

test(a_solution_with_a_shape_is_refused,
     [ error(representation_error(scasp_universal_disequality), _) ]) :-
    scasp(not api_shaped(_)).

:- end_tests(api_sld).

		 /*******************************
		 *          SHOWING		*
		 *******************************/

:- begin_tests(api_show).

test(the_program_a_query_would_run) :-
    with_output_to(string(Text), scasp_show(flies(_), code)),
    assertion(sub_string(Text, _, _, _, "?- flies(")),
    assertion(sub_string(Text, _, _, _, "bird(tweety)")),
    assertion(sub_string(Text, _, _, _, "not penguin")),
    assertion(\+ sub_string(Text, _, _, _, "colour")).

%   An option given as a bare name means `true`.

test(a_bare_option_name_is_true) :-
    with_output_to(string(Bare), scasp_show(flies(_), code(duals))),
    with_output_to(string(Full), scasp_show(flies(_), code(duals(true)))),
    assertion(Bare == Full),
    with_output_to(string(Plain), scasp_show(flies(_), code)),
    assertion(Bare \== Plain).

%   The duals are what `not p` *tries*, with the helpers `dualise.pl`
%   generates inlined as a disjunction — which is the one thing a reader
%   of a dual wants and the reason this is not `portray_clause/1` over
%   the compiled terms.

test(and_what_not_p_tries) :-
    with_output_to(string(Text),
                   scasp_show(flies(_), code(query(false), user(false),
                                              duals(true)))),
    assertion(sub_string(Text, _, _, _, "not flies(")),
    assertion(sub_string(Text, _, _, _, ";")),
    assertion(\+ sub_string(Text, _, _, _, "$dual")).

test(and_the_check_it_has_to_pass) :-
    with_output_to(string(Text),
                   scasp_show(chosen(_), code(query(false), user(false),
                                               constraints(true)))),
    assertion(sub_string(Text, _, _, _, "check(1)")),
    assertion(sub_string(Text, _, _, _, "the global constraint")),
    assertion(sub_string(Text, _, _, _, "not chosen(red)")).

%   The one section that shows a directive, since the compiler turns
%   those into clauses and they stop being visible as themselves.

test(and_the_terms_as_collected) :-
    with_output_to(string(Text),
                   scasp_show(day(_), code(query(false), user(false),
                                            source(true)))),
    assertion(sub_string(Text, _, _, _, ":- domain day")).

test(and_the_clauses_it_compiles_to) :-
    with_output_to(string(Text),
                   scasp_show(flies(_), code(query(false), user(false),
                                              compiled(true)))),
    assertion(sub_string(Text, _, _, _, "$dual$flies$1")),
    assertion(\+ sub_string(Text, _, _, _, "bird(tweety).")).

test(anything_else_is_refused,
     [ error(domain_error(scasp_show, tree)) ]) :-
    scasp_show(flies(_), tree).

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
    scasp_query_clauses(Query, Terms),
    program_annotations(Terms, Annotations).

test(a_description_travels_with_the_query) :-
    annotations(flies(_), A),
    assertion(described(A, flies(polly), 'polly can fly')),
    assertion(described(A, not flies(polly), 'polly cannot fly')).

%   The same through scasp/2, which is how a program gets them without
%   reaching into the library's own modules.

test(scasp_gives_the_annotations_on_request, [nondet]) :-
    scasp(flies(X), [annotations(A)]),
    assertion(described(A, flies(X), _)).

test(the_later_description_is_the_more_specific_one) :-
    annotations(flies(_), A),
    assertion(described(A, flies(tweety), 'the famous one flies')).

test(a_signature_the_query_cannot_reach_is_left_behind) :-
    scasp_query_clauses(flies(_), Terms),
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
    scasp_query_clauses(flies(_), Terms),
    assertion(memberchk((:- domain shade = [dark, light]), Terms)).

test(a_description_changes_no_answer, all(X == [tweety])) :-
    scasp(flies(X)).

%   The query reaches `neg(api_sinks/1)` and never api_sinks/1, which has
%   no clauses; the description is about both.

test(a_description_of_a_negated_predicate_travels_with_it) :-
    annotations(-api_sinks(_), A),
    assertion(described(A, -api_sinks(cork), 'cork is known not to sink')).

:- end_tests(api_describe).

		 /*******************************
		 *           IN WORDS		*
		 *******************************/

/** Two routes to a printed answer, and they are different on purpose.

    `scasp_human/1,2` **prints**, which is what a program wants.  The
    `?`-shortcuts do not: they leave the answer where the toplevel can
    find it and it is shown as part of the answer, after the bindings and
    named together with them.  So a test for the shortcuts asks what the
    toplevel would be handed, not what reached the terminal.
*/

:- begin_tests(api_human).

test(an_answer_in_words) :-
    with_output_to(string(Text), once(scasp_human(flies(_)))),
    assertion(sub_string(Text, _, _, _, "the famous one flies")),
    assertion(sub_string(Text, _, _, _, "Justification")).

test(a_literal_with_no_description_falls_back) :-
    with_output_to(string(Text), once(scasp_human(flies(_)))),
    assertion(sub_string(Text, _, _, _, "bird holds for tweety")).

%   It prints the answer and leaves nothing for the toplevel, which
%   would otherwise print it a second time.

test(it_leaves_nothing_for_the_toplevel) :-
    once(( with_output_to(string(_), scasp_human(flies(_))),
           phrase(sscasp:scasp_residuals, Residuals)
         )),
    assertion(Residuals == []).

test(the_model_alone) :-
    with_output_to(string(Text),
                   once(scasp_human(flies(_), [show_justification(false)]))),
    assertion(sub_string(Text, _, _, _, "Model")),
    assertion(\+ sub_string(Text, _, _, _, "Justification")).

%   The shortcuts hand the toplevel a goal to portray.  Nothing is handed
%   over unless the query asked, which is what separates `? Q` from
%   `scasp(Q)`.

test(the_toplevel_is_handed_the_model) :-
    once(( scasp(flies(_), [show_model(true)]),
           phrase(sscasp:scasp_residuals, Residuals)
         )),
    assertion(Residuals = [show_model(_, _)]).

test(and_the_justification_when_asked) :-
    once(( scasp(flies(_), [show_model(true), show_justification(true)]),
           phrase(sscasp:scasp_residuals, Residuals)
         )),
    assertion(Residuals = [show_model(_, _), show_justification(_, _)]).

test(and_nothing_when_it_did_not) :-
    once(( scasp(flies(_), []),
           phrase(sscasp:scasp_residuals, Residuals)
         )),
    assertion(Residuals == []).

test(and_it_portrays_as_the_machine_form) :-
    once(( scasp(flies(_), [show_model(true)]),
           phrase(sscasp:scasp_residuals, [Goal|_])
         )),
    with_output_to(string(Text), print(Goal)),
    assertion(sub_string(Text, _, _, _, "flies(tweety)")),
    assertion(\+ sub_string(Text, _, _, _, "can fly")).

test(and_in_words_when_that_was_asked) :-
    once(( scasp(flies(_), [show_model(human)]),
           phrase(sscasp:scasp_residuals, [Goal|_])
         )),
    with_output_to(string(Text), print(Goal)),
    assertion(sub_string(Text, _, _, _, "the famous one flies")).

:- end_tests(api_human).

		 /*******************************
		 *          HYGIENE		*
		 *******************************/

/** No test may rest on a predicate this file does not define

`scasp/2` collects by **name**: a goal is looked up where the query's module
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

		 /*******************************
		 *       WHOSE DIRECTIVE		*
		 *******************************/

/** Both libraries read the same spellings and represent them
    differently, and both install their expansions in `user`, where they
    see every file in the process.  So each asks whether the file being
    compiled imported *it* — `sscasp.pl:ours/1` here,
    `scasp/dyncall.pl:ours/0` there — and a file gets exactly one
    library's reading of `-p(X)`, `:- pred`, `#pred`, `:- scasp_dynamic`
    and `false :- Body`.

    This unit is about the reader rather than the solver: what the file
    holds after loading, not what a query does with it.  It matters
    because `make check` puts both libraries in one process, and before
    the guards the answer depended on which had loaded first.
*/

:- begin_tests(api_expansion).

%   A `-p(X)` head is a clause of `-/1` carrying `p(X)`.  The other
%   library would have made it a predicate named `'-api_known_dead'`.

test(a_negated_head_stays_a_clause_of_the_negation_predicate) :-
    assertion(clause(test_api:(-(api_known_dead(_))), _)),
    assertion(\+ current_predicate(test_api:'-api_known_dead'/1)).

test(and_the_solver_finds_it, [set(X == [yorick])]) :-
    scasp(-api_known_dead(X)).

%   `:- pred p(dom)` is a *signature* here and a description in the old
%   library, whose expansion raises on this spelling rather than
%   declining it.

test(a_signature_is_read_as_a_signature) :-
    assertion(sscasp_dyncall:directive(test_api, pred(shading(shade)))),
    assertion(\+ current_predicate(test_api:pr_pred_predicate/4)).

%   `:- scasp_dynamic p/1` declares `p/1` and `-/1`.  The old library
%   declares the interned `'-p'/1` instead, which nothing here would
%   ever put a clause in.

test(a_dynamic_declaration_declares_our_negation_predicate) :-
    assertion(predicate_property(test_api:api_dyn_bird(_), (dynamic))),
    assertion(predicate_property(test_api:(-(_)), (dynamic))),
    assertion(\+ current_predicate(test_api:'-api_dyn_bird'/1)).

%   `false :- Body` becomes a clause of `'scasp$constraint'/1` in its own
%   module.  The old library makes it a clause of `-/0`.

test(a_global_constraint_is_a_clause_of_its_module) :-
    assertion(test_api:'scasp$constraint'(_)),
    assertion(\+ current_predicate(test_api:(-)/0)).

:- end_tests(api_expansion).

/** `p` and `-p` are one predicate, so a module that exports `p` has
    exported `-p` with it: a `-p(X)` called where `p` is imported is
    looked up in the module that holds `p`.  The old implementation
    interns `-p` as `'-p'`, which a module has to export by name.
*/

:- begin_tests(api_negation_across_modules,
               [ setup(api_neg_modules),
                 cleanup(api_neg_cleanup)
               ]).

test(a_negation_is_found_where_its_predicate_is, all(X == [b])) :-
    scasp(api_neg_rules:api_neg_ok(X)).

:- end_tests(api_negation_across_modules).

api_neg_modules :-
    api_neg_cleanup,
    assertz(api_neg_guide:api_neg_contra(a)),
    assertz(api_neg_guide:(-(api_neg_contra(b)))),
    api_neg_guide:export(api_neg_contra/1),
    api_neg_rules:import(api_neg_guide:api_neg_contra/1),
    assertz(api_neg_rules:(api_neg_ok(X) :- -api_neg_contra(X))).

%   The clauses go; the export and import may stay, since both are
%   idempotent and setup declares them again.

api_neg_cleanup :-
    retractall(api_neg_guide:api_neg_contra(_)),
    retractall(api_neg_guide:(-(_))),
    retractall(api_neg_rules:api_neg_ok(_)).

/** A global constraint or a directive has the lifetime of the program
    it is part of.  Both used to be recorded in a table keyed by the
    module's name, which outlived both a reload and the module itself.
*/

:- dynamic api_lifetime_file/1.

:- begin_tests(api_constraint_lifetime,
               [ setup(make_lifetime_file),
                 cleanup(delete_lifetime_file)
               ]).

%   Deleted from the source and reloaded, it is gone.  It survived make/0.

test(a_constraint_removed_from_the_file_goes_on_reload, fail) :-
    lifetime_file(File),
    write_lifetime_file(File, [ "api_lt_p.", "api_lt_q.",
                                "false :- api_lt_q." ]),
    load_files(File, [if(true), silent(true)]),
    assertion(\+ api_lifetime:scasp(api_lt_p, [collect(api_lt_q)])),
    write_lifetime_file(File, [ "api_lt_p.", "api_lt_q." ]),
    load_files(File, [if(true), silent(true)]),
    \+ api_lifetime:scasp(api_lt_p, [collect(api_lt_q)]).

%   One asserted at run time is not part of the file, and a reload keeps
%   it, as it keeps any other asserted clause.

test(a_constraint_asserted_at_run_time_survives_a_reload, fail) :-
    lifetime_file(File),
    write_lifetime_file(File, [ "api_lt_p.", "api_lt_q." ]),
    load_files(File, [if(true), silent(true)]),
    scasp_assert(api_lifetime:(false :- api_lt_q)),
    load_files(File, [if(true), silent(true)]),
    api_lifetime:scasp(api_lt_p, [collect(api_lt_q)]).

%   The same for a directive.  `api_lt_a` has no clauses, so `api_lt_p`
%   holds only while it is declared abducible.

test(a_directive_removed_from_the_file_goes_on_reload, fail) :-
    lifetime_file(File),
    write_lifetime_file(File, [ "api_lt_p :- api_lt_a.",
                                ":- scasp_directive(abducible(api_lt_a))." ]),
    load_files(File, [if(true), silent(true)]),
    assertion(api_lifetime:scasp(api_lt_p, [])),
    write_lifetime_file(File, [ "api_lt_p :- api_lt_a." ]),
    load_files(File, [if(true), silent(true)]),
    api_lifetime:scasp(api_lt_p, []).

test(a_directive_given_at_run_time_survives_a_reload) :-
    lifetime_file(File),
    write_lifetime_file(File, [ "api_lt_p :- api_lt_a." ]),
    load_files(File, [if(true), silent(true)]),
    scasp_directive(api_lifetime:abducible(api_lt_a)),
    load_files(File, [if(true), silent(true)]),
    api_lifetime:scasp(api_lt_p, []),
    !.

%   in_temporary_module/3 names its module from the random state, so the
%   same seed gives the same name.  The second program has no constraint
%   and must not inherit the first one's.  The goals are qualified
%   because in_temporary_module/3 calls them in the new module.

test(a_temporary_module_does_not_inherit_constraints) :-
    set_random(seed(42)),
    in_temporary_module(M1,
                        forall(member(C, [api_lt_b, (false :- api_lt_b)]),
                               test_api:scasp_assert(M1:C)),
                        \+ test_api:scasp(M1:api_lt_b)),
    set_random(seed(42)),
    in_temporary_module(M2,
                        test_api:scasp_assert(M2:api_lt_b),
                        test_api:scasp(M2:api_lt_b)),
    assertion(M1 == M2).

%   One file for the unit: a second file declaring the same module is a
%   permission error rather than a reload.

make_lifetime_file :-
    tmp_file(api_lifetime, Base),
    file_name_extension(Base, pl, File),
    retractall(test_api:api_lifetime_file(_)),
    assertz(test_api:api_lifetime_file(File)).

lifetime_file(File) :-
    test_api:api_lifetime_file(File).

write_lifetime_file(File, Clauses) :-
    setup_call_cleanup(
        open(File, write, Out),
        ( format(Out, ":- module(api_lifetime, []).~n", []),
          format(Out, ":- use_module(library(sscasp)).~n", []),
          forall(member(C, Clauses), format(Out, "~w~n", [C]))
        ),
        close(Out)).

delete_lifetime_file :-
    forall(( test_api:api_lifetime_file(File),
             exists_file(File)
           ),
           delete_file(File)).

:- end_tests(api_constraint_lifetime).

		 /*******************************
		 *    CHANGING THE PROGRAM	*
		 *******************************/

/** `scasp/2` reads the module's clauses at query time, so a program
    that changes between queries needs no invalidation — the next
    answer is about what is there now.  What `scasp_assert/1` adds over
    `assertz/1` is the two things an s(CASP) clause can be that a Prolog
    clause cannot: a classically negated head, and a global constraint.

    Everything here asserts into `test_api` by name.  plunit gives each
    unit a module of its own, so an unqualified `scasp_assert/1` would
    put the clause where the *next* test cannot see it — the same reason
    the units above write `assertz(test_api:...)`.

    The predicates are `as shared`, which is `dynamic` rather than
    `thread_local`: `test_all.pl` runs units in parallel and a
    `thread_local` clause asserted in one thread is invisible in
    another.  One test below checks that the *default* is the other way.
*/

:- begin_tests(api_dynamic).

%   Only the constraints this unit adds.  `test_api` also has two declared
%   at load time that other units rely on, and plunit runs units in
%   parallel.

empty_program :-
    scasp_abolish(test_api:api_dyn_bird/1),
    scasp_abolish(test_api:api_dyn_penguin/1),
    scasp_abolish(test_api:api_dyn_p/0),
    scasp_retractall(test_api:(false :- api_dyn_flies(_))).

%   A fact added is an answer, and one taken away is not.

test(a_fact_asserted_is_an_answer,
     [ setup(empty_program), cleanup(empty_program), set(X == [tweety]) ]) :-
    scasp_assert(test_api:api_dyn_bird(tweety)),
    scasp(test_api:api_dyn_flies(X)).

test(and_retracting_it_takes_the_answer_away,
     [ setup(empty_program), cleanup(empty_program), fail ]) :-
    scasp_assert(test_api:api_dyn_bird(tweety)),
    scasp_retract(test_api:api_dyn_bird(tweety)),
    scasp(test_api:api_dyn_flies(_)).

test(a_rule_may_be_asserted_too,
     [ setup(empty_program), cleanup(empty_program), set(X == [polly]) ]) :-
    scasp_assert(test_api:api_dyn_bird(polly)),
    scasp_assert(test_api:(api_dyn_penguin(Y) :- api_dyn_bird(Y), fail)),
    scasp(test_api:api_dyn_flies(X)),
    Y = Y.

%   The position argument is accepted and ignored.  A caller written
%   against `library(scasp)` passes one, and this solver keeps no source
%   references to attach it to.

test(a_position_is_accepted_and_ignored,
     [ setup(empty_program), cleanup(empty_program), set(X == [tweety]) ]) :-
    scasp_assert(test_api:api_dyn_bird(tweety), 'nowhere.pl':42),
    scasp(test_api:api_dyn_flies(X)).

		 /*******************************
		 *     CLASSICAL NEGATION	*
		 *******************************/

/** The difference from `library(scasp)` worth knowing when porting: the
    old implementation interns `-p(X)` into a predicate named `'-p'`, so
    its `scasp_assert/1` renames the functor.  Here `-p(X)` is a clause
    of `-/1` carrying `p(X)`, so the term is asserted as written.
*/

test(a_negated_head_is_asserted_as_written,
     [ setup(empty_program), cleanup(empty_program) ]) :-
    scasp_assert(test_api:api_dyn_bird(tweety)),
    scasp_assert(test_api:(-api_dyn_penguin(X) :- api_dyn_bird(X))),
    assertion(\+ current_predicate(test_api:'-api_dyn_penguin'/1)),
    assertion(clause(test_api:(-(api_dyn_penguin(_))), _)).

test(and_is_proved_under_its_own_name,
     [ setup(empty_program), cleanup(empty_program), set(X == [tweety]) ]) :-
    scasp_assert(test_api:api_dyn_bird(tweety)),
    scasp_assert(test_api:(-api_dyn_penguin(Y) :- api_dyn_bird(Y))),
    scasp(test_api:(-api_dyn_penguin(X))),
    Y = Y.

%   `scasp_abolish/1` is more than `retractall/1` because it empties
%   **both** polarities: a program that has forgotten `p` but still
%   proves `-p` is not one the caller asked for.

test(abolish_empties_both_polarities,
     [ setup(empty_program), cleanup(empty_program) ]) :-
    scasp_assert(test_api:api_dyn_penguin(polly)),
    scasp_assert(test_api:(-api_dyn_penguin(sam))),
    scasp_abolish(test_api:api_dyn_penguin/1),
    assertion(\+ clause(test_api:api_dyn_penguin(_), _)),
    assertion(\+ clause(test_api:(-(api_dyn_penguin(_))), _)).

%   And it empties rather than abolishes, so the next assert does not
%   have to re-establish the scope.

test(and_leaves_the_predicate_declared,
     [ setup(empty_program), cleanup(empty_program) ]) :-
    scasp_abolish(test_api:api_dyn_bird/1),
    assertion(predicate_property(test_api:api_dyn_bird(_), (dynamic))).

		 /*******************************
		 *     GLOBAL CONSTRAINTS	*
		 *******************************/

/** `false/0` is a built-in and cannot be given clauses, so a global
    constraint is **recorded** rather than asserted — which is what the
    `false :- Body` term expansion does at load time.  Both spellings
    mean the same thing.
*/

test(a_global_constraint_rules_an_answer_out,
     [ setup(empty_program), cleanup(empty_program), fail ]) :-
    scasp_assert(test_api:api_dyn_bird(tweety)),
    scasp_assert(test_api:(false :- api_dyn_flies(tweety))),
    scasp(test_api:api_dyn_flies(_)).

test(and_the_directive_spelling_means_the_same,
     [ setup(empty_program), cleanup(empty_program), fail ]) :-
    scasp_assert(test_api:api_dyn_bird(tweety)),
    scasp_assert(test_api:(:- api_dyn_flies(tweety))),
    scasp(test_api:api_dyn_flies(_)).

test(and_it_can_be_retracted_again,
     [ setup(empty_program), cleanup(empty_program), set(X == [tweety]) ]) :-
    scasp_assert(test_api:api_dyn_bird(tweety)),
    scasp_assert(test_api:(false :- api_dyn_flies(tweety))),
    scasp_retract(test_api:(false :- api_dyn_flies(tweety))),
    scasp(test_api:api_dyn_flies(X)).

		 /*******************************
		 *        THE DECLARATION	*
		 *******************************/

/** Declaring `p/1` also declares `-/1`, which holds `-p(X)` — and every
    other classically negated head in the module.  So its scope is
    decided once, by the first declaration to reach it, rather than per
    predicate.  The old implementation gives each one its own `'-p'/1`
    and can scope them separately; that is the price of not interning.
*/

test(declaring_a_predicate_declares_the_negated_half) :-
    assertion(predicate_property(test_api:(-(_)), (dynamic))).

%   And declares **only** that one.  `:- scasp_dynamic` is one of the
%   four spellings both libraries claim (`sscasp.pl:claimed/3`), and the
%   old one expands it to declare the *interned* `'-p'/1` — a predicate
%   this tree never puts a clause in, while leaving `-/1`, which holds
%   them, undeclared.  Each library now asks whether the file imported
%   it, so neither claims the other's.  Found by this suite failing, not
%   by reading.

test(and_no_interned_predicate,
     [ forall(member(Name, ['-api_dyn_bird', '-api_dyn_penguin'])) ]) :-
    assertion(\+ current_predicate(test_api:Name/1)).

test(and_the_default_is_thread_local) :-
    scasp_dynamic(test_api:api_dyn_private/1),
    assertion(predicate_property(test_api:api_dyn_private(_), thread_local)).

test(a_conjunction_declares_each_one) :-
    scasp_dynamic(test_api:(api_dyn_a/1, api_dyn_b/2)),
    assertion(predicate_property(test_api:api_dyn_a(_), thread_local)),
    assertion(predicate_property(test_api:api_dyn_b(_, _), thread_local)).

test(and_a_variable_is_refused,
     [ error(instantiation_error) ]) :-
    scasp_dynamic(test_api:_).

test(as_is_something_that_is_not_a_predicate,
     [ error(type_error(predicate_indicator, tweety)) ]) :-
    scasp_dynamic(test_api:tweety).

test(and_so_is_an_unbound_scope,
     [ error(instantiation_error) ]) :-
    scasp_dynamic(test_api:(api_dyn_scope/1 as _)).

test(abolishing_a_variable_is_refused,
     [ error(instantiation_error) ]) :-
    scasp_abolish(test_api:_).

:- end_tests(api_dynamic).

		 /*******************************
		 *           SANDBOX		*
		 *******************************/

/** What SWISH may run.  `module(M)` asserts into M and adds to its
    imports for good, so an option list that carries it — or that is not
    a proper list, and might — is not declared safe.
*/

:- begin_tests(api_sandbox).

:- use_module(library(sandbox), [safe_goal/1]).

test(a_query_with_ordinary_options_is_safe) :-
    safe_goal(scasp(flies(_), [model(_), tree(_)])).

test(naming_a_module_is_not,
     [ error(permission_error(_, _, _), _) ]) :-
    safe_goal(scasp(flies(_), [module(lists)])).

test(nor_is_an_open_option_list,
     [ error(_) ]) :-
    safe_goal(scasp(flies(_), _)).

%   A program SWISH loads is loaded sandboxed, and the directives an
%   s(CASP) program writes have to get through: each writes only into
%   the module being loaded.  They used to be refused as cross-module
%   directives, every one of them.

test(the_declarations_load_sandboxed,
     [ setup(tmp_file_stream(text, File, Out)),
       cleanup(delete_file(File))
     ]) :-
    format(Out, ":- use_module(library(sscasp)).~n\c
                 :- abducible api_sbx_p.~n\c
                 :- domain api_sbx_d = [1, 2].~n\c
                 :- pred api_sbx_q(api_sbx_d).~n\c
                 :- describe api_sbx_q(_X) :: \"@(X) is q\".~n\c
                 :- show api_sbx_q/1.~n\c
                 :- scasp_dynamic api_sbx_fact/1.~n\c
                 api_sbx_q(1).~n", []),
    close(Out),
    Module = api_sandbox_program,
    Module:load_files(File, [sandboxed(true), silent(true)]),
    Module:scasp_query_clauses(api_sbx_q(_), Terms),
    forall(member(D, [abducible(_), domain(_), pred(_), describe(_, _),
                      show(_)]),
           assertion(memberchk((:- D), Terms))),
    assertion(predicate_property(Module:api_sbx_fact(_), dynamic)).

:- end_tests(api_sandbox).

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
queried(api_anything(_)).
queried(api_second_a(_, _)).
queried(api_shaped(_)).
queried(api_sign(_, _)).
queried(api_calls_nowhere(_)).
queried(api_sld_one(_)).
queried(api_sld_two(_)).
queried(api_sld_three(_)).
queried(api_sld_four(_)).
queried(window(_)).
queried(api_dyn_flies(_)).
queried(-api_known_dead(_)).

test(every_collected_predicate_is_this_module_s, [forall(queried(Goal))]) :-
    scasp_query_clauses(test_api:Goal, Terms),
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
