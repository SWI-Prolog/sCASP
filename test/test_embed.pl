:- module(test_embed,
          [ test_embed/0
          ]).
:- use_module('../prolog/scasp').
:- use_module(scasp_plunit).
:- use_module(library(plunit)).

/** <module> Tests for begin_scasp/end_scasp

Nothing tested this interface before.  It is the one that accepts s(CASP)
source almost verbatim - `#pred`, `#show`, `#abducible`, `-p(X)` heads,
`:- Constraint.` as a global constraint and a trailing `?- Query.` - by
rewriting those forms while the block is being read.  The programs below
are written the way they would be in a `.pl` file fed to the `scasp`
executable, which is the point: the same source has to work both ways.

## Obsolete

`begin_scasp/1,2` … `end_scasp/0` is obsolete; `scasp/2` over ordinary Prolog
clauses replaces it (`test_dyncall.pl`).  This suite is kept running so the API
does not rot silently while it still exists, but a reimplementation is under no
obligation to reproduce it.

The *language* features exercised here — `#pred`, `#show`, `#abducible`,
`-p(X)` heads, `:- Constraint.`, `?- Query.` — are not obsolete, and are
covered independently of blocks by the golden corpus.
*/

test_embed :-
    run_tests([ embed_birds,
                embed_negation,
                embed_constraints,
                embed_query,
                embed_directives,
                embed_exports
              ]).

		 /*******************************
		 *       CLASSIC NEGATION	*
		 *******************************/

:- begin_scasp(birds, [flies/1, -flies/1, bird/1]).

#pred flies(X) :: '@(X) can fly'.
#pred -flies(X) :: '@(X) can not fly'.

penguin(sam).
wounded(john).
bird(tweety).
bird(X) :- penguin(X).
bird(X) :- wounded(X).
ab(X) :- penguin(X).
ab(X) :- wounded(X).
flies(X) :- bird(X), not ab(X).
-flies(X) :- ab(X).

:- end_scasp.

:- begin_tests(embed_birds).

test(only_tweety_flies, all(X == [tweety])) :-
    flies(X).
test(penguins_do_not_fly, fail) :-
    flies(sam).
test(wounded_birds_do_not_fly, fail) :-
    flies(john).
test(classical_negation_wrapper_is_generated, set(X == [john, sam])) :-
    -flies(X).
test(model_is_available_after_the_call) :-
    once(flies(tweety)),
    scasp_model(Model),
    assertion(memberchk(bird(tweety), Model)),
    assertion(memberchk(not ab(tweety), Model)).
test(justification_is_available_after_the_call) :-
    once(flies(tweety)),
    scasp_justification(Tree, []),
    assertion(Tree = _-_).
test(exported_predicates_are_plain_prolog) :-
    assertion(predicate_property(flies(_), defined)).

:- end_tests(embed_birds).

		 /*******************************
		 *          EVEN LOOP		*
		 *******************************/

:- begin_scasp(pq, [p/1, q/1]).

p(X) :- not q(X).
q(X) :- not p(X).

:- end_scasp.

:- begin_tests(embed_negation).

test(even_loop_succeeds) :-
    assertion(p(1)).
test(both_branches_are_answers) :-
    assertion(p(1)),
    assertion(q(1)).
test(model_records_the_negated_literal) :-
    once(p(1)),
    scasp_model(Model),
    assertion(memberchk(not q(1), Model)).

:- end_tests(embed_negation).

		 /*******************************
		 *        GLOBAL CONSTRAINT	*
		 *******************************/

% Inside a begin_scasp block `:- Body.` is an s(CASP) global constraint,
% not a Prolog directive.  This is the difference from test_dyncall.pl,
% where the same thing must be written `false :- Body.`.

:- begin_scasp(choice, [chosen/1, rejected/1]).

colour(red).
colour(blue).
chosen(X) :- colour(X), not rejected(X).
rejected(X) :- colour(X), not chosen(X).

:- chosen(red).

:- end_scasp.

:- begin_tests(embed_constraints).

test(constraint_leaves_only_blue, all(X == [blue])) :-
    chosen(X).
% Red must be rejected, and blue may be: the constraint forbids choosing
% red, but nothing forces blue to be chosen, so there is also a model in
% which neither is.

test(both_can_be_rejected, set(X == [blue, red])) :-
    rejected(X).
test(red_is_rejected) :-
    rejected(red).

:- end_tests(embed_constraints).

		 /*******************************
		 *         EMBEDDED QUERY	*
		 *******************************/

% A trailing `?- Query.` is accepted inside the block, as in a source file
% for the scasp executable.  It is recorded, not run at load time.

:- begin_scasp(withquery, [r/1]).

r(1).
r(2).

?- r(X).

:- end_scasp.

:- begin_tests(embed_query).

test(embedded_query_does_not_prevent_calling, set(X == [1,2])) :-
    r(X).

:- end_tests(embed_query).

		 /*******************************
		 *          DIRECTIVES		*
		 *******************************/

:- begin_scasp(directives, [holds/0, shown/1]).

#abducible maybe_it.
holds :- maybe_it.

#show shown/1.
shown(1).

:- end_scasp.

:- begin_tests(embed_directives).

test(abducible_can_be_assumed) :-
    assertion(holds).
test(abducible_is_in_the_model) :-
    once(holds),
    scasp_model(Model),
    assertion(memberchk(maybe_it, Model)).
test(show_directive_does_not_break_solving) :-
    assertion(shown(1)).

:- end_tests(embed_directives).

		 /*******************************
		 *           EXPORTS		*
		 *******************************/

:- begin_scasp(exports, [visible/1]).

visible(1).
hidden(2).

:- end_scasp.

:- begin_tests(embed_exports).

test(exported_predicate_is_callable) :-
    assertion(visible(1)).
test(unexported_predicate_has_no_wrapper) :-
    assertion(\+ catch(hidden(2), error(existence_error(_,_),_), fail)).
test(listing_shows_the_compiled_program) :-
    with_output_to(string(S), scasp_listing(exports, [])),
    assertion(sub_string(S, _, _, _, "visible")).

:- end_tests(embed_exports).
