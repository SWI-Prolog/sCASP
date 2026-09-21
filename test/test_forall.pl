:- module(test_forall,
          [ test_forall/0
          ]).
:- use_module('../prolog/scasp').
:- use_module(scasp_plunit).
:- use_module(library(plunit)).
:- use_module(library(lists), [member/2]).

/** <module> The forall construct and the four algorithms for it

comp_duals.pl turns a variable that a rule body introduces into a
`forall` in the dual, because `not p(X)` holds only if *every* value of
that variable fails the body.  solve.pl has four implementations,
selected by the `scasp_forall` flag: `all` (the default), `all_c`,
`prev` and `sasp`, the last two inherited from the Ciao original.

The programs below are chosen so that the right answer can be worked out
by hand, which is what makes them useful for judging the algorithms
against each other rather than against one another's output.
*/

test_forall :-
    run_tests([ forall_over_facts,
                forall_over_constraint,
                forall_algorithms_agree,
                forall_arity
              ]).

		 /*******************************
		 *      A BODY VARIABLE		*
		 *******************************/

%   likes(X) :- food(F), eats(X, F).
%   food(cake).  food(rice).
%   eats(ann, cake).
%
%   `not likes(X)` must quantify over F: it holds of X when X eats *no*
%   food.  ann eats cake, so likes(ann); bob eats nothing, so not
%   likes(bob).

:- begin_tests(forall_over_facts).

food(cake).
food(rice).
eats(ann, cake).
likes(X) :- food(F), eats(X, F).

test(the_eater_likes_something, [nondet]) :-
    scasp(likes(ann)).
test(only_the_eater, all(X == [ann])) :-
    scasp(likes(X)).
test(a_non_eater_likes_nothing, [nondet]) :-
    scasp(not likes(bob)).
test(the_eater_is_not_a_non_eater, fail) :-
    scasp(not likes(ann)).

:- end_tests(forall_over_facts).

		 /*******************************
		 *  A CONSTRAINED BODY VARIABLE	*
		 *******************************/

%   s :- t(Y), Y #> 3.
%   t(Y) :- Y #> 0.
%
%   The forall variable carries a CLP(Q) constraint rather than ranging
%   over facts.  s holds - take Y = 4, which satisfies both Y #> 0 and
%   Y #> 3 - so `not s` must fail.

:- begin_tests(forall_over_constraint).

t(Y) :- Y #> 0.
s :- t(Y), Y #> 3.

test(the_constraints_are_satisfiable) :-
    scasp(s).
test(so_the_negation_fails, fail) :-
    scasp(not s).

% u is the other way round: no Y satisfies both, so u fails and `not u`
% holds.

u :- v(Y), Y #> 3.
v(Y) :- Y #< 1.

test(unsatisfiable_conjunction_fails, fail) :-
    scasp(u).
test(so_its_negation_holds, [nondet]) :-
    scasp(not u).

:- end_tests(forall_over_constraint).

		 /*******************************
		 *   THE FOUR ALGORITHMS		*
		 *******************************/

%   The same program under each algorithm.  `all` is the default;
%   `prev` is what --dcc forces (options.pl adds forall(prev) when dcc
%   is given), so its answers matter beyond the flag itself.

:- begin_tests(forall_algorithms_agree).

w :- x(Y), Y #> 3.
x(Y) :- Y #> 0.

count(Alg, Goal, N) :-
    with_forall(Alg, ( findall(x, scasp(Goal, []), L), length(L, N) )).

test(every_algorithm_proves_the_positive, forall(member(Alg, [all, all_c, prev, sasp]))) :-
    count(Alg, w, N),
    assertion(N >= 1).

% w holds, so `not w` must have no answer under any algorithm.  `prev`
% and `sasp` give two, which means they prove w and not w at once.

% w holds - Y = 4 satisfies both x(Y) and Y #> 3 - so `not w` must have
% no answer, under every algorithm.
%
% `prev` and `sasp` used to give two, proving w and not w at once, which
% mattered beyond the flag because --dcc forces prev.  Two bugs, the
% first hiding the second: dump_clpq_var/3 dropped the constraint store,
% so the clpq case of check_unbound/2 never ran; and the code behind it
% left the Proved assoc unbound on two of its three branches.

test(every_algorithm_refutes_the_negation,
     [ forall(member(Alg, [all, all_c, prev, sasp])) ]) :-
    count(Alg, not w, N),
    assertion(N == 0).

:- end_tests(forall_algorithms_agree).

		 /*******************************
		 *        ARITY CLASH		*
		 *******************************/

%   q/1 and q/2 with a body variable, which is the shape that made the
%   generated forall pick up the wrong arity once - the regression
%   test/all_programs/forall_arity.pl records it.
%
%   q(ha) needs q(a,X) with X > 1; q(a,2) gives X = 2, so q(ha) holds
%   and p(ha) holds.  p(a) needs q(a,_), which q(a,2) satisfies, so p(a)
%   holds too and `not p(a)` fails.

:- begin_tests(forall_arity).

p(X) :- q(X, _).
q(ha) :- q(a, X), X #> 1.
q(a, 2).

test(the_fact_satisfies_the_guard) :-
    scasp(q(ha)).
test(p_holds_of_the_fact_argument) :-
    scasp(p(a)).
test(negation_of_p_fails, fail) :-
    scasp(not p(a)).

:- end_tests(forall_arity).
