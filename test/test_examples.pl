:- module(test_examples,
          [ test_examples/0
          ]).
:- use_module(under_test).
:- use_module(scasp_plunit, []).        % sets the library search path
:- use_module(library(lists), [member/2]).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).

/** <module> The examples in examples/, against either library

An example is documentation, so it is written the way a user writes a
program: `:- use_module(library(scasp)).`  `swipl -Dsscasp` makes that
the new library (`prolog/scasp.pl`), so each runs against either one
unedited, and this suite checks that it still answers what it did.

Only the examples written against what both libraries export are here.
The rest use the old library's own modules (`library(scasp/human)`,
`scasp/html`, `scasp/output`, `scasp/json`) or its obsolete
`begin_scasp/end_scasp` blocks; `examples/README.md` lists them.

An example that is not a module file is loaded into a module of its own,
`ex_<file>`, so that its predicates do not land in `user`.
*/

test_examples :-
    run_tests(examples).

%!  answers(+File, +Query, -Answers) is det.
%
%   The distinct answers to Query against the example File, a path
%   relative to `examples/`, as the query term instantiated: a sorted
%   set in which each variable is named, without its constraints.
%
%   Distinct because how many answers a solution set arrives in is the
%   implementation's choice: `rps.pl` answers `winner(_, _)` three times.
%   The constraints are left out because the two libraries say them
%   differently; what the tests pin is which answers there are.

answers(File, Query, Answers) :-
    example_module(File, Module),
    findall(Query, scasp(Module:Query, []), Answers0),
    answer_set(Answers0, Answers).

answer_set(Answers, Set) :-
    findall(Key,
            ( member(Answer, Answers),
              copy_term(Answer, Key, _Constraints),
              numbervars(Key, 0, _)
            ),
            Keys),
    sort(Keys, Set).

%!  expect(+Answers, -Set) is det.
%
%   The expected answers written as terms, in the form answers/3 gives.

expect(Answers, Set) :-
    answer_set(Answers, Set).

example_module(File, Module) :-
    source_file(example_module(_, _), Here),
    file_directory_name(Here, TestDir),
    file_directory_name(TestDir, Root),
    atomic_list_concat([Root, '/examples/', File], Path),
    file_base_name(File, Base),
    file_name_extension(Name, _, Base),
    atom_concat(ex_, Name, Module),
    load_files(Module:Path, [silent(true), if(not_loaded)]).

:- begin_tests(examples).

test(birds_which_fly) :-
    answers('dyncall/birds.pl', flies(_), Answers),
    expect([flies(tweety)], Expected),
    assertion(Answers == Expected).

%   The third is anything that is not a bird: `X ∉ [john, tweety]`, which
%   both libraries answer.  `sam` is a penguin, which the program does not
%   make a bird.

test(birds_which_do_not) :-
    answers('dyncall/birds.pl', -flies(_), Answers),
    expect([-flies(sam), -flies(john), -flies(_)], Expected),
    assertion(Answers == Expected).

test(findall_over_a_constraint) :-
    answers('dyncall/solve_findall.pl', validate([1,2,3]), Answers),
    expect([validate([1,2,3])], Expected),
    assertion(Answers == Expected).

%   An even loop: p and q exclude each other, and p(X) holds of anything.

test(pq_even_loop) :-
    answers('dyncall/pq.pl', p(_), Answers),
    expect([p(_)], Expected),
    assertion(Answers == Expected).

%   The players and throws are abducible, so the answer binds nothing.

test(rps_has_a_winner) :-
    answers('rps.pl', winner(_, _), Answers),
    expect([winner(_, _)], Expected),
    assertion(Answers == Expected).

:- end_tests(examples).
