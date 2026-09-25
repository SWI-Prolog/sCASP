:- module(scasp_coverage_by_test,
          [ coverage_by_test/0,
            coverage_by_test/1                  % +Options
          ]).
:- use_module(library(apply), [maplist/2, convlist/3, include/3]).
:- use_module(library(lists), [memberchk/2, append/3]).
:- use_module(library(option), [option/3]).
:- use_module(library(ordsets), [ord_subtract/3, ord_union/3]).
:- use_module(library(ansi_term), [ansi_format/3]).
:- use_module(library(prolog_coverage), [coverage/2]).
:- use_module(library(plunit)).

:- use_module(test_scasp, [golden_program/3]).

/** <module> Which golden tests actually contribute coverage

Runs each golden program on its own under library(prolog_coverage), then
greedily computes the smallest set of programs that together cover the
same clauses of the module under analysis.  A program that adds nothing
is one that only repeats what another already exercises.

    ?- coverage_by_test.
    ?- coverage_by_test([module(scasp_solve)]).

This was `test_scasp.pl --cov-by-test` before that file became a plunit
suite.  For ordinary per-file or per-clause coverage there is nothing to
write: wrap the run instead.

    ?- coverage(run_tests(golden), [dir(cov)]).
*/

:- dynamic
    cov_module/1,
    covers/3,
    current_test/1.

cov_module(scasp_solve).

%!  coverage_by_test is det.
%!  coverage_by_test(+Options) is det.
%
%   Options:
%
%     - module(+Module)
%       Module whose clauses are analysed.  Default `scasp_solve`.

coverage_by_test :-
    coverage_by_test([]).

coverage_by_test(Options) :-
    option(module(Module), Options, scasp_solve),
    setup_call_cleanup(
        set_cov_module(Module, Old),
        coverage_by_test_(Options),
        set_cov_module(Old, _)).

set_cov_module(Module, Old) :-
    cov_module(Old),
    retractall(cov_module(_)),
    asserta(cov_module(Module)).

coverage_by_test_(_Options) :-
    retractall(covers(_,_,_)),
    forall(golden_program(Name, _File, TestOptions),
           run_one(Name, TestOptions)),
    minimal_set_of_files(CoveredClauses, CoverContributions),
    retractall(covers(_,_,_)),
    sep_line,
    format("Coverage contribution by test~n"),
    format("~w ~`.t ~w~66| ~t~w~72|~n", ['Test','Covers','New']),
    maplist(list_contribution, CoverContributions),
    sep_line,
    format("List of clauses~nClause ~`.t State~72|~n", []),
    covered_clauses(CoveredClauses),
    format("~nEnd of the report~n", []).

%!  run_one(+Name, +TestOptions) is det.
%
%   Run one golden test under coverage.  A test plunit would not run -
%   `blocked` for one that does not terminate - is skipped here too.

run_one(_Name, TestOptions) :-
    memberchk(blocked(_), TestOptions),
    !.
run_one(Name, _TestOptions) :-
    setup_call_cleanup(
        asserta(current_test(Name), Ref),
        ignore(coverage(run_tests(golden:Name, [silent(true)]), [])),
        erase(Ref)).

:- multifile
    prolog_coverage:report_hook/2.

prolog_coverage:report_hook(Succeeded, Failed) :-
    current_test(Test),
    cov_module(Module),
    module_property(Module, file(Target)),
    convlist(tag_clause(Module, Target, +), Succeeded, STagged),
    convlist(tag_clause(Module, Target, -), Failed,    FTagged),
    append(STagged, FTagged, Tagged),
    sort(Tagged, Which),                % Sort by line
    length(Which, N),
    assertz(covers(Test, N, Which)).

tag_clause(Module, File, Symbol, Clause, cov(Line, Symbol, PI)) :-
    clause_property(Clause, file(File)),
    clause_property(Clause, line_count(Line)),
    clause_property(Clause, predicate(Module:PI)).

sep_line :-
    format("~n~`=t~78|~n", []).

list_contribution(test(File,Covers,New)) :-
    contrib_style(New, Style),
    ansi_format(Style, "~w ~`.t ~d~66| ~t~d~72|~n", [File,Covers,New]).

contrib_style(0, fg(127,127,127)) :- !.
contrib_style(_, []).

covered_clauses(CoveredClauses) :-
    cov_module(Module),
    findall(CIF, clause_in_module(Module, CIF), CIFs),
    sort(1, =<, CIFs, OCIFs),
    covered_clauses(OCIFs, CoveredClauses).

clause_in_module(Module, cif(Line, PI)) :-
    module_property(Module, file(File)),
    prolog_coverage:clause_source(Clause, File, Line),
    clause_property(Clause, predicate(Module:PI)),
    \+ ( PI = (Name/_Arity),
         sub_atom(Name, 0, _, _, $)
       ).

covered_clauses([], _).
covered_clauses([cif(L, P)|RestC], Covered) :-
    (   memberchk(cov(L,+,P), Covered)
    ->  Message = 'COVERED'
    ;   memberchk(cov(L,-,P), Covered)
    ->  Message = 'NEG COVERED'
    ;   Message = 'NO'
    ),
    format("~t~d~4| ~q ~46t ~w~72|~n", [L, P, Message]),
    covered_clauses(RestC, Covered).

%!  minimal_set_of_files(-SetOfClauses, -Minimal) is det.
%
%   @arg SetOfClauses is a set of cov(Line,Symbol,PI)
%   @arg Minimal is a list of test(Test,CoveredCount,NewCoveredCount)

minimal_set_of_files(SetOfClauses, [test(F0,N0,N0)|CFiles]) :-
    findall(t(N,File,Which), covers(File, N, Which), Covering),
    sort(Covering, [t(N0, F0, S0)|RestF]),
    grow_minimal_set(RestF, CFiles, S0, SClausesCovered),
    sort(SClausesCovered, SetOfClauses).

grow_minimal_set([], [], S, S).
grow_minimal_set([t(N1,F1,S1)|RestF], [test(F1,N1,NewCount)|CFiles],
                 Clauses0, Clauses) :-
    ord_subtract(S1, Clauses0, New),
    length(New, NewCount),
    ord_union(S1, Clauses0, Clauses1),
    grow_minimal_set(RestF, CFiles, Clauses1, Clauses).
