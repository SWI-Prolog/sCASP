:- module(sscasp_corpus,
          [ corpus/3,                   % ?Corpus, ?Directory, ?What
            corpus_program/2,           % ?Name, ?File
            comparable/2,               % ?Name, ?File
            disposition/2,              % ?Name, ?Disposition
            strict/1,                   % ?Name

            corpus_models/2,            % +File, -Models
            golden_models/2,            % +File, -Models
            model_key/2,                % +Model, -Key
            agrees/1,                   % +File
            answers_agree/1             % +File
          ]).
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/run), [scasp/3]).
:- use_module(library(sscasp/source), [read_program/2]).
:- use_module(library(lists), [member/2, memberchk/2, append/3]).

/** <module> The new implementation against the golden corpus

The `.pass` files record what the **old** implementation answers, and
pointing the new one at the same programs is the sharpest comparison
available here: real programs, written by other people, exercising features
no random generator reaches.

This module is the whole description of that comparison — which programs,
what each is called, which do not agree and why — and `test_corpus.pl` is
the plunit units that run it.  Keeping the two apart is worth a file: the
list of what does not agree is the **to-do list for finishing the
reimplementation**, and a to-do list buried among test bodies is one nobody
reads.

It is a *different* kind of evidence from `stable_models.pl`.  The oracle
shares no design decision with either implementation and so can say who is
right; the corpus shares all of them with the old one and can only say
whether the two agree.  Both are worth having, and this one is what says
whether the reimplementation is finished.

## What is compared

The **models**, as sets of literals, with both sides reduced to skeletons:
the golden data inlines a constraint store as `'| '(X, {...})` and names
variables `'$VAR'(N)`, while `scasp/3` leaves real variables carrying
attributes.  Reducing both to plain positionally-numbered variables compares
what the answer *is about* while leaving what it is *constrained to* out —
which the differential against the oracle covers instead.

## Two strengths of agreement

Models legitimately differ where the proofs do: the model is a function of
the proof (`doc/design-next.md` open question 5), so a solver reaching an
answer by another route carries a different model with it.  `hanoi_scasp_7`
is the plainest case — the golden model is a strict *subset* of ours, missing
the `not negmove(...)` that proving each `move(...)` needs — and `family`
picks the other of two symmetric facts.  Neither is a disagreement about
what the program means.

So there are two questions and `answers_agree/1` asks the weaker one: do the
two implementations bind the **query** the same way?  That is what was asked
for, and it is what a difference in it means something about.  `agrees/1`
additionally requires the models to match, which is worth keeping as the
stricter check but is not the definition of correct.
*/

%!  corpus(?Corpus, ?Directory, ?What) is nondet.
%
%   The corpora, by the directory each lives in.  The names are the
%   directories' own rather than invented ones, so that a program's name
%   here says where to find it.

corpus(all_programs, 'all_programs',
       "the regression corpus: real programs, one feature crossing another").
corpus(sasp,         'all_programs/sasp',
       "programs from the original s(ASP) distribution").
corpus(min_programs, 'min_programs',
       "one feature each, cut down to the smallest program that shows it").
corpus(dcc,          'all_programs/dcc',
       "needs --dcc; its golden data is only valid under that flag").

%   Not a corpus: `test/le_programs/` are **embedded** examples — Prolog
%   modules queried through `scasp/2` — with no `.pass` data, driven by
%   `test_le.pl` against the old tree.  Running them against the new one
%   means porting them to `scasp/2`, which is work of its own.

%!  corpus_program(?Name, ?File) is nondet.
%
%   Name is `Corpus/Base` and File the source.  Qualified because the
%   corpora overlap: `hanoi_7` is in two of them and `bec_loan` in two
%   others, and a bare name could not say which was meant.

corpus_program(Corpus/Name, File) :-
    corpus(Corpus, Directory, _),
    corpus_dir(Directory, Dir),
    atom_concat(Dir, '/*.pl', Pattern),
    expand_file_name(Pattern, Files),
    member(File, Files),
    has_golden(File),
    file_base_name(File, Base),
    file_name_extension(Name, _, Base).

corpus_dir(Directory, Dir) :-
    source_file(corpus_program(_, _), Here),
    file_directory_name(Here, Sscasp),
    file_directory_name(Sscasp, Test),
    atomic_list_concat([Test, /, Directory], Dir).

%!  comparable(?Name, ?File) is nondet.
%
%   The programs that must agree.  Everything with a disposition is left
%   out, and its own test asserts it still does not agree.

comparable(Name, File) :-
    corpus_program(Name, File),
    \+ disposition(Name, _).

		 /*******************************
		 *      WHAT DOES NOT AGREE	*
		 *******************************/

%!  disposition(?Name, ?Disposition) is nondet.
%
%   Programs where the two implementations differ, and why.  **Two
%   kinds, and they mean opposite things:**
%
%   | `gap(Reason)`    | the reimplementation does not match, and should |
%   | `golden(Reason)` | the old implementation is wrong and the `.pass` records it |
%
%   Only the first is a to-do list.  Both assert that the program still
%   *fails* to agree, so neither can outlive the problem it records —
%   the discipline `known_issues.pl` uses, for the same reason: a list of
%   what is missing is only useful while something checks it still is.
%
%   Reasons for a `gap`:
%
%   | `feature`    | a language feature that is not implemented |
%   | `slow`       | not answered within the time a test may take |
%   | `incomplete` | finishes, and misses answers it should have found |

%   One of the event calculus family is left.  `bec_loan`'s query
%   `?- holdsAt(F, T).` answers in 0.027 s with the NMR check off, where
%   it used to spend 6,677 ancestor frames walking backwards in time;
%   with the check on it answers in **376 s** against 0.85 s for the old
%   implementation, so what is left is a factor of 440 and not a hang.
%   `CLAUDE.md` has the measurements.  It is in two corpora.

disposition(all_programs/bec_loan, gap(slow)).
disposition(min_programs/bec_loan, gap(slow)).

%   `all_programs/hanoi_scasp_7` and `dcc/hamilton_c` used to be here.
%   Both say `#show`, and the golden model is the shown part only, which
%   there was no reading an answer out of until `#show` was implemented.
%   The first now agrees on the model as well (see strict/1).  The second
%   agrees on the answer and not on the model: its golden data was
%   recorded under `--dcc`, which this implementation does not have.

%   And two where the reimplementation is **right**.  Both were found by
%   widening the corpus to `min_programs`, which is what widening it was
%   for.
%
%   `solve_not_is` asks `?- invalid_value(X)` of
%
%       invalid_value(X) :- not valid_value(X).
%       valid_value(V)   :- coord(X), coord(Y), V is X+Y.
%       coord(1). coord(2). coord(3).
%
%   The old implementation answers `invalid_value(2)` as well as the
%   constrained answer.  `valid_value(2)` holds — 1+1 — so it should not,
%   and its own justification shows why it does: under `not
%   valid_value(2)` it tries `2 is 2+1`, `2 is 2+2` and `2 is 2+3` and
%   never `2 is 1+1`.  One clause of the dual is missing.

disposition(min_programs/solve_not_is,
            golden("the old implementation's dual of `V is X+Y` misses a case")).

%   `stream_reasoning` answers `valid_stream(1, p(a))`, which is
%   cancelled: `higher_prio(2, 1)`, `stream(2, q(a))` and
%   `incompt(p(a), q(a))` all hold.  Our constrained answer excludes it —
%   `p(A)` with `A ∉ [a, b]` — and is the same answer with the cancelled
%   individuals taken out.

disposition(min_programs/stream_reasoning,
            golden("the old implementation misses a cancellation")).

		 /*******************************
		 *         THE STRICT ONES	*
		 *******************************/

%!  strict(?Name) is nondet.
%
%   Programs whose *models* match as well, which is more than agreement
%   on the answer requires.  Not every program can: a model is a function
%   of the proof, so a solver reaching the same answer another way
%   carries a different model with it.
%
%   The list is a ratchet.  Nothing here may stop matching; a program
%   that starts matching belongs on it.

strict(all_programs/classic_negation_inconstistent).
strict(all_programs/forall_arity).
strict(all_programs/hanoi).
strict(all_programs/hanoi_7).
strict(all_programs/hanoi_scasp_7).
strict(all_programs/haplotype).
strict(all_programs/min_model).
strict(all_programs/olon_unreachable).
strict(all_programs/pq).
strict(all_programs/rat).
strict(all_programs/solve_findall).
strict(all_programs/vars).

strict(sasp/hanoi_7).
strict(sasp/'left-rec-succ_BAD').
strict(sasp/loop_pos).
strict(sasp/loop_succ_pos).
strict(sasp/natnum).
strict(sasp/queens_old).

strict(min_programs/solve_findall_superset).
strict(min_programs/solve_not_true).

has_golden(File) :-
    golden_file(File, Pass),
    exists_file(Pass).

golden_file(File, Pass) :-
    file_name_extension(Base, _, File),
    file_name_extension(Base, pass, Pass).

%!  golden_models(+File, -Models) is semidet.
%
%   The models the old implementation answers with.  A `.pass` file is a
%   single `Stacks-Models` term, written by the old tree and read here in
%   `sscasp_ops`, which declares the same operators: loading the old tree
%   to read its data would put both in the process the new one is tested in.

golden_models(File, Models) :-
    golden_file(File, Pass),
    setup_call_cleanup(
        open(Pass, read, In),
        read_term(In, _Stacks-Models, [module(sscasp_ops)]),
        close(In)).

%!  corpus_models(+File, -Models) is det.
%
%   Every model the new implementation answers with.  A program with no
%   query has none, which is not the same as a query with no answer — but
%   every corpus program has one.

corpus_models(File, Models) :-
    read_program(File, Source),
    Source.query \== none,
    Terms = Source.terms,
    Query = Source.query,
    findall(Model, scasp(Terms, Query, Model), Models).

%!  answers_agree(+File) is semidet.
%
%   The two implementations bind the query the same way.  The golden
%   data records models rather than bindings, so the answers are read
%   back out of them: the query literal is in the model of any answer
%   that proved it.

answers_agree(File) :-
    read_program(File, Source),
    Query = Source.query,
    Query \== none,
    golden_models(File, Golden),
    model_answers(Golden, Query, Theirs),
    corpus_models(File, Ours),
    model_answers(Ours, Query, Ours1),
    Theirs == Ours1.

model_answers(Models, Query, Answers) :-
    findall(Key,
            ( member(Model, Models),
              member(Literal, Model),
              \+ \+ Literal = Query,
              copy_term(Literal, Copy),
              model_key([Copy], Key)
            ),
            Keys),
    sort(Keys, Answers).

%!  agrees(+File) is semidet.

agrees(File) :-
    golden_models(File, Golden),
    corpus_models(File, Ours),
    model_keys(Golden, Keys),
    model_keys(Ours, Keys).

model_keys(Models, Keys) :-
    findall(Key, (member(M, Models), model_key(M, Key)), Keys0),
    msort(Keys0, Keys).

%!  model_key(+Model, -Key) is det.
%
%   A model as a sorted list of literal skeletons.

model_key(Model, Key) :-
    copy_term(Model, Copy, _),
    skeleton(Copy, Plain, [], _),
    numbervars(Plain, 0, _),
    msort(Plain, Key).

skeleton(Var, Var, S, S) :-
    var(Var),
    !.
skeleton('| '(X, _), Plain, S0, S) :-
    !,
    skeleton(X, Plain, S0, S).
skeleton('$VAR'(N), Var, S0, S) :-
    !,
    (   memberchk(N-Var, S0)
    ->  S = S0
    ;   S = [N-Var|S0]
    ).
skeleton(Term, Plain, S0, S) :-
    compound(Term),
    !,
    Term =.. [Name|Args],
    skeletons(Args, Plains, S0, S),
    Plain =.. [Name|Plains].
skeleton(Term, Term, S, S).

skeletons([], [], S, S).
skeletons([A|As], [P|Ps], S0, S) :-
    skeleton(A, P, S0, S1),
    skeletons(As, Ps, S1, S).
