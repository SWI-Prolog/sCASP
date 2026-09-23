:- module(sscasp_corpus,
          [ corpus_program/2,           % ?Name, ?File
            corpus_models/2,            % +File, -Models
            golden_models/2,            % +File, -Models
            model_key/2,                % +Model, -Key
            agrees/1,                   % +File
            answers_agree/1             % +File
          ]).
:- use_module(library(sscasp/ops)).
:- use_module(library(sscasp/run), [sscasp/3]).
:- use_module(library(sscasp/source), [read_program/2]).
:- use_module('../prolog/scasp/ops', []).       % operators of the golden data
:- use_module(library(lists), [member/2, memberchk/2, append/3]).

/** <module> The new implementation against the golden corpus

`test/all_programs/` is the corpus the old implementation is pinned to, and
its `.pass` files record what that implementation answers.  Pointing the new
one at the same programs is the sharpest comparison available here: real
programs, written by other people, exercising features no random generator
reaches.

It is a *different* kind of evidence from `stable_models.pl`.  The oracle
shares no design decision with either implementation and so can say who is
right; the corpus shares all of them with the old one and can only say
whether the two agree.  Both are worth having, and this one is what says
whether the reimplementation is finished.

## What is compared

The **models**, as sets of literals, with both sides reduced to skeletons:
the golden data inlines a constraint store as `'| '(X, {...})` and names
variables `'$VAR'(N)`, while `sscasp/3` leaves real variables carrying
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

%!  corpus_program(?Name, ?File) is nondet.
%
%   Name is a corpus program with golden data, File its source.

corpus_program(Name, File) :-
    source_file(corpus_program(_, _), Here),
    file_directory_name(Here, Dir),
    atom_concat(Dir, '/all_programs/*.pl', Pattern),
    expand_file_name(Pattern, Files),
    member(File, Files),
    has_golden(File),
    file_base_name(File, Base),
    file_name_extension(Name, _, Base).

has_golden(File) :-
    golden_file(File, Pass),
    exists_file(Pass).

golden_file(File, Pass) :-
    file_name_extension(Base, _, File),
    file_name_extension(Base, pass, Pass).

%!  golden_models(+File, -Models) is semidet.
%
%   The models the old implementation answers with.  A `.pass` file is a
%   single `Stacks-Models` term, read with the operators that wrote it.

golden_models(File, Models) :-
    golden_file(File, Pass),
    setup_call_cleanup(
        open(Pass, read, In),
        read_term(In, _Stacks-Models, [module(scasp_ops)]),
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
    findall(Model, sscasp(Terms, Query, Model), Models).

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
