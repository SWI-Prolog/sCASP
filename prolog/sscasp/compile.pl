:- module(sscasp_compile,
          [ compile_program/3           % +Module, +Terms, -Program
          ]).
:- use_module(ops).
:- use_module(annotations, [program_annotations/2,
                                            annotation_clauses/2]).
:- use_module(dualise,     [predicate_duals/6, translate_goal/3]).
:- use_module(domains,     [annotation/2]).
:- use_module(mangle,      [demangle/2]).
:- use_module(graph,       [olon_clauses/3]).
:- use_module(nmr,         [nmr_check/4]).
:- use_module(library(lists), [append/3, member/2]).
:- use_module(library(apply), [maplist/3, maplist/4, foldl/4]).

/** <module> Compiling a program

Takes a program as a list of terms and returns everything the solver will
need, as a value:

    program{ clauses:   [...],   % the whole compiled program
             nmr:       nmr{...}, % what in it is the NMR check
             unbounded: [...] }   % duals still carrying an unbounded forall

`clauses` is everything, the NMR check included.  `nmr` *describes* part of
it — its goal, which of the clauses are its, and what each sub-check is for —
rather than holding clauses the program does not.

Each clause is translated, head and body, into the mangled names, and each
predicate gets its duals.  The **domain** declarations bring clauses of their
own — `:- domain day = [mon, tue]` gives two facts — and go through the same
path as the user's: a domain is a predicate, so `not day(X)` has to be
constructive like any other negation.

## The report

`unbounded` lists the predicates whose duals quantify a variable that no
declared domain bounds.  It costs nothing now that `for_domain/3` exists: a
quantifier with a domain *is* a `for_domain/3`, so a surviving `forall/2` is
one with none, and the report is a scan.

    p(X) :- q(X), not t(X, Y).          unbounded: [p/1]

    :- pred t(_, thing).                unbounded: []

A sub-check is reported as `check(Index, Source)`, since it has no name of
its own to give — `Source` is the predicate whose rule is on a loop, or the
atom `constraint`.

**This is not decision 5's undecidability analysis, and does not claim to
be.** An unbounded quantifier is often decided at run time anyway: the
solver's `forall` checks whether the clause duals between them cover the
universe, and for a variable constrained only by disequalities they do.  What
the report says is narrower and still worth having — here are the places
where declaring a domain would replace a search over the universe with an
enumeration, and where `doc/bugs.md` #15 can bite if the complement is a
shape no store can represent.

## Closing the program

A predicate called but never defined is false of everything, so its negation
is true of everything.  Rather than leave the call undefined and depend on
what that does, both are written out:

    m$plain$q$1(_) :- fail.
    m$dual$q$1(_).

## The NMR check

A global constraint and a rule on an odd loop over negation both say that
something must not hold, and neither is tested by proving the query.  So both
become a denial and the NMR check is the conjunction of their negations;
`graph.pl` finds the loops and `nmr.pl` builds the check.  The solver runs it
once per answer, by calling `Program.nmr.goal`.
*/

%!  compile_program(+Module, +Terms, -Program) is det.

compile_program(Module, Terms, Program) :-
    program_annotations(Terms, Annotations),
    annotation_clauses(Annotations, DomainClauses),
    program_clauses(Terms, UserClauses),
    append(DomainClauses, UserClauses, Clauses),
    constraints(Terms, Constraints),
    predicate_groups(Clauses, Groups),
    maplist(compile_predicate(Module, Annotations), Groups, Compiled),
    append(Compiled, Translated),
    olon_clauses(Clauses, Olon, _Ordinary),
    check_sources(Groups, Olon, Constraints, Sources),
    nmr_check(Module, Annotations, Sources, Nmr),
    append(Translated, Nmr.clauses, Defined),
    close_program(Defined, Closure),
    append(Defined, Closure, All),
    unbounded(All, Nmr.checks, Unbounded),
    Program = program{ clauses:   All,
                       nmr:       Nmr,
                       unbounded: Unbounded }.

%!  check_sources(+Groups, +Olon, +Constraints, -Sources) is det.
%
%   Everything the NMR check has to check, in the order it will call
%   them: the rules on an odd loop, the program's global constraints, and
%   the consistency of each classically negated predicate.
%
%   That last one is a constraint the *language* imposes rather than the
%   program: `p` and `-p` are different atoms, and nothing but this stops
%   a program deriving both.  It is generated only where both polarities
%   have clauses — if either has none it is false of everything, and the
%   conjunction could not hold anyway.

check_sources(Groups, Olon, Constraints, Sources) :-
    maplist(olon_source, Olon, OlonSources),
    maplist(constraint_source, Constraints, ConstraintSources),
    findall(Source, consistency_source(Groups, Source), Consistency),
    append([OlonSources, ConstraintSources, Consistency], Sources).

olon_source(Clause, olon(Clause)).
constraint_source(Body, constraint(Body)).

consistency_source(Groups, consistency(Name/Arity, Denial)) :-
    member(key(neg, Name/Arity)-_, Groups),
    memberchk(key(plain, Name/Arity)-_, Groups),
    functor(Positive, Name, Arity),
    Denial = (Positive, -(Positive)).

%!  compile_predicate(+Module, +Annotations, +Group, -Clauses) is det.
%
%   The translated clauses of one predicate, then its duals.

compile_predicate(Module, Annotations, key(Polarity, PI)-Clauses, Compiled) :-
    maplist(translate_clause(Module), Clauses, Translated),
    maplist(strip_polarity, Clauses, Bare),
    predicate_duals(Module, Annotations, Polarity, PI, Bare, Duals),
    append(Translated, Duals, Compiled).

%!  strip_polarity(+Clause, -Bare) is det.
%
%   `-p(X) :- Body' has `-'/1 for its functor, which is not the predicate
%   being defined.  The dual is built from `p(X) :- Body'; that it proves
%   the negation of *classical* `-p' rather than of `p' is carried by the
%   `dualneg' kind, not by the term.

strip_polarity((-(Head) :- Body), (Head :- Body)) :- !.
strip_polarity(-(Head), Head) :- !.
strip_polarity(Clause, Clause).

%!  translate_clause(+Module, +Clause, -Translated) is det.
%
%   Head and body both go through `translate_goal/3`, so a clause of `p`
%   becomes a clause of `m$plain$p$1` calling `m$plain$q$1`.  The head is
%   *not* abstracted: only the dual needs head unification made explicit,
%   and the positive clause is better left as the author wrote it.

translate_clause(Module, (Head :- Body), (NewHead :- NewBody)) :-
    !,
    translate_goal(Module, Head, NewHead),
    translate_body(Module, Body, NewBody).
translate_clause(Module, Fact, NewFact) :-
    translate_goal(Module, Fact, NewFact).

translate_body(Module, Body, NewBody) :-
    (   Body = (A, B)
    ->  translate_body(Module, A, NA),
        translate_body(Module, B, NB),
        NewBody = (NA, NB)
    ;   translate_goal(Module, Body, NewBody)
    ).

		 /*******************************
		 *        READING A PROGRAM	*
		 *******************************/

%!  program_clauses(+Terms, -Clauses) is det.
%
%   The clauses, leaving out the directives — annotations and global
%   constraints alike.

program_clauses(Terms, Clauses) :-
    findall(Term, (member(Term, Terms), \+ Term = (:- _)), Clauses).

%!  constraints(+Terms, -Constraints) is det.
%
%   The global constraints: a `:- Body` that is not an annotation.

constraints(Terms, Constraints) :-
    findall(Body,
            ( member((:- Body), Terms),
              \+ annotation(Body, _)
            ),
            Constraints).

%!  predicate_groups(+Clauses, -Groups) is det.
%
%   The clauses grouped by the predicate they define, in the order the
%   predicates first appear.  The key carries the polarity, so `p` and
%   `-p` are different predicates — which they are.

predicate_groups(Clauses, Groups) :-
    foldl(add_clause, Clauses, [], Groups).

add_clause(Clause, Groups0, Groups) :-
    clause_key(Clause, Key),
    (   select_group(Key, Groups0, Before, Clauses, After)
    ->  append(Clauses, [Clause], More),
        append(Before, [Key-More|After], Groups)
    ;   append(Groups0, [Key-[Clause]], Groups)
    ).

select_group(Key, Groups, Before, Clauses, After) :-
    append(Before, [Key-Clauses|After], Groups),
    !.

%!  clause_key(+Clause, -Key) is det.

clause_key((Head :- _), Key) :- !, head_key(Head, Key).
clause_key(Fact, Key) :- head_key(Fact, Key).

head_key(-(Head), key(neg, Name/Arity)) :-
    !,
    head_parts(Head, Name, Arity).
head_key(Head, key(plain, Name/Arity)) :-
    head_parts(Head, Name, Arity).

head_parts(Head, Name, Arity) :-
    (   atom(Head)
    ->  Name = Head, Arity = 0
    ;   functor(Head, Name, Arity)
    ).

		 /*******************************
		 *      CLOSING THE PROGRAM	*
		 *******************************/

%!  close_program(+Clauses, -Closure) is det.
%
%   Closure defines the predicates Clauses call but do not define.  An
%   undefined predicate is false of everything — `:- fail' — and its dual
%   is therefore true of everything: a fact.
%
%   This is what makes the result a *closed* program: every call in it has
%   a definition, so running it cannot raise an existence error and the
%   solver need not treat "no clauses" as a case of its own.

close_program(Clauses, Closure) :-
    findall(Name/Arity,
            ( member(Clause, Clauses),
              clause_head(Clause, Head),
              functor(Head, Name, Arity)
            ),
            Defined0),
    sort(Defined0, Defined),
    findall(Name/Arity,
            ( member(Clause, Clauses),
              clause_body(Clause, Body),
              body_goal(Body, Goal),
              callable(Goal),
              functor(Goal, Name, Arity),
              demangle(Name, pred(_, Kind, _, _)),
              closable(Kind),
              \+ memberchk(Name/Arity, Defined)
            ),
            Called0),
    sort(Called0, Called),
    maplist(closing_clause, Called, Closure).

%   Only the two outermost kinds can be reached without being generated:
%   the helpers (`dual.1', `dual.2.inner') are made by the same pass that
%   makes the calls to them.

closable(plain).
closable(neg).
closable(dual).
closable(dualneg).

closing_clause(Name/Arity, Clause) :-
    functor(Head, Name, Arity),
    demangle(Name, pred(_, Kind, _, _)),
    (   closable_dual(Kind)
    ->  Clause = Head                   % true of everything
    ;   Clause = (Head :- fail)         % false of everything
    ).

closable_dual(dual).
closable_dual(dualneg).

%!  body_goal(+Body, -Goal) is nondet.
%
%   Every goal in Body, looking through the control constructs — the
%   quantifiers included, since `for_domain/3' carries the generator that
%   names the domain predicate.

body_goal(Var, _) :-
    var(Var),
    !,
    fail.
body_goal((A, B), Goal) :- !, (body_goal(A, Goal) ; body_goal(B, Goal)).
body_goal((A ; B), Goal) :- !, (body_goal(A, Goal) ; body_goal(B, Goal)).
body_goal((A -> B), Goal) :- !, (body_goal(A, Goal) ; body_goal(B, Goal)).
body_goal(\+(A), Goal) :- !, body_goal(A, Goal).
body_goal(not(A), Goal) :- !, body_goal(A, Goal).
body_goal(forall(_, A), Goal) :- !, body_goal(A, Goal).
body_goal(for_domain(_, Gen, A), Goal) :-
    !,
    (body_goal(Gen, Goal) ; body_goal(A, Goal)).
body_goal(Goal, Goal).

		 /*******************************
		 *          THE REPORT		*
		 *******************************/

%!  unbounded(+Clauses, -Predicates) is det.
%
%   The predicates whose duals still carry a `forall/2`.  Since a
%   quantifier with a domain is a `for_domain/3`, a surviving `forall/2`
%   is one nothing bounds — so this is decision 5a's report, and it is a
%   scan rather than an analysis.

unbounded(Clauses, Checks, Predicates) :-
    findall(PI,
            ( member(Clause, Clauses),
              clause_head(Clause, Head),
              functor(Head, Mangled, _),
              unbounded_source(Mangled, Checks, PI),
              clause_body(Clause, Body),
              has_forall(Body)
            ),
            PIs),
    sort(PIs, Predicates).

%!  unbounded_source(+Mangled, +Checks, -Source) is semidet.
%
%   What to call the predicate a quantifier was found in.  For one
%   deriving from a user predicate that is the predicate; for a sub-check
%   there is no such name, so it is `check(Index, Source)` — which is
%   more use than the index alone, since two global constraints would
%   otherwise be told apart only by a number nothing else mentions.

unbounded_source(Mangled, _, Name/Arity) :-
    demangled_pred(Mangled, Name, Arity),
    !.
unbounded_source(Mangled, Checks, check(Index, Source)) :-
    demangle(Mangled, prog(_, Kind, _)),
    compound(Kind),
    arg(1, Kind, Index),
    integer(Index),
    memberchk(check(Index, Source, _), Checks).

clause_head((Head :- _), Head) :- !.
clause_head(Fact, Fact).

clause_body((_ :- Body), Body) :- !.
clause_body(_, true).

%   Structural, not sub_term/2: that unifies, so a pattern would match
%   any unbound variable in the clause and find a quantifier everywhere.

has_forall(Term) :-
    compound(Term),
    (   Term = forall(_, _)
    ->  true
    ;   arg(_, Term, Arg),
        has_forall(Arg)
    ).

demangled_pred(Mangled, Name, Arity) :-
    demangle(Mangled, pred(_, _, Name, Arity)).
