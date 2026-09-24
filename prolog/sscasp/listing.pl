:- module(sscasp_listing,
          [ program_listing/2,          % +Program, -Listing
            portray_listing/3,          % +Terms, ?Query, +Options
            listing_section/3,          % ?Section, ?Default, ?Title
            titled/2                    % +Title, +Options
          ]).
:- use_module(ops).
:- use_module(mangle,  [demangle/2]).
:- use_module(compile, [compile_program/3]).
:- use_module(render,  [portray_rule/2, portray_query/2]).
:- use_module(library(option), [option/2, option/3]).
:- use_module(library(ansi_term), [ansi_format/3]).
:- use_module(library(lists), [member/2, append/3, memberchk/2]).
:- use_module(library(apply), [maplist/3, foldl/4]).

/** <module> The compiled program, in the language it was written in

`compile.pl` produces a program over mangled functors — `scasp$dual.1$r$1`
is the dual of the first clause of `r/1` — because the solver works on
functor names.  This reads it back:

    not r(A) :-
        forall([B],
               (   not p(A)
               ;   p(A), not q(B)
               ;   p(A), q(B), s(A, B)
               )).

which is what `r(X) :- p(X), q(Y), not s(X, Y).` means when it fails, and
the one thing a reader of a dual actually wants to know.

## The helpers are inlined as a disjunction

`dualise.pl` writes a dual as a small hierarchy: `not p` is the
**conjunction** of one helper per clause of `p`, and that helper has one
**clause** per way its body can fail.  Printed as it stands that is five
predicates with generated names for a two-clause `p`, and the reader has to
put them back together.

So a helper call becomes the *disjunction* of its clauses' bodies.
Disjunction and not separate clauses, which is the whole trick: a
conjunction of two three-clause helpers is one body with two `;` groups,
where splitting it into clauses would be nine of them.

A helper is inlined only where it is called **once**, which is how they are
generated — one per clause, one per check — and it is checked rather than
assumed.  What is left keeps a name: `dualchk(1)` prints as `check(1)`,
titled from `nmr.checks` by whatever it came from.

## What it is not

A listing, not a program.  `check(1)` is not a predicate anybody can call
and a body may hold a `forall/2` that only the solver understands, so this
reads back as a term and not as something to run.  The old implementation's
`--code` *is* loadable (`--compiled`), and gets there by printing the
generated names as they are.
*/

%!  program_listing(+Program, -Listing) is det.
%
%   Listing is `listing{user, duals, constraints}`, each a list of
%   **predicates**, each a list of clauses, in the source language.

program_listing(Program, Listing) :-
    Clauses = Program.clauses,
    Checks = Program.nmr.checks,
    helpers(Clauses, Helpers),
    exclude_helpers(Clauses, Helpers, Kept),
    maplist(inlined(Helpers), Kept, Inlined),
    foldl(sorted_into, Inlined, l([], [], []), l(User0, Duals0, Checks0)),
    maplist(predicates, [User0, Duals0, Checks0], [User, Duals, Constraints]),
    titled(Constraints, Checks, Titled),
    Listing = listing{ user:        User,
                       duals:       Duals,
                       constraints: Titled }.

%!  listing_section(?Section, ?Default, ?Title) is nondet.
%
%   The sections, whether each is shown unless asked otherwise, and what
%   to call it.  The duals and the check are off by default for the
%   reason the old implementation has: they are what the *compiler* made
%   of the program, and most of the time the question is about the
%   program.

listing_section(query,       true,  'Query').
listing_section(user,        true,  'User predicates').
listing_section(duals,       false, 'Dual rules').
listing_section(constraints, false, 'Integrity constraints').

%!  portray_listing(+Terms, ?Query, +Options) is det.
%
%   Write the program by section.  Terms is the program as read or as
%   collected; Query is what to show under `% Query`, or unbound for a
%   program with none.  Options say which sections are wanted, by the
%   names and defaults `listing_section/3` gives, plus
%
%     - colour(Boolean)
%       Default `true`.
%
%   Nothing is compiled unless a section that needs it was asked for: a
%   caller showing only the query or only the source terms should not
%   pay for the duals.

portray_listing(Terms, Query, Options) :-
    (   wanted_section(Options, _)
    ->  compile_program(scasp, Terms, Program),
        program_listing(Program, Listing),
        forall(( listing_section(Section, _, Title),
                 wanted(Section, Options)
               ),
               show_section(Section, Title, Query, Listing, Options))
    ;   true
    ).

wanted_section(Options, Section) :-
    listing_section(Section, _, _),
    wanted(Section, Options),
    !.

wanted(Section, Options) :-
    listing_section(Section, Default, _),
    Asked =.. [Section, Wanted],
    option(Asked, Options, Default),
    Wanted == true.

show_section(query, Title, Query, _, Options) :-
    !,
    (   nonvar(Query)
    ->  titled(Title, Options),
        portray_query(Query, Options)
    ;   true
    ).
show_section(Section, Title, _, Listing, Options) :-
    Predicates = Listing.get(Section),
    Predicates \== [],
    !,
    titled(Title, Options),
    forall(member(Predicate, Predicates),
           show_predicate(Predicate, Options)).
show_section(_, _, _, _, _).

show_predicate(title(Comment)-Clauses, Options) :-
    !,
    (   Comment == ''
    ->  true
    ;   commented(Comment, Options)
    ),
    show_clauses(Clauses, Options).
show_predicate(Clauses, Options) :-
    show_clauses(Clauses, Options).

show_clauses(Clauses, Options) :-
    forall(member(Clause, Clauses), portray_rule(Clause, Options)),
    nl.

%!  titled(+Title, +Options) is det.
%
%   A section heading, as a comment so that what is written stays
%   loadable-looking even where it is not.

titled(Title, Options) :-
    nl,
    commented(Title, Options).

commented(Text, Options) :-
    (   option(colour(false), Options)
    ->  format("% ~w~n", [Text])
    ;   ansi_format(comment, "% ~w~n", [Text])
    ).

		 /*******************************
		 *         THE HELPERS		*
		 *******************************/

%!  helpers(+Clauses, -Helpers) is det.
%
%   The generated intermediates, as `Name-Clauses`.  A predicate counts
%   only if every call to it is the one call it was generated for —
%   `dualise.pl` writes one helper per clause and `nmr.pl` one per
%   sub-check, so that holds, and checking it costs one walk.

helpers(Clauses, Helpers) :-
    findall(Name-Body,
            ( member(Clause, Clauses),
              clause_parts(Clause, Head, Body),
              functor(Head, Name, _),
              helper_name(Name)
            ),
            Candidates),
    findall(Name,
            ( member(Name-_, Candidates),
              call_count(Clauses, Name, Count),
              Count =\= 1
            ),
            Shared),
    findall(Name-Grouped,
            ( member(Name-_, Candidates),
              \+ memberchk(Name, Shared),
              findall(Clause,
                      ( member(Clause, Clauses),
                        clause_parts(Clause, Head, _),
                        functor(Head, Name, _)
                      ),
                      Grouped0),
              Grouped0 = Grouped
            ),
            Pairs),
    sort(1, @<, Pairs, Helpers).

helper_name(Name) :-
    demangle(Name, Spec),
    spec_kind(Spec, Kind),
    compound(Kind),
    compound_name_arity(Kind, Functor, Args),
    helper_kind(Functor, Args).

helper_kind(dual, _).
helper_kind(dualneg, _).
helper_kind(dualabducible, _).
helper_kind(dualabducibleneg, _).
helper_kind(dualchk, Args) :-
    Args >= 2.

spec_kind(pred(_, Kind, _, _), Kind).
spec_kind(prog(_, Kind, _), Kind).

%!  call_count(+Clauses, +Name, -Count) is det.

call_count(Clauses, Name, Count) :-
    findall(x,
            ( member(Clause, Clauses),
              clause_parts(Clause, _, Body),
              calls(Body, Name)
            ),
            Xs),
    length(Xs, Count).

calls(Body, Name) :-
    body_goal(Body, Goal),
    nonvar(Goal),
    functor(Goal, Name, _).

body_goal(Var, _) :-
    var(Var),
    !,
    fail.
body_goal((A, B), Goal) :-
    !,
    (   body_goal(A, Goal)
    ;   body_goal(B, Goal)
    ).
body_goal((A ; B), Goal) :-
    !,
    (   body_goal(A, Goal)
    ;   body_goal(B, Goal)
    ).
body_goal(forall(_, Inner), Goal) :-
    !,
    body_goal(Inner, Goal).
body_goal(for_domain(_, _, Inner), Goal) :-
    !,
    body_goal(Inner, Goal).
body_goal(findall(_, Inner, _), Goal) :-
    !,
    body_goal(Inner, Goal).
body_goal(not(Inner), Goal) :-
    !,
    body_goal(Inner, Goal).
body_goal(Goal, Goal).

exclude_helpers([], _, []).
exclude_helpers([Clause|Clauses], Helpers, Kept) :-
    clause_parts(Clause, Head, _),
    functor(Head, Name, _),
    (   memberchk(Name-_, Helpers)
    ->  Kept = Rest
    ;   Kept = [Clause|Rest]
    ),
    exclude_helpers(Clauses, Helpers, Rest).

%!  inlined(+Helpers, +Clause, -Inlined) is det.
%
%   The clause with every helper call replaced by the disjunction of that
%   helper's clause bodies, and every remaining goal demangled.

inlined(Helpers, Clause, Inlined) :-
    clause_parts(Clause, Head, Body),
    source_goal(Head, SourceHead),
    expand_body(Body, Helpers, [], Expanded),
    (   Expanded == true
    ->  Inlined = SourceHead
    ;   Inlined = (SourceHead :- Expanded)
    ).

expand_body(Var, _, _, Var) :-
    var(Var),
    !.
expand_body((A, B), Helpers, Seen, Expanded) :-
    !,
    expand_body(A, Helpers, Seen, EA),
    expand_body(B, Helpers, Seen, EB),
    conjoin(EA, EB, Expanded).
expand_body((A ; B), Helpers, Seen, (EA ; EB)) :-
    !,
    expand_body(A, Helpers, Seen, EA),
    expand_body(B, Helpers, Seen, EB).
expand_body(forall(Vars, Inner), Helpers, Seen, forall(Vars, Expanded)) :-
    !,
    expand_body(Inner, Helpers, Seen, Expanded).
expand_body(for_domain(V, D, Inner), Helpers, Seen,
            for_domain(V, D, Expanded)) :-
    !,
    expand_body(Inner, Helpers, Seen, Expanded).
expand_body(findall(T, Inner, L), Helpers, Seen, findall(T, Expanded, L)) :-
    !,
    expand_body(Inner, Helpers, Seen, Expanded).

%   `not` survives translation where its argument is a built-in —
%   `not findall(T, G, L)` is one — so it is a wrapper here and not a
%   literal.  Without this clause the goal under it keeps its mangled
%   name, which `test_listing.pl:listing_corpus` is what noticed.

expand_body(not(Inner), Helpers, Seen, not(Expanded)) :-
    !,
    expand_body(Inner, Helpers, Seen, Expanded).
expand_body(Goal, Helpers, Seen, Expanded) :-
    nonvar(Goal),
    functor(Goal, Name, _),
    memberchk(Name-Clauses, Helpers),
    \+ memberchk(Name, Seen),
    !,
    disjunction(Clauses, Goal, Helpers, [Name|Seen], Expanded).
expand_body(Goal, _, _, Source) :-
    source_goal(Goal, Source).

%   Each clause of the helper contributes one branch, with its head
%   unified against the call so that the arguments line up.  A helper
%   with no clauses is `fail`, which is what "there is no way for this to
%   hold" says.

disjunction([], _, _, _, fail).
disjunction([Clause|Clauses], Goal, Helpers, Seen, Expanded) :-
    branch(Clause, Goal, Helpers, Seen, Branch),
    (   Clauses == []
    ->  Expanded = Branch
    ;   disjunction(Clauses, Goal, Helpers, Seen, Rest),
        Expanded = (Branch ; Rest)
    ).

branch(Clause, Goal, Helpers, Seen, Branch) :-
    copy_term(Clause, Fresh),
    clause_parts(Fresh, Head, Body),
    (   Head = Goal
    ->  expand_body(Body, Helpers, Seen, Branch)
    ;   Branch = fail
    ).

conjoin(true, B, B) :- !.
conjoin(A, true, A) :- !.
conjoin(A, B, (A, B)).

		 /*******************************
		 *        THE VOCABULARY		*
		 *******************************/

%!  source_goal(+Goal, -Source) is det.
%
%   A mangled literal as the program spells it: `not p(X)`, `-p(X)`,
%   `not -p(X)`.  A sub-check keeps a name of its own, `check(1)`, since
%   there is nothing in the source it came from.  Anything not mangled is
%   a built-in and passes through.

source_goal(Goal, Source) :-
    nonvar(Goal),
    (   compound(Goal)
    ->  compound_name_arguments(Goal, Name, Args)
    ;   Name = Goal,
        Args = []
    ),
    demangle(Name, Spec),
    !,
    spec_literal(Spec, Args, Source).
source_goal(Goal, Goal).

spec_literal(pred(_, Kind, Name, _), Args, Literal) :-
    !,
    (   Args == []
    ->  Plain = Name
    ;   compound_name_arguments(Plain, Name, Args)
    ),
    once(wrap(Kind, Plain, Literal)).
spec_literal(prog(_, dualchk(Index), _), _, check(Index)) :- !.
spec_literal(prog(_, chk(Index), _), _, denial(Index)) :- !.
spec_literal(prog(_, nmr, _), _, global_constraints).

%!  wrap(+Kind, +Goal, -Literal) is det.
%
%   The four kinds a *user* literal has, plus the four `compile.pl`
%   writes for an abducible — an abducible is not a literal the program
%   spells, so it is spelled here.
%
%   The last clause is a catch-all, and it is there because a listing
%   must not **fail**: a kind nobody thought of prints as
%   `generated(Kind, Goal)`, which says what it is, where failing would
%   have taken the whole listing with it.

wrap(plain,    Goal, Goal).
wrap(neg,      Goal, -(Goal)).
wrap(dual,     Goal, not(Goal)).
wrap(dualneg,  Goal, not(-(Goal))).
wrap(abducible,        Goal, abducible(Goal)).
wrap(abducibleneg,     Goal, abducible(-(Goal))).
wrap(dualabducible,    Goal, not(abducible(Goal))).
wrap(dualabducibleneg, Goal, not(abducible(-(Goal)))).
wrap(Kind, Goal, generated(Kind, Goal)).

		 /*******************************
		 *         THE SECTIONS		*
		 *******************************/

sorted_into(Clause, l(User, Duals, Checks), Sorted) :-
    clause_parts(Clause, Head, _),
    section_of(Head, Section),
    (   Section == user
    ->  Sorted = l([Clause|User], Duals, Checks)
    ;   Section == duals
    ->  Sorted = l(User, [Clause|Duals], Checks)
    ;   Section == constraints
    ->  Sorted = l(User, Duals, [Clause|Checks])
    ;   Sorted = l(User, Duals, Checks)
    ).

%!  section_of(+Head, -Section) is det.
%
%   A function of the head alone.  Written that way rather than as
%   `head_section(Head, Section)` tried per section, which is the same
%   table read wrong: the cut is inside the clause, so a mismatched
%   *second* argument fails the head unification before reaching it and
%   the catch-all takes everything.

section_of(not(_), duals) :- !.
section_of(-(_), user) :- !.
section_of(abducible(_), user) :- !.
section_of(check(_), constraints) :- !.
section_of(global_constraints, none) :- !.
section_of(denial(_), none) :- !.
section_of(_, user).

%!  predicates(+Clauses, -Predicates) is det.
%
%   Clauses grouped by the predicate they define, in the order they were
%   compiled — which is the order the program was written in.

predicates(Reversed, Predicates) :-
    reverse(Reversed, Clauses),
    group(Clauses, Predicates).

group([], []).
group([Clause|Clauses], [[Clause|Mine]|Rest]) :-
    key(Clause, Key),
    same_key(Clauses, Key, Mine, Others),
    group(Others, Rest).

same_key([], _, [], []).
same_key([Clause|Clauses], Key, Mine, Others) :-
    (   key(Clause, Key)
    ->  Mine = [Clause|More],
        same_key(Clauses, Key, More, Others)
    ;   Mine = More,
        Others = [Clause|Rest],
        same_key(Clauses, Key, More, Rest)
    ).

%   Keyed by what is *shown*, wrappers included: `-p/1` and `p/1` are two
%   predicates and a listing that ran their clauses together would say
%   they were one.

%   A sub-check is its own group: each came from a different thing and
%   each is headed by which.

key(Clause, Head) :-
    clause_parts(Clause, Head, _),
    Head = check(_),
    !.
key(Clause, Wrappers-Name/Arity) :-
    clause_parts(Clause, Head, _),
    peel(Head, [], Wrappers, Bare),
    (   compound(Bare)
    ->  functor(Bare, Name, Arity)
    ;   Name = Bare,
        Arity = 0
    ).

peel(not(Inner), W0, W, Bare) :-
    !,
    peel(Inner, [not|W0], W, Bare).
peel(-(Inner), W0, W, Bare) :-
    !,
    peel(Inner, [-|W0], W, Bare).
peel(abducible(Inner), W0, W, Bare) :-
    !,
    peel(Inner, [abducible|W0], W, Bare).
peel(generated(Kind, Inner), W0, W, Bare) :-
    !,
    peel(Inner, [Kind|W0], W, Bare).
peel(Bare, W, W, Bare).

%!  titled(+Predicates, +Checks, -Titled) is det.
%
%   Each sub-check with a comment saying what it came from, which
%   `nmr.checks` records: a global constraint, the consistency of a
%   classically negated predicate, or a rule on an odd loop.

titled([], _, []).
titled([Clauses|Rest], Checks, [title(Title)-Clauses|Titled]) :-
    (   Clauses = [Clause|_],
        clause_parts(Clause, check(Index), _),
        memberchk(check(Index, What, Source), Checks)
    ->  check_title(What, Source, Title)
    ;   Title = ''
    ),
    titled(Rest, Checks, Titled).

check_title(constraint, Body, Title) :-
    !,
    format(atom(Title), 'the global constraint  :- ~p', [Body]).
check_title(consistency(PI), _, Title) :-
    !,
    format(atom(Title), 'the consistency of ~p', [PI]).
check_title(PI, _, Title) :-
    format(atom(Title), 'the odd loop over ~p', [PI]).

clause_parts((Head :- Body), Head, Body) :- !.
clause_parts(Head, Head, true).
