/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(sscasp_listing,
          [ program_listing/2,          % +Program, -Listing
            portray_program/3,          % @Terms, @Query, +Options
            portray_listing/3,          % @Terms, @Query, +Options
            titled/2,                   % +Title, +Options
            commented/2                 % +Text, +Options
          ]).
:- use_module(ops).
:- use_module(mangle,  [demangle/2, kind_literal/3]).
:- use_module(compile, [compile_program/3]).
:- use_module(run,     [scasp_program/2]).
:- use_module(render,  [portray_rule/2, portray_query/2]).
:- use_module(library(option), [option/2, option/3]).
:- use_module(library(ansi_term), [ansi_format/3]).
:- use_module(library(lists), [member/2, reverse/2]).
:- use_module(library(listing), [portray_clause/3]).
:- use_module(library(apply), [maplist/3, foldl/4, convlist/3, include/3,
                                 exclude/3]).
:- use_module(library(aggregate), [aggregate_all/3]).

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
%
%   @arg Program is the `program{clauses, nmr, ...}` dict of
%       compile_program/3; its `clauses` and `nmr.checks` are read.
%   @arg Listing holds under `constraints` a `title(Comment)-Clauses`
%       pair per predicate rather than a bare clause list.

:- det(program_listing/2).

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

%!  portray_program(@Terms, @Query, +Options) is det.
%
%   portray_listing/3 between two sections that are not the compiled
%   program read back, and so are not `listing_section/3`'s: the terms
%   as given, and the clauses `emit.pl` makes for the solver to run.
%   The executable and `scasp_show/2` both write this.  Options are
%   portray_listing/3's, plus
%
%     - source(Boolean)
%       The terms as given, which is the only section that shows a
%       directive.  Default `false`.
%     - source_title(+Title)
%       Its title.  Default `'Program as read'`.
%     - compiled(Boolean)
%       The emitted clauses.  Default `false`.

:- det(portray_program/3).

portray_program(Terms, Query, Options) :-
    (   option(source(true), Options)
    ->  option(source_title(Title), Options, 'Program as read'),
        titled(Title, Options),
        forall(member(Term, Terms), show_term(Term))
    ;   true
    ),
    portray_listing(Terms, Query, Options),
    (   option(compiled(true), Options)
    ->  titled('Emitted clauses', Options),
        commented('run in a module importing sscasp_runtime', Options),
        scasp_program(Terms, Clauses),
        forall(member(Clause, Clauses), show_term(Clause))
    ;   true
    ).

%   With `sscasp/ops.pl`'s operator table, not the caller's.  A caller
%   that has not imported them — a plunit unit, say, or a module that
%   only uses `scasp/2` — would otherwise be shown `not(penguin(X))`
%   where the program says `not penguin(X)`, which is the same term and
%   not the same answer to "what would this run".

show_term(Term) :-
    portray_clause(current_output, Term, [module(sscasp_ops)]).

%!  portray_listing(@Terms, @Query, +Options) is det.
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
%   caller showing only the query should not pay for the duals.  Query
%   `none` is the same as unbound.
%
%   @arg Options are also handed to portray_rule/2 and portray_query/2,
%       so `variable_names/1` and `indent/1` apply there.

:- det(portray_listing/3).

portray_listing(Terms, Query, Options) :-
    (   needs_program(Options)
    ->  compile_program(scasp, Terms, Program),
        program_listing(Program, Listing)
    ;   Listing = none
    ),
    forall(( listing_section(Section, _, Title),
             wanted(Section, Options)
           ),
           show_section(Section, Title, Query, Listing, Options)).

needs_program(Options) :-
    listing_section(Section, _, _),
    Section \== query,
    wanted(Section, Options),
    !.

wanted(Section, Options) :-
    listing_section(Section, Default, _),
    Asked =.. [Section, Wanted],
    option(Asked, Options, Default),
    Wanted == true.

show_section(query, Title, Query, _, Options) =>
    (   nonvar(Query),
        Query \== none
    ->  titled(Title, Options),
        portray_query(Query, Options)
    ;   true
    ).
show_section(Section, Title, _, Listing, Options),
    get_dict(Section, Listing, Predicates),
    Predicates \== [] =>
    titled(Title, Options),
    forall(member(Predicate, Predicates),
           show_predicate(Predicate, Options)).
show_section(_, _, _, _, _) =>
    true.

show_predicate(title(Comment)-Clauses, Options) =>
    (   Comment == ''
    ->  true
    ;   commented(Comment, Options)
    ),
    show_clauses(Clauses, Options).
show_predicate(Clauses, Options) =>
    show_clauses(Clauses, Options).

show_clauses(Clauses, Options) :-
    forall(member(Clause, Clauses), portray_rule(Clause, Options)),
    nl.

%!  titled(+Title, +Options) is det.
%!  commented(+Text, +Options) is det.
%
%   A section heading, as a comment so that what is written stays
%   loadable-looking even where it is not.  `commented/2` is the same
%   line without the break before it, for a second line under a heading.

:- det(titled/2).
:- det(commented/2).

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

%!  helpers(@Clauses, -Helpers) is det.
%
%   The generated intermediates, as `Name-Clauses`.  A predicate counts
%   only if every call to it is the one call it was generated for —
%   `dualise.pl` writes one helper per clause and `nmr.pl` one per
%   sub-check, so that holds, and checking it costs one walk.
%
%   @arg Clauses are the program's compiled clauses, over mangled names.
%   @arg Helpers is a list of `Name-Clauses`, Name the mangled functor.

helpers(Clauses, Helpers) :-
    convlist(helper_clause_name, Clauses, Candidates0),
    sort(Candidates0, Candidates),
    exclude(shared_helper(Clauses), Candidates, Names),
    maplist(helper_clauses(Clauses), Names, Helpers).

helper_clause_name(Clause, Name) :-
    clause_named(Name, Clause),
    helper_name(Name).

shared_helper(Clauses, Name) :-
    call_count(Clauses, Name, Count),
    Count =\= 1.

helper_clauses(Clauses, Name, Name-Grouped) :-
    include(clause_named(Name), Clauses, Grouped).

clause_named(Name, Clause) :-
    clause_parts(Clause, Head, _),
    functor(Head, Name, _).

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

%!  call_count(@Clauses, +Name, -Count) is det.
%
%   @arg Count is the number of calls in the bodies of Clauses to a
%       predicate named Name, of any arity — calls, not clauses: two in
%       one body count twice.

call_count(Clauses, Name, Count) :-
    aggregate_all(count,
                  ( member(Clause, Clauses),
                    clause_parts(Clause, _, Body),
                    calls(Body, Name)
                  ),
                  Count).

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

exclude_helpers([], _, Kept) =>
    Kept = [].
exclude_helpers([Clause|Clauses], Helpers, Kept) =>
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
%
%   @arg Helpers is the `Name-Clauses` list of helpers/2.

:- det(inlined/3).

inlined(Helpers, Clause, Inlined) :-
    clause_parts(Clause, Head, Body),
    source_goal(Head, SourceHead),
    expand_body(Body, Helpers, [], Expanded),
    (   Expanded == true
    ->  Inlined = SourceHead
    ;   Inlined = (SourceHead :- Expanded)
    ).

expand_body(Var, _, _, Expanded), var(Var) =>
    Expanded = Var.
expand_body((A, B), Helpers, Seen, Expanded) =>
    expand_body(A, Helpers, Seen, EA),
    expand_body(B, Helpers, Seen, EB),
    conjoin(EA, EB, Expanded).
expand_body((A ; B), Helpers, Seen, Expanded) =>
    Expanded = (EA ; EB),
    expand_body(A, Helpers, Seen, EA),
    expand_body(B, Helpers, Seen, EB).
expand_body(forall(Vars, Inner), Helpers, Seen, Expanded) =>
    Expanded = forall(Vars, EInner),
    expand_body(Inner, Helpers, Seen, EInner).
expand_body(for_domain(V, D, Inner), Helpers, Seen, Expanded) =>
    Expanded = for_domain(V, ED, EInner),
    expand_body(D, Helpers, Seen, ED),      % the generator is a literal too
    expand_body(Inner, Helpers, Seen, EInner).
expand_body(findall(T, Inner, L), Helpers, Seen, Expanded) =>
    Expanded = findall(T, EInner, L),
    expand_body(Inner, Helpers, Seen, EInner).

%   `not` survives translation where its argument is a built-in —
%   `not findall(T, G, L)` is one — so it is a wrapper here and not a
%   literal.  Without this clause the goal under it keeps its mangled
%   name, which `test_listing.pl:listing_corpus` is what noticed.

expand_body(not(Inner), Helpers, Seen, Expanded) =>
    Expanded = not(EInner),
    expand_body(Inner, Helpers, Seen, EInner).

%   A domain check (`scasp_domain_error`) is `must_be/2`, which is what
%   it does: raise unless the value is in the domain.  Only when ground,
%   which `must_be/2` does not say, but a listing is not a program.

expand_body('$domain_check'(Call, Domain, _PI), _, _, Expanded) =>
    arg(1, Call, Value),
    Expanded = must_be(Domain, Value).
expand_body(Goal, Helpers, Seen, Expanded),
    functor(Goal, Name, _),
    memberchk(Name-Clauses, Helpers),
    \+ memberchk(Name, Seen) =>
    disjunction(Clauses, Goal, Helpers, [Name|Seen], Expanded).
expand_body(Goal, _, _, Source) =>
    source_goal(Goal, Source).

%   Each clause of the helper contributes one branch, with its head
%   unified against the call so that the arguments line up.  A helper
%   with no clauses is `fail`, which is what "there is no way for this to
%   hold" says.

disjunction([], _, _, _, Expanded) =>
    Expanded = fail.
disjunction([Clause|Clauses], Goal, Helpers, Seen, Expanded) =>
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

conjoin(true, B, AB) => AB = B.
conjoin(A, true, AB) => AB = A.
conjoin(A, B, AB) => AB = (A, B).

		 /*******************************
		 *        THE VOCABULARY		*
		 *******************************/

%!  source_goal(@Goal, -Source) is det.
%
%   A mangled literal as the program spells it: `not p(X)`, `-p(X)`,
%   `not -p(X)`.  A sub-check keeps a name of its own, `check(1)`, since
%   there is nothing in the source it came from.  Anything not mangled is
%   a built-in and passes through.

source_goal(Goal, Source),
    compound(Goal),
    compound_name_arguments(Goal, Name, Args),
    demangle(Name, Spec) =>
    spec_literal(Spec, Args, Source).
source_goal(Goal, Source), atom(Goal), demangle(Goal, Spec) =>
    spec_literal(Spec, [], Source).
source_goal(Goal, Source) =>
    Source = Goal.

spec_literal(pred(_, Kind, Name, _), Args, Literal) =>
    (   Args == []
    ->  Plain = Name
    ;   compound_name_arguments(Plain, Name, Args)
    ),
    wrap(Kind, Plain, Literal).
spec_literal(prog(_, dualchk(Index), _), _, Literal) =>
    Literal = check(Index).
spec_literal(prog(_, chk(Index), _), _, Literal) =>
    Literal = denial(Index).
spec_literal(prog(_, nmr, _), _, Literal) =>
    Literal = global_constraints.
spec_literal(_, _, _) =>
    fail.

%!  wrap(+Kind, @Goal, -Literal) is det.
%
%   The four kinds a *user* literal has, plus the four `compile.pl`
%   writes for an abducible — an abducible is not a literal the program
%   spells, so it is spelled here.
%
%   The last clause is a catch-all, and it is there because a listing
%   must not **fail**: a kind nobody thought of prints as
%   `generated(Kind, Goal)`, which says what it is, where failing would
%   have taken the whole listing with it.
%
%   @arg Kind is the `Kind` field of the `pred/4` spec that
%       `mangle.pl:demangle/2` gives.

wrap(Kind, Goal, Literal), kind_literal(Kind, Goal, Literal0) =>
    Literal = Literal0.
wrap(abducible,        Goal, Literal) => Literal = abducible(Goal).
wrap(abducibleneg,     Goal, Literal) => Literal = abducible(-(Goal)).
wrap(dualabducible,    Goal, Literal) => Literal = not(abducible(Goal)).
wrap(dualabducibleneg, Goal, Literal) => Literal = not(abducible(-(Goal))).
wrap(Kind, Goal, Literal) => Literal = generated(Kind, Goal).

		 /*******************************
		 *         THE SECTIONS		*
		 *******************************/

sorted_into(Clause, Sorted0, Sorted) :-
    Sorted0 = l(User, Duals, Checks),
    clause_parts(Clause, Head, _),
    section_of(Head, Section),
    (   Section == user
    ->  Sorted = l([Clause|User], Duals, Checks)
    ;   Section == duals
    ->  Sorted = l(User, [Clause|Duals], Checks)
    ;   Section == constraints
    ->  Sorted = l(User, Duals, [Clause|Checks])
    ;   Sorted = Sorted0
    ).

%!  section_of(@Head, -Section) is det.
%
%   A function of the head alone.  Written that way rather than as
%   `head_section(Head, Section)` tried per section, which is the same
%   table read wrong: with the section in the head, a mismatched
%   *second* argument fails the head unification before the clause is
%   committed to and the catch-all takes everything.  So the section is
%   unified in the body, after the head has chosen the clause.
%
%   @arg Head is a head as source_goal/2 spells it.
%   @arg Section is `user`, `duals`, `constraints`, or `none` for a
%       clause that is not listed.

section_of(not(_), Section)          => Section = duals.
section_of(-(_), Section)            => Section = user.
section_of(abducible(_), Section)    => Section = user.
section_of(check(_), Section)        => Section = constraints.
section_of(global_constraints, Section) => Section = none.
section_of(denial(_), Section)       => Section = none.
section_of(_, Section)               => Section = user.

%!  predicates(@Clauses, -Predicates) is det.
%
%   Clauses grouped by the predicate they define, in the order they were
%   compiled — which is the order the program was written in.
%
%   @arg Clauses arrive in *reverse* order, as sorted_into/3 collects
%       them.
%   @arg Predicates is a list of clause lists.

:- det(predicates/2).

predicates(Reversed, Predicates) :-
    reverse(Reversed, Clauses),
    group(Clauses, Predicates).

group([], Groups) =>
    Groups = [].
group([Clause|Clauses], Groups) =>
    Groups = [[Clause|Mine]|Rest],
    key(Clause, Key),
    same_key(Clauses, Key, Mine, Others),
    group(Others, Rest).

same_key([], _, Mine, Others) =>
    Mine = [],
    Others = [].
same_key([Clause|Clauses], Key, Mine, Others) =>
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

peel(not(Inner), W0, W, Bare) =>
    peel(Inner, [not|W0], W, Bare).
peel(-(Inner), W0, W, Bare) =>
    peel(Inner, [-|W0], W, Bare).
peel(abducible(Inner), W0, W, Bare) =>
    peel(Inner, [abducible|W0], W, Bare).
peel(generated(Kind, Inner), W0, W, Bare) =>
    peel(Inner, [Kind|W0], W, Bare).
peel(Bare0, W0, W, Bare) =>
    W = W0,
    Bare = Bare0.

%!  titled(+Predicates, +Checks, -Titled) is det.
%
%   Each sub-check with a comment saying what it came from, which
%   `nmr.checks` records: a global constraint, the consistency of a
%   classically negated predicate, or a rule on an odd loop.
%
%   @arg Predicates is a list of clause lists, as predicates/2 makes.
%   @arg Checks is `nmr.checks`, a list of `check(Index, What, Source)`.
%   @arg Titled pairs each clause list as `title(Comment)-Clauses`,
%       Comment `''` for one that is not a sub-check.

titled([], _, Titled) =>
    Titled = [].
titled([Clauses|Rest], Checks, Titled0) =>
    Titled0 = [title(Title)-Clauses|Titled],
    (   Clauses = [Clause|_],
        clause_parts(Clause, check(Index), _),
        memberchk(check(Index, What, Source), Checks)
    ->  check_title(What, Source, Title)
    ;   Title = ''
    ),
    titled(Rest, Checks, Titled).

check_title(constraint, Body, Title) =>
    format(atom(Title), 'the global constraint  :- ~p', [Body]).
check_title(consistency(PI), _, Title) =>
    format(atom(Title), 'the consistency of ~p', [PI]).
check_title(-(Name/Arity), _, Title) =>
    format(atom(Title), 'the odd loop over -~q/~w', [Name, Arity]).
check_title(PI, _, Title) =>
    format(atom(Title), 'the odd loop over ~q', [PI]).

%   Semidet, unlike `clauses.pl`'s: callers here match a head pattern.

clause_parts((Head :- Body), Head, Body) :- !.
clause_parts(Head, Head, true).
