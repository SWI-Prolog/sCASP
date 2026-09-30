# Known bugs and defects

Found while building the test suite (Stages 0–2 of the rewrite). Everything
here was reproduced; the reproducers are minimal unless stated otherwise.

Open items are first, because those are the ones still to decide on. The
fixed ones are listed at the end for reference — they say what the tests
now guard.

Several open items carry an **executable statement of the answer they
ought to give**, marked *Semantics branch* below: the test is written twice,
under `:- if(expect_semantics)`, and `make check_semantics` runs the second
branch so the failure spells out what is wanted. See `doc/test-strategy.md`.

---

## Open

### 1. Two s(CASP) calls in one conjunction fail

*Semantics branch:* `test_dyncall.pl:two_calls_in_one_conjunction_both_succeed`.

```prolog
?- scasp(p(_)), scasp(q(_)).
ERROR: determinism_error(scasp_solve:stack_parents/3, det, fail, property)
```

Six-line reproducer; also fails for two `begin_scasp` wrappers of the *same*
unit, which is exactly what `embed.pl` documents as working:

> successive sCASP calls in one conjunction accumulate one joint model

**Cause, first layer.** `scasp_call/1` (`embed.pl:321`) passes the previous
call's stack to the next `solve/4`:

```prolog
scasp_stack(StackIn),
solve(Compiled, StackIn, StackOut, Model),
save_model(Model),
Compiled = M:_,                       % TBD: Properly handle the module
save_stack(M:StackOut).
```

`save_stack/1` stores the stack *qualified with its module*, and
`scasp_stack/1` returns it that way, so the second call hands `M:Stack` to
`solve/4`, which wants a plain list. No clause of `stack_parents/3` matches
`:/2`, it fails, and the `:- det` declaration turns that into the error above.

**Cause, second layer.** Passing the plain list gets one step further and then
fails in `justification_tree/3`:

```
existence_error(matching_rule,
                scasp_stack:stack_tree([], _, [], [_/[]]))
```

Two concatenated proofs have two roots; `stack_tree/4` builds a single tree.
So the fix needs a decision about what the justification of a conjunction
*is*, not a one-line change. That is why it is not fixed here.

Pinned by `test_dyncall.pl:two_calls_in_one_conjunction_fail`, which will fail
once the behaviour changes.

### 2. `test/all_programs/clpq_equality.pl` does not terminate

```prolog
-mutually_exclusive(S1, S2) :- member(X, S1), member(X, S2).
mutually_exclusive(S1, S2)  :- not -mutually_exclusive(S1, S2).
member(X1, [X2|_]) :- X1 #= X2.
member(X, [_|R])   :- member(X, R).
?- mutually_exclusive([1,2], [3,4]).
```

Runs past any timeout. `test/min_programs/clpq_intercept.pl` is the same shape
with `#=<` instead of `#=` and terminates (with no models), so the two are
worth comparing. Recorded in `test/scasp/known_issues.pl`.

### 3. `call/1` and `forall/2` cannot be used as goals

`test/min_programs/solve_call.pl` and `solve_forall.pl`. The solver reports
``scasp_predicate `call/1' does not exist`` and yields no model.

`forall/2` is a control construct that `comp_duals.pl` *generates* for dual
rules; a program cannot write it. Via `scasp/2` it fails differently, with
`permission_error(scasp, procedure, '$apply':forall/2)`, because the name
resolves to the SWI built-in. Both recorded in `test/scasp/known_issues.pl`.

### 4. The tabling path is dead code

`predicates.pl:42`:

```prolog
:- if(fail).
table_predicate(M:Goal) => ...
:- else.
table_predicate(_) :- fail.
:- endif.
```

`table_predicate/1` therefore always fails, which makes unreachable:

- `solve.pl:214-222`, the `table_predicate` branch of `solve_goal/8`
- `solve_goal_table_predicate/8` (`solve.pl:424`)
- both `\+ table_predicate(...)` guards (`solve.pl:563`, `:571`)
- **all 63 lines of `clp/call_stack.pl`**, whose only callers are those two
  branches; its exported `dump_rules/3` has no caller at all

`#table`, `handle_table_directives/1` and `pr_table_predicate/1` compile all
the way through and are then ignored. Decide whether tabling is coming back or
whether this should go.

### 5. `scasp_load/2` compiles into its caller's module

`scasp_load(:Sources, +Options)` asserts the `pr_*/N` representation into the
module of its caller, and `clean_pr_program/1` wipes what was there first. Two
threads loading different programs from the same module therefore destroy each
other's program — the second `clean_pr_program/1` removes the clauses the first
is solving against, and the solve hangs.

It is why the golden tests all have to live in one plunit unit: two units
compiling into `test_scasp` on separate jobs hang on their first test.
`scasp/2` does not have the problem, because `dyncall.pl` compiles into an
`in_temporary_module/3`.

### 6. `load_compiled.pl` is broken and unused

- asserts `pr_rule/2` (lines 51, 60) where the rest of the system uses
  `pr_rule/3`
- asserts into the calling module, not the `M` it was given
- `read_compiled_files(M:S)` is called with a qualified term but its clauses
  match `[]` / `[F|Fs]`
- `read_compiled_source/1` has **no caller** anywhere in `prolog/`
- header says `@tbd: Not compatible with module handling.`

So `scasp -c` / `--compiled` cannot work. Either fix or delete.

### 7. `source_ref.pl` is O(n²) and never freed

```prolog
assert_scasp_source_reference(File, Pos, Ref) :-
    scasp_source_reference(Ref, File, Pos), !.
```

The first clause scans every recorded reference for every term read, so
recording is quadratic in program size. Ids are allocated by "first solution
+ 1", which breaks if anything else asserts into the table.

The table was also `dynamic` rather than `thread_local`, so it was shared by
every thread and never freed. `test_scasp.pl` resets it before each program —
otherwise the `goal_origin/2` clause numbers depend on what was loaded earlier
in the process, and golden data compared at `exact` becomes order-dependent —
and that reset threw away the references another thread was using: running the
golden tests beside the dyncall units under `run_tests/2`'s `jobs(4)` failed
eleven of them. It is `thread_local` now, which fixes the sharing and means
the table goes away with the thread. The quadratic scan and the id allocation
remain.

### 8. `opt_rule(detail(print_tree, ...))` is dead

`options.pl:157` reads the option `print_tree`, but `opt_type/3` names it
`tree`, so the rule never fires. It cannot simply be renamed: the rule
rewrites `tree(true)` to `tree(mid)`, and `solve_results/4` (`main.pl:266`)
matches on `tree(true)`, so enabling it would stop the justification being
computed at all.

Nothing is currently broken by it — `--short`/`--mid`/`--long` reach
`filter_tree/4` through the raw option list — except that conflicting detail
options are not diagnosed for trees the way they are for `--code`.

### 9. `exec_neg_findall/5` is a stub

*Semantics branch:* `test_builtins.pl:negated_findall_holds_when_the_findall_fails`.

`solve.pl:526`:

```prolog
% TODO: What to do with the negation of findall/3 (if required)
exec_neg_findall(Goal, _, _, _, _) :-
    verbose(format('PENDING: execution of not ~p \n', [Goal])),
    fail.
```

`not findall(...)` therefore always fails.

### 10. `my_copy_term/4`'s fallback branch would not compile

`solve.pl:1116-1140`, the `:- else.` of
`:- if(current_predicate(copy_term_nat/4))`, calls `ord_subtract/3` at line
1136. `library(ordsets)` is not imported by `solve.pl` and `ord_subtract`
appears nowhere else in the file. The branch is for a SWI-Prolog that no
longer exists; if it were ever taken it would raise an existence error.

### 11. Duplicated dead clauses

- `solve.pl:456` and `solve.pl:468` are the same `clp_builtin` clause of
  `solve_goal_builtin/2`; the first cuts, so the second is unreachable.
- `comp_duals.pl:213-218` and `:220-225` are six character-identical
  `dual_goal/2` clauses, under the comments `% constraint` and `% clpq/r`.

### 12. Module dependency cycles

- `common` ↔ `program` — exists only for `has_prefix/2`, four lines of atom
  munging that belong next to `split_functor/3` in `common.pl`
- `clp/clpq` ↔ `clp/disequality`
- `clp/disequality` → `verbose` → `modules` → ... — **a constraint store
  depending on the tracing module**, and `solve.pl`'s hot loop runs through
  `verbose/1` and `scasp_trace_goal/2` on every goal
- `html` ↔ `listing`, plus `listing` → `human` → `html` → `listing`

### 13. `:- pred` trips the singleton check

```prolog
:- pred shown(Thing) :: '@(Thing) is shown'.
Warning: Singleton variables: [Thing]
```

The variable occurs twice in the source but only once after the term
expansion, which is what the singleton check sees. Every file in
`test/le_programs/` carries `:- style_check(-singleton)` for this reason.
The expansion could keep the variable reachable, or `pred/1` could suppress
the check the way `begin_scasp` does.

### 14. `∉` cannot be used inside a program

*Semantics branch:* `test_disequality.pl:not_in_is_usable_in_a_program`.

`library(scasp)` exports `∉/2` with an operator and documents it as "not
element of", but `predicates.pl` does not list it in `scasp_builtin/1`. So a
program that uses it is treated as calling a user predicate, `dyncall.pl` tries
to collect that predicate's clauses, and they call `ground/1`:

```prolog
not_one_of(X) :- X ∉ [a, b].

?- scasp(not_one_of(c)).
ERROR: No permission to scasp procedure `system:ground/1'
```

As a plain goal it works correctly, constraining an unbound left argument and
testing a bound one. Pinned both ways by `test_disequality.pl:diseq_not_in`.

### 15. Negating a structured head is incomplete for an unbound query

*Semantics branch:* `test_compound.pl:the_unbound_answer_admits_every_shape_that_holds`.

```prolog
wrap(f(X)) :- item(X).
item(a).
```

`not wrap(T)` holds of every T except `f(a)`. Asked about a ground term it
says so — `not wrap(g(a))` and `not wrap(b)` both succeed. Asked about an
unbound one it gives a single answer, `f(Y)` with `Y ∉ [a]`: the shape that
is *not* `f(_)` is never offered, so the general answer is strictly weaker
than the set of ground instances that hold.

The dual has the missing clause —

```prolog
not o_wrap_1_vh2(A, B) :- A \= f(B).
not o_wrap_1_vh2(A, B) :- A = f(B), not item(B).
```

— but under `forall(B, ...)` an unbound `A` cannot satisfy `A \= f(B)`, since
the two unify, so only the second branch survives.

Pinned by `test_compound.pl:compound_negation_is_incomplete`, which fails the
day the general answer appears.

**A finite domain fixes it**, today, with no new machinery — guarding the
variable turns the incomplete answer into the complete one:

```prolog
dom(f(a)). dom(f(b)). dom(g(a)). dom(b).
nw(T) :- dom(T), not wrap(T).
?- nw(T).                 % T = f(b) ; T = g(a) ; T = b
```

which is why `doc/design-next.md` §6a proposes declaring such domains rather
than writing the guard by hand.

**The criterion that detects it** is in `doc/design-next.md` §5: a
`forall`-quantified variable occurring inside a compound on the other side of a
disequality, so that covering the rest of the domain would need "A is not of
the form `f(_)`" — a universally quantified disequality no store can express.
It is statically detectable at dual generation and flags 20 of the 52 test
programs, `member/2` and `queens.pl` among them; most of those never call the
negation in the mode that realises it, which is why the check belongs in a
report rather than a refusal.

**This is a known limitation of the published method, not a porting error.**
Marple et al., *Computing Stable Models of Normal Logic Programs Without
Grounding* — the paper the dual construction comes from — works the same case
through for `p([X|T]) :- q(X), p(T).` and states that calling the resulting dual
with a structure containing unbound variables makes the disunification in its
first clause "encounter illegal cases", in their implementation a fatal error.
This port does not error; it silently drops the answer instead. Same cause, a
quieter symptom. See `doc/literature.md`.

**Not a head-rewriting problem.** The obvious suspect was the removed
`prep_args2/7` (see Fixed #22): it decomposed a structured head argument one
level further than `abstract_structures/4` does, so it might have been the
intended repair. It is not. Writing the three formulations by hand —

```prolog
w1(f(X)) :- item(X).                   % compound in the head
w2(Y)    :- Y = f(X), item(X).         % what abstract_structures/4 produces
w3(Y)    :- Y = f(Z), Z = X, item(X).  % what prep_args2/7 would have produced
```

— `w1` and `w2` compile identically, and `w3` compiles to a genuinely longer
dual with a branch of its own for the inner `B \= C`. All three give the same
single answer. So no rewriting of the head recovers the missing one; the
fix, if there is to be one, has to be a constructive representation of "A does
not unify with `f(_)`", which the disequality store cannot express today.
`test_compound.pl:compound_head_rewriting_is_neutral` pins the equivalence.

### 16. Standard order on an unbound variable is not a constraint

*Semantics branch:* `test_builtins.pl:builtin_standard_order` and
`builtin_arithmetic_comparison`. Both behaviours are now **decided** — see
*Resolution* below — so the expected answers are written out even though
neither is implemented yet.

`@</2` and friends are defined on every term, and a variable precedes
everything else in the standard order. So

```prolog
tlt(X, Y) :- X @< Y.
?- scasp(tlt(X, b)).
```

succeeds with `X` still unbound — and nothing then stops `X` being bound to
`z`, which is not `@< b`. Every other s(CASP) answer about an unbound variable
carries a constraint that the variable must respect; this one does not, so
these operators are only meaningful on ground terms.

The arithmetic comparisons have a different problem: `aeq(_, 2)` and
`not aeq(_, 2)` *both* fail, because `exec_goal/1` catches the instantiation
error and fails rather than reporting it (#17).

**Resolution.** Two decisions.  The arithmetic one is implemented in the
new library (`solve.pl:compare_values/1`); standard order is not yet.

*Arithmetic* (`=:=`, `=\=`, and for the same reason `<`, `>`, `=<`,
`>=`, whose duals are each other) — **constrain**. `X =:= 2` on an unbound `X`
constrains it exactly as `X #= 2` does, and the negation constrains the other
way:

```prolog
?- scasp(aeq(X, 2)).        X = 2.
?- scasp(not aeq(X, 2)).    X = X | {X #<> 2}.
```

This keeps an unbound query answerable, which is the point of s(CASP), and
makes `=:=` agree with `#=` rather than introduce a second, weaker meaning.

*Standard order* (`@<`, `@>`, `@=<`, `@>=`) — **raise, but only where the
outcome depends on a variable**. Standard order settles many comparisons
before reaching one: `f(X) @< g(a)` holds whatever `X` is, because the names
decide it, and those stay usable. Only a comparison whose deciding position
is an unbound variable raises:

```prolog
?- scasp(tlt(f(X), g(a))).   true.                 % names decide it
?- scasp(tlt(X, b)).         instantiation_error.
?- scasp(tlt(f(X), f(a))).   instantiation_error.  % X vs a decides it
```

Requiring ground arguments outright would have been simpler to specify but
rejects the decidable cases.

**The criterion is about where the comparison is decided, not about
unifiability.** The tempting shortcut — *undecidable iff the two terms
unify* — is wrong in the unsafe direction:

```prolog
f(X, a)  and  f(b, c)     % do NOT unify: a and c clash at argument 2
?- f(X, a) @< f(b, c).    % true, with X unbound
?- X = z, f(X, a) @< f(b, c).
false.                    % standard order decided at argument 1, where X is
```

Unification fails at argument 2, but standard order decides at argument 1, so
a rule based on unifiability would accept an answer that a later binding
overturns. (It is also wrong the other way, though harmlessly: `X @< X`
unifies yet is stably false.)

**Deferred: this wants support below s(CASP).** The same question has come up
before outside this project, and the natural home for it is a `compare/3`
variant that reports a third outcome when the decision rests on comparing a
variable against something, rather than silently ordering the variable first.
With that primitive the s(CASP) side is a few lines; without it, every caller
re-implements the walk below.

So the implementation has to walk both terms the way standard order does —
arity, then name, then arguments left to right — and stop at the first
position that decides the comparison. If either side there is an unbound
variable, raise; otherwise the comparison rests on ground structure and is
stable. Equivalently: unification must fail *at the deciding position*, not
merely somewhere. Pinned both ways by
`test_builtins.pl:nor_when_a_later_clash_hides_the_deciding_argument`.

### 17. Querying an atom with no rules raises, where the body of a rule fails

An atom that occurs in a program but has no rules is simply underivable: it
is in the Herbrand base, and every answer set leaves it out. Used in a rule
*body* s(CASP) agrees — but asked as a query it raises.

```prolog
q :- p.
?- q.        % fails silently, correctly: p is underivable, so q is
?- not p.    % existence_error(scasp_predicate, p/0)
```

The unique answer set of that program is `{}`, so `not p` holds in it and
`?- not p.` should succeed. `compile.pl:check_existence/2` throws
unconditionally for the query, without consulting the `scasp_unknown` flag or
the `unknown(Mode)` option — unlike `pr_rules.pl:check_existence/1`, which
honours both. So `unknown(fail)` does not suppress it, and the query is held
to a stricter rule than the same literal in a body.

`scasp/2` does the same, and adds a second wart: the predicate indicator in
the error carries the interned module name, so the message a user sees is

```
scasp_predicate `'tmp-1-4183594005075597591:p'/0' does not exist
```

naming a temporary module that existed only for that query. `modules.pl`
interns the module into the functor for the solver's benefit; the error should
be un-interned before it is reported.

Found by the differential harness (`test/differential.pl`): it is the only
disagreement with the stable-model definition across the hand-written battery
in `test_differential.pl`. Pinned there by
`undefined_atom_in_a_query_raises_rather_than_answering`.

### 18. `--forall=prev` and `--forall=sasp` lose answers, and `--dcc` inherits it

Both cuts in `solve_goal_forall/8` sit in the *disequality* branch — the case
where the goal succeeded with the quantified variable constrained to a finite
set of prohibited values, which `exec_with_neg_list/10` then has to check.

```prolog
(   current_prolog_flag(scasp_forall, prev) ->  !  ;   true ),
exec_with_neg_list(...),
(   current_prolog_flag(scasp_forall, sasp) ->  !  ;   true ),
```

`prev` cuts **before** the check, committing to the first solution of the goal
before knowing whether its prohibited set can be completed. If the completion
then fails, the whole `forall` fails — although another solution might have
completed. `sasp` cuts after, which is milder but still discards alternative
covers.

Measured over the corpus, comparing the *set of query bindings* against the
uncut default:

| Program | `all` | `prev` | `sasp` |
|---|---:|---:|---:|
| `all_programs/bec_loan.pl` | **4** | **0** | **0** |
| `min_programs/stream_reasoning.pl` | 4 | 3 | 3 |
| `min_programs/solve_not_is.pl` | 2 | 1 | 1 |
| `all_programs/sasp/lion.pl` | 1 | 1 | **0** |

`bec_loan` is the clear case: four answers become none.

**`--dcc` inherits this.** `options.pl:opt_rule/1` adds `forall(prev)` when
`--dcc` is given and *errors* on any other value, so there is no way to run
`--dcc` with a complete `forall`. `scasp --dcc -s0 test/all_programs/bec_loan.pl`
reports no models where the default reports four. The `.pass` data under
`test/all_programs/dcc/` was recorded that way and may be incomplete for the
same reason.

`--forall=all_c` is not an answer either: it does not terminate on `queens.pl`
or on `bec_loan.pl`.

So the only complete setting is the default. `doc/design-next.md` §5c argues
that the flag should be removed rather than defaulted, since the four values
are not four algorithms but one case-dispatch plus two unsound cuts.

### 19. `--assume` has no visible effect

`solve.pl` reads `scasp_assume` in the coinductive-success branch and calls
`mark_prev_goal/3` to rewrite the matching ancestor as `assume(Goal)`. Setting
the flag changes nothing in the justification of `p :- not q. q :- not p.`, and
`mark_prev_goal/3` is never reached. `model_wrapper(assume)` in `modules.pl` is
unreached for the same reason. Either the flag no longer does what it says, or
it needs a program shape that is not obvious.

### 20. Unsupported built-ins in embedded programs

`test/le_programs/list-scasp.pl` fails with
`permission_error(scasp, procedure, system:once/1)`. `once/1` is a compiled
built-in, so `body_calls/3` refuses it. Whether `once/1`, `forall/2` and
friends should be supported is a design question; at present the diagnosis is
at least accurate.

### 21. A loop through `findall/3` is answered, in both implementations

    q.
    p :- findall(X, r(X), []).
    r(1) :- p.

has no answer set: `p` needs `r/1` to have no solutions, and `r(1)` holds
whenever `p` does.  Both implementations answer `?- p.` with the model `[p]`
and `?- r(1).` with `[p, r(1)]`, a model whose `p` its own `r(1)` refutes.

A collection is a *non-monotone* dependency — more solutions of its goal can
make it false — and neither call graph sees it as one: the new tree's
`clauses.pl:body_literal/2` looks through `findall/3` with the polarity
outside it, so the loop is a positive one and no NMR sub-check is made.
Counting the arc as negative too would generate one, but it is not clear it
would be enough: the `findall/3` was answered `[]` before `r(1)` was proved,
and nothing re-checks a collection against what was proved after it.  What
ASP means by an aggregate in a loop is itself a choice (stratification, or
one of the non-stratified semantics), so this waits on deciding which.
`test/test_builtins.pl:builtin_findall` pins it, with the semantics branch
asking for no answer.

---

## Fixed

Each is covered by a test, so the suite now guards against regression.

| # | Defect | Where |
|---|---|---|
| 1 | `--plaindual`/`-d` parsed and then silently discarded — `opt_type/3` and `opt_help/2` existed, no `set_option/1` clause | `options.pl` |
| 2 | `--variant` had `opt_help/2` but no `opt_type/3`, so it could not be passed at all | `options.pl` |
| 3 | `scasp_real` read by `main.pl` and `json.pl` but never created | `options.pl` |
| 4 | `scasp --version` and `--help` exited 1: `catch_with_backtrace/3` caught the `unwind(halt(0))` | `main.pl` |
| 5 | `#show`/`#pred`/`#prolog` expanded to **static** `pr_*` facts, so `clean_pr_program/1` raised "No permission to modify static procedure" | `dyncall.pl` |
| 6 | Inside `begin_scasp`, every directive except `#discontiguous`/`#pred` leaked to the dyncall expansions and was then filtered away | `embed.pl` |
| 7 | `scasp_compiled/1` omitted `pr_prolog_predicate/2` and `pr_dcc_predicate/2`, which `clean_pr_program/1` retracts | `predicates.pl` |
| 8 | `error_position/3` failed without `base/1`/`stream/1`, turning a syntax error into a silent failure of `directive/4` | `input.pl` |
| 9 | A program defining `forall/2` crashed with `No rule matches qualify_body/3`; now a proper error naming the predicate | `modules.pl`, `dyncall.pl` |
| 10 | `--html` without `--tree` crashed: `solve_results/4` omits the `tree` key, the HTML renderer reads `Answer.tree` unconditionally | `options.pl` |
| 11 | `scasp/1` documented in the pldoc since the module was written but never defined | `dyncall.pl` |
| 12 | `dump_clpq_var/3` re-wrapped an argument its callers had already wrapped, so `clpqr_dump_constraints/3` was asked for the store of a *list* rather than of a variable and returned `[]` for every constrained variable. That made the `clpq(...)` case of `check_unbound/2` unreachable, and with it `dual_clpq/2`, `dual_clpq_/2` and `exec_with_clpq_constraints/11` — about 40 lines | `clp/clpq.pl` |
| 13 | `exec_with_clpq_constraints/11` left `ProvedOut02` unbound on two of its three branches, so the recursive call was handed an unbound assoc and `get_assoc/3` raised `$btree_find_node/5: Arguments are not sufficiently instantiated`. Hidden behind the previous one: the code had never run | `solve.pl` |
| 14 | `--forall=prev` and `--forall=sasp` proved both `w` and `not w` for `w :- x(Y), Y #> 3` with `x(Y) :- Y #> 0`, where `Y = 4` satisfies both so `w` holds. A consequence of the two above — the forall could not see the constraint store it was supposed to negate. `--dcc` forces `prev`, so it reached past the flag. All four algorithms now agree | `clp/clpq.pl`, `solve.pl` |
| 15 | `inf/2` and `sup/2` are listed in `clp_interval/1` and handled by `solve_goal_builtin/2`, but `solve.pl` never imported them from `clp/clpq.pl`. `exec_goal/1` catches every `error(_,_)` and fails, so the existence error became a silent "no models" | `solve.pl` |
| 16 | A module name containing `:` corrupted every literal in the model *and* the justification: `'family-scasp+http':'//tests.com:is_a_parent_of'(john,mary)`. Split at the first colon instead of the last | `modules.pl` |
| 17 | `sandbox:safe_meta_predicate/1` declared multifile while `safe_meta/2` was defined; `?+++` missing from the safe list | `scasp.pl` |
| 18 | `library(http/json)` moved; warned on every run | `main.pl` |
| 19 | `=:=`, `=\=`, `@<`, `@>`, `@=<` and `@>=` had `dual_goal/2` entries in `comp_duals.pl` but were missing from `prolog_builtin/1`, so `scasp/2` refused them and the file front end compiled them and then silently yielded no model. Added to `prolog_builtin/1`; `solve_goal_builtin/2` already had the case | `predicates.pl` |
| 20 | `comp_duals.pl` carried the six `dual_goal/2` clauses for the CLP(Q) comparisons twice over, under the comments `% constraint` and `% clpq/r`. Removed the second copy; the compiled program is byte-identical with and without `--plaindual` | `comp_duals.pl` |
| 21 | Public-name typos `unqualify_justitication_tree/3` (alias kept) and `contrained_neg/2` | `stack.pl`, `solve.pl` |
| 22 | `prep_args2/7` and the last clause of `prep_args/7` were unreachable behind `abstract_structures/4`; ~50 lines of a second, never-used head-rewriting mechanism, plus a counter argument threaded unchanged through `prep_args/7` | `comp_duals.pl` |
| 23 | `exec_goal/1` caught **every** error from a built-in and failed, so a goal and its negation both failed where neither could be evaluated, and defects in the program being solved were hidden. Errors now propagate; `scasp` reports them and exits 1 | `solve.pl` |
| 24 | `apply_clpq_constraints(A #<> B+C)` evaluated `B+C` with an unguarded `is/2`, raising when reached with `B` unbound (`_ #<> _+1` in `bec_loan.pl`). Guarded with `ground/1`, so it now falls through to the general disequality clause | `clp/clpq.pl` |
| 25 | `min_programs/clpq_intercept.pl` compared two *lists* with `=\=`, an arithmetic operator, where it meant `\=`. The type error was swallowed, so the program yielded no models and "passed" that way. Fixed; it now reasons about which segments intercept | `test/min_programs/` |

Test-infrastructure defects fixed at the same time: `-q` resolved the renamed
`test/programs/`; `test_scasp.pl` declared `prolog_cover:report_hook/2` while
defining `prolog_coverage:report_hook/2`; `test_clp_disequality.pl` imported a
`clp_disequality` that no longer resolves; `.gitlab-ci.yml` was Ciao's and
tested nothing; `doc/Tricks.md` paths were stale; and the plunit suites
resolved `library(scasp)` to the installed pack rather than this tree.
