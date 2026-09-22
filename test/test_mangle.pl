:- module(test_mangle,
          [ test_mangle/0
          ]).
:- use_module(scasp_plunit, []).        % sets the library search path
:- use_module(library(sscasp/mangle)).
:- use_module(library(plunit)).
:- use_module(library(lists), [member/2]).

/** <module> Functor name mangling

`sscasp/mangle.pl` implements `doc/design-next.md` decision 2.  What these
check is mostly the round trip — every name the compiler can produce has to
come back as the thing it was made from — and the failure modes, since the
scheme's claim over the one it replaces is that a bad name *fails* rather than
decoding to something else.

Every name has four fields.  A predicate deriving from no user predicate — the
NMR check and its sub-checks — has the `Name` field empty, which is
unambiguous because a predicate must have a name.
*/

test_mangle :-
    run_tests([ mangle_round_trip,
                mangle_kinds,
                mangle_failure,
                mangle_errors
              ]).

		 /*******************************
		 *        THE ROUND TRIP	*
		 *******************************/

:- begin_tests(mangle_round_trip).

%   One per shape the compiler produces, from doc/design-next.md.

spec(pred(m, plain,            flies, 1), 'm$plain$flies$1').
spec(pred(m, neg,              cold,  1), 'm$neg$cold$1').
spec(pred(m, dual,             flies, 1), 'm$dual$flies$1').
spec(pred(m, dualneg,          cold,  1), 'm$dualneg$cold$1').
spec(pred(m, dual(2),          flies, 1), 'm$dual.2$flies$1').
spec(pred(m, dual(2, inner),   flies, 1), 'm$dual.2.inner$flies$1').
spec(pred(m, dualneg(2),       cold,  1), 'm$dualneg.2$cold$1').
spec(prog(m, nmr, 0),                     'm$nmr$$0').
spec(prog(m, chk(1), 0),                  'm$chk.1$$0').
spec(prog(m, chk(2), 1),                  'm$chk.2$$1').
spec(prog(m, false, 0),                   'm$false$$0').

test(mangles_as_expected, [forall(spec(Spec, Expected))]) :-
    mangle(Spec, Mangled),
    assertion(Mangled == Expected).

test(demangles_back, [forall(spec(Spec, Mangled))]) :-
    demangle(Mangled, Back),
    assertion(Back == Spec).

test(every_shape_is_recognised_as_mangled, [forall(spec(_, Mangled))]) :-
    assertion(mangled(Mangled)).

%   A zero-arity user predicate is a predicate like any other.

test(arity_zero) :-
    mangle(pred(m, plain, p, 0), Mangled),
    assertion(Mangled == 'm$plain$p$0'),
    demangle(Mangled, Back),
    assertion(Back == pred(m, plain, p, 0)).

%   The module is a field, so a long temporary module name survives the
%   round trip even though it makes for an unlovely functor.

test(a_temporary_module_name) :-
    Module = 'tmp-1-4183594005075597591',
    mangle(pred(Module, plain, p, 1), Mangled),
    demangle(Mangled, Back),
    assertion(Back == pred(Module, plain, p, 1)).

:- end_tests(mangle_round_trip).

		 /*******************************
		 *           KINDS		*
		 *******************************/

%   The Kind field is the only one carrying sub-structure, and the only one
%   that can, because it never holds user data — so its separator needs no
%   reserving.

:- begin_tests(mangle_kinds).

test(an_index_comes_back_as_an_integer) :-
    demangle('m$dual.2$p$1', pred(_, Kind, _, _)),
    assertion(Kind == dual(2)),
    Kind = dual(N),
    assertion(integer(N)).

test(a_tag_comes_back_as_an_atom) :-
    demangle('m$dual.2.inner$p$1', pred(_, Kind, _, _)),
    assertion(Kind == dual(2, inner)).

test(a_plain_kind_is_an_atom) :-
    demangle('m$plain$p$1', pred(_, Kind, _, _)),
    assertion(Kind == plain),
    assertion(atom(Kind)).

test(a_deeper_kind_round_trips) :-
    mangle(pred(m, a(1, b, 2), p, 1), Mangled),
    assertion(Mangled == 'm$a.1.b.2$p$1'),
    demangle(Mangled, Back),
    assertion(Back == pred(m, a(1, b, 2), p, 1)).

:- end_tests(mangle_kinds).

		 /*******************************
		 *          FAILING		*
		 *******************************/

%   The claim over the scheme this replaces: a name that is not one of ours
%   *fails* to demangle rather than decoding to something plausible.

:- begin_tests(mangle_failure).

test(an_ordinary_name_is_not_mangled, [fail]) :-
    demangle(flies, _).
test(nor_is_one_with_underscores, [fail]) :-
    demangle(o_p_2_vh2, _).
test(a_wrong_field_count_fails, [fail]) :-
    demangle('a$b$c', _).
test(too_many_fields_fails, [fail]) :-
    demangle('a$b$c$0$x', _).
test(a_non_numeric_arity_fails, [fail]) :-
    demangle('m$plain$p$x', _).
test(a_stray_dollar_in_a_name_fails_rather_than_decoding, [fail]) :-
    demangle('m$plain$a$b$1', _).
test(a_non_atom_is_not_mangled, [fail]) :-
    demangle(1, _).

test(mangled_agrees_with_demangle) :-
    assertion(\+ mangled(flies)),
    assertion(mangled('m$plain$flies$1')).

:- end_tests(mangle_failure).

		 /*******************************
		 *          REJECTED		*
		 *******************************/

:- begin_tests(mangle_errors).

test(a_dollar_in_the_module,
     [ throws(error(permission_error(use, reserved_character($), _), _)) ]) :-
    mangle(pred('a$b', plain, p, 1), _).
test(a_dollar_in_the_name,
     [ throws(error(permission_error(use, reserved_character($), _), _)) ]) :-
    mangle(pred(m, plain, 'a$b', 1), _).
test(a_dollar_in_a_program_level_module,
     [ throws(error(permission_error(use, reserved_character($), _), _)) ]) :-
    mangle(prog('a$b', nmr, 0), _).

%   An empty name means "derives from no user predicate", so a pred/4 spec
%   may not claim one.

test(an_empty_predicate_name,
     [ throws(error(type_error(predicate_name, _), _)) ]) :-
    mangle(pred(m, plain, '', 1), _).

test(a_negative_arity,
     [ throws(error(type_error(nonneg, _), _)) ]) :-
    mangle(pred(m, plain, p, -1), _).
test(an_unknown_spec,
     [ throws(error(type_error(mangle_spec, _), _)) ]) :-
    mangle(something(else), _).
test(a_kind_that_is_neither_atom_nor_compound,
     [ throws(error(type_error(mangle_kind, _), _)) ]) :-
    mangle(pred(m, 1, p, 1), _).

:- end_tests(mangle_errors).
