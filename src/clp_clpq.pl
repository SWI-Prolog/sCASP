:- module(clp_clpq,
          [ is_clpq_var/1,
            clpqr_dump_constraints/3,
            disequality_clpq/2,
            entails/2,
            entail_terms/2,
            dual_clpq/2,
            apply_clpq_constraints/1,
            dump_clpq_var/3
          ]).
% from clpqr_ops.pl
:- op(700, xfx, [(.=.),(.<>.),(.<.),(.=<.),(.>.),(.>=.)]).

% ------------------------------------------------------------- %%

/** <module> Extension of the constraint solver CLP(Q)

This module inport the constraint  solve package  for CLP(Q)  and extend
its functionalities (among others) with: dual_clpq/2 provide the dual of
a  constraint  store  in  run-time to  evaluate the  forall/4 predicate.
pretty_print/1 used by portray_attribute/2 to print the constraints of a
variable.

@author Joaquin Arias
*/


% ------------------------------------------------------------- %%

:- use_module(clp_disequality_rt).

:- use_module(engine(attributes)).

:- if(exists_source(library(clpqr/dump))).
:- use_module(library(clpq)).
:- use_module(library(clpr), []).                                % avoid undef
:- use_module(library(clpqr/dump), [dump/3]).

:- op(1200, xfx, =>).

clpqr_dump_constraints(Target, NewVars, Constraints), is_list(Target) =>
    maplist(to_clpq_var, Target, Target2),
    dump(Target2, NewVars, Constraints0),
    maplist(ciao_constraint, Constraints0, Constraints).

to_clpq_var(X, V) :-
    (   is_clpq_var(X)
    ->  V = X
    ;   true
    ).

ciao_constraint(A=B,     Ciao) => Ciao = (A.=.B).
ciao_constraint(A-B=\=0, Ciao) => Ciao = (A.<>.B).
ciao_constraint(A>B,     Ciao) => Ciao = (A.>.B).
ciao_constraint(A>=B,    Ciao) => Ciao = (A.>=.B).
ciao_constraint(A<B,     Ciao) => Ciao = (A.<.B).
ciao_constraint(A=<B,    Ciao) => Ciao = (A.=<.B).

clpq_entailed(C) :-
    entailed(C).
clpq_meta(C) :-
    clpqr_meta(C).

% from Ciao library/clpqr/clpqr_meta.pl
clpqr_meta(A) :- var(A), !, fail.
clpqr_meta((A,B)) :- !, clpqr_meta(A), clpqr_meta(B).
clpqr_meta((A;B)) :- !, ( clpqr_meta(A) ; clpqr_meta(B) ).
clpqr_meta([]) :- !.
clpqr_meta(X) :- X = [_|_], !, clpqr_meta_list(X).
clpqr_meta(A) :-
        translate_meta_clp(A).

clpqr_meta_list([]) :- !.
clpqr_meta_list([A|As]) :- !,
        clpqr_meta(A),
        clpqr_meta_list(As).

translate_meta_clp(A.=.B)  => to_rat(A, AR), to_rat(B, BR), {AR =:= BR}.
translate_meta_clp(A.<>.B) => to_rat(A, AR), to_rat(B, BR), {AR =\= BR}.
translate_meta_clp(A.<.B)  => to_rat(A, AR), to_rat(B, BR), {AR  <  BR}.
translate_meta_clp(A.=<.B) => to_rat(A, AR), to_rat(B, BR), {AR =<  BR}.
translate_meta_clp(A.>.B)  => to_rat(A, AR), to_rat(B, BR), {AR  >  BR}.
translate_meta_clp(A.>=.B) => to_rat(A, AR), to_rat(B, BR), {AR  >= BR}.
% for bec_light.pl
translate_meta_clp(A < B)  => to_rat(A, AR), to_rat(B, BR), {AR  <  BR}.
translate_meta_clp(A > B)  => to_rat(A, AR), to_rat(B, BR), {AR  >  BR}.

to_rat(rat(N,D), Rat) => Rat = N/D.
to_rat(Term,     Rat) => Rat = Term.



is_clpq_var(X) :-
    attvar(X),
    clp_type(X, clpq).

:- else.
:- use_module(library(clpq/clpq_dump), [clpqr_dump_constraints/3]).
:- use_package(clpq).

is_clpq_var(X) :-
    var(X),
    get_attribute(X, A),
    A \= att(_, _, _).

:- endif.

% ------------------------------------------------------------- %%

apply_clpq_constraints(A .<>. B + C) :-
    get_neg_var(A,[Num]),
    number(Num),
    Num is B + C, !.
apply_clpq_constraints(A .<>. B) :- !,       % JW: Why not simply {A =\= B}?
    (
        apply_clpq_constraints(A .<. B)
    ;
        apply_clpq_constraints(A .>. B)
    ).
apply_clpq_constraints(Constraints) :-
    clpq_meta(Constraints).

dump_clpq_var([Ground], [NewVar], Constraints) :-
    ground(Ground),
    Constraints = [NewVar .=. Ground].
dump_clpq_var(Var, NewVar, Constraints) :-
    \+ ground(Var),
    clpqr_dump_constraints([Var], [NewVar], Constraints).

dual_clpq([Unique], [Dual]) :-
    dual_clpq_(Unique, Dual).
dual_clpq([Init, Next|Is], Dual) :-
    (
        dual_clpq([Init], Dual)
    ;
        dual_clpq([Next|Is], NextDual),
        Dual = [Init|NextDual]
    ).
dual_clpq_(A .<. B, A .>=. B).
dual_clpq_(A .=<. B, A .>. B).
dual_clpq_(A .>. B, A .=<. B).
dual_clpq_(A .>=. B, A .<. B).
dual_clpq_(A .<>. B, A .=. B).
%dual_clpq_(A .=. B, A .<>. B).
dual_clpq_(A .=. B, A .>. B).
dual_clpq_(A .=. B, A .<. B).


disequality_clpq(A, B) :-
    \+ is_clpq_var(B), !,
    (
        apply_clpq_constraints([A .>. B])
    ;
        apply_clpq_constraints([A .<. B])
    ).

% Success if StoreA >= StoreB
entails(VarA, (VarB, StoreB)) :-
    dump_clpq_var(VarA, VarB, StoreA),
    clpq_meta(StoreB),
    clpq_entailed(StoreA).

% Success if StoreA >= StoreB
store_entails(StoreA, StoreB) :-
    clpq_meta(StoreB),
    clpq_entailed(StoreA).

% Success if S >= Goal
entail_terms(Goal, S) :-
    \+ \+ Goal = S,
    clp_varset(Goal, VsGoal),
    VsGoal \= [],
    clp_varset(S, VsS),
    clpqr_dump_constraints(VsGoal, DumpVars, StoreGoal),
    clpqr_dump_constraints(VsS, DumpVars, StoreVsS),
    store_entails(StoreVsS,StoreGoal).

clp_varset(Term, ClpVars) :-
    term_attvars(Term, Vars),
    include(is_clpq_var, Vars, ClpVars).

% :- set_prolog_flag(write_attributes, portray).

itf:attr_portray_hook(_, A) :-
    \+ \+ ( clpqr_dump_constraints([A], [X], Constraints),
            (   Constraints == []
            ->  write(A)
            ;   reverse(Constraints, RC),
                pretty_print(RC, X)
            )
          ).

pretty_print([], _).
pretty_print([C], X) :- pretty_print_(C, X).
pretty_print([C1, C2|Cs], X) :- pretty_print_(C1, X), display(', '), pretty_print([C2|Cs], X).

pretty_print_(nonzero(Var), X) =>
    (   Var == X
    ->  format('nonzero(\u2627)')
    ;   write(nonzero(Var))
    ).
pretty_print_(rat(A,B), _)     =>
    format('~w/~w', [A, B]).
pretty_print_(C, X), compound(C), C =.. [Op, A, B] =>
    (   pretty_op(Op, Pretty)
    ->  true
    ;   Pretty = Op
    ),
    pretty_print_(A, X), format(' ~q ', [Pretty]), pretty_print_(B, X), !.
pretty_print_(A, X) =>
    (   A == X
    ->  format('\u2627')
    ;   write(A)
    ).

pretty_op(.<., <).
pretty_op(.=<., =<).
pretty_op(.>., >).
pretty_op(.>=., >=).
pretty_op(.=., =).
pretty_op(.\=., \=).
