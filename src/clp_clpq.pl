:- module(clp_clpq,
          []).
:- expects_dialect(ciao).
% from clpqr_ops.pl
:- op(700, xfx, [(.=.),(.<>.),(.<.),(.=<.),(.>.),(.>=.)]).

%% ------------------------------------------------------------- %%
:- use_package(assertions).
:- doc(title, "Extension of the constraint solver CLP(Q)").
:- doc(author, "Joaquin Arias").
:- doc(filetype, module).

:- doc(module, "

This module inport the constraint solve package for CLP(Q) and extend
its functionalities (among others) with:

 @pred{dual_clpq/2} provide the dual of a constraint store in run-time
to evaluate the @pred{forall/4} predicate.

 @pred{pretty_print/1} used by @pred{portray_attribute/2} to print the
constraints of a variable.

").

%% ------------------------------------------------------------- %%

:- use_module(clp_disequality_rt).

:- use_module(engine(attributes)).

:- use_module(library(clpq/clpq_dump), [clpqr_dump_constraints/3]).
:- use_package(clpq).

%% ------------------------------------------------------------- %%

is_clpq_var(X) :-
    var(X),
    get_attribute(X, A),
    A \= att(_, _, _).

apply_clpq_constraints(A .<>. B + C) :-
    get_neg_var(A,[Num]),
    num(Num),
    Num is B + C, !.
apply_clpq_constraints(A .<>. B) :- !,
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

% disequality_clpq(A,B) :-
%       \+ is_clpq_var(B), !,
%       (
%           apply_clpq_constraints([A .>. B])
%       ;
%           apply_clpq_constraints([A .<. B])
%       ).

% loop_list_clpq([A|As],[B|Bs]) :-
%       (
%           loop_var_clpq(A,B)
%       ;
%           A = B,
%           loop_list_clpq(As,Bs)
%       ).

% loop_var_clpq(A,B) :-
%       is_clpq_var(A),
%       \+ is_clpq_var(B), !,
%       display(a),nl,
%       dump_clpq_var([A], [B], Const),
%       dual_clpq(Const, Dual),
%       apply_clpq_constraints(Dual).
% loop_var_clpq(B,A) :-
%       is_clpq_var(A),
%       \+ is_clpq_var(B), !,
%       display(b),nl,
%       dump_clpq_var([A], [B], Const),
%       dual_clpq(Const, Dual),
%       apply_clpq_constraints(Dual).
% loop_var_clpq(A,B) :-
%       is_clpq_var(A),
%       is_clpq_var(B), !,
%       display(c),nl,
%       apply_clpq_constraints([A .<>. B]).



% Success if StoreA >= StoreB
entails(VarA, (VarB, StoreB)) :-
    dump_clpq_var(VarA, VarB, StoreA),
    clpq_meta(StoreB),
    clpq_entailed(StoreA).

% Success if StoreA >= StoreB
store_entails(StoreA, StoreB) :-
    clpq_meta(StoreB),
    clpq_entailed(StoreA).

% Success if B >= A
entail_list(A, B) :-
    dump_clpq_var(A, X, StoreA),
    StoreA \= [],
    dump_clpq_var(B, X, StoreB),
    StoreB \= [],
    clpq_meta(StoreA),
    clpq_entailed(StoreB).

%% Success if S >= Goal
:- use_module(library(terms_vars)).
:- use_module(library(terms_check)).
entail_terms(Goal, S) :-
    \+ \+ Goal = S,
    clp_varset(Goal, VsGoal),
    VsGoal \= [],
    clp_varset(S, VsS),
    clpqr_dump_constraints(VsGoal, DumpVars, StoreGoal),
    clpqr_dump_constraints(VsS, DumpVars, StoreVsS),
    store_entails(StoreVsS,StoreGoal).

clp_varset(Term, ClpVars) :-
    varset(Term, Vars),
    clp_vars(Vars, ClpVars).
clp_vars([], []).
clp_vars([C|Vs], [C|Rs]) :- is_clpq_var(C), !, clp_vars(Vs, Rs).
clp_vars([_|Vs], Rs) :- clp_vars(Vs, Rs).

:- multifile portray_attribute/2.
% portray_attribute(_,A) :-
%       clpqr_dump_constraints(A, A, Constraints),
%       (
%           Constraints == [] ->
%           display(A)
%       ;
%           display('{ '),
%           display(A), display(' '),
%           display(Constraints),
% %         pretty_print(Constraints),
%           display(' }')
%       ).

portray_attribute(_, A) :-
    clpqr_dump_constraints(A, A, Constraints),
    (
        Constraints == [] ->
        display(A)
    ;
        display(' {'),
        display(A),
        display('~['),
        reverse(Constraints, RC),
        pretty_print(RC),
        display(']} ')
    ).


pretty_print([]).
pretty_print([C]) :- pretty_print_(C).
pretty_print([C1, C2|Cs]) :- pretty_print_(C1), display(', '), pretty_print([C2|Cs]).
pretty_print_(nonzero(Var)) :- display(nonzero(Var)), !.
pretty_print_(R) :- struct(R), R =.. [rat, A, B], display(A), !, display(/), display(B).
pretty_print_(C) :- struct(C), C =.. [Op, A, B], pretty_print_(A), display(' '), display_op(Op), display(' '), pretty_print_(B), !.
pretty_print_(A) :- display(A).

display_op(Op) :- pretty_op(Op, Pop), !, display(Pop).
display_op(Op) :- display(Op).

pretty_op(.<., <).
pretty_op(.=<., =<).
pretty_op(.>., >).
pretty_op(.>=., >=).
pretty_op(.=., =).
pretty_op(.\=., \=).
