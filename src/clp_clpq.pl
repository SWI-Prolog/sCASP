:- module(clp_clpq,_).


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

 @pred{prety_print/1} used by @pred{portray_attribute/2} to print the
constraints of a variable.

").

%% ------------------------------------------------------------- %%

:- use_module(engine(attributes)).

:- use_module(library(clpq/clpq_dump), [clpqr_dump_constraints/3]).
:- use_package(clpq).

%% ------------------------------------------------------------- %%

is_clpq_var(X) :-
	get_attribute(X,A),
	A \= att(_,_,_).

apply_clpq_constraints(Constraints) :-
	clpq_meta(Constraints).

dump_clpq_var([Ground],[NewVar],Constraints) :-
	ground(Ground),
	Constraints = [NewVar .=. Ground].
dump_clpq_var(Var,NewVar,Constraints) :-
	\+ ground(Var),
	clpqr_dump_constraints([Var],[NewVar],Constraints).

dual_clpq([Unique], [Dual]) :-
	dual_clpq_(Unique, Dual).
dual_clpq([Init, Next|Is], Dual) :-
	(
	    dual_clpq([Init], Dual)
	;
	    dual_clpq([Next|Is], NextDual),
	    Dual = [Init, NextDual]
	).
dual_clpq_(A .<. B, A .>=. B).
dual_clpq_(A .=<. B, A .>. B).
dual_clpq_(A .>. B, A .=<. B).
dual_clpq_(A .>=. B, A .>. B).
dual_clpq_(A .<>. B, A .=. B).
%dual_clpq_(A .=. B, A .<>. B).
dual_clpq_(A .=. B, A .>. B).
dual_clpq_(A .=. B, A .<. B).


% Success if StoreA >= StoreB
entails(VarA, (VarB,StoreB)) :-
	dump_clpq_var(VarA, VarB, StoreA),
	clpq_meta(StoreB),
	clpq_entailed(StoreA).

% Success if StoreA >= StoreB
store_entails(StoreA, StoreB) :-
	clpq_meta(StoreB),
	clpq_entailed(StoreA).



:- multifile portray_attribute/2.
portray_attribute(_,A) :-
	clpqr_dump_constraints(A, A, Constraints),
	(
	    Constraints == [] ->
	    display(A)
	;
	    display('{ '),
	    prety_print(Constraints),
	    display(' }')
	).

prety_print([]).
prety_print([C]) :- prety_print_(C).
prety_print([C1,C2|Cs]) :- prety_print_(C1), display(', '), prety_print([C2|Cs]).
prety_print_(C) :- C =.. [Op,A,B], display(A), display(' '), display(Op), display(' '), display(B).

