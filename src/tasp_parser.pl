:- module(main, _).
	% [
        %                 main/1
        %              ]).

:- use_module(library(file_utils)).
:- use_module(library(terms_vars)).

:- op(700, fx, [not]).
:- op(700, xfx, [include,.\=.]).


:- dynamic
	current_option/2,
	pr_rule/2,
	query/1,
	user_predicate/1,
	counter/2,
	temp/1.

test :- main(['tt.pl']).

main(_) :-
	cleanup([
		    current_option/2,
		    pr_rule/2,
		    query/1,
		    user_predicate/1,
		    counter/2,
		    temp/1
		]),
		fail.

main(Args) :-
	parse_args(Args, Options, Sources),
	set_options(Options),
 	assert_sources(Sources),
	assert_dual.
% 	solve_query(Query),

%main(_).

assert_dual :-
	user_predicate(Pred),
	predicate_dual(Pred,Conjuction),
	assert(pr_rule(not(Pred),Conjuction)),
	fail.
assert_dual.	


predicate_dual(Head,_Conjuction) :-
	init(cl_id,0),
	pr_rule(Head,Body),
	incr(cl_id,1,Id),
	varset(Head,VH),
	varset(Body,VB),
	display(diff_vars(VB,VH,ForallVars)),nl,
	diff_vars(VB,VH,ForallVars),
	display(diff_vars(VB,VH,ForallVars)),nl,
	(
	    ForallVars==[] ->
	    assert(temp(Head-dual(Id,Head))),
	    clause_dual(dual(Id,Head),Body)	    
	;
	    Head =.. [Name|Args],
	    append(Args,ForallVars,NewArgs),
	    NewHead =.. [Name|NewArgs],
	    assert(temp(Head-forall(ForallVars,dual(Id,NewHead)))),
	    clause_dual(dual(Id,NewHead),Body)
	),
 	fail.
predicate_dual(Head,Conjuction) :-
	collect_predicate_dual(Head,Conjuction).

collect_predicate_dual(Head,Conjuction) :-
	retract(temp(Head-Rule)),
	!,
	Conjuction = [Rule | Rest],
	collect_predicate_dual(Head,Rest).
collect_predicate_dual(_,[]).
	

clause_dual(Head, Clause) :-
	clause_dual_(Head,[],Clause).

clause_dual_(_,_,[]).
clause_dual_(Head,PrevL,[B|Body]) :-
	neg_literal(B,NegB),
	append(PrevL,[NegB],NewBody),
	assert(pr_rule(Head,NewBody)),
	append(PrevL,[B],NextL),
	clause_dual_(Head,NextL,Body).

neg_literal(L,NegL) :-
	neg_literal_(L,NegL), !.

neg_literal_(not(C),C) :-
	\+ \+ user_predicate(C).
neg_literal_(not(C),fail) :-
	format('Warning: The goal not(~w) will fail\n\n',[C]),
	\+ user_predicate(C).
neg_literal_(A=B,A.\=.B).
neg_literal_(A.\=.B,A=B).
neg_literal_(C,not(C)) :-
	\+ \+ user_predicate(C).
neg_literal_(C,true) :-
	format('Warning: The goal not(~w) will succeed\n\n',[C]),
	\+ user_predicate(C).

init(Name,InitValue) :-
	retractall(counter(_,_)),
	assert(counter(Name,InitValue)).
incr(Name,IncrValue,NewValue) :-
	(
	    counter(Name,CurrentValue) ->
	    NewValue is CurrentValue + IncrValue,
	    init(Name, NewValue)
	;
	    NewValue = IncrValue,
	    init(Name, NewValue)
	).
	

pair_to_list((A,B),[A|Ls]) :- !, pair_to_list(B,Ls).
pair_to_list((A),[A]).

assert_sources([]) :- !,
	display('Error: No input file specified!\n\n'),
	fail.
assert_sources(Sources) :-
	read_sources(Sources),
	(
	    query(_)
	;
	    read(Pairs),
	    pair_to_list(Pairs,Query),
	    assert(query(Query))
	).

read_sources([]).
read_sources([S|Ss]) :-
	file_to_terms(S, TermS),
	parse_terms(TermS),
	read_sources(Ss).

parse_terms([]).
parse_terms([T | Ts]) :-
	parse_term(T), !,
	parse_terms(Ts).

%% TODO - linearize the argument of the head.
parse_term(:-(A,B)) :- pair_to_list(B,Bs),linearize((A,Bs),(La,Lb)), assert(pr_rule(La,Lb)), add_predicate(La).
parse_term(?-(Q)) :- assert(query([Q])).
parse_term(:-(include(File))) :- read_sources([File]).
parse_term(include(#,File)) :- read_sources([File]).
parse_term(A) :- linearize((A,[]),(La,Lb)), assert(pr_rule(La,Lb)), add_predicate(La).

linearize((A,B), (La, Lb)) :-
	A =.. [Name|Args],
	linearize_args(Args,NewArgs,Unifier),
	La =.. [Name|NewArgs],
	append(Unifier, B, Lb).

linearize_args([],[],[]).
linearize_args([A|Arg],[V|NewV],NewCs) :-
	linearize_args(Arg,NewV,NewC),
	linearize_arg(A,NewV,V,C), !,
	append(C,NewC,NewCs).

linearize_arg(Var,V,Var,[]) :-
	var(Var),
	not_in(Var,V).
linearize_arg(Var,V,NVar,[NVar=Var]) :-
	var(Var),
	\+ not_in(Var,V).
linearize_arg(NonVar,_,NVar,[NVar=NonVar]) :-
	\+ var(NonVar).


not_in(_,[]).
not_in(V1, [V2|R]) :-
	V1 \== V2,
	not_in(V1,R).
	
	

add_predicate(Pred) :-
	(
	    user_predicate(Pred) ->
	    true %% Already added
	;
	    assert(user_predicate(Pred))
	).

set_options(Options) :-
	set_default_options,
	set_user_options(Options).

set_default_options :-
	set(verbose,0).

set_user_options([]).
set_user_options([O | Os]) :-
	(
	    set_user_option(O) ->
	    set_user_options(Os)
	;
	    format('Error: the option ~w is not supported!\n\n',[O]),
	    fail
	).

set_user_option('-h') :- help.
set_user_option('-?') :- help.
set_user_option('--help') :- help.
set_user_option('-v') :- set(verbose, 1).
set_user_option('--verbose') :- set(verbose, 1).

set(Option, Value) :-
	retractall(current_option(Option, _)),
	assert(current_option(Option,Value)).

help :-
        display('Usage: TCLP(asp) [options] InputFile(s)\n\n'),
        display('TCLP(asp) computes stable models of ungrounded normal logic programs.\n'),
        display('Command-line switches are case-sensitive!\n\n'),
        display(' General Options:\n\n'),
        display('  -h, -?, --help     Print this help message and terminate.\n'),
        display('  -i, --interactive  Run in user / interactive mode.\n'),
        display('  -a, --auto         Run in automatic mode (no user interaction).\n'),
        display('  -sN                Compute N answer sets, where N >= 0. 0 for all.\n'),
        display('                     Ignored unless running in automatic mode.\n'),
        display('  -v, --verbose      Enable verbose progress messages.\n'),
        display('  -vv, --veryverbose Enable very verbose progress messages.\n'),
        display('  -j                 Print proof tree for each solution.\n'),
        display('  -w                 Generate html file with proof tree for each solution.\n'),
        display('  -g                 Generate the program transdisplayion (+ duals and nmr_check)\n'),
	display('                     displayed with pr_rule/2 in a new file named <NAME_pr.pl>.\n'),
        display('  -n                 Hide goals added to solution by global consistency checks.\n'),
        display('  -la                Print a separate list of succeeding abducibles with each\n'),
        display('                     CHS. List will only be displayed if at least one abducible\n'),
        display('                     has succeeded.\n'),
	abort.


parse_args([],[],[]).
parse_args([O | Args], [O | Os], Ss) :-
	atom_concat('-',_,O),!,
	parse_args(Args, Os, Ss).
parse_args([S | Args], Os, [S | Ss]) :-
	parse_args(Args, Os, Ss).


cleanup([]).
cleanup([N/A|Xs]) :-
	length(List,A),
	Pred =.. [N|List],
	retractall(Pred),
	cleanup(Xs).
