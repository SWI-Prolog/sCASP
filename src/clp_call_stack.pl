:- module(clp_call_stack, _).

%% ------------------------------------------------------------- %%
:- use_package(assertions).
:- doc(title, "Call stack constraint solver and TCLP interface.").
:- doc(author, "Joaquin Arias").
:- doc(filetype, module).

:- doc(module, "

This module contains the code to handle @var{StackIn}, @var{StackOut}
and @var{Model} as attributes in order to check entailment with the
TCLP framework of CIAO.

@pred{~>/2} is the predicate used to get the attribute from the
attributed variable.

@pred{<~/2} is the predicate used to put the term as an attribute.

").

%% ------------------------------------------------------------- %%
:- use_module(clp_disequality_rt).
:- op(700, xfx, [(.\=.), (.=.)]).

:- use_package(attr).

:- op(700, xfx, [(~>), (<~), (<~>)]).

%% ------------------------------------------------------------- %%
:- doc(section, "Main predicates").

A ~> Att :- get_attr_local(A, rules(Att)).
A <~ Att :- put_attr_local(A, rules(Att)).

dump_rules([],     [],     []).
dump_rules([X|Xs], [_|Ns], [D|Ds]) :-
    get_attr_local(X, rules(D)),
    dump_rules(Xs, Ns, Ds).
dump_rules([X|Xs], Ns, Ds) :-
    \+ get_attr_local(X, rules(_)),
    dump_rules(Xs, Ns, Ds).

%% Attributes predicates %%
:- multifile attr_unify_hook/2, attribute_goals/3, attr_portray_hook/2.
attr_unify_hook(rules(Att), B) :- get_attr_local(B, rules(AttB)), Att = AttB.
attr_unify_hook(neg(A), B) :- not_unify(B,A).
attribute_goals(X) --> [X ~> G], {get_attr_local(X, rules(G))}.
attribute_goals(X) --> [X.\=.G], {get_attr_local(X, neg(G))}.
attr_portray_hook(rules(Att), A) :- format(" ~w  .is ~w ", [A, Att]).
attr_portray_hook(neg(Att),   A) :- format("~w.\\=.~w", [A, Att]).
%% Attributes predicates %%


%% ------------------------------------------------------------- %%
:- doc(section, "TCLP interface").

%% %% TCLP interface %%
%% call_domain_projection([],[]).
%% call_domain_projection([X|Xs], [D|Ds]) :-
%%     call_domain_projection_(X, D), !,
%%     call_domain_projection(Xs,Ds).
%% call_entail([], []).
%% call_entail([D1|D1s], [D2|D2s]) :-
%%     call_entail_(D1,D2),
%%     call_entail(D1s,D2s).
%% call_store_projection(_, St, St).

%% answer_domain_projection([],     []). 
%% answer_domain_projection([X|Xs], [D|Ds]) :-
%%     answer_domain_projection_(X, D), !,
%%     answer_domain_projection(Xs, Ds).
%% answer_check_entail([],       [],       _).
%% answer_check_entail([D1|D1s], [D2|D2s], R) :-
%%     answer_check_entail_( D1, D2, R, _),
%%     answer_check_entail(D1s, D2s, R).
%% answer_store_projection(_, St, St).

%% apply_answer([],     []).
%% apply_answer([V|Vs], [A|Ans]) :- 
%%     apply_answer_(V, A),
%%     apply_answer(Vs, Ans).
%% %% TCLP interface %%

%% :- discontiguous
%%     call_domain_projection_/2,
%%     call_entail_/2,
%%     answer_domain_projection_/2,
%%     answer_check_entail_/4,
%%     apply_answer_/2.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %% call_stack TCLP interface %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% call_domain_projection_(X, D) :- X ~> D.
%% call_entail_(stack(D1), stack(D2)) :-
%%     sub_list(D2,D1).

%% answer_domain_projection_(X, D) :- X ~> D.
%% answer_check_entail_( stack(D1), stack(D2), 1,  _) :- sub_list(D2,D1).
%% answer_check_entail_( model(_), _, 1, _).

%% apply_answer_(X, model(P)) :- X <~ model(P).
%% apply_answer_(X, stack(P)) :- \+ X ~> _, X <~ stack(P).
%% apply_answer_(X, stack(_)) :- X ~> _.



%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %% disequality TCLP interface %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% call_domain_projection_(X, List) :- dump_neg_list(X, List).
%% call_entail_( neg(List), neg(List)).

%% answer_domain_projection_(X, List) :- dump_neg_list(X, List).
%% answer_check_entail_( neg(List), neg(List), 1, _).
%% %% To use entailment...
%% % answer_check_entail_(_, neg(List1), neg(List2), 1, _) :-
%% %       entail_neg_list(List2, List1), !.
%% % answer_check_entail_(_, neg(List1), neg(List2), -1, _) :-
%% %       entail_neg_list(List1, List2).

%% apply_answer_(X, neg(List)) :- not_unify(X, List).



%% %%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %% CLP(Q) TCLP interface %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%

%% :- use_module(library(clpq/clpq_dump), [clpqr_dump_constraints/3]).
%% :- use_package(clpq).
%% call_domain_projection_(X, st(X,S)) :- clpqr_dump_constraints(X, X, S).
%% call_entail_(st(X,_S), st(X,S2)) :- clpq_entailed(S2).

%% answer_domain_projection_(X, st(X,S1)) :- clpqr_dump_constraints(X, X, S1).
%% answer_check_entail_(st(X,_S1), st(X,S2), 1, _) :- clpq_entailed(S2).
%% %% To use entailment...
%% % answer_check_entail_(_, neg(List1), neg(List2), 1, _) :-
%% %       entail_neg_list(List2, List1), !.
%% % answer_check_entail_(_, neg(List1), neg(List2), -1, _) :-
%% %       entail_neg_list(List1, List2).

%% apply_answer_(X, st(X,S1)) :- clpq_meta(S1).

%% ------------------------------------------------------------- %%

:- use_module(library(terms_check)).
sub_list(D1,D2) :-
    append(_, X, D2), subsumes_term(D1,X).

    
