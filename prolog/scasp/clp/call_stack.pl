:- module(scasp_clp_call_stack,
          [ (~>)/2,
            (<~)/2,
            dump_rules/3,

            op(700, xfx, ~>),
            op(700, xfx, <~)
          ]).

/** <module> Call stack constraint solver

This  module  contains  the  code  to  handle `StackIn`,  `StackOut` and
`Model`  as  attributes  in  order  to  check  entailment with  the TCLP
framework of CIAO. ~>/2 is the predicate used to get the  attribute from
the attributed variable. <~/2 is the predicate used to  put the  term as
an attribute.

@author Joaquin Arias
*/


:- use_module(disequality, [not_unify/2, op(_,_,.\=.)]).


A ~> Att :- get_attr(A, clp_call_stack, rules(Att)).
A <~ Att :- put_attr(A, clp_call_stack, rules(Att)).

dump_rules([],     [],     []).
dump_rules([X|Xs], [_|Ns], [D|Ds]) :-
    get_attr(X, clp_call_stack, rules(D)),
    dump_rules(Xs, Ns, Ds).
dump_rules([X|Xs], Ns, Ds) :-
    \+ get_attr(X, clp_call_stack, rules(_)),
    dump_rules(Xs, Ns, Ds).


		 /*******************************
		 *        ATTRIBUTE HOOKS	*
		 *******************************/

:- multifile
    attr_unify_hook/2,
    attribute_goals/3,
    attr_portray_hook/2.

attr_unify_hook(rules(Att), B) :-
    get_attr(B, clp_call_stack, rules(AttB)),
    Att = AttB.
attr_unify_hook(neg(A), B) :- not_unify(B,A).

attribute_goals(X) -->
    [X ~> G],
    { get_attr(X, clp_call_stack, rules(G))
    }.
attribute_goals(X) -->
    [X.\=.G],
    { get_attr(X, clp_call_stack, neg(G))
    }.

attr_portray_hook(rules(Att), A) :- format(" ~w  .is ~w ", [A, Att]).
attr_portray_hook(neg(Att),   A) :- format("~w.\\=.~w", [A, Att]).


