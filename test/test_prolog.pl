:- use_module('../prolog/scasp/prolog').
:- use_module('../prolog/scasp/stack').

:- meta_predicate
    proof(0).

proof(Goal) :-
    scasp_prolog(Goal, Tree),
    print_justification_tree(Tree, []).
