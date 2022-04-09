:-module('turingcomplete-scasp', []).
source_lang(en).
% s(CASP) Programming 
:- use_module(library(scasp)).
% Uncomment to suppress warnings
%:- style_check(-discontiguous).
:- style_check(-singleton).
:- set_prolog_flag(scasp_forall, prev).
:- dynamic says_that_and_lead_to_and_after_performing/6, it_changes_from_to_and_from_to/5, is_left_after_and_is_left_after/4, moves_to_and_to/5, the_head_of_is_leaving/3, some_tm_goes_from_to/2.
some_tm_goes_from_to(A, B) :-
    it_changes_from_to_and_from_to(q0, [], C, A, D),
    reverse(C, E),
    append(E, D, B).
it_changes_from_to_and_from_to(qf, A, A, B, B).
it_changes_from_to_and_from_to(A, B, C, D, E) :-
    the_head_of_is_leaving(D, F, G),
    says_that_and_lead_to_and_after_performing(_,
                                               A,
                                               F,
                                               H,
                                               I,
                                               J),
    has_as_head_before(K, I, G),
    moves_to_and_to(J, B, L, K, M),
    it_changes_from_to_and_from_to(H, L, C, M, E).
the_head_of_is_leaving([], b, []).
the_head_of_is_leaving(A, B, C) :-
    has_as_head_before(A, B, C).
moves_to_and_to(left, A, B, C, D) :-
    is_left_after_and_is_left_after(A, B, C, D).
moves_to_and_to(stay, A, A, B, B).
moves_to_and_to(right, A, B, C, D) :-
    has_as_head_before(B, E, A),
    has_as_head_before(C, E, D).
is_left_after_and_is_left_after([], [], A, B) :-
    has_as_head_before(B, b, A).
is_left_after_and_is_left_after(A, B, C, D) :-
    has_as_head_before(A, E, B),
    has_as_head_before(D, E, C).
/* Scenario machine one */
says_that_and_lead_to_and_after_performing(rule1, q0, 1, q0, 1, right).
says_that_and_lead_to_and_after_performing(rule2, q0, b, qf, 1, stay).
/* % */ 
/** <examples> */
?- some_tm_goes_from_to([1,1,1,b],FinalState).
/* **/
