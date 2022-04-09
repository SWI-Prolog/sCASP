:-module('impossibleancestor-scasp', []).
source_lang(en).
% s(CASP) Programming 
:- use_module(library(scasp)).
% Uncomment to suppress warnings
%:- style_check(-discontiguous).
:- style_check(-singleton).
:- set_prolog_flag(scasp_forall, prev).
:- dynamic is_an_ancestor_of/2, is_a_parent_of/2, is_a_grandparent_of/2, is_impossible/1.
#pred is_an_ancestor_of(B,C) :: ' @(B:person) is an  ancestor  of  @(C:person) '.
#pred is_a_parent_of(B,C) :: ' @(B:person) is a  parent  of  @(C:person) '.
#pred is_a_grandparent_of(B,C) :: ' @(B:person) is a  grandparent  of  @(C:person) '.
#pred is_impossible(B) :: ' @(B:person) is impossible '.
is_a_grandparent_of(A, B) :-
    is_a_parent_of(A, C),
    is_a_parent_of(C, B).
is_an_ancestor_of(A, B) :-
    is_a_parent_of(A, B).
is_an_ancestor_of(A, B) :-
    is_a_parent_of(A, C),
    is_an_ancestor_of(C, B).
/* Scenario one
is_a_parent_of(john, mary).
is_a_parent_of(mary, alice).
% */ 
/* Scenario two */
is_a_parent_of(john, mary).
is_a_parent_of(mary, john).
is_impossible(A) :-
    is_an_ancestor_of(A, A).
/* % */ 
/** <examples>
?- ? is_a_grandparent_of(_170772,_170774).
?- ? is_an_ancestor_of(_170760,_170762). */
?- is_impossible(_170750).
/* **/
