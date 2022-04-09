:-module('family-scasp+http://tests.com', []).
source_lang(en).
% s(CASP) Programming 
:- use_module(library(scasp)).
% Uncomment to suppress warnings
%:- style_check(-discontiguous).
:- style_check(-singleton).
:- set_prolog_flag(scasp_forall, prev).
:- dynamic is_a_parent_of/2, is_a_grandparent_of/2.
#pred is_a_parent_of(B,C) :: ' @(B:person) is a  parent  of  @(C:person) '.
#pred is_a_grandparent_of(B,C) :: ' @(B:person) is a  grandparent  of  @(C:person) '.
is_a_grandparent_of(A, B) :-
    is_a_parent_of(A, C),
    is_a_parent_of(C, B).
/* Scenario one */
is_a_parent_of(john, mary).
is_a_parent_of(mary, john).
% */ 
/** <examples> */
?- is_a_grandparent_of(_102530,_102532).
/* **/
