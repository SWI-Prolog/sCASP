:-module('subset-scasp', []).
source_lang(en).
% s(CASP) Programming 
:- use_module(library(scasp)).
% Uncomment to suppress warnings
%:- style_check(-discontiguous).
:- style_check(-singleton).
:- set_prolog_flag(scasp_forall, prev).
:- dynamic is_a_subset_of/2, is_a_set/1, belongs_to/2.
#pred is_a_subset_of(B,C) :: ' @(B:set) is a  subset  of  @(C:set) '.
#pred is_a_set(B) :: ' @(B:thing) is a  set '.
#pred belongs_to(B,C) :: ' @(B:thing)  belongs  to  @(C:set) '.
is_a_subset_of(A, B) :-
    is_a_set(A),
    is_a_set(B),
    forall(belongs_to(C, A), belongs_to(C, B)).
forall(A,B) :- not(A, not(B)). 
/* Scenario one 
is_a_set(family_one).
is_a_set(family_two).
belongs_to('Bob', family_one).
belongs_to('Alice', family_one).
belongs_to('Alice', family_two).
% */ 
/* Scenario two */
is_a_set(['Alice', 'Bob']).
is_a_set(['Alice']).
belongs_to(A, B) :-
    member(A, B).
/* % */ 
/** <examples> */
?- is_a_subset_of(SubSet,Set).
/** ?- ? is_a_subset_of(_10660,_10662).
**/
