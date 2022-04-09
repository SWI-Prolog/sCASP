:-module('list-scasp', []).
source_lang(en).
% s(CASP) Programming 
:- use_module(library(scasp)).
% Uncomment to suppress warnings
%:- style_check(-discontiguous).
:- style_check(-singleton).
:- set_prolog_flag(scasp_forall, prev).
:- dynamic the_concatenation_of_then_is/3, is_a_subset_of/2, followed_by_is/3, is_a_set/1, belongs_to/2.
#pred the_concatenation_of_then_is(B,C,D) :: ' the  concatenation  of  @(B:list) then @(C:list) is @(D:list) '.
#pred is_a_subset_of(B,C) :: ' @(B:set) is a  subset  of  @(C:set) '.
#pred is_a_set(B) :: ' @(B:thing) is a  set '.
#pred belongs_to(B,C) :: ' @(B:thing)  belongs  to  @(C:set) '.
is_a_subset_of(A, B) :-
    is_a_set(A),
    is_a_set(B),
    forall(belongs_to(C, A), belongs_to(C, B)).
the_concatenation_of_then_is([], A, A).
the_concatenation_of_then_is([A|B], C, [A|D]) :-
    the_concatenation_of_then_is(B, C, D).
followed_by_is(A, B, C) :-
    has_as_head_before(C, A, B).
forall(A,B) :- not(wrong(A,B)).
wrong(A,B) :- once(A), not(once(B)).  
 
/* Scenario one */
is_a_set(family_one).
is_a_set(family_two).
belongs_to('Bob', family_one).
belongs_to('Alice', family_one).
belongs_to('Alice', family_two).
/* % */ 
/** <examples> */
?- is_a_subset_of(Family_one,Family_two).
/** ?- ? is_a_subset_of(family_two,family_one).
?- ? is_a_subset_of(_259336,family_one).
?- ? is_a_subset_of(family_two,_259326).
?- ? is_a_subset_of(_259312,which_family).
?- ? has_as_head_before(_259286,a,[b,c]).
?- ? followed_by_is(_259260,_259262,[b,c]).
?- ? the_concatenation_of_then_is(_259228,_259230,[a,b,c]).
**/
