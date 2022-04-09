:-module('citizenshiptrust-scasp', []).
source_lang(en).
% s(CASP) Programming 
:- use_module(library(scasp)).
% Uncomment to suppress warnings
%:- style_check(-discontiguous).
:- style_check(-singleton).
:- set_prolog_flag(scasp_forall, prev).
:- dynamic is_settled_in_the_UK_on/2, is_born_in_on/3, is_a_British_citizen_on/2, is_the_mother_of/2, is_the_father_of/2, is_qualified_to_determine_fatherhood/1, acquires_British_citizenship_on/2, says_that/2, is_after_commencement/1.
#pred is_settled_in_the_UK_on(B,C) :: ' @(B:person) is settled in the  UK on @(C:date) '.
#pred is_born_in_on(B,C,D) :: ' @(B:person) is born in @(C:place) on @(D:date) '.
#pred is_a_British_citizen_on(B,C) :: ' @(B:person) is a  British  citizen on @(C:date) '.
#pred is_the_mother_of(B,C) :: ' @(B:person) is the  mother  of  @(C:person) '.
#pred is_the_father_of(B,C) :: ' @(B:person) is the  father  of  @(C:person) '.
#pred is_qualified_to_determine_fatherhood(B) :: ' @(B:person) is qualified  to  determine  fatherhood '.
#pred acquires_British_citizenship_on(B,C) :: ' @(B:person)  acquires  British  citizenship on @(C:date) '.
#pred says_that(B,C) :: ' @(B:person)  says  that  @(C:sentence) '.
#pred is_after_commencement(B) :: ' @(B:date) isafter commencement '.
acquires_British_citizenship_on(A, B) :-
    is_born_in_on(A, the_UK, B),
    is_after_commencement(B),
    is_a_parent_of(C, A),
    is_citizen_or_settled(C, B). 
is_a_parent_of(C,A) :-
	is_the_mother_of(C, A).
is_a_parent_of(C,A) :-
	is_the_father_of(C, A).
is_citizen_or_settled(C, B) :-
	is_a_British_citizen_on(C, B).
is_citizen_or_settled(C, B) :-
	is_settled_in_the_UK_on(C, B).
is_the_father_of(A, B) :-
    says_that(C, is_the_father_of(A, B)),
    is_qualified_to_determine_fatherhood(C).
is_the_father_of(A, B) :-
    says_that(A, is_the_father_of(A, B)).
/* Scenario alice
is_born_in_on('John', the_UK, 1633737600.0).
is_after_commencement(1633737600.0).
is_the_mother_of('Alice', 'John').
is_a_British_citizen_on('Alice', 1633737600.0).
% */ 
/* Scenario harry
is_born_in_on('John', the_UK, 1633737600.0).
is_after_commencement(1633737600.0).
is_the_father_of('Harry', 'John').
is_settled_in_the_UK_on('Harry', 1633737600.0).
% */ 
/* Scenario trust_harry
is_born_in_on('John', the_UK, 1633737600.0).
is_after_commencement(1633737600.0).
says_that('Harry', is_the_father_of('Harry', 'John')).
is_settled_in_the_UK_on('Harry', 1633737600.0).
% */ 
/* Scenario alice_harry
is_born_in_on('John', the_UK, 1633737600.0).
is_after_commencement(1633737600.0).
is_the_mother_of('Alice', 'John').
says_that('Alice', is_the_father_of('Harry', 'John')).
is_qualified_to_determine_fatherhood('Alice').
is_settled_in_the_UK_on('Harry', 1633737600.0).
% */ 
/** <examples> */
?- acquires_British_citizenship_on(_340998,_341000).
/* */
