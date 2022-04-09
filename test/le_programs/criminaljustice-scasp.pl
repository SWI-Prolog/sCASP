:-module('criminaljustice-scasp', []).
source_lang(en).
% s(CASP) Programming 
:- use_module(library(scasp)).
% Uncomment to suppress warnings
%:- style_check(-discontiguous).
:- style_check(-singleton).
:- set_prolog_flag(scasp_forall, prev).
:- dynamic has_with_in_under_of/6, exception_to_applies_to_under_of/4, has_been_made_aware_that/2, has_in_under/4, is_in_the_language/2, is_in_danger_of/2, is_an_exception_to/2, is_represented_by/2, is_involved_in/2, is_in/3, has_in/3, understands/2, speaks/2, knows/2, is_ongoing/1, has/2.
#pred has_with_in_under_of(B,C,D,E,F,G) :: ' @(B:person)  has  @(C:right)  with  @(D:thing) in @(E:proceeding)  under  @(F:article)  of  @(G:law) '.
#pred exception_to_applies_to_under_of(B,C,D,E) :: ' exception  to  @(B:article)  applies  to  @(C:person)  under  @(D:article)  of  @(E:law) '.
#pred has_been_made_aware_that(B,C) :: ' @(B:person)  has  been  made  aware  that  @(C:fact) '.
#pred has_in_under(B,C,D,E) :: ' @(B:person)  has  @(C:right) in @(D:proceeding)  under  @(E:article) '.
#pred is_in_the_language(B,C) :: ' @(B:proceeding) isin the  language  @(C:language) '.
#pred is_in_danger_of(B,C) :: ' @(B:person) isin danger  of  @(C:thing) '.
#pred is_an_exception_to(B,C) :: ' @(B:exception) is an  exception  to  @(C:article) '.
#pred is_represented_by(B,C) :: ' @(B:person) is represented  by  @(C:lawyer) '.
#pred is_involved_in(B,C) :: ' @(B:person) is involved in @(C:proceeding) '.
#pred is_in(B,C,D) :: ' @(B:person) is @(C:status) in @(D:proceeding) '.
#pred has_in(B,C,D) :: ' @(B:person)  has  @(C:right) in @(D:proceeding) '.
#pred understands(B,C) :: ' @(B:person)  understands  @(C:language) '.
#pred speaks(B,C) :: ' @(B:person)  speaks  @(C:language) '.
#pred knows(B,C) :: ' @(B:person)  knows  @(C:language) '.
#pred is_ongoing(B) :: ' @(B:proceeding) is ongoing '.
#pred has(B,C) :: ' @(B:proceeding)  has  @(C:status) '.
has_with_in_under_of(A, the_right_to_interpretation, the_trial, B, 'article_2_(_1_)', 'Directive_2010_(_64_)') :-
    is_in(A, criminal_subject, B),
    is_in_the_language(B, C),
    not knows(A, C).
has_with_in_under_of(A, the_right_to_interpretation, B, C, 'article_2_(_2_)', 'Directive_2010_(_64_)') :-
    is_in(A, criminal_subject, C),
    is_represented_by(A, B),
    knows(B, D),
    not knows(A, D).
has_with_in_under_of(A, the_right_to_access_materials, the_trial, B, 'article_7_(_3_)', 'Directive_2012_(_13_)') :-
    is_in(A, criminal_subject, B),
    has(A, document_New_Evidence),
    not exception_to_applies_to_under_of('article_7_(_3_)', A, _, _).
exception_to_applies_to_under_of('article_7_(_3_)', A, 'article_7_(_4_)', 'Directive_2012_(_13_)') :-
    is_in_danger_of(A, B),
    member(B, [life, fundamental_rights, public_interest]).
has_with_in_under_of(A, the_right_to_interpretation, B, C, article_10, 'Italian_law') :-
    is_in(A, criminal_subject, C),
    is_represented_by(A, B),
    knows(B, D),
    not knows(A, D).
is_in(A, criminal_subject, B) :-
    is_involved_in(A, B),
    is_a(B, criminal),
    has_been_made_aware_that(A, is_involved_in(A, B)),
    is_ongoing(B).
is_in(A, criminal_subject, B) :-
    is_involved_in(A, B),
    is_a(B, europeanArrestWarrant),
    has_been_made_aware_that(A, is_involved_in(A, B)),
    is_ongoing(B).
is_involved_in(A, B) :-
    is_in(A, accused, B).
is_involved_in(A, B) :-
    is_in(A, suspect, B).
is_ongoing(A) :-
    has(A, started),
    not has(A, concluded).
knows(A, B) :-
    speaks(A, B),
    understands(A, B).
/* Scenario one
is_in(galileo, suspect, proc001).
has_been_made_aware_that(galileo, is_involved_in(galileo, proc001)).
understands(galileo, german).
is_represented_by(galileo, francesco).
speaks(francesco, german).
understands(francesco, german).
is_a(proc001, criminal).
has(proc001, started).
member(proc001, german).
has(galileo, document_New_Evidence).
% */ 
/* Scenario two
is_in_danger_of(galileo, life).
% */ 
/** <examples> */
?- exception_to_applies_to_under_of(_512492,_512494,_512496,_512498).
/* ?- ? has_with_in_under_of(_512472,_512474,_512476,_512478,_512480,_512482).
**/
