:-module('obligation-scasp', []).
source_lang(en).
% s(CASP) Programming 
:- use_module(library(scasp)).
% Uncomment to suppress warnings
%:- style_check(-discontiguous).
:- style_check(-singleton).
:- set_prolog_flag(scasp_forall, prev).
:- dynamic it_is_permitted_that_designates_by_at_that/4, cures_the_failure_of_on_that/4, pays_to_on/4, notifies_on_that/4, fails_to_fulfil_that/3, is_on_or_before/2, performs_at/3, has_that/3, occurs_at/2, defaults_on/2.
#pred cures_the_failure_of_on_that(B,C,D,E) :: ' @(B:borrower)  cures  the  failure  of  @(C:obligation) on @(D:day)  that  @(E:requirement) '.
#pred pays_to_on(B,C,D,E) :: ' @(B:borrower)  pays  @(C:amount)  to  @(D:lender) on @(E:date) '.
#pred notifies_on_that(B,C,D,E) :: ' @(B:lender)  notifies  @(C:borrower) on @(D:date)  that  @(E:message) '.
#pred fails_to_fulfil_that(B,C,D) :: ' @(B:borrower)  fails  to  fulfil  @(C:obligation)  that  @(D:requirement) '.
#pred performs_at(B,C,D) :: ' @(B:party)  performs  @(C:action) at @(D:time) '.
#pred has_that(B,C,D) :: ' @(B:person)  has  @(C:obligation)  that  @(D:requirement) '.
#pred occurs_at(B,C) :: ' @(B:event)  occurs at @(C:time) '.
#pred defaults_on(B,C) :: ' @(B:borrower)  defaults on @(C:date) '.
has_that(the_borrower, _, pays_to_on(the_borrower, 550, the_lender, 1433116800.0)).
has_that(the_borrower, _, pays_to_on(the_borrower, 525, the_lender, 1464739200.0)).
defaults_on(the_borrower, A) :-
    has_that(the_borrower, B, C),
    fails_to_fulfil_that(the_borrower, B, C),
    notifies_on_that(the_lender,
                     the_borrower,
                     D,
                     fails_to_fulfil_that(the_borrower, B, C)),
    is_days_after(A, 2, D),
    not cures_the_failure_of_on_or_before_that(the_borrower, B, E, A, C).
cures_the_failure_of_on_or_before_that(The_borrower, B, E, A, C) :-
	cures_the_failure_of_on_that(The_borrower, B, E, C),
        is_on_or_before(E, A). 
fails_to_fulfil_that(the_borrower, _, pays_to_on(the_borrower, A, the_lender, B)) :-
    not pays_to_on(the_borrower, A, the_lender, B).
cures_the_failure_of_on_that(the_borrower, _, A, pays_to_on(the_borrower, B, the_lender, _)) :-
    pays_to_on(the_borrower, B, the_lender, A).
is_on_or_before(A, B) :-
    is_days_after(B, C, A),
    C#>=0.
is_days_after(B,C,A) :-
    B #= C+A. 
/* Scenario test */
notifies_on_that(the_lender, the_borrower, 1464825600.0, fails_to_fulfil_that(the_borrower, _, pays_to_on(the_borrower, 525, the_lender, 1464739200.0))).
pays_to_on(the_borrower, 525, the_lender, 1464825600.0).
/* % */ 
/** <examples>
?- ? defaults_on(_362960,_362962). */
?- cures_the_failure_of_on_that(the_borrower,_362936,_362938,pays_to_on(the_borrower,_362946,the_lender,_362950)).
/* **/
