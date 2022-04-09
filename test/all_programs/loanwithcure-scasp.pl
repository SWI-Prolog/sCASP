:-module('loanwithcure-scasp', []).
source_lang(en).
% s(CASP) Programming 
:- use_module(library(scasp)).
% Uncomment to suppress warnings
:- style_check(-discontiguous).
:- style_check(-singleton).
:- set_prolog_flag(scasp_forall, prev).
:- dynamic cures_the_failure_of_on_or_before/3, pays_to_on/4, notifies_on_that/4, fails_on_to_fulfil/3, the_loan_is_accelerated_on/1, is_on_or_before/2, is_that/2, defaults_on/2, has/2.
#pred cures_the_failure_of_on_or_before(B,C,D) :: ' @(B:borrower)  cures  the  failure  of  @(C:obligation) onorbefore @(D:date) '.
#pred pays_to_on(B,C,D,E) :: ' @(B:borrower)  pays  @(C:amount)  to  @(D:lender) on @(E:date) '.
#pred notifies_on_that(B,C,D,E) :: ' @(B:lender)  notifies  @(C:borrower) on @(D:date)  that  @(E:message) '.
#pred fails_on_to_fulfil(B,C,D) :: ' @(B:borrower)  fails on @(C:date)  to  fulfil  @(D:obligation) '.
#pred the_loan_is_accelerated_on(B) :: ' the  loan is accelerated on @(B:date) '.
#pred is_that(B,C) :: ' @(B:obligation) is that  @(C:description) '.
#pred defaults_on(B,C) :: ' @(B:borrower)  defaults on @(C:date) '.
#pred has(B,C) :: ' @(B:person)  has  @(C:obligation) '.
is_on_or_before(A, B) :-
    is_days_after(B, C, A),
    C#>=0.
has(the_borrower, obligation1).
is_that(obligation1, pays_to_on(the_borrower, 550, the_lender, 1433116800.0)).
has(the_borrower, obligation2).
is_that(obligation2, pays_to_on(the_borrower, 525, the_lender, 1464739200.0)).
defaults_on(the_borrower, A) :-
    has(the_borrower, B),
    fails_on_to_fulfil(the_borrower, C, B),
    notifies_on_that(the_lender,
                     the_borrower,
                     D,
                     fails_on_to_fulfil(the_borrower, C, B)),
    is_days_after(A, 2, D),
    not cures_the_failure_of_on_or_before(the_borrower, B, A).
cures_the_failure_of_on_or_before(the_borrower, A, B) :-
    is_that(A, pays_to_on(the_borrower, C, the_lender, _)),
    pays_to_on(the_borrower, C, the_lender, D),
    notifies_on_that(the_borrower,
                     the_lender,
                     E,
                     pays_to_on(the_borrower, C, the_lender, D)),
    is_on_or_before(D, B),
    is_on_or_before(E, B).
fails_on_to_fulfil(the_borrower, A, B) :-
    is_that(B, pays_to_on(the_borrower, C, the_lender, A)),
    not pays_to_on(the_borrower, C, the_lender, A).
/* Scenario payment */
notifies_on_that(the_lender, the_borrower, 1464998400.0, fails_on_to_fulfil(the_borrower, 1464739200.0, obligation2)).
pays_to_on(the_borrower, 525, the_lender, 1465084800.0).
notifies_on_that(the_borrower, the_lender, 1465171200.0, pays_to_on(the_borrower, 525, the_lender, 1465084800.0)).
/* % */ 
/** <examples> */
?- defaults_on(Who,When).
/* **/
