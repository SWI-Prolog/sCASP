:-module('itispermittedthat-scasp', []).
source_lang(en).
% s(CASP) Programming 
:- use_module(library(scasp)).
% Uncomment to suppress warnings
%:- style_check(-discontiguous).
:- style_check(-singleton).
:- set_prolog_flag(scasp_forall, prev).
:- dynamic 'Automatic_Early_Termination_applies_to_for_of_Default'/2, of_Default_occurs_with_respect_to_at/3, gives_notice_to_at_that/4, and_are_at_most_days_apart/3, is_on_or_before/2, designates_at_that/3, the_Schedule_specifies_that/1, 'It_is_permitted_that'/1, is_continuing_at/2, occurs_at/2.
#pred 'Automatic_Early_Termination_applies_to_for_of_Default'(B,C) :: ' Automatic  Early  Termination  applies  to  @(B:party)  for  @(C:Event)  of  Default '.
#pred of_Default_occurs_with_respect_to_at(B,C,D) :: ' @(B:Event)  of  Default  occurs  with  respect  to  @(C:party) at @(D:time) '.
#pred gives_notice_to_at_that(B,C,D,E) :: ' @(B:party)  gives  notice  to  @(C:party) at @(D:time)  that  @(E:message) '.
#pred is_on_or_before(B,C) :: ' @(B:date) isonorbefore @(C:date) '.
#pred designates_at_that(B,C,D) :: ' @(B:party)  designates at @(C:time)  that  @(D:eventuality) '.
#pred the_Schedule_specifies_that(B) :: ' the  Schedule  specifies  that  @(B:specification) '.
#pred 'It_is_permitted_that'(B) :: ' It is permitted  that  @(B:eventuality) '.
#pred is_continuing_at(B,C) :: ' @(B:Event) is continuing at @(C:time) '.
#pred occurs_at(B,C) :: ' @(B:event)  occurs at @(C:time) '.
'It_is_permitted_that'(designates_at_that(A, B, occurs_at('Early_Termination_in_respect_of_all_outstanding_Transactions', C))) :-
    of_Default_occurs_with_respect_to_at(D, E, F),
    is_continuing_at(D, B),
    gives_notice_to_at_that(A,
                            E,
                            B,
                            occurs_at(D, F)),
    is_on_or_before(B, C),
    and_are_at_most_days_apart(C, B, 20),
    not the_Schedule_specifies_that('Automatic_Early_Termination_applies_to_for_of_Default'(E, D)).
the_Schedule_specifies_that('Automatic_Early_Termination_applies_to_for_of_Default'(the_other_party, the_Event)).
/* Scenario one */
of_Default_occurs_with_respect_to_at(event001, 'Bob', 'Monday').
is_continuing_at(event001, 'Tuesday').
gives_notice_to_at_that('Alice', 'Bob', 'Tuesday', occurs_at(event001, 'Monday')).
is_on_or_before('Tuesday', 'Friday').
and_are_at_most_days_apart('Friday', 'Tuesday', 20).
/* % */ 
/** <examples> */
?- 'It_is_permitted_that'(designates_at_that(_262628,_262630,occurs_at(_262636,_262638))).
/* **/
