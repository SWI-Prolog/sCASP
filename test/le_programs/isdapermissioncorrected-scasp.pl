:-module('isdapermissioncorrected-scasp', []).
source_lang(en).
% s(CASP) Programming 
:- use_module(library(scasp)).
% Uncomment to suppress warnings
%:- style_check(-discontiguous).
:- style_check(-singleton).
:- set_prolog_flag(scasp_forall, prev).
:- dynamic 'Automatic_Early_Termination_applies_to_for_of_Default'/2, of_Default_occurs_with_respect_to_at/3, is_not_more_than_days_after/3, gives_notice_to_at_that/4, is_on_or_before/2, the_Schedule_specifies_that/1, it_is_permitted_that/1, is_continuing_at/2, occurs_at/2, designates_that/2.
#pred 'Automatic_Early_Termination_applies_to_for_of_Default'(B,C) :: ' Automatic  Early  Termination  applies  to  @(B:party)  for  @(C:event)  of  Default '.
#pred of_Default_occurs_with_respect_to_at(B,C,D) :: ' @(B:event)  of  Default  occurs  with  respect  to  @(C:party) at @(D:time) '.
#pred is_not_more_than_days_after(B,C,D) :: ' @(B:time) isnot more  than  @(C:number)  days after @(D:time) '.
#pred gives_notice_to_at_that(B,C,D,E) :: ' @(B:party)  gives  notice  to  @(C:party) at @(D:time)  that  @(E:message) '.
#pred is_on_or_before(B,C) :: ' @(B:date) isonorbefore @(C:date) '.
#pred the_Schedule_specifies_that(B) :: ' the  Schedule  specifies  that  @(B:specification) '.
#pred it_is_permitted_that(B) :: ' it is permitted  that  @(B:eventuality) '.
#pred is_continuing_at(B,C) :: ' @(B:event) is continuing at @(C:time) '.
#pred occurs_at(B,C) :: ' @(B:event)  occurs at @(C:time) '.
#pred designates_that(B,C) :: ' @(B:party)  designates  that  @(C:eventuality) '.
it_is_permitted_that(designates_that(A, occurs_at('Early_Termination_in_respect_of_all_outstanding_Transactions', B))) :-
    of_Default_occurs_with_respect_to_at(C, D, E),
    is_continuing_at(C, F),
    gives_notice_to_at_that(A,
                            D,
                            F,
                            occurs_at(C, E)),
    is_on_or_before(F, B),
    is_not_more_than_days_after(B, 20, F),
    not the_Schedule_specifies_that('Automatic_Early_Termination_applies_to_for_of_Default'(D, C)).
/* Scenario one */
of_Default_occurs_with_respect_to_at(event001, 'Bob', 'Monday').
is_continuing_at(event001, 'Tuesday').
gives_notice_to_at_that('Alice', 'Bob', 'Tuesday', occurs_at(event001, 'Monday')).
is_on_or_before('Tuesday', 'Friday').
is_not_more_than_days_after('Friday', 20, 'Tuesday').
/* % */ 
/** <examples> */
?- it_is_permitted_that(designates_that(_288372,occurs_at(_288378,_288380))).
/** ?- ? not the_Schedule_specifies_that('Automatic_Early_Termination_applies_to_for_of_Default'('Bob',e001)).
?- ? the_Schedule_specifies_that('Automatic_Early_Termination_applies_to_for_of_Default'(_288336,_288338)).
**/
