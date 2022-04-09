:-module('minicontract-scasp', []).
source_lang(en).
% s(CASP) Programming 
:- use_module(library(scasp)).
% Uncomment to suppress warnings
%:- style_check(-discontiguous).
:- style_check(-singleton).
:- set_prolog_flag(scasp_forall, prev).
:- dynamic the_service_recipient_maintains_all_communication_within_the_confines_of/1, the_service_recipient_delivers_requested_information_before/1, is_also_signed_by_the_service_recipient/1, 'The_terms_of_the_contract_are_met'/0, is_signed_by_the_service_provider/1, the_service_is_delivered_before/1, is_a_valid_contract/1.
#pred the_service_recipient_maintains_all_communication_within_the_confines_of(B) :: ' the  service  recipient  maintains  all  communication  within  the  confines  of  @(B:domain) '.
#pred the_service_recipient_delivers_requested_information_before(B) :: ' the  service  recipient  delivers  requested  information before @(B:date) '.
#pred is_also_signed_by_the_service_recipient(B) :: ' @(B:contract) is also  signed  by  the  service  recipient '.
#pred 'The_terms_of_the_contract_are_met' :: ' The  terms  of  the  contract  are  met '.
#pred is_signed_by_the_service_provider(B) :: ' @(B:contract) is signed  by  the  service  provider '.
#pred the_service_is_delivered_before(B) :: ' the  service is delivered before @(B:date) '.
#pred is_a_valid_contract(B) :: ' @(B:contract) is a  valid  contract '.
is_a_valid_contract(A) :-
    is_signed_by_the_service_provider(A),
    is_also_signed_by_the_service_recipient(A).
'The_terms_of_the_contract_are_met' :-
    the_service_is_delivered_before(1654423200.0),
    the_service_recipient_maintains_all_communication_within_the_confines_of(domain),
    the_service_recipient_delivers_requested_information_before(1654077600.0).
/* Scenario one */
the_service_is_delivered_before(1654423200.0).
the_service_recipient_maintains_all_communication_within_the_confines_of(domain).
the_service_recipient_delivers_requested_information_before(1654077600.0).
is_signed_by_the_service_provider(the_contract).
is_also_signed_by_the_service_recipient(the_contract).
/* % */ 
/** <examples> */
?- is_a_valid_contract(_200388).
/* ?- ? 'The_terms_of_the_contract_are_met'.
**/
