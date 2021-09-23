:- module(pas_patient,
          [ patient_data/1,

            measurement/2,
            history/1,
            case_diagnosis/1,
            case_evidence/1,
            case_contraindication/1
          ]).
:- use_module(library(scasp)).

:- thread_local
    measurement/2,
    history/1,
    case_diagnosis/1,
    case_evidence/1,
    case_contraindication/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Patient Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Evidence
:- pred evidence(accf_stage_c) :: 'the patient is in ACCF stage C'.
:- pred evidence(pregnancy) :: 'the patient is pregnant or planning to get pregnant'.
:- pred evidence('E') :: 'the patient is/has @(E)'.

%% Diagnosis
:- pred diagnosis(hf_with_reduced_ef) ::
       'the patient is diagnosed with heart failure with reduced ejection fraction'.
:- pred diagnosis('D') :: 'the patient is diagnosed with @(D)'.

%% History
:- pred history('H') :: 'the patient has a history of @(H)'.

%% Measurement
:- pred measurement('M','V') :: 'there is a measurement of @(M) of @(V)'.

patient_data(Data) :-
    clean,
    maplist(finding, Data).

finding(Term) :-
    predicate_property(Term, dynamic),
    !,
    assertz(Term).
finding(Term) :-
    domain_error(patient_data, Term).

clean :-
    retractall(measurement(_,_)),
    retractall(history(_)),
    retractall(case_diagnosis(_)),
    retractall(case_evidence(_)),
    retractall(case_contraindication(_)).
