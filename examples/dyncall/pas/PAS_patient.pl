:- module(pas_patient,
          [ patient_data/1,

            case_measurement/2,
            case_history/1,
            case_diagnosis/1,
            case_evidence/1,
            case_contraindication/1,
            '-case_contraindication'/1
          ]).
:- use_module(library(scasp)).

:- scasp_dynamic((
    case_measurement/2,
    case_history/1,
    case_diagnosis/1,
    case_evidence/1,
    case_contraindication/1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Patient Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Evidence
:- pred case_evidence(accf_stage_c) ::
        'the patient is in ACCF stage C [case data]'.
:- pred case_evidence(pregnancy) ::
        'the patient is pregnant or planning to get pregnant [case data]'.
:- pred case_evidence('E') ::
        'the patient is/has @(E) [case data]'.

%% Diagnosis
:- pred case_diagnosis('D') ::
        'the patient is diagnosed with @(D) [case data]'.

%% History
:- pred case_history('H') ::
        'the patient has a history of @(H) [case data]'.

%% Measurement
:- pred case_measurement('M','V') ::
        'there is a measurement of @(M) of @(V) [case data]'.

patient_data(Data) :-
    clean,
    maplist(finding, Data).

finding(Term) :-
    predicate_property(Term, dynamic),
    !,
    scasp_assert(Term).
finding(Term) :-
    domain_error(patient_data, Term).

clean :-
    scasp_abolish(case_measurement/2),
    scasp_abolish(case_history/1),
    scasp_abolish(case_diagnosis/1),
    scasp_abolish(case_evidence/1),
    scasp_abolish(case_contraindication/1).
