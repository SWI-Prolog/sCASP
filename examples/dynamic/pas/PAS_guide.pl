:- module(pas_guide,
          [ reason/1,
            contraindication/1,
            '-contraindication'/1,
            concomitant/2,
            indispensable/2,
            second_line/2,
            incompatibility/2
          ]).
:- use_module(library(scasp)).
:- use_module('PAS_rules').
:- use_module('PAS_patient').

:- discontiguous
    evidence/1,
    diagnosis/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CHF Guide
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Guide Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% :- pred reason(T) :: 'there is a reason for @(T)'.
:- pred contraindication('T') ::
        'there is a danger in taking @(T)'.
:- pred -contraindication('T') ::
        '@(T:treatment) is not harmful'.
:- pred second_line('T0','T') ::
        '@(T:treatment) is the second choice of @(T0)'.
:- pred concomitant('T0','T') ::
        '@(T:treatment) is concomitant if @(T0) is chosen'.
:- pred indispensable('T0','T') ::
        '@(T:treatment) is indisplensable if @(T0) is chosen'.
:- pred incompatibility('T0','T') ::
        '@(T:treatment) is incompatible with @(T0)'.

:- pred diagnosis(hf_with_reduced_ef) ::
        'the patient is diagnosed with heart failure with reduced \c
         ejection fraction'.

:- pred diagnosis('D') ::
        'the patient is diagnosed with @(D)'.

:- pred evidence(accf_stage_c) ::
        'the patient is in ACCF stage C'.
:- pred evidence(pregnancy) ::
        'the patient is pregnant or planning to get pregnant'.
:- pred evidence('E') ::
        'the patient is/has @(E)'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Treatments Information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 1. Reason: What are the reason for a treatment

%Digoxin
%% "Digoxin can be beneficial in patients with HFrEF, unless
%% contraindicated, to decrease hospitalizations for HF."

reason(digoxin) :-
    evidence(accf_stage_c),
    diagnosis(hf_with_reduced_ef).

%Beta_blocker
reason(beta_blockers) :-
    evidence(accf_stage_c),
    diagnosis(hf_with_reduced_ef).

%Anticoagulation
reason(anticoagulation) :-
    evidence(accf_stage_c),
    diagnosis(hf_with_reduced_ef),
    evidence(cardioembolic_stroke_risk_factor),
    diagnosis(atrial_fibrillation).

%Ace_inhibitors
reason(ace_inhibitors) :-
    evidence(accf_stage_c),
    diagnosis(hf_with_reduced_ef).

%Diuretics
reason(diuretics) :-
    evidence(accf_stage_c),
    diagnosis(hf_with_reduced_ef).

%aldosterone_antagonist
reason(aldosterone_antagonist) :-
    evidence(accf_stage_c),
    evidence(lvef_equal_or_less_than_35_precent),
    evidence(nyha_class_4),
    evidence(normal_kidney_function),
    evidence(potassium_less_than_5_0).

%Hydralazine and Isosorbide Dinitrate
reason(hydralazine) :-
    evidence(accf_stage_c),
    evidence(nyha_class_4),
    evidence(african_american),
    not discarded(ace_inhibitors),
    not discarded(beta_blockers).

reason(isosorbide_dinitrate) :-
    evidence(accf_stage_c),
    evidence(nyha_class_4),
    evidence(african_american),
    not discarded(ace_inhibitors),
    not discarded(beta_blockers).


% Nonpharmacological Interventions
reason(specific_education) :-
    evidence(accf_stage_c),
    diagnosis(hf_with_reduced_ef).

reason(exercise_training) :-
    evidence(accf_stage_c),
    diagnosis(hf_with_reduced_ef).

reason(sodium_restriction) :-
    evidence(accf_stage_c),
    diagnosis(hf_with_reduced_ef).

reason(continuous_positive_airway_pressure) :-
    evidence(accf_stage_c),
    diagnosis(hf_with_reduced_ef),
    evidence(sleepApnea).

reason(cardiac_rehabilitation) :-
    evidence(accf_stage_c),
    diagnosis(hf_with_reduced_ef).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 2. contraindication: When is a danger in taking a treatment

%Diagoxin
contraindication(digoxin) :-
    evidence(atrioventricular_block).

%Ace_inhibitors
contraindication(ace_inhibitors):-
    case_history(angioedema).
contraindication(ace_inhibitors):-
    evidence(pregnancy).

%Anticoagulation
%% "Anticoagulation is not recommended in patients with chronic HFrEF
%% without AF, a prior thromboembolic event, or a cardioembolic
%% source."
contraindication(anticoagulation) :-
    diagnosis(hf_with_reduced_ef),
    not evidence(cardioembolic_source),
    not diagnosis(atrial_fibrillation),
    not case_history(thromboembolism).

contraindication(exercise_training) :-
    evidence(can_not_improve_functional_status).

contraindication(X) :-
    case_contraindication(X).

-contraindication(X) :-
    -case_contraindication(X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4. second_line: What is the second choise if a treatment is not
%% available

%Arbs
%% "ARBs are recommended in patients with HFrEF with current or prior
%% symptoms who are ACE inhibitor intolerant, unless contraindicated,
%% to reduce morbidity and mortality."

second_line(ace_inhibitors, arbs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 5. concomitat: What treatment is automatically chosen if is not
%% prohibited

%Concomitat
%% "Diuretics should generally be combined with an ACE inhibitor, beta
%% blocker, and aldosterone antagonist.  Few patients with HF will be
%% able to maintain target weight without the use of diuretics."

concomitant(ace_inhibitors, diuretics).
concomitant(beta_blockers, diuretics).
concomitant(aldosterone_antagonist, diuretics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 6. indispensables: What treatment must be chosen

%Indispensables
%% "In patients with a current or recent case_history of fluid retention,
%% beta blockers should not be prescribed without diuretics"
indispensable(beta_blockers, diuretics) :-
    case_history(fluid_retention).

indispensable(hydralazine, isosorbide_dinitrate).
indispensable(hydralazine, beta_blockers).
indispensable(hydralazine, ace_inhibitors).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7. incompatibility: Which treatment can not be chosen together

%Incompatibilities
%% "Routine combined use of an ACE inhibitor, ARB, and aldosterone
%% antagonist is potentially harmful for patients with HFrEF."

incompatibility(ace_inhibitors,arbs).
incompatibility(aldosterone_antagonist,arbs).
incompatibility(ace_inhibitors,aldosterone_antagonist).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% AUXILIAR PREDICATES INSIDE GUIDE

% A1 evidence inference from patient information
evidence(cardioembolic_stroke_risk_factor):-
    case_history(hypertension).
evidence(cardioembolic_stroke_risk_factor):-
    diagnosis(diabetes).
evidence(cardioembolic_stroke_risk_factor):-
    case_history(stroke).
evidence(cardioembolic_stroke_risk_factor):-
    case_history(ischemic_attack).
evidence(cardioembolic_stroke_risk_factor):-
    evidence(over_75_years_old).

evidence(over_75_years_old):-
    case_measurement(age, Data), Data > 75.

% A2 diagnosis inference from patient information
diagnosis(hf_with_reduced_ef) :-
    case_measurement(lvef, Data), Data < 0.4.
evidence(creatinine_equal_or_less_than_2) :-
    case_measurement(creatinine, Data), Data =< 2.
evidence(glomerular_filtration_rate_greater_than_30) :-
    case_measurement(glomerular_filtration_rate, Data), Data > 30.

evidence(normal_kidney_function) :-
    evidence(creatinine_equal_or_less_than_2).
evidence(normal_kidney_function) :-
    evidence(glomerular_filtration_rate_greater_than_30).

evidence(potassium_less_than_5_0) :-
    case_measurement(potassium, Data), Data < 5.

evidence(lvef_equal_or_less_than_35_precent) :-
    case_measurement(lvef, Data), Data =< 0.35.

evidence(X) :-
    case_evidence(X).
diagnosis(X) :-
    case_diagnosis(X).

