% Created from PAS files from
% http://www.cliplab.org/papers/sCASP-ICLP2020/,
% the data accompagnying the paper "Justifications for
% Goal-Directed Constraint Answer Set Programming" by Joaquín Arias at
% all, ICLP 2020.
%
% Load using
%
%   swipl pas.pl
%
% Run the demo query as
%
%   ?- chose(ace_inhibitors).

:- use_module('../../prolog/scasp').
:- use_module('../../prolog/scasp/human').

test :-
    set_prolog_flag(scasp_show_justification, false),
    set_prolog_flag(scasp_show_model, false),
    chose(ace_inhibitors),
    !,
    scasp_justification(Tree, []),
    human_justification_tree(Tree),
    save_tree(Tree).

save_tree(M:Tree) :-
    message_to_string(scasp_justification(Tree,
                                          [ depth(1),
                                            module(M)
                                          ]),
                      String),
    setup_call_cleanup(open(us, write, Out),
                       write(Out, String),
                       close(Out)).


:- begin_scasp(rules_pred, []).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The Physician Advisory interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#pred chose(T) :: '@(T:treatment) has been chosen'.
#pred recommendation(T) :: 'it is a recommendation to use @(T)'.
#pred discarded(T) :: '@(T) is discarded'.
% #pred available(T) :: 'available holds for @(T)'.
#pred exclude(T) :: '@(T:treatment) is excluded'.

#show chose/1, recommendation/1, discarded/1.
#show not contraindication/1.
#show not evidence/1, not history/1, not diagnosis/1.
#show not exclude/1, not discarded/1.
:- end_scasp.

:- begin_scasp(rules, []).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                        %%
%     The Physician advisory system      %%
%                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Include the Advisor interface
#include(rules_pred).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The doctor need to select certain treatments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chose(X) :- recommendation(X),  not exclude(X), not discarded(X).

discarded(X) :- not available(X).
available(X) :- not discarded(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% There are some rules to chose/recommentd treatments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Aggressive Reasoning: The aggressive reasoning pattern can be
% stated as "take an action (e.g., recommend treatment) inot f there
% is a reason; no evidence of contraindication means there is no
% danger in taking that action".

recommendation(T) :-
    reason(T),
    not contraindication(T).

% 2. Conservative Reasoning: This reasoning pattern is stated as "A
% reason for a recommendation is not enough; evidence that the
% recommendation is not harmful must be available".

recommendation(T) :-
    reason(T),
    -contraindication(T).

% 3. Anti-recommendation: The anti-recommendation pattern is stated
% as "a choice can be prohibited if evidence of contraindication can
% be found"

% contraindication/1.

% 4. Preference: The preference pattern is stated as "use the
% second-line choice when the first-line choice is not available".

recommendation(T2) :-
    second_line(T1,T2),
    reason(T1),
    contraindication(T1),
    not contraindication(T2).

% 5. Concomitant Choice: The concomitant choice pattern is stated as
% "if a choice is made, some other choices are automatically in
% effect unless they are prohibited."

exclude(T0) :-
    concomitant(T0,T),
    recommendation(T),
    not chose(T).

% 6. Indispensable Choice: The indispensable choice pattern is stated
% as "if a choice is made, some other choices must also be made; if
% those choices can't be made, then the first choice is revoked".

exclude(T0) :-
    indispensable(T0,T),
    not chose(T).

% 7. Incompatible Choice: The incompatibility pattern is stated as
% "some choices cannot be in effect at the same time"

exclude(T2) :-
    incompatibility(T1,T2),
    not discarded(T1).
exclude(T1) :-
    incompatibility(T1,T2),
    not discarded(T2).
:- end_scasp.

:- begin_scasp(guide_pred, []).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Guide Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% #pred reason(T) :: 'there is a reason for @(T)'.
#pred contraindication(T) :: 'there is a danger in taking @(T)'.
#pred -contraindication(T) :: '@(T:treatment) is not harmful'.
#pred second_line(T0,T) :: '@(T:treatment) is the second choice of @(T0)'.
#pred concomitant(T0,T) :: '@(T:treatment) is concomitant if @(T0) is chosen'.
#pred indispensable(T0,T) :: '@(T:treatment) is indisplensable if @(T0) is chosen'.
#pred incompatibility(T0,T) :: '@(T:treatment) is incompatible with @(T0)'.
:- end_scasp.


:- begin_scasp(guide, []).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHF Guide
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Include the Physician Advisor System
#include(rules).
% Include the Guide interface
#include(guide_pred).
#discontiguous(evidence/1).
#discontiguous(measurement/2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Treatments Information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1. Reason: What are the reason for a treatment

%Digoxin
% "Digoxin can be beneficial in patients with HFrEF, unless
% contraindicated, to decrease hospitalizations for HF."

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
% 2. contraindication: When is a danger in taking a treatment

%Diagoxin
contraindication(digoxin) :-
    evidence(atrioventricular_block).

%Ace_inhibitors
contraindication(ace_inhibitors):-
    history(angioedema).
contraindication(ace_inhibitors):-
    evidence(pregnancy).

%Anticoagulation
% "Anticoagulation is not recommended in patients with chronic HFrEF
% without AF, a prior thromboembolic event, or a cardioembolic
% source."
contraindication(anticoagulation) :-
    diagnosis(hf_with_reduced_ef),
    not evidence(cardioembolic_source),
    not diagnosis(atrial_fibrillation),
    not history(thromboembolism).

contraindication(exercise_training) :-
    evidence(can_not_improve_functional_status).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4. second_line: What is the second choise if a treatment is not
% available

%Arbs
% "ARBs are recommended in patients with HFrEF with current or prior
% symptoms who are ACE inhibitor intolerant, unless contraindicated,
% to reduce morbidity and mortality."

second_line(ace_inhibitors, arbs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 5. concomitat: What treatment is automatically chosen if is not
% prohibited

%Concomitat
% "Diuretics should generally be combined with an ACE inhibitor, beta
% blocker, and aldosterone antagonist.  Few patients with HF will be
% able to maintain target weight without the use of diuretics."

concomitant(ace_inhibitors, diuretics).
concomitant(beta_blockers, diuretics).
concomitant(aldosterone_antagonist, diuretics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 6. indispensables: What treatment must be chosen

%Indispensables
% "In patients with a current or recent history of fluid retention,
% beta blockers should not be prescribed without diuretics"
indispensable(beta_blockers, diuretics) :-
    history(fluid_retention).

indispensable(hydralazine, isosorbide_dinitrate).
indispensable(hydralazine, beta_blockers).
indispensable(hydralazine, ace_inhibitors).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 7. incompatibility: Which treatment can not be chosen together

%Incompatibilities
% "Routine combined use of an ACE inhibitor, ARB, and aldosterone
% antagonist is potentially harmful for patients with HFrEF."

incompatibility(ace_inhibitors,arbs).
incompatibility(aldosterone_antagonist,arbs).
incompatibility(ace_inhibitors,aldosterone_antagonist).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% AUXILIAR PREDICATES INSIDE GUIDE

% A1 evidence inference from patient information
evidence(cardioembolic_stroke_risk_factor):-
    history(hypertension).
evidence(cardioembolic_stroke_risk_factor):-
    diagnosis(diabetes).
evidence(cardioembolic_stroke_risk_factor):-
    history(stroke).
evidence(cardioembolic_stroke_risk_factor):-
    history(ischemic_attack).
evidence(cardioembolic_stroke_risk_factor):-
    evidence(over_75_years_old).

evidence(over_75_years_old):-
    measurement(age, Data), Data > 75.

% A2 diagnosis inference from patient information
diagnosis(hf_with_reduced_ef) :-
    measurement(lvef, Data), Data < 0.4.
evidence(creatinine_equal_or_less_than_2) :-
    measurement(creatinine, Data), Data =< 2.
evidence(glomerular_filtration_rate_greater_than_30) :-
    measurement(glomerular_filtration_rate, Data), Data > 30.

evidence(normal_kidney_function) :-
    evidence(creatinine_equal_or_less_than_2).
evidence(normal_kidney_function) :-
    evidence(glomerular_filtration_rate_greater_than_30).

evidence(potassium_less_than_5_0) :-
    measurement(potassium, Data), Data < 5.

evidence(lvef_equal_or_less_than_35_precent) :-
    measurement(lvef, Data), Data =< 0.35.

:- end_scasp.


:- begin_scasp(patient_pred, []).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Patient Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Evidence
#pred evidence(accf_stage_c) :: 'the patient is in ACCF stage C'.
#pred evidence(pregnancy) :: 'the patient is pregnant or planning to get pregnant'.
#pred evidence(E) :: 'the patient is/has @(E)'.

% Diagnosis
#pred diagnosis(hf_with_reduced_ef) ::
       'the patient is diagnosed with heart failure with reduced ejection fraction'.
#pred diagnosis(D) :: 'the patient is diagnosed with @(D)'.

% History
#pred history(H) :: 'the patient has a history of @(H)'.

% Measurement
#pred measurement(M,V) :: 'there is a measurement of @(M) of @(V)'.
:- end_scasp.


:- begin_scasp(patient).
% Include the Patient interface
#include(patient_pred).
#discontiguous evidence/1.
#discontiguous measurement/2.
#discontiguous history/1.

%* Demographics *%
measurement(age, 76).
evidence(african_american).
evidence(male).

%* Assessments *%
evidence(nyha_class_4).
evidence(accf_stage_c).

%* Contraindications *%
contraindication(continuous_positive_airway_pressure).

%* Diagnoses *%
diagnosis(ischemic_heart_disease).
diagnosis(hypertension).
diagnosis(diabetes).
diagnosis(atrial_fibrillation).

%* Dosages *%

%* Evidence *%
evidence(sleepApnea).
evidence(angina).

%* Illness History *%
history(stroke).
history(ischemic_attack).

%* Measurements *%
measurement(lvef, 0.35).
measurement(heart_rate, 72).
measurement(creatinine, 1.9).
measurement(glomerular_filtration_rate, 55).
measurement(potassium, 4.2).

%* Medication History *%
history(ace_inhibitors).
history(beta_blockers).
:- end_scasp.

:- begin_scasp(query,
               [ chose/1
               ]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Queries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Including CHF guides
#include(guide).

% Introducing Patient-01 profile
#include(patient).

% %---------- Anticoagulation: There are 4 reasons to recommend it
% ?- recommendation(anticoagulation).
% ?- chose(anticoagulation).

% %---------- There is one reason to recommend beta_blockers
% ?- recommendation(beta_blockers).
% %---------- Beta_blockers concomitants the choice of diuretics
% ?- chose(beta_blockers).

% %---------- Aldosterone_antagonist vs. Ace_inhibitors: They are incompatible
% %           While we can observe the reasons to be recommended
% ?- recommendation(aldosterone_antagonist), recommendation(ace_inhibitors).
% %---------- We can not choose both of them at the same time
% ?- chose(aldosterone_antagonist), chose(ace_inhibitors).      % NO MODELS
% %---------- E.g., Ace_inhibitors can be chosen:
% %  if Aldosterone_antagonist is discarded
% %  if arbs is also discarded
% % and if diuretics is chosen (it is concomitant)
 ?- chose(ace_inhibitors).


% %---------- Second-line choices: arbs is the second choice for ace_inhibitors:
% %           Scenario A: ace_inhibitors can be chosen -> arbs is not recommended
% ?- chose(arbs).   % NO MODELS

% %           Scenario B: ace_inhibitors has contraindication -> arbs is recommended
% contraindication(ace_inhibitors).  % UNCOMMENT THIS FACT
% ?- chose(arbs).

% %---------- Multiple indispensables treatments: hydralazine/isosorbide_dinitrate...
% ?- chose(hydralazine).
:- end_scasp.
