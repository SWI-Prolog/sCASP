:- use_module(library(scasp)).

:- use_module('PAS_rules').
:- use_module('PAS_guide').
:- use_module('PAS_patient').

case(1,
     [ measurement(age, 76),
       case_evidence(african_american),
       case_evidence(male),

       %* Assessments *%
       case_evidence(nyha_class_4),
       case_evidence(accf_stage_c),

       %* Contraindications *%
       case_contraindication(continuous_positive_airway_pressure),

       %* Diagnoses *%
       case_diagnosis(ischemic_heart_disease),
       case_diagnosis(hypertension),
       case_diagnosis(diabetes),
       case_diagnosis(atrial_fibrillation),

       %* Dosages *%

       %* Evidence *%
       case_evidence(sleepApnea),
       case_evidence(angina),

       %* Illness History *%
       history(stroke),
       history(ischemic_attack),

       %* Measurements *%
       measurement(lvef, 0.35),
       measurement(heart_rate, 72),
       measurement(creatinine, 1.9),
       measurement(glomerular_filtration_rate, 55),
       measurement(potassium, 4.2),

       %* Medication History *%
       history(ace_inhibitors),
       history(beta_blockers)
     ]).

%!  solve(+Query, +Case)
%
%   Examples
%
%      ?- solve(chose(ace_inhibitors), 1).

solve(Query, Case) :-
    case(Case, Data),
    patient_data(Data),
    scasp(Query).


%% %---------- Anticoagulation: There are 4 reasons to recommend it
% ?- recommendation(anticoagulation).
% ?- chose(anticoagulation).

%% %---------- There is one reason to recommend beta_blockers
% ?- recommendation(beta_blockers).
%% %---------- Beta_blockers concomitants the choice of diuretics
% ?- chose(beta_blockers).

%% %---------- Aldosterone_antagonist vs. Ace_inhibitors: They are incompatible
%% %           While we can observe the reasons to be recommended
% ?- recommendation(aldosterone_antagonist), recommendation(ace_inhibitors).
%% %---------- We can not choose both of them at the same time
% ?- chose(aldosterone_antagonist), chose(ace_inhibitors).      % NO MODELS
%% %---------- E.g., Ace_inhibitors can be chosen:
%% %  if Aldosterone_antagonist is discarded
%% %  if arbs is also discarded
%% % and if diuretics is chosen (it is concomitant)
% ?- chose(ace_inhibitors).


%% %---------- Second-line choices: arbs is the second choice for ace_inhibitors:
%% %           Scenario A: ace_inhibitors can be chosen -> arbs is not recommended
% ?- chose(arbs).   % NO MODELS

%% %           Scenario B: ace_inhibitors has contraindication -> arbs is recommended
% contraindication(ace_inhibitors).  % UNCOMMENT THIS FACT
% ?- chose(arbs).

%% %---------- Multiple indispensables treatments: hydralazine/isosorbide_dinitrate...
% ?- chose(hydralazine).
