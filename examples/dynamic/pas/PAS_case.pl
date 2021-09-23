:- use_module(library(scasp)).
:- use_module(library(scasp/human)).

:- use_module('PAS_rules').
:- use_module('PAS_guide').
:- use_module('PAS_patient').

case(1,
     [ case_measurement(age, 76),
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
       case_history(stroke),
       case_history(ischemic_attack),

       %* Measurements *%
       case_measurement(lvef, 0.35),
       case_measurement(heart_rate, 72),
       case_measurement(creatinine, 1.9),
       case_measurement(glomerular_filtration_rate, 55),
       case_measurement(potassium, 4.2),

       %* Medication History *%
       case_history(ace_inhibitors),
       case_history(beta_blockers)
     ]).

load_case(Case) :-
    case(Case, Data),
    patient_data(Data).

%!  solve(+Query, +Case)
%
%   Solves Query and display the result in _human_ format. The latter is
%   a hack for now, suppressing the normal output.
%
%   Examples
%
%      ?- solve(chose(ace_inhibitors), 1).

solve(Query, Case) :-
    load_case(Case),
    setup_call_cleanup(
        set_output(false, false, State),
        ( scasp(Query),
          scasp_justification(Tree, []),
          human_justification_tree(Tree),
          save_tree(Tree, us)
        ),
        restore_output(State)).

set_output(Model, Justification, s(OldModel, OldJustification)) :-
    current_prolog_flag(scasp_show_justification, OldJustification),
    current_prolog_flag(scasp_show_model, OldModel),
    set_prolog_flag(scasp_show_justification, Justification),
    set_prolog_flag(scasp_show_model, Model).

restore_output(s(OldModel, OldJustification)) :-
    set_prolog_flag(scasp_show_justification, OldJustification),
    set_prolog_flag(scasp_show_model, OldModel).

%!  save_tree(+Tree, +File)
%
%   Write the justification tree to File.   Used for comparison with the
%   original version.

save_tree(Tree, File) :-
    message_to_string(scasp_justification(Tree,
                                          [ depth(1)
                                          ]),
                      String),
    setup_call_cleanup(open(File, write, Out),
                       write(Out, String),
                       close(Out)).

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
