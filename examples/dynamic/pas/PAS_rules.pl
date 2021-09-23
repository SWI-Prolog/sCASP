:- module(pas_rules,
          [ chose/1,
            discarded/1
          ]).
:- use_module(library(scasp)).
:- use_module('PAS_guide').
:- use_module('PAS_patient').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                        %%
%%     The Physician advisory system      %%
%%                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred chose('T') :: '@(T:treatment) has been chosen'.
:- pred recommendation('T') :: 'it is a recommendation to use @(T)'.
:- pred discarded('T') :: '@(T) is discarded'.
:- pred available('T') :: 'available holds for @(T)'.
:- pred exclude('T') :: '@(T:treatment) is excluded'.

:- show chose/1, recommendation/1, discarded/1.
:- show not contraindication/1.
:- show not evidence/1, not history/1, not diagnosis/1.
:- show not exclude/1, not discarded/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The doctor need to select certain treatments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chose(X) :- recommendation(X),  not exclude(X), not discarded(X).

discarded(X) :- not available(X).
available(X) :- not discarded(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% There are some rules to chose/recommentd treatments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 1. Aggressive Reasoning: The aggressive reasoning pattern can be
%% stated as "take an action (e.g., recommend treatment) inot f there
%% is a reason; no evidence of contraindication means there is no
%% danger in taking that action".

recommendation(T) :-
    reason(T),
    not contraindication(T).

%% 2. Conservative Reasoning: This reasoning pattern is stated as "A
%% reason for a recommendation is not enough; evidence that the
%% recommendation is not harmful must be available".

recommendation(T) :-
    reason(T),
    -contraindication(T).

%% 3. Anti-recommendation: The anti-recommendation pattern is stated
%% as "a choice can be prohibited if evidence of contraindication can
%% be found"

%% contraindication/1.

%% 4. Preference: The preference pattern is stated as "use the
%% second-line choice when the first-line choice is not available".

recommendation(T2) :-
    second_line(T1,T2),
    reason(T1),
    contraindication(T1),
    not contraindication(T2).

%% 5. Concomitant Choice: The concomitant choice pattern is stated as
%% "if a choice is made, some other choices are automatically in
%% effect unless they are prohibited."

exclude(T0) :-
    concomitant(T0,T),
    recommendation(T),
    not chose(T).

%% 6. Indispensable Choice: The indispensable choice pattern is stated
%% as "if a choice is made, some other choices must also be made; if
%% those choices can't be made, then the first choice is revoked".

exclude(T0) :-
    indispensable(T0,T),
    not chose(T).

%% 7. Incompatible Choice: The incompatibility pattern is stated as
%% "some choices cannot be in effect at the same time"

exclude(T2) :-
    incompatibility(T1,T2),
    not discarded(T1).
exclude(T1) :-
    incompatibility(T1,T2),
    not discarded(T2).
