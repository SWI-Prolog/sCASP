%% Fever4 - introduce data to the temp register to filter


# pred diagnosis(P,D) :: 'The diagnosis of @(P:patient) is @(D)'.
diagnosis(P,'fever') :- fever(P,T).
diagnosis(P,'no fever') :- no_fever(P,T).

# pred fever(P,T) :: 'There is a fever for @(P:patient) with @(T:temperature)'.
# pred not fever(P,T) :: 'There is no fever for @(P:patient) with @(T:temperature)'.
fever(Patient,Temp) :-
    temp(Patient,Temp),
    high_temp(Temp),
    not no_fever(Patient,Temp).

# pred no_fever(P,T) :: 'For @(P:patient) with @(T:temperature) we conclude that there is no fever'.
# pred not no_fever(P,T) :: 'For @(P:patient) with @(T:temperature) we conclude that there is a fever'.
no_fever(Patient,Temp) :-
    temp(Patient,Temp),
    not high_temp(Temp),
    not fever(Patient,Temp).

# pred high_temp(T) :: 'A temperature with @(T:value) is high'.
# pred not high_temp(T) :: 'A temperature with @(T:value) is not high'.
high_temp(T) :- T #> 38.

# pred temp(P,T) :: 'The temperature for @(P:patient) has @(T:value)'.
temp(P,T) :- reg_temp(P,T,D), recent_reg(D).
temp(P,T) :- not reg_person(P).

# pred recent_reg(D) :: 'A register at @(D:time point) is recent'.
# pred not recent_reg(D) :: 'A register at @(D:time point) is not recent'.
recent_reg(D) :- D #> -3.

# pred reg_temp(P,T,D) :: 'There is a register, for @(P:patient), with @(T:temperature), at @(D:time point)'.
# pred not reg_temp(P,T,D) :: 'There is no register, for @(P:patient), with @(T:temperature), at @(D:time point)'.
reg_temp('Juan',37,-2).
reg_temp('Pedro',39,-2).
reg_temp('Jose',39,-4).
reg_temp('Jose',37,-6).

# pred reg_person(P) :: 'There is a valid register for @(P:patient)'.
# pred not reg_person(P) :: 'There is no valid register for @(P:patient)'.
reg_person(P) :- reg_temp(P,T,D), recent_reg(D).

%%%%%%%%   Queries %%%%%%%
?- diagnosis('Luisa',T).   %% Two possible models
?- diagnosis('Juan',T).    %% Juan has no fever
?- diagnosis('Pedro',T).   %% Pedro has fever
?- diagnosis('Jose',T).    %% The reg of Peter is old
