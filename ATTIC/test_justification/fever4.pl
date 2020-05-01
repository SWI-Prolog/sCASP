%% Fever4 - introduce data to the temp register to filter


#pred diagnosis(P,D) :: 'The diagnosis of @(P:patient) is @(D)'.
diagnosis(P,'fever') :- fever(P,T).
diagnosis(P,'no fever') :- no_fever(P,T).

%% # pred fever(P,T) :: 'There is a fever for @(P:patient), with @(T:temperature)'.
%% # pred not fever(P,T) :: 'There is no fever for @(P:patient), with @(T:temperature)'.
fever(Patient,Temp) :-
    temp(Patient,Temp),
    high_temp(Temp).

%% # pred no_fever(P,T) :: 'For @(P:patient), with @(T:temperature) we conclude that there is no fever'.
%% # pred not no_fever(P,T) :: 'For @(P:patient), with @(T:temperature) we conclude that there is a fever'.
no_fever(Patient,Temp) :-
    temp(Patient,Temp),
    not high_temp(Temp).

#pred high_temp(T) :: 'It is high @(T:temperature)'.
#pred not high_temp(T) :: 'It is not high @(T:temperature)'.
high_temp(T) :- T #> 38.

%# pred temp(P,T) :: 'For @(P:patient) the @(T:temperature)'.
temp(P,T) :- temp1(P,T).
temp(P,T) :- temp2(P,T).

# pred temp1(P,T) :: 'It is known that @(P:patient) has @(T:temperature)'.
temp1(P,T) :- reg_person_temp(P,T).
# pred temp2(P,T) :: 'It is consider that @(P:patient) has @(T:temperature)'.
temp2(P,T) :- not reg_person(P).

# pred recent_reg(D) :: 'A register at @(D:time point) is recent'.
# pred not recent_reg(D) :: 'A register at @(D:time point) is not recent'.
recent_reg(D) :- D #> -3.

# pred reg_temp(P,T,D) :: 'There is a register, for @(P:patient), with @(T:temperature), at @(D:time point)'.
# pred not reg_temp(P,T,D) :: 'There is no register, for @(P:patient), with @(T:temperature), at @(D:time point)'.
reg_temp('Juan',37,-2).
reg_temp('Pedro',39,-2).
reg_temp('Jose',39,-4).
reg_temp('Jose',37,-6).

# pred reg_person_temp(P,T) :: 'There is a valid register for @(P:patient) with @(T:temperature)'.
%% # pred not reg_person_temp(P,T) :: 'There is no valid register for @(P:patient) with @(T:temperature)'.
reg_person_temp(P,T) :- reg_temp(P,T,D), recent_reg(D).
%% # pred reg_person(P) :: 'There is a valid register for @(P:patient)'. %
# pred not reg_person(P) :: 'There is no valid register for @(P:patient)'.
reg_person(P) :- reg_person_temp(P,T).

%%%%%%%%   Queries %%%%%%%

#show diagnosis/2, temp/2, reg_temp/3.

?- diagnosis('Luisa',D).   %% Two possible models
?- diagnosis('Juan',D).    %% Juan has no fever
?- diagnosis('Pedro',D).   %% Pedro has fever
%?- diagnosis('Jose',D).    %% The reg of Jose is old
?- diagnosis(X,D).         %% Give all diagnosis


