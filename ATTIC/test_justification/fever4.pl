%% Fever4 - introduce data to the temp register to filter



# pred diagnosis(P,T) :: 'The diagnosis for @(P) with a temperature of @(T) is that:'.
diagnosis(P,T) :- fever(P,T).
diagnosis(P,T) :- no_fever(P,T).

# pred fever(Person,Temp) :: '@(Person) has a fever'.
# pred no_fever(Person,Temp) :: '@(Person) has no fever'.
fever(Person,Temp) :-
    temp(Person,Temp),
    high_temp(Temp),
    not no_fever(Person,Temp).

no_fever(Person,Temp) :-
    temp(Person,Temp),
    not high_temp(Temp),
    not fever(Person,Temp).

high_temp(T) :- T #> 38.

temp(P,T) :- recent_reg(D), reg_temp(P,T,D).
temp(P,T) :- not reg_person(P).

# pred recent_reg(D) :: 'The registered temperatured is valid. @(D) is greater than -3'.
# pred not recent_reg(D) :: 'The registered temperatured has expired. @(D) is not greater than -3'.
recent_reg(D) :- D #> -3.

# pred reg_temp(Person,Temp,D) :: 'The registered temperarture of @(Person) is @(Temp) at time @(D)'.
# pred not reg_temp(Person,Temp,D) :: 'We do not have a registered temperature for @(Person) of @(Temp) at time @(D)'.
reg_temp('Juan',37,-2).
reg_temp('Pedro',39,-2).
reg_temp('Jose',39,-4).
reg_temp('Jose',37,-6).

# pred not reg_person(Person) :: 'There are no valid registerd temperature of @(Person)'.
%reg_person(P) :- reg_temp(P,T,D).
reg_person(P) :- reg_temp(P,T,D), recent_reg(D).

%%%%%%%%   Queries %%%%%%%
?- diagnosis('Luisa',T).   %% Two possible models
?- diagnosis('Juan',T).    %% Juan has no fever
?- diagnosis('Pedro',T).   %% Pedro has fever
?- diagnosis('Jose',T).    %% The reg of Peter is old
