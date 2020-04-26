

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

temp(P,T) :- reg_temp(P,T).
temp(P,T) :- not reg_person(P).

# pred reg_temp(Person,Temp) :: 'The registered temperarture of @(Person) is @(Temp)'.
# pred not reg_temp(Person,Temp) :: 'We do not have a registered temperature for @(Person) of @(Temp)'.
reg_temp('Juan',37).
reg_temp('Pedro',39).


# pred not reg_person(Person) :: 'There are no registerd temperature of @(Person)'.
reg_person(P) :- reg_temp(P,T).


%%%%%%%%   Queries %%%%%%%
?- diagnosis('Luisa',T).   %% Two possible models
%?- diagnosis('Juan',T).    %% Juan has no fever
%?- diagnosis('Pedro',T).   %% Pedro has fever
