%Assume each task takes exactly 1 hour, and each time slot is 1 hour

make_schedule:-schedule(breakfast,_),
           schedule(lunch,_),
           schedule(dinner,_),
           schedule(asp_coding_1,_),
           schedule(asp_coding_2,_),
           schedule(homework,_),
           schedule(homework_discussion,_),
           schedule(snack,_).

constraint(breakfast,T):-T>=6,T<10.
constraint(lunch,T):-T>=11,T<13.
constraint(dinner,T):-T>=17,T<19.
constraint(snack,T):-schedule(lunch,T1),schedule(dinner,T2),T>T1,T<T2.

constraint(homework_discussion,T):-schedule(homework,T1),T>T1.
           
constraint(asp_coding_1,_).
constraint(asp_coding_2,_).
constraint(homework,_).

time_slot(X):-X>=0,X<24.

%A task may or may not be scheduled at a specific time.
schedule(Task,Time):-time_slot(Time),constraint(Task,Time),not not_schedule(Task,Time).
not_schedule(Task,Time):-not schedule(Task,Time).

