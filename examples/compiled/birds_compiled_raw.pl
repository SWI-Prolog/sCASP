% QUERY:
?- flies(X).

% USER PREDICATES:
penguin(sam).

wounded_bird(john).

bird(tweety).
bird(Var0) :-
     penguin(Var0).
bird(Var0) :-
     wounded_bird(Var0).

ab(Var0) :-
     penguin(Var0).
ab(Var0) :-
     wounded_bird(Var0).

flies(Var0) :-
     bird(Var0),
     not ab(Var0).

-flies(Var0) :-
     ab(Var0).
-flies(Var0) :-
     -bird(Var0).

-wounded_bird(Var0) :-
     not wounded_bird(Var0).

-bird(Var0) :-
     not bird(Var0).

-penguin(Var0) :-
     not penguin(Var0).

-ab(Var0) :-
     not ab(Var0).


% DUAL RULES:
not o_-ab_1(Var0) :-
     ab(Var0).

not -ab(Var0) :-
     not o_-ab_1(Var0).

not o_-penguin_1(Var0) :-
     penguin(Var0).

not -penguin(Var0) :-
     not o_-penguin_1(Var0).

not o_-wounded_bird_1(Var0) :-
     wounded_bird(Var0).

not -wounded_bird(Var0) :-
     not o_-wounded_bird_1(Var0).

not o_-bird_1(Var0) :-
     bird(Var0).

not -bird(Var0) :-
     not o_-bird_1(Var0).

not o_-flies_1(Var0) :-
     not ab(Var0).

not o_-flies_2(Var0) :-
     not -bird(Var0).

not -flies(Var0) :-
     not o_-flies_1(Var0),
     not o_-flies_2(Var0).

not o_flies_1(Var0) :-
     not bird(Var0).
not o_flies_1(Var0) :-
     bird(Var0),
     ab(Var0).

not flies(Var0) :-
     not o_flies_1(Var0).

not o_ab_1(Var0) :-
     not penguin(Var0).

not o_ab_2(Var0) :-
     not wounded_bird(Var0).

not ab(Var0) :-
     not o_ab_1(Var0),
     not o_ab_2(Var0).

not o_bird_1(Var0) :-
     Var0 \= tweety.

not o_bird_2(Var0) :-
     not penguin(Var0).

not o_bird_3(Var0) :-
     not wounded_bird(Var0).

not bird(Var0) :-
     not o_bird_1(Var0),
     not o_bird_2(Var0),
     not o_bird_3(Var0).

not o_wounded_bird_1(Var0) :-
     Var0 \= john.

not wounded_bird(Var0) :-
     not o_wounded_bird_1(Var0).

not o_penguin_1(Var0) :-
     Var0 \= sam.

not penguin(Var0) :-
     not o_penguin_1(Var0).

not o_false.


% INTEGRITY CONSTRAINTS:
not o__chk_1_1(Var0) :-
     not -ab(Var0).
not o__chk_1_1(Var0) :-
     -ab(Var0),
     not ab(Var0).

not o__chk_1_1 :-
     forall(Var0,not o__chk_1_1(Var0)).

not o_chk_1 :-
     not o__chk_1_1.

not o__chk_2_1(Var0) :-
     not -penguin(Var0).
not o__chk_2_1(Var0) :-
     -penguin(Var0),
     not penguin(Var0).

not o__chk_2_1 :-
     forall(Var0,not o__chk_2_1(Var0)).

not o_chk_2 :-
     not o__chk_2_1.

not o__chk_3_1(Var0) :-
     not -wounded_bird(Var0).
not o__chk_3_1(Var0) :-
     -wounded_bird(Var0),
     not wounded_bird(Var0).

not o__chk_3_1 :-
     forall(Var0,not o__chk_3_1(Var0)).

not o_chk_3 :-
     not o__chk_3_1.

not o__chk_4_1(Var0) :-
     not -bird(Var0).
not o__chk_4_1(Var0) :-
     -bird(Var0),
     not bird(Var0).

not o__chk_4_1 :-
     forall(Var0,not o__chk_4_1(Var0)).

not o_chk_4 :-
     not o__chk_4_1.

not o__chk_5_1(Var0) :-
     not -flies(Var0).
not o__chk_5_1(Var0) :-
     -flies(Var0),
     not flies(Var0).

not o__chk_5_1 :-
     forall(Var0,not o__chk_5_1(Var0)).

not o_chk_5 :-
     not o__chk_5_1.

global_constraint :-
     not o_chk_1,
     not o_chk_2,
     not o_chk_3,
     not o_chk_4,
     not o_chk_5.

