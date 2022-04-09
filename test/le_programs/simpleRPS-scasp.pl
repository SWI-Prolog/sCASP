:-module('simpleRPS-scasp', []).
source_lang(en).
% s(CASP) Programming 
:- use_module(library(scasp)).
% Uncomment to suppress warnings
%:- style_check(-discontiguous).
:- style_check(-singleton).
:- set_prolog_flag(scasp_forall, prev).
:- dynamic the_game_is_a_draw/0, inputs_and/3, gets/2, beats/2.
#pred the_game_is_a_draw :: ' the  game is a  draw '.
#pred inputs_and(B,C,D) :: ' @(B:person)  inputs  @(C:choice) and @(D:amount) '.
#pred gets(B,C) :: ' @(B:person)  gets  @(C:amount) '.
#pred beats(B,C) :: ' @(B:choice)  beats  @(C:choice) '.
beats(scissors, paper).
beats(paper, rock).
beats(rock, scissors).
gets(A, B) :-
    inputs_and(A, C, D),
    inputs_and(E, F, G),
    A\=E,
    beats(C, F),
    B is D+G.
the_game_is_a_draw :-
    inputs_and(A, B, _),
    inputs_and(C, B, _),
    A\=C.
gets(A, B) :-
    the_game_is_a_draw,
    inputs_and(A, _, B).
/* Scenario mbj */
inputs_and(miguel, paper, 100).
inputs_and(bob, paper, 1000).
/* % */ 
/** <examples> */
?- gets(Who,How_much).
/* ?- ? the_game_is_a_draw.
?- ? jacinto\=bob.
**/

