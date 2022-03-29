:- use_module(library(scasp)).
:- use_module(library(scasp/human)).

#pred player(G, X) :: '@(X) played in @(G)'.
#pred winner(Game,Player) :: 'The winner of @(Game) is @(Player)'.
#pred throw(Player,Sign) :: '@(Player) threw @(Sign)'.
#pred beat(Sign,OtherSign) :: '@(Sign) beats @(OtherSign)'.

beat(rock,scissors).
beat(scissors,paper).
beat(paper,rock).

#abducible player(Game, Player).
#abducible throw(Player, Sign).

winner(Game,Player) :-
  player(Game, Player),
  player(Game, OtherPlayer),
  throw(Player,Sign),
  throw(OtherPlayer,OtherSign),
  beat(Sign,OtherSign).

