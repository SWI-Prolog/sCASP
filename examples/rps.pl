:- use_module(library(scasp)).

#pred player(_G, _X) :: '@(X) played in @(G)'.
#pred winner(_Game, _Player) :: 'The winner of @(Game) is @(Player)'.
#pred throw(_Player, _Sign) :: '@(Player) threw @(Sign)'.
#pred beat(_Sign, _OtherSign) :: '@(Sign) beats @(OtherSign)'.

beat(rock,scissors).
beat(scissors,paper).
beat(paper,rock).

#abducible player(_Game, _Player).
#abducible throw(_Player, _Sign).

winner(Game,Player) :-
  player(Game, Player),
  player(Game, OtherPlayer),
  throw(Player,Sign),
  throw(OtherPlayer,OtherSign),
  beat(Sign,OtherSign).



