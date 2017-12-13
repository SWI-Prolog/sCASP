% false.
bird(tux).
penguin(tux).
bird(tweety).
chicken(tweety).

flies(tux) :- bird(tux), not -flies(tux).

-flies(tux) :- bird(tux), not flies(tux).
-flies(tux) :- penguin(tux).

flies(tweety) :- bird(tweety), not -flies(tweety).

-flies(tweety) :- bird(tweety), not flies(tweety).
-flies(tweety) :- penguin(tweety).

:- flies(tux), -flies(tux).
:- flies(tweety), -flies(tweety).

flies(tux).
