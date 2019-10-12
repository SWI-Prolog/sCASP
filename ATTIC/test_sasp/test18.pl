% { bird(tweety), flies(tweety), not -flies(tweety) }, { -flies(tweety), bird(tweety), not flies(tweety) }
bird(tweety).

flies(tweety) :- bird(tweety), not -flies(tweety).

-flies(tweety) :- bird(tweety), not flies(tweety).
