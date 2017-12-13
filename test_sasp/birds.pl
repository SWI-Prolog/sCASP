% Given 3 birds, which can fly?

penguin(sam). % sam is a penguin
wounded_bird(john). % john is wounded
bird(tweety). % tweety is just a bird

% penguines and wounded birds are still birds
bird(X) :- penguin(X).
bird(X) :- wounded_bird(X).

% penguins and wounded birds are abnormal
ab(X) :- penguin(X).
ab(X) :- wounded_bird(X).

% birds can fly if they are not abnormal
flies(X) :- bird(X), not ab(X).

% explicit closed world assumptions
-flies(X) :- ab(X).
-flies(X) :- -bird(X).

-wounded_bird(X) :- not wounded_bird(X).

-bird(X) :- not bird(X).

-penguin(X) :- not penguin(X).

-ab(X) :- not ab(X).
