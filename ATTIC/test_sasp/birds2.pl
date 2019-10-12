% Given 3 birds, which can fly?

penguin(sam). % sam is a penguin
wounded_bird(john). % john is wounded
bird(tweety). % tweety is just a bird

% penguins and wounded birds are still birds
bird(X) :- penguin(X).
bird(X) :- wounded_bird(X).

% penguins and wounded birds are abnormal
ab(X) :- penguin(X).
ab(X) :- wounded_bird(X).

% birds can fly if they are not abnormal
flies(X) :- bird(X), not ab(X).
