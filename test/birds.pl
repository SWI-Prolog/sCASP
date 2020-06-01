% Given 3 birds, which can fly?

#pred penguin(X) :: '@(X) is a penguin'.
penguin(sam). % sam is a penguin
#pred wounded_bird(X) :: '@(X) is a wounded bird'.
wounded_bird(john). % john is wounded

#pred bird(X) :: '@(X) is a bird'.
bird(tweety). % tweety is just a bird
% penguines and wounded birds are still birds
bird(X) :- penguin(X,Y), s(C).
bird(X) :- wounded_bird(X).

%% #pred ab(X) :: '@(X) is abnormal'.
%% #pred not ab(X) :: '@(X) is not abnormal'.
% penguins and wounded birds are abnormal
ab(X) :- penguin(X).
ab(X) :- wounded_bird(X).

#pred flies(X) :: '@(X) can fly'.
% birds can fly if they are not abnormal
flies(X) :- bird(X), not ab(X).

#pred -flies(X) :: '@(X) can not fly'.
% explicit closed world assumptions
-flies(X) :- ab(X).
-flies(X) :- -bird(X).

-wounded_bird(X) :- not wounded_bird(X).

-bird(X) :- not bird(X).

-penguin(X) :- not penguin(X).

-ab(X) :- not ab(X).


?- flies(X).
