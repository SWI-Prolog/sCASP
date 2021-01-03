

%p(X, Y, Z):- all_pos(X, Y), all_pos(Y, Z).

% When the above works, try the one below
p(X, Y, Z):- all_pos(X, Y), all_pos(Y, Z), all_pos(X, Z).

all_pos(A, B):- A .>. B.
all_pos(A, B):- A .=<. B.

q:- not p(X, Y, Z).


?-  not q. %% Should succeed
