:- use_module(library(pio)).

lines(_, [])           --> call(eos), !.
lines(N, [N-Line|Lines]) --> meaningful_line(N, NN, Line), !, lines(NN, Lines).
lines(N, [N-Line|Lines]) --> line(Line), {NN is N+1}, lines(NN, Lines).

eos([], []).

line('')   --> ( "\n" ; call(eos) ), !.
line(Line) --> [_], line(Line).

meaningful_line(N, NN, Symbol-Functor) --> 
    margin(Symbol), rest_stat, spaces, predicate(N, NN, Predicate),  {atom_codes(Functor, Predicate)}. 

margin('+') --> ("m+" ; call(eos)), !.
margin('-') --> ("m-" ; call(eos)), !.
margin('-') --> ("m#" ; call(eos)), !.
margin(S)   --> [_], margin(S). 

rest_stat --> [L], {L\=32}, rest_stat. 
rest_stat --> [].

spaces --> " ", spaces. 
spaces --> [].

predicate(N, NN, [])    --> colon_dash, line(_), {NN is N+1}. 
predicate(N, NN, R)     --> "\n", {N1 is N+1}, predicate(N1, NN, R). 
predicate(N, NN, [L|R]) -->  [L], predicate(N, NN, R). 

colon_dash --> ":-". 

write_cover([]).
write_cover([N-(S-F)|Rest]) :- format(" ~w ~w ~w \n\n\n", [N,S,F]), write_cover(Rest). 
write_cover([N-_|Rest]) :- format(" ~w \n", [N]), write_cover(Rest). 


% Sample usage: ?- phrase_from_file(lines(1, Ls), 'test.cov').
