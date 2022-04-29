:- use_module(library(pio)).

lines([])           --> call(eos), !.
lines([Line|Lines]) --> meaningful_line(Line), !, lines(Lines).
lines([Line|Lines]) --> rest_of_margin, line(Line), lines(Lines).

eos([], []).

line('')   --> ( "\n" ; call(eos) ), !.
line(Line) --> [_], line(Line).

meaningful_line((N,Symbol,Predicate)) --> 
    margin((N,Symbol)), spaces, predicate(PredicateInCodes),  
    {string_codes(String, PredicateInCodes), !, check_term_string(Predicate, String)}. 

check_term_string(F/N-Predicate, String) :-
    catch(
        ( term_string(Predicate, String), functor(Predicate, F, N) ), 
    _Exception, (%format("Caught: ~q~n",[Exception]), 
    fail)).

margin((L,S))   --> pre_margin(L), symbol_margin(S), rest_of_margin. 

pre_margin(N) --> before_127m, spaces, number_(N), rest_of_margin. 

before_127m --> ("127m"; call(eos)). 
before_127m --> [_], before_127m.  

number_(N) --> digits_(Digits), {Digits\=[], number_codes(N, Digits)}. 

digits_([]) --> (" "; call(eos)), !. 
digits_([N|R]) --> [N], digits_(R). 

symbol_margin('+') --> "m+", !.
symbol_margin('-') --> "m-", !.
symbol_margin('#') --> "m#", !.
symbol_margin(S)  --> [_], symbol_margin(S). 

rest_of_margin --> ("0m";call(eos)), !.
rest_of_margin --> [_], rest_of_margin. 

spaces --> " ", spaces. 
spaces --> [].

predicate(End)    --> end_of_rule_head(End), !. 
predicate(R)     --> "\n", !, rest_of_margin, predicate(R). 
predicate([L|R]) -->  [L], predicate(R). 

end_of_rule_head(End) --> colon_dash(End) ; right_arrow(End). 

colon_dash(End) --> ")", spaces, ":-", {string_codes(")", End)}.
right_arrow(End) --> ")", spaces, "=>", {string_codes(")", End)}. 

write_cover([]).
write_cover([(N,S,F)|Rest]) :- format(" ~w ~w ~w \n", [N,S,F]), write_cover(Rest). 
write_cover([_|Rest]) :- %format(" ~w \n", [N]), 
    write_cover(Rest). 


% Sample usage: ?- phrase_from_file(lines(Ls), 'test.cov'), write_cover(Ls).
