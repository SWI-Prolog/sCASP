:- module(read_cov,
          [ load_cover/1
          ]).

:- use_module(library(pio)).

:- dynamic clauseAt/3.

lines([])           --> call(eos), !.
lines([Line|Lines]) --> meaningful_line(Line), !, lines(Lines).
lines([Line|Lines]) --> rest_of_margin, line(Line), lines(Lines).

eos([], []).

line('')   --> ( "\n" ; call(eos) ), !.
line(Line) --> [_], line(Line).

meaningful_line((N,Symbol,Predicate)) --> 
    margin((N,Symbol)), spaces, predicate(String), end_of_rule_head, !,
    {%format("-------------- ~w ~w |~w|\n", [N, Symbol, String]), 
     check_term_string(Predicate, String)}. 

check_term_string(F/N, String) :-
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

predicate(Predicate) --> 
    predicate_name(NameInCodes), !, % { string_codes(Name, NameInCodes), format(" ~w ", [Name]) }, 
    list_of_arguments(0, ArgsInCodes), % { string_codes(Args, ArgsInCodes), format(" ~w \n", [Args]) },  
    {append(NameInCodes, ArgsInCodes, PredicateInCodes), string_codes(Predicate, PredicateInCodes)}.

list_of_arguments(0, [])    --> []. 
list_of_arguments(1, [41])  --> ")", !. % close the list
list_of_arguments(N, [40|R]) --> [40], !, {NN is N+1}, list_of_arguments(NN, R). % ( 
list_of_arguments(N, [41|R])  --> [41], !, {NN is N-1}, list_of_arguments(NN, R). % )
list_of_arguments(N, R)     --> "\n", !, rest_of_margin, list_of_arguments(N, R). 
list_of_arguments(N, [L|R]) -->  [L], %{(N=0, not(member(L, [40, 41, 44, 46]))); true}, 
    !, list_of_arguments(N, R). 

predicate_name([L|R]) -->  [L], {not(member(L, [40, 41, 44, 46,58, 59, 62, 60, 61]))}, !, predicate_name(R). % (),.:;><= these are not allowed in predicate names
predicate_name([])    --> []. 

end_of_rule_head --> colon_dash ; right_arrow. 

colon_dash --> spaces, ":-". 
right_arrow --> spaces, "=>". 

write_cover([]).
write_cover([(N,S,F)|Rest]) :- format(" ~w ~w ~w \n", [N,S,F]), 
    write_cover(Rest). 
write_cover([_|Rest]) :- %format(" ~w \n", [N]), 
    write_cover(Rest). 

assert_cover([]).
assert_cover([(N,S,F)|Rest]) :- %format(" ~w ~w ~w \n", [N,S,F]), 
    assertz(clauseAt(N,S,F)), 
    assert_cover(Rest). 
assert_cover([_|Rest]) :- %format(" ~w \n", [N]), 
    assert_cover(Rest). 

load_cover(File) :-
    %format("loading file ~w\n", [File]), 
    phrase_from_file(lines(Ls), File), 
    %format("found lines ~w\n", [Ls]), 
    assert_cover(Ls). 

% Sample usage: ?- phrase_from_file(lines(Ls), 'test.cov'), write_cover(Ls).
