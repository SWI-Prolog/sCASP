% dimensioning

% from: https://www.swi-prolog.org/pldoc/doc/_SWI_/library/prolog_codewalk.pl

:- dynamic
        calls/2.

assert_call_graph :-
        retractall(calls(_, _)),
        prolog_walk_code([ trace_reference(_),
                           on_trace(assert_edge),
                           source(false)
                         ]),
        predicate_property(calls(_,_), number_of_clauses(N)),
        format('Got ~D edges~n', [N]).

assert_preds :-
        retractall(pred_(_)),
        prolog_walk_code([ trace_reference(_),
                           on_trace(assert_pred),
                           source(false)
                         ]),
        predicate_property(pred_(_), number_of_clauses(N)),
        format('Got ~D preds~n', [N]).

assert_edge(Callee, Caller, _Where) :-
        calls(Caller, Callee), !.
assert_edge(Callee, Caller, _Where) :-
        assertz(calls(Caller, Callee)).
        
assert_pred(Callee, Caller, _Where) :-
        (pred_(Caller); pred_(Callee)), !.
assert_pred(Callee, _Caller, _Where) :-
        \+ pred_(Callee), !, 
        assertz(pred_(Callee)).
assert_pred(_Callee, Caller, _Where) :-
        \+ pred_(Caller), 
        assertz(pred_(Caller)).

        
% count(scasp_solve)
count(M) :- 
	setof(P, (pred_(M:P)), L),
	length(L, N), 
	%print_list(L), 
	format('Got ~D predicates calls~n', [N]),
	filter_duplicates(L, [], LL),  
	length(LL, NN), 
	%print_list(LL), 
	format('with ~D predicates ~n', [NN]),
	count_clauses(M, LL, NNN), 
	format('and ~D clauses ~n', [NNN]). 
	
	
print_list([]).
print_list([P|R]) :- format('~q\n', [P]), print_list(R). 


filter_duplicates([], Final, Final). 
filter_duplicates([H|T], Pre, Final) :-
	functor(H, F, A), functor(P2, F, A),
	((\+variant(P2, Pre), \+predicate_property(P2, built_in)) -> 
		filter_duplicates(T, [P2|Pre], Final)
	;	filter_duplicates(T, Pre, Final) 
	). 
	
count_clauses(M, Preds, N) :-
	findall(P:-B, (member(P, Preds), clause(M:P,B)), L),
	%print_list(L),
	length(L, N).  

variant(E1, [E2|_]) :- E1 =@= E2. 
variant(E, [_|R]) :- variant(E, R). 



	
