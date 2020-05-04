:- module(scasp_io, [
    load_program/1,
    write_program/0,
    process_query/3,
    ask_for_more_models/0,
    allways_ask_for_more_models/0,
    init_counter/0,
    increase_counter/0,
%    print_output/1,
    print_justification_tree/1, % justification tree
    print_model/1,      % model
    print_unifier/2,    % bindings
%    print_term/3,
    pretty_term/4,
    print_check_calls_calling/2,
    if_user_option/2,
    set/2,
    parse_args/3,
    current_option/2,
    counter/2,
    set_options/1,
    answer_counter/1
    ]).


%% ------------------------------------------------------------- %%
:- use_package(assertions).
:- doc(title, "Module for input / output predicates").
:- doc(author, "Joaquin Arias").
:- doc(filetype, module).

:- doc(module, "

This module contains the code used to load, parser, translate and
print the program and results of the evaluation. It uses the
implementation of s(ASP) by @em{Marple} ported to CIAO by @em{Joaquin
Arias} in the folder @file{./src/sasp/}.

").

%% ------------------------------------------------------------- %%

:- use_module('./sasp/output').
:- reexport('./sasp/output', [
    pr_rule/2,
    pr_query/1,
    pr_user_predicate/1,
    pr_table_predicate/1,
    pr_show_predicate/1,
    pr_pred_predicate/1
                            ]).
:- use_module('./sasp/main').

%% ------------------------------------------------------------- %%

:- op(700, xfx, ['#=' ,
                 '#<>',
                 '#<' ,
                 '#>' ,
                 '#=<',
                 '#>='
                 ]).

:- op(700, xfx, ['::']).

:- op(700, xfx, ['│']). %% such as


%% ------------------------------------------------------------- %%

:- pred load_program(Files) : list(Files) #"Call s(aso) to generate
    and assert the translation of the progam (with dual and
    nmr_check)".

:- dynamic loaded_file/1.
load_program([]) :-
    display('ERROR: No imput file specified!'),nl,nl,
    help. %% halt.
load_program(X) :-
    retractall(loaded_file(_)),
    (
        list(X) ->
        Files = X
    ;
        Files = [X]
    ),
    main(['-g'| Files]),
    assert(loaded_file(Files)).

:- pred write_program/0 #"Call c(asp) to print the source code of the
translation of the programs already loaded by @pred{load_program/1}".

write_program :-
    loaded_file(Files),
    main(['-d0'|Files]).

:- dynamic cont/0.

:- pred process_query(Q, Query, TotalQuery) #"Initialize internal
flags to allows the generation of multiples models in the interaction
and top-level mode (even when the query is ground). Returns in
@var{TotalQuery} a list with the sub_goals in @var{Q} and
@em{global_constraints} to run the nmr_check".

process_query(Q,Query,TotalQuery) :-
    revar(Q,A),
    (
        list(A) -> As = A ; As = [A]
    ),
    (
        As = [not(_)|_] ->
        Query = [true|As]
    ;
        Query = As
    ),
    retractall(cont),
    (
        ground(Query) -> assert(cont) ; true
    ),
    ( current_option(no_nmr,on) ->
        append(Query, [true], TotalQuery)
    ;
        append(Query, [global_constraints], TotalQuery)
    ).

:- pred ask_for_more_models/0 #"Ask if the user want to generate more
models (interactive and top-level mode)".

ask_for_more_models :-
    (
        cont, print('next ? '), get_char(R),true, R \= '\n' ->
        get_char(_),
        statistics(runtime,_),
        fail
    ;
        true
    ).

:- pred ask_for_more_models/0 #"Ask if the user want to generate more
models (execution from console)".

allways_ask_for_more_models :-
    (
        print(' ? '), get_char(R),true, R \= '\n' ->
        get_char(_),
        nl,
        statistics(runtime,_),
        fail
    ;
        true
    ).

:- pred init_counter/0 #"Reset the value of answer_counter to 0".

:- dynamic answer_counter/1.
init_counter :-
    retractall(answer_counter(_)),
    assert(answer_counter(0)).

:- pred increase_counter/0 #"Add 1 to the current value of
answer_counter".

increase_counter :-
    answer_counter(N),
    N1 is N + 1,
    retractall(answer_counter(N)),
    assert(answer_counter(N1)).

%% :- pred print_output(StackOut) #"Print the justification tree using
%% @var{StackOut}, the final call stack".

%% Print output predicates to presaent the results of the query
%% print_output(StackOut) :-
%%     print_stack([],_,StackOut), nl,
%%     true.

:- pred print_justification_tree(StackOut) #"Print the justification
tree using @var{StackOut}, the final call stack".

%% Print output predicates to presaent the results of the query
print_justification_tree(StackOut) :-
    format('\nJUSTIFICATION_TREE:',[]),
    print_tree_stack(StackOut),
    true.

:- pred print_model(Model) #"Print the partial model of the program
using @var{Model}.".

%% The model is obtained from the model.
% TODO: use the StackOut instead of the model.
print_model(Model) :-
    format('\nMODEL:\n',[]),
    select_printable_literals(Model,Printable),
    print_model_(Printable), nl.

:- pred print_unifier(Vars,PVars) #" Predicate to print @var{PVars} =
@var{Vars} the binding of the variables in the query".

print_unifier(Bindings,PVars) :-
    format('BINDINGS:',[]),
    print_unifier_(Bindings,PVars).
    
print_unifier_([],[]).
print_unifier_([Binding|Bs],[PV|PVars]) :-
    ( PV == Binding ->
        true
    ;
        (current_option(human,on) ->
            format(' \n~p is ~p',[PV,Binding])
        ;
            ( Binding =.. [_,PB,{PConst}], PV = PB ->
                format(" \n~p",[PConst])
            ;
                format(" \n~p = ~p",[PV,Binding])
            )
        )
    ),
    print_unifier_(Bs,PVars).



select_printable_literals([],[]) :- !.
select_printable_literals([X|Xs],NSs) :-
    select_printable_literals(X,S), !,
    select_printable_literals(Xs,Ss),
    append(S,Ss,NSs).
select_printable_literals(X,[X]) :-
    printable_literal(X), !.
select_printable_literals(_,[]).


print_model_([]).
print_model_([Last]) :- 
    print(Last).
print_model_([First,Second|Rest]) :-
    print(First),
    display(' ,  '),
    print_model_([Second|Rest]).

%printable_literal(not(X)) :- printable_literal(X).
printable_literal(X) :-
    X \= 'global_constraints',
    X \= 'o_nmr_check',
    X \= chs(_),
    (
        pr_show_predicate(_) ->
        pr_show_predicate(X)
    ;
        X \= proved(_)
    ).

%% print_j(Justification,I) :-
%%     print_model(Justification),
%%     nl,
%%     print_j_(Justification,I).
%% print_j_([],_).
%% print_j_([A,[]],I):- !,
%%     tab(I), print(A), print('.'), nl.
%% print_j_([A,[]|B],I):- !,
%%     tab(I), print(A), print(','), nl,
%%     print_j_(B,I).
%% print_j_([A,ProofA|B],I):-
%%     tab(I), print(A), print(' :-'), nl,
%%     I1 is I + 4, print_j_(ProofA,I1),
%%     print_j_(B,I).

%% The stack is generated adding the last calls in the head (to avoid
%% the use of append/3). To print the stack, it is reversed.

%% NOTE that a model could be generated during the search with some
%% calls in the stack which are not present in the model (e.g. the
%% model of path(1,4) for the path/2 program - more details in the
%% file README)

%% print_stack(Stack) :-
%%     reverse(Stack, RStack),
%%     nl,
%%     nl,nl,
%%     %    print(RStack),nl.
%%     print_s(RStack).

print_tree_stack(Stack) :-
    print_s(Stack),!.




%% Initial interpreters...
query2([]).
query2([X|Xs]) :-
    query2(Xs),
    query2(X).
query2(X) :-
    pr_rule(X, Body),
    query2(Body).


%:- table query3/3.
query3([X|Xs], I, O) :-
    format('Calling ~w \t with stack = ~w', [X, I]), nl,
    query3(X,  [X|I], O1),
    query3(Xs, O1,    O).
query3([], I, I) :- !.
query3(X,  I, O) :-
    pr_rule(X, Body),
    query3(Body, I, O).



print_constraints('│',_,Const) :-
    format("~w",[Const]).
print_constraints('∉',PB,(Const)) :- !,
    print_constraints_not(PB,Const).
print_constraints('∉',PB,(Const,Cs)) :-
    print_constraints_not(PB,Const),
    format(", ",[]),
    print_constraints('∉',PB,Cs).
print_constraints_not(PB,Const) :-
    format("~w \= ~w",[PB,Const]).


:- pred print_check_calls_calling(Goal, StackIn) #"Auxiliar predicate
to print @var{StackIn} the current stack and @var{Goal}. This
predicate is executed when the flag @var{check_calls} is
@em{on}. NOTE: use check_calls/0 to activate the flag".

print_check_calls_calling(Goal,I) :-
    reverse([('¿'+Goal+'?')|I],RI),
    format('\n---------------------Calling ~w-------------',[Goal]),
    print_s(RI),!.

:- data sp_tab/1, pr_repeat/2.
print_s([A|Stack]) :-
    retractall(sp_tab(_)),
    retractall(pr_repeat(_,_)),
%    nl,tab(0),
    print_human_term(A,0,_),
    print_s_(Stack,4,0).

print_s_([],_,_) :-
    print_human('.'), nl.
print_s_([[]|As],I,I0) :- !,
    (  sp_tab(I) ->
        retract(sp_tab(I)),
        I1 = I
    ;
        I1 is I - 4
    ),
    print_s_(As,I1,I0).
print_s_([A|As],I,I0) :- !,
    (
        I0 > I ->
        retractall(pr_repeat(I0,_)),
        print_human('.')
    ;
        I0 < I ->
        print_human(' :-')
    ;
        print_human(',')
    ),
%    nl,tab(I),
    ( [A|As] == [global_constraints,o_nmr_check,[],[],[]] ->
        print_zero_nmr(A,I,I1)
    ;
        ( [A|As] == [o_nmr_check,[],[],[]] ->
            print_zero_nmr(A,I,I1)
        ;
            print_human_term(A,I,I1)
        )
    ),
    print_s_(As,I1,I).


print_human_term(A,I,I1) :-
    pr_human_term((A::Human),Type), !,
    (   current_option(short,on), Type \= pred ->
        asserta(sp_tab(I)),
        I1 = I
    ;
        nl,tab(I),call(Human),
        I1 is I + 4
    ).
        
print_zero_nmr(A,I,I1) :-
    ( current_option(short,on) ->
        asserta(sp_tab(I)),
        I1 = I,
        nl
    ;
        nl,tab(I),
        (   current_option(human,on) ->
            (  A = global_constraints ->
                format('The global constraints hold',[])
            ;
                format('There are no non-monotonic-rules to be checked',[])
            )
        ;
            print(A)
        ),
        I1 is I + 4
    ).

pr_pred_short(A) :-
    pr_pred_predicate(A), !.
pr_pred_short(proved(A)::(Human,format(', already justified',[]))) :-
    pr_pred_predicate(A::Human),
    (   sp_tab(I) ->
        (  pr_repeat(I,A) ->
            fail
        ;
            assert(pr_repeat(I,A))
        )
    ;
        true
    ).
pr_pred_short(chs(A)::(format('Assume: ',[]), Human)) :-
    pr_pred_predicate(A::Human).

pr_human_term((A::Human),Type) :-
    ( current_option(human,on) ->
        (   pr_pred_short(A::Human) ->
            Type = pred
        ;
            (   pr_pred_default(A::Human) ->
                (   member(A,[global_constraints]) ->
                    Type = pred
                ;
                    Type = default
                )
            ;
                Type = error,
                Human = write(A)
            )
        )
    ;
        Type = plain,
        Human = print(A)
    ).

print_human(Conector) :-
    ( current_option(human,on) ->
        human(Conector,A)
    ;
        A = Conector
    ),
    write(A).

human('.','').
human(',','').
human(' :-','').

%% Predefine human rules  & introduced during execution (forall...)
:- dynamic new_pr_pred_default/1.
pr_pred_default( proved(A)    :: (B,format(', already justified',[]))) :- !,
    pr_human_term(A::B,_).
pr_pred_default(A) :- new_pr_pred_default(A),!.
pr_pred_default( (A=A)        :: format('~w is ~w',[A,A])).
pr_pred_default(true          :: format('\r',[])).
pr_pred_default(Operation     :: format('~w is ~w ~w',[HA,HOp,B])) :-
    Operation =.. [Op,A,B],
    human_op(Op,HOp),
    ( A = '$'(Var) ->
        HA = Var
    ;
        HA = A
    ).
pr_pred_default(not(Auxiliar) :: Human) :-
    Auxiliar =.. [Aux|Args],
    atom_chars(Aux,['o','_'|Rs]),
    append(Pred,['.'|Num],Rs),
    number_chars(N,Num),
    atom_chars(Pr,Pred),
    (   Pr == chk ->
        Human = format('For the rule number ~w ',[N])
    ;
        ( Pr == '_chk' ->
            Human = format('There is no contradiction ',[])
        ;
            Predicate =.. [Pr|Args],
            pr_human_term(not(Predicate)::PrH,_),
            Human = (format('~w) ',[N]),PrH)
        )
    ).
pr_pred_default(Forall        :: Human) :-
    Forall = forall(_,_),
    find_vars_description(Forall,[Var|_],Descripcion),
    (  member(@(Var:VHuman),Descripcion), VHuman \= '' ->
        Human = format('For any possible ~p',[VHuman])
    ;
        Human = format('For any possible value',[])
    ).
pr_pred_default(CHS        :: (format('Assume: ',[]),Human)) :-
    CHS = chs(Pred),
    pr_human_term(Pred::Human,_).
pr_pred_default(global_constraints :: format('The global constraints hold',[])).
pr_pred_default(o_nmr_check        :: format('Let\'s check the non-monotonic-rules',[])).
pr_pred_default(Other              :: format(F,[Name|NArgs])) :-
    ( Other = not(Pre) ->
        F0 = 'There is no conclusion of'
    ;
        Pre = Other,
        F0 = 'There is a conclusion of'
    ),
    Pre =.. [Name|Args],
    F1 = ' ~w',
    other_format(Args,NArgs,F2),
    atom_concat(F0,F1,Fm), atom_concat(Fm,F2,F).

other_format([],[],'').
other_format([V|Vs],[@(V:'')|NVs],F) :-
    other_format(Vs,NVs,F0),
    atom_concat(', for ~p',F0,F).
                
    

find_vars_description(Forall,Vars,Descripcion) :-
    find_vars_description_(Forall,Vars,not(Call),Descripcion),
    Call =.. [Name|Args], append(Prev,Vars,Args),
    length(Args,Len),length(FreeArgs,Len),
    FreeCall =.. [Name|FreeArgs],
    (  new_pr_pred_default(not(FreeCall) :: _) ->
        true
    ;
        process_new_call01(Name,Prev,NewPrev,Human01),
        process_new_call02(Vars,Descripcion,Free,Human02),
        append(NewPrev,Free,NewFreeArgs), NewFreeCall =.. [Name|NewFreeArgs],
        assert(new_pr_pred_default(not(NewFreeCall)::(Human01,Human02)))
    ).

process_new_call01(_,[],[],format('There is no contradiction',[])) :- !.
process_new_call01(Name,Prev,FreePrev,Human1) :-
    atom_chars(Name,[o,'_'|Names]), append(Preds,['.'|Nums],Names), number_chars(_,Nums),atom_chars(Pred,Preds),
    length(Prev,Len), length(FreePrev,Len), PrevCall =.. [Pred|FreePrev],
    (  pr_pred_predicate(not(PrevCall)::Human1) ->
        true
    ;
        pr_pred_default(not(PrevCall)::Human1)
    ).
            
process_new_call02([V],Descripcion,[F],(H)) :- !,
    (  member(@(V:VHuman),Descripcion) ->
        H = format(', for ~p',[@(F:VHuman)])
    ;
        H = format(', for ~p',[@(F:'')])
    ).
process_new_call02([V0,V1|Vars],Descripcion,[F|Fs],(H,Hs)) :-
    process_new_call02([V0],Descripcion,[F],H),
    process_new_call02([V1|Vars],Descripcion,Fs,Hs).


find_vars_description_(forall(V,Rs),[V|Vs],Call,Description) :- !,
    find_vars_description_(Rs,Vs,Call,Description).
find_vars_description_(Rs,[],Rs,Description) :-
    find_last_description(Rs,Description).

find_last_description(Rs,Description) :-
    findall(Body, pr_rule(Rs,Body), Cls),
    reverse(Cls,[Body|_]), %% the last clause has all the literals of a forall
    find_description(Body,Description).

find_description([],[]).  %% no more var description
find_description([L|Ls],D) :-
    ( pr_pred_predicate(L::format(_,D0)) ->
        true
    ;
        D0 = []
    ),
    find_description(Ls,Ds),
    append(D0,Ds,D).
    

:- multifile portray/1.
portray(@(Var:_)) :- var(Var), !,
    print(Var).
portray(@(X:'')) :- !,
    human_protray_default(X).
portray(@(X:NX)) :- !,
    human_protray(X:NX).
portray('$'(X)) :-
        write(X).
portray(Constraint) :-
    Constraint =.. [Op,A,B],
    pretty_clp(_,Op), !,
    format("~p ~w ~p",[A,Op,B]).

% para la conclusion xx no tentemos texto.
human_protray_default(A '│' B) :- !,
    print(A), print(' '), human_protray_(B).
human_protray_default('$'(X)) :- !, write(X).
human_protray_default(X) :- write(X).

human_protray((A '│' B):NX) :- !,
    format('a ~w ~w ',[NX,A]),
    human_protray_(B).
human_protray('$'(_X):NX) :- !,
    format('any ~w',[NX]).
human_protray(X:NX) :-
    format('the ~w ~w',[NX,X]).

human_protray_({X,Y,Z}) :- !,
    print_c(X), print(', '), human_protray_({Y,Z}).
human_protray_({X,Z}) :- !,
    print_c(X), print(', and '), human_protray_({Z}).
human_protray_({X}) :-
    print_c(X).

print_c(Operation) :-
    Operation =.. [Op,_,B],
    human_op(Op,HOp),
    format('~w ~w',[HOp,B]).

human_op(#= ,'equal').
human_op(#<>,'not equal').
human_op(#< ,'less than').
human_op(#> ,'greater than').
human_op(#=<,'less or equal').
human_op(#>=,'greater or equal').
human_op(\=, 'not equal').
human_op(=,  '').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Print pretty term
%% (Also variables with attributes)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% print_term(D0,D1,A) :-
%%     pretty_term(D0,D1,A,P),
%%     print(P).

%% :- data next_letra/1.
%% %% init_mydict :-
%% %%     retractall(next_letra(_)),
%% %%     assert(next_letra(1)).

%% next_letra(L1) :-
%%     retract(next_letra(L0)),!,
%%     L1 is L0 + 1,
%%     assert(next_letra(L1)).
%% next_letra(0).

lookup_mydict(D0,D1,A,L) :-
    ( lookup_mydict_(D0,A,L) ->
        D1 = D0
    ;
        length(D0,L),
        D1 = [(A,L)|D0]
    ).

lookup_mydict_([],_,_) :- !, fail.
lookup_mydict_([(V,L)|_],A,L) :- V == A, !.
lookup_mydict_([_|Rs],A,L) :- lookup_mydict_(Rs,A,L).

:- use_module(engine(attributes)).
pretty_term(D0,D1,A,PA) :-
    var(A), !,
    lookup_mydict(D0,D1,A,N),
    Letter is N mod 26 + 0'A,
    atom_codes(L,[Letter]),
    (   N>=26 ->
        Rest is N//26,
        atom_number(AtomRest,Rest),
        atom_concat(L,AtomRest,PVar)
    ;   PVar=L
    ),
    ( get_attribute(A,Att) ->
        pretty_portray_attribute(Att,A,PVar,PA)
    ;
        PA = '$'(PVar)
    ).
pretty_term(D0,D0,[],[]) :- !.
pretty_term(D0,D2,[A|As],[PA|PAs]) :- !,
    pretty_term(D0,D1,A,PA),
    pretty_term(D1,D2,As,PAs).
pretty_term(D0,D0,rat(A,B),A/B) :- !.
pretty_term(D0,D1,Functor,PF) :-
    Functor =..[Name|Args], !,
    pretty_term(D0,D1,Args,PArgs),
    (   pretty_clp(Name,PName) ->
        simple_operands(PArgs,SArgs),
        PF =.. [PName|SArgs]
    ;   pretty_clp(_,Name) ->
        simple_operands(PArgs,SArgs),
        PF =.. [Name|SArgs]
    ;
        PF =.. [Name|PArgs]
    ).
pretty_term(D0,D0,A,'?'(A)).

simple_operands([A,B],[SA,SB]) :-
    simple_operand(A,SA),
    simple_operand(B,SB).
simple_operand(Operand,'$'(Var)) :-
    Operand =.. ['│', Var, _], !.
%% simple_operand(Operand,SOperand) :-
%%     struct(Operand),
%%     Operand =.. [Op|Args], !,
%%     simple_operands(Args,SArgs),
%%     SOperand =.. [Op|SArgs].
simple_operand(A,A).
 

:- use_module(library(clpq/clpq_dump), [clpqr_dump_constraints/3]).
pretty_portray_attribute(Att,A,PVar,PA) :-
    pretty_portray_attribute_(Att,A,PVar,PA),!.
pretty_portray_attribute(_Att,_,PVar,PVar).

%:- use_module(library(formulae)).
pretty_portray_attribute_(att(_,false,att(clp_disequality_rt,neg(List),_)),_,PVar,PA) :-
    (  List == [] ->
        PA=PVar
    ;
        pretty_disequality(PVar,List,Const),
        PA =.. ['│', PVar, {Const}]
    ).
pretty_portray_attribute_(_,A,PVar,PA) :-
    clpqr_dump_constraints(A, PVar, Constraints),
    (  Constraints == [] ->
        PA=PVar
    ;
        sort(Constraints,Sort),
        reverse(Sort,RConstraints),
        pretty_constraints(RConstraints,Const),
        PA =.. ['│', PVar, {Const}]
    ).

pretty_disequality(PVar,[A],(PVar \= A)) :- !.
pretty_disequality(PVar,[A|As],(PVar \= A, Cs)) :-
    pretty_disequality(PVar,As,Cs).
    
pretty_constraints([A],(C)) :- !,
    pretty_constraints_(A,C).
pretty_constraints([A|As],(C,Cs)) :-
    pretty_constraints_(A,C),
    pretty_constraints(As,Cs).
pretty_constraints_(A,C) :-
    A =.. [Op,X,Y],
    pretty_rat(X,PX),
    pretty_rat(Y,PY),
    ( pretty_clp(Op,P_Op) ->
        C =.. [P_Op,PX,PY]
    ;
        format("WARNING: clp operator ~w not defined\n",[Op]),
        C =.. [Op,PX,PY]
    ).    
pretty_constraints_(A,A).
pretty_rat(rat(A,B),A/B) :- !.
pretty_rat(A,A).

pretty_clp(N,PN) :- pretty_clp_(N,PN), !.

pretty_clp_(.=.,  '#=' ).
pretty_clp_(.<>., '#<>').
pretty_clp_(.<.,  '#<' ).
pretty_clp_(.>.,  '#>' ).
pretty_clp_(.=<., '#=<').
pretty_clp_(.>=., '#>=').
pretty_clp_(\=, \=).
    
        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Set options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic current_option/2, counter/2.

set_options(Options) :-
    set_default_options,
    set_user_options(Options).

set_default_options :-
    set(answers,-1),
    set(verbose,0).

set_user_options([]).
set_user_options([O | Os]) :-
    (
        set_user_option(O) ->
        set_user_options(Os)
    ;
        format('ERROR: The option ~w is not supported!\n\n',[O]),
        help,
        fail
    ).

set_user_option('-h') :- help.
set_user_option('-?') :- help.
set_user_option('--help') :- help.
set_user_option('-i') :- set(interactive, on).
set_user_option('--interactive') :- set(interactive, on).
set_user_option('-a').
set_user_option('--auto').
set_user_option(Option) :- atom_chars(Option,['-','s'|Ns]),number_chars(N,Ns),set(answers,N).
set_user_option(Option) :- atom_chars(Option,['-','n'|Ns]),number_chars(N,Ns),set(answers,N).
set_user_option('-v') :- set(check_calls, on).
set_user_option('--verbose') :- set(check_calls, on).
set_user_option('-w') :- set(warning, on).
set_user_option('--warning') :- set(warning, on).
set_user_option('-no') :- set(no_nmr, on).
set_user_option('--no_nmr') :- set(no_nmr, on).
set_user_option('-j') :- set(print_tree, on), set(process_stack, on).
set_user_option('-j0') :- set(print_tree, on), set(process_stack, on).
set_user_option('--justification') :- set(print_tree, on), set(process_stack, on).
set_user_option('--human_all') :- set(human, on), set(print_tree, on), set(process_stack, on).
set_user_option('--human_short') :- set(human, on), set(short,on),set(print_tree, on), set(process_stack, on).
set_user_option('--html') :- set(html, on), set(process_stack, on).
set_user_option('--server') :- set(server, on), set(html, on), set(process_stack, on).
set_user_option('-d0') :- set(write_program, on).

:- pred if_user_option(Name, Call) : (ground(Name), callable(Call))
#"If the flag @var{Name} is on them the call @var{Call} is executed".

if_user_option(Name,Call) :-
    (
        current_option(Name,on) ->
        call(Call)
    ;
        true
    ).

:- pred set(Option, Value) #"Used to set-up the user options".

set(Option, Value) :-
    retractall(current_option(Option, _)),
    assert(current_option(Option,Value)).

help :-
    display('Usage: scasp [options] InputFile(s)\n\n'),
    display('s(CASP) computes stable models of ungrounded normal logic programs.\n'),
    display('Command-line switches are case-sensitive!\n\n'),
    display(' General Options:\n\n'),
    display('  -h, -?, --help        Print this help message and terminate.\n'),
    display('  -i, --interactive     Run in user / interactive mode.\n'),
    display('  -a, --auto            Run in automatic mode (no user interaction).\n'),
    display('  -sN, -nN              Compute N answer sets, where N >= 0. 0 for all.\n'),
    display('  -v, --verbose         Enable verbose progress messages.\n'),
    display('  -w, --warning         Enable warning messages (failing in variant loops).\n'),
    display('  -j, --justification   Print proof tree for each solution.\n'),
    display('  --human               Print the whole proof tree in (predefine) natural language.\n'),
    display('  --human_short         Print the proof tree in natural language (only annotated predicates).\n'),
    display('  --html                Generate the proof tree in a file named InputFiles(s).html.\n'),
    display('  --server              Generate the proof tree in the file named justification.html.\n'),
    display('  -d0                   Print the program translated (with duals and nmr_check).\n'),
    display('\n'),
    abort.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse arguments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred parse_args(Args, Options, Sources) #"Select from the list of
arguments in @var{Args} which are the user-options, @var{Options} and
which are the program files, @var{Sources}".

parse_args([],[],[]).
parse_args([O | Args], [O | Os], Ss) :-
    atom_concat('-',_,O),!,
    parse_args(Args, Os, Ss).
parse_args([S | Args], Os, [S | Ss]) :-
    parse_args(Args, Os, Ss).



:- use_module('html/html_head').
:- use_module('html/jquery_tree').
:- use_module('html/html_tail').
:- pred print_html(Query, Model, StackOut) #"Generate a html
file with the model and the justification tree of the @var{Sources}
for the @var{Query} using @var{Model} and @var{StackOut} resp.".

%% Print output predicates to presaent the results of the query
print_html(Query, Model, StackOut) :-
    write('\nBEGIN HTML JUSTIFICATION'),
    ( current_option(server,on) ->
        Name=justification
    ;
        loaded_file(Sources),
        create_file_name(Sources,Name)
    ),
    atom_concat(Name,'.html',File),
%    File = 'html/justification.html',
    open_output_file(Stream,File,Current),
    if(
        (
            load_html_head(Head),
            print(Head),
            print('<h3>Query</h3>'),nl,
            print_html_query(Query),nl,
            br,br,nl,
            print('<h3>Model</h3>'),nl,
            select_printable_literals(Model,Printable),
            print_model_(Printable),
            br,br,
            nl,print('<h3> Justification <button onclick="expand()">Expand All</button><button onclick="depth(+1)">+1</button><button onclick="depth(-1)">-1</button><button onclick="collapse()">Collapse All</button></h3>'),nl,nl,
            nl,print(' <ul class="tree">'),nl,nl,
            print_html_stack(StackOut),
            nl,print('</ul>'),nl,nl,
            load_jquery_tree(Jquery_tree),
            print(Jquery_tree),nl,nl,
            load_html_tail(Tail),
            print(Tail),nl
        ),true,true),
    close_output_file(Stream,Current),
    write(' and END\n'), 
    !.


create_file_name([Ss],F) :-
    name(Ss,StringS),
    create_file_name_(StringS,StringF),
    name(F,StringF).
create_file_name([A,B|Ss],Fs) :-
    create_file_name([A],F),
    create_file_name([B|Ss],Fm),
    atom_concat(F,'-',F1),
    atom_concat(F1,Fm,Fs).
create_file_name_([46|_],[]) :- !.
create_file_name_([],[]) :- !.
create_file_name_([L|Ls],[L|F2]) :-
    create_file_name_(Ls,F2).


:- use_module(library(terms_check)).
print_html_query([PQ,Bindings,PVars]) :-
        print('<b>\n  <font color=blue>?-</font> '),nl,
        print_body(PQ),
        nl,br,br,nl,
        print_html_unifier(Bindings,PVars),
        print(' ?'),
        br,nl,
        print('</b>').

print_html_unifier([],[]).
print_html_unifier([Binding|Bs],[PV|PVars]) :-
    ( PV == Binding ->
        true
    ;
        ( Binding =.. [_,PB,{PConst}], PV = PB ->
            format(" <br> \n~p",[PConst])
        ;
            format(" <br> \n~p = ~p",[PV,Binding])
        )
    ),
    print_html_unifier(Bs,PVars).

:- data li_tab/1.
print_html_stack([A|StackOut]) :-
    retractall(li_tab(_)),
    nl,tab(5),print('<li> '),
%    nl,tab(5),print('  '),
    print_html_term(A,5,_),     
    print_html_stack_(StackOut,9,5).

print_html_stack_([],I,I0) :-
    print_human('.'),
    nl,tab(I0), print('</li> '),
    close_ul(I0,I).
print_html_stack_([[]|As],I,I0) :- !,
    (  li_tab(I) ->
        retract(li_tab(I)),
        I1 = I
    ;
        I1 is I - 4
    ),
    print_html_stack_(As,I1,I0).
print_html_stack_([A|As],I,I0) :- !,
    (
        I0 > I ->
            print_human('.'),
            nl,tab(I0), print('</li> '),
            close_ul(I0,I)
    ;
        I0 < I ->
            print_human(' :-'),
            nl,tab(I0), print('  <ul>')
    ;
        print_human(','),
        nl,tab(I0), print('</li>')
    ),
    %    nl,tab(I),print('<li> '),
    %    nl,tab(I),print('  '),
    ( [A|As] == [global_constraints,o_nmr_check,[],[],[]] ->
        print_html_zero_nmr(A,I,I1)
    ;
        ( [A|As] == [o_nmr_check,[],[],[]] ->
            print_html_zero_nmr(A,I,I1)
        ;
            print_html_term(A,I,I1)
        )
    ),
    print_html_stack_(As,I1,I).

print_html_term(A,I,I1) :-
    pr_human_term((A::Human),Type),
    (   current_option(short,on), Type \= pred ->
        assert(li_tab(I)),
        I1 = I
    ;
        nl,tab(I),print('<li> '),
        nl,tab(I),call(Human),
        I1 is I + 4
    ).
print_html_zero_nmr(A,I,I1) :-
    ( current_option(short,on) ->
        assert(li_tab(I)),
        I1 = I
    ;
        nl,tab(I),print('<li> '),
        nl,tab(I),
        (   current_option(human,on) ->
            (  A = global_constraints ->
                format('The global constraints hold',[])
            ;
                format('There are no non-monotonic-rules to be checked',[])
            )
        ;
            print(A)
        ),
        I1 is I + 4
    ).


%% print_html_term(Constraint) :-
%%     Constraint =.. [Op,A,B],
%%     pretty_clp(_,Op), !,
%%     format("~w ~w ~w",[A,Op,B]).
%% print_html_term(A) :- print(A).

close_ul(I,I) :- !.
close_ul(I0,I) :-
    I0 > I,
    I1 is I0 - 4,
    ( I1 > 2 ->
        nl,tab(I1), print('</ul> '),
        nl,tab(I1), print('</li> ')
    ;
        true
    ),
    close_ul(I1,I).


%% print_list([],_).
%% print_list([X|Xs],L) :-
%%     tabs(L),
%%     print('<li> '),
%%     print_item(X,L),
%%     print('</li>'),nl,
%%     print_list(Xs,L).

%! tab_html(+Level:int) is det
% Write Level spaces.
%
% @param Level The level to tabs to.
tab_html(N) :-
    N > 0,
    N1 is N - 1,
    write('&nbsp;'),
    !,
    tab_html(N1).
tab_html(0).

print_body([]) :- print('true.').
print_body([X]):-
    print(X),print('.').
print_body([X,Y|Xs]):-
    print(X),print(','),tab_html(2),nl,
    print_body([Y|Xs]).

open_output_file(Stream,File,Current) :-
    current_output(Current),
    open(File,append,_F),close(_F), %% if File does not exists open it
    open(File,write,Stream),
    set_output(Stream).
close_output_file(Stream,Current) :-
    set_output(Current),
    close(Stream).
    
br :- print('<br>').

