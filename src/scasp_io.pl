:- module(scasp_io, [
    load_program/1,
    write_program/0,
    process_query/3,
    ask_for_more_models/0,
    allways_ask_for_more_models/0,
    init_counter/0,
    increase_counter/0,
    print_query/1,      % query
    print_justification_tree/1, % justification tree
    print_model/1,      % model
    select_printable_literals/3,
    print_unifier/2,    % bindings
    pretty_term/4,
    print_check_calls_calling/2,
    if_user_option/2,
    set/2,
    parse_args/3,
    current_option/2,
    counter/2,
    set_options/1,
    answer_counter/1,
    print_html/3
    ]).
:- expects_dialect(ciao).
:- style_check(-singleton).
:- op(900, fy, user:not).

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

:- use_module('./sasp/comp_duals').
:- use_module('./sasp/nmr_check').
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

:- use_module('./scasp_load_compiled').

%% ------------------------------------------------------------- %%

:- op(700, xfx, ['#=' ,
                 '#<>',
                 '#<' ,
                 '#>' ,
                 '#=<',
                 '#>='
                 ]).

:- op(700, xfx, ['::']).

:- op(700, xfx, ['| ']). %% such as


%% ------------------------------------------------------------- %%

:- if(current_prolog_flag(version_data, swi(_,_,_,_))).
scasp_update :-
    pack_upgrade(scasp).
:- else.
:- pred scasp_update/0 #"update the bundle sCASP using ciao".
:- use_module(library(process)).
scasp_update :-
    format('\n=> First, ciao would remove the bundle sCASP - ciao rm sCASP\n\n',[]),

    (  catch(process_call(path(ciao), [rm, 'sCASP'], []),_,fail) ->
        format('\n=> Done.\n\n',[])
    ;
        format('\n=> Don\'t worry, let\'s try to install s(CAPS) anyway.\n\n',[])
    ),

    fail.
scasp_update :-
    format('\n=> Secondly, ciao would get the updated bundle sCASP - ciao get gitlab.software.imdea.org/ciao-lang/sCASP\n\n',[]),

    (  catch(process_call(path(ciao), [get, 'gitlab.software.imdea.org/ciao-lang/sCASP'], []),_,fail) ->
        format('\n=> Done, s(CASP) has been updated.\n\n',[])
    ;
        format('\n=> Something went wrong. Try again.\n\n',[])
    ),

    fail.
scasp_update :-
    halt.
:- endif.

:- pred scasp_version/0 #"print the current version of s(CASP)".
scasp_version :-
    format('s(CASP) version ~p\n',['0.21.05.28']),
    halt.


:- pred load_program(Files) : list(Files) #"Call s(aso) to generate
    and assert the translation of the progam (with dual and
    nmr_check)".

:- dynamic loaded_file/1.
load_program([]) :-
    display('ERROR: No imput file specified!'),nl,nl,
    s_help, abort. % halt.
load_program(C) :-
    retractall(loaded_file(_)),
    current_option(compiled, on), !,
    (   is_list(C) ->
        Files = C
    ;
        Files = [C]
    ),
    read_compiled_source(C),
    assert(loaded_file(Files)).
load_program(X) :-
    retractall(loaded_file(_)),
    (
        is_list(X) ->
        Files = X
    ;
        Files = [X]
    ),
    main(['-g'| Files]),
    assert(loaded_file(Files)).

:- pred write_program/0 #"Call c(asp) to print the source code of the
translation of the programs already loaded by @pred{load_program/1}".

write_program :-
    print_human_program.
%% Hiden option for debugging
write_program_sasp :-
    loaded_file(Files),
    main(['-d0'|Files]).

:- dynamic cont/0.

:- pred process_query(Q, Query, TotalQuery) #"Initialize internal
flags to allows the generation of multiples models in the interaction
and top-level mode (even when the query is ground). Returns in
@var{TotalQuery} a list with the sub_goals in @var{Q} and
@em{o_nmr_check} to run the global constraints".

process_query(Q,Query,TotalQuery) :-
    revar(Q,A),
    (
        is_list(A) -> As = A ; As = [A]
    ),
    (
        As = [not(_)|_] ->
            %        Query = [true|As]
            Query = As
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
        append(Query, [o_nmr_check], TotalQuery)
    ).

:- pred ask_for_more_models/0 #"Ask if the user want to generate more
models (interactive and top-level mode)".

ask_for_more_models :-
    (
        cont, format('next ? ', []), get_char(R),true, R \= '\n' ->
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
        format(' ? ', []), get_char(R), R \= '\n' ->
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

:- pred print_query(Query) #"Print the  @var{Query}".

:- use_module(library(formulae)).
print_query([not(o_false)]) :- !,
    format('% QUERY: Query not defined\n', []).
print_query([true,A|As]) :- !,
    print_query([A|As]).
print_query(Query) :-
    format('% QUERY:',[]),
    (   current_option(human,on) ->
        format('I would like to know if', []),
        print_human_body(Query)
    ;
        list_to_conj(Query,ConjPQ),
        format('?- ~p.\n',[ConjPQ])
    ).


:- pred print_justification_tree(StackOut) #"Print the justification
tree using @var{StackOut}, the final call stack".

%% Print output predicates to presaent the results of the query
print_justification_tree(StackOut) :-
    format('\nJUSTIFICATION_TREE:',[]),
    print_s(StackOut), !,
    true.

:- pred print_model(Model) #"Print the partial model of the program
using @var{Model}.".

%% The model is obtained from the model.
% TODO: use the StackOut instead of the model.
print_model(Model) :-
    format('\nMODEL:\n',[]),
    print_model_(Model).

print_model_(Model):-
    select_printable_literals(Model,[],Selected),
    reverse(Selected, Printable),
    format('{ ', []),
    printable_model_(Printable),
    format(' }\n', []).

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
        ( Binding =.. [_,PB,{PConst}], PV = $(PB) ->
            (current_option(human,on) ->
                format(' \n~p',[@(Binding:'')])
            ;
                format(" \n~p",[PConst])
            )
        ;
            (current_option(human,on) ->
                format(' \n~p equal ~p',[PV,@(Binding:'')])
            ;
                format(" \n~p = ~p",[PV,Binding])
            )
        )
    ),
    print_unifier_(Bs,PVars).



select_printable_literals([],Ac,Ac) :- !.
select_printable_literals([X|Xs],Ac0,Ac1) :- !,
    select_printable_literals(X,Ac0,Acm),
    select_printable_literals(Xs,Acm,Ac1).
select_printable_literals(X,Ac0,[X|Ac0]) :-
    printable_literal(X),
    \+ member(X,Ac0), !.   %% Remove repeated literals.
select_printable_literals(_,Ac0,Ac0).


printable_model_([]).
printable_model_([Last]) :-
    print(Last).
printable_model_([First,Second|Rest]) :-
    print(First),
    (  printingHTML ->
        format(',  ', []),
        tab_html(5)
    ;
        format(',  ', [])
    ),
    printable_model_([Second|Rest]).

%printable_literal(not(X)) :- printable_literal(X).
printable_literal(X) :-
    X \= abducible(_),
    \+ aux_predicate(X),
    \+ neg_aux_predicate(X),
    X \= 'o_nmr_check',
    X \= chs(_),
    (
        pr_show_predicate(_) ->
        pr_show_predicate(X)
    ;
        X \= proved(_)
    ).


% TODO: remove if it is not needed
%% Initial interpreters...
query2([]).
query2([X|Xs]) :-
    query2(Xs),
    query2(X).
query2(X) :-
    pr_rule(X, Body),
    query2(Body).


% TODO: remove if it is not needed
%:- table query3/3.
query3([X|Xs], I, O) :-
    format('Calling ~w \t with stack = ~w', [X, I]), nl,
    query3(X,  [X|I], O1),
    query3(Xs, O1,    O).
query3([], I, I) :- !.
query3(X,  I, O) :-
    pr_rule(X, Body),
    query3(Body, I, O).


% TODO: remove if it is not needed
print_constraints('| ',_,Const) :-
    format("~w",[Const]).
print_constraints('∉',PB,(Const)) :- !,
    print_constraints_not(PB,Const).
print_constraints('∉',PB,(Const,Cs)) :-
    print_constraints_not(PB,Const),
    format(", ",[]),
    print_constraints('∉',PB,Cs).
print_constraints_not(PB,Const) :-
    format("~w \\= ~w",[PB,Const]).


:- pred print_check_calls_calling(Goal, StackIn) #"Auxiliar predicate
to print @var{StackIn} the current stack and @var{Goal}. This
predicate is executed when the flag @var{check_calls} is
@em{on}. NOTE: use check_calls/0 to activate the flag".

print_check_calls_calling(Goal,I) :-
    reverse(I,RI),
    format('\n--------------------- Calling:  ~w -------------',[Goal]),
    print_check_stack(RI,4), !,
    nl,
%    print(('¿'+Goal+'?')),nl,
    retractall(sp_tab(_)),
    retractall(pr_repeat(_,_)),
    retractall(pr_print(_)).

:- pred print_check_stack/2 #"simple output of the stack to run faster
during verboser".
print_check_stack([],_).
print_check_stack([[]|As],I) :- !,
    I1 is I - 4,
    print_check_stack(As,I1).
print_check_stack([A|As],I) :-
    nl, tab(I), print(A),
    I1 is I + 4,
    print_check_stack(As,I1).


:- pred print_s/1 #"output tree by the terminal".
:- data((sp_tab/1, pr_repeat/2, pr_print/1)).
print_s(Stack) :-
    retractall(sp_tab(_)),
    retractall(pr_repeat(_,_)),
    retractall(pr_print(_)),
    print_s_(Stack,0,0).

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
    (  I0 > I ->
        retractall(pr_repeat(I0,_))
    ;
        true
    ),
    ( [A|As] == [o_nmr_check,[],[],[]] ->
        print_zero_nmr(A,I,I1)
    ;
        print_human_term(A,I,I1)
    ),
    print_s_(As,I1,I).


:- pred print_zero_nmr/3 #"".
print_zero_nmr(_,I,I1) :-
    (   current_option(short,on) ->
        asserta(sp_tab(I)),
        I1 = I
    ;
        nl,tab(I),
        (   current_option(human,on) ->
            format('There are no nmr to be checked',[])
        ;
            write(global_constraint)
        ),
        I1 is I + 4
    ).

:- pred print_human_term/3 #"".
print_human_term(A,I,I1) :-
    pr_human_term((A::Human),Type), !,
    (   current_option(mid,on), Type \= (pred), Type \= mid ->
        asserta(sp_tab(I)),
        I1 = I
    ;
        (   current_option(short,on), Type \= (pred) ->
            asserta(sp_tab(I)),
            I1 = I
        ;
            (   retract(pr_print(Sp)) ->
                (   Sp > I ->
                    print_human('.')
                ;
                    Sp < I,
                    print_human(' :-')
                ;
                    print_human(',')
                )
            ;
                true
            ),
            nl,tab(I),call(Human),
            I1 is I + 4,
            asserta(pr_print(I))
        )
    ).



pr_human_term((Term :: TermHuman), Type) :-
    pr_pred_term(Term :: Human, T), !,  %% To obtain the Type
    (   T = (pred) ->
        Type = (pred)
    ;
        pr_show_predicate(Term), !,   %% Output predicates selected by #show
        Type = (pred)
    ;
        Term = chs(Chs),
        pr_show_predicate(Chs), !,
        if(current_option(assume,on), Type = T, Type = pred)
    ;
        Term = assume(Chs),
        pr_show_predicate(Chs), !,
        Type = (pred)
    ;
        Type = T
    ),
    (   current_option(human,on) ->
        TermHuman = Human
    ;
        Term = o_nmr_check,
        TermHuman = write(global_constraint)
    ;
        TermHuman = print(Term)
    ).



pr_pred_term(A, pred) :-
    pr_pred_predicate(A), !.
pr_pred_term(chs(A)::(format('it is assumed that ',[]), Human), Type) :- !,
    pr_human_term(A::Human, T),
    if(current_option(assume,on), Type = default, Type = T).
pr_pred_term(assume(A)::(format('we assume that ',[]), Human), Type) :- !,
    pr_human_term(A::Human, Type).
pr_pred_term(proved(A)::(Human,format(', justified above',[])), Type) :- !,
    pr_human_term(A::Human, T),
    (   sp_tab(I) ->
        (  pr_repeat(I,A) ->
            Type = default
        ;
            assert(pr_repeat(I,A)),
            Type = T
        )
    ;
        Type = T
    ).
pr_pred_term(GlobalConstraint :: Human, pred) :-
    GlobalConstraint = o_nmr_check, !,
    Human = format('The global constraints hold',[]).
pr_pred_term(A, pred) :-
    pr_pred_global_constraint(A, pred), !.
pr_pred_term(A, mid) :-
    pr_pred_classical_neg(A, _), !.
pr_pred_term(A, Type) :-
    pr_pred_negated(A, T), !,
    (   current_option(neg,on) ->
        ( T = (pred) ->
            Type = (pred)
        ;
            Type = mid
        )
    ;
        Type = default
    ).
pr_pred_term(A, Type) :-
    pr_pred_default(A), !,
    A = (Term::_),
    (   Term \= not(_), user_predicate(Term) ->
        Type = mid
    ;
        Type = default
    ).
pr_pred_term( Error :: print(Error) , default ).


print_human(Conector) :-
    (   current_option(human,on) ->
        human(Conector,A)
    ;
        A = Conector
    ),
    write(A).

human('.','.').
human(',',', and').
human(' :-',', because').


pr_pred_classical_neg(ClassicalNeg :: Human , Type) :-
    ClassicalNeg =.. [NegName|Arg],
    atom_concat('-',Name,NegName), !,
    Predicate =.. [Name|Arg],
    pr_human_term( Predicate :: PrH , Type ),
    Human = ( format('it is not the case that ',[]), PrH ).

pr_pred_global_constraint(not(Global_Constraint) :: Human,pred) :-
    Global_Constraint =.. [Aux|Args],
    atom_chars(Aux,['o','_'|Rs]),
    append(Pred,['_'|Num],Rs),
    catch(number_chars(N,Num), error(syntax_error(_),_), fail),
    atom_chars(Pr,Pred),
    Pr == chk, !,
    H0 = format('the global constraint number ~p holds',[N]),
    pr_var_default(Args,H1),
    Human = (H0, H1).

pr_pred_negated(not(Predicate) :: Human, Type ) :-
    \+ aux_predicate(Predicate),
    pr_human_term( Predicate :: PrH , Type ), !,
    Human = ( format('there is no evidence that ',[]), PrH ).



pr_pred_default( (A=A)        :: format('~p is ~p',[A,A])) :- !.
pr_pred_default(true          :: format('\r',[])) :- !.
pr_pred_default(Operation     :: format('~p is ~p ~p',[HA,HOp,B])) :-
    Operation =.. [Op,A,B],
    human_op(Op,HOp),
    ( A = '$'(Var) ->
        HA = Var
    ;
        HA = A
    ), !.
%% Note o_chk_N are handled by pr_pred_negated as global constraints
pr_pred_default(not(Auxiliar) :: Human) :-
    Auxiliar =.. [Chk|Args],
    %% For o__chk_N1_N2
    atom_concat(o__chk_,Code,Chk), !,
    atom_chars(Code, Chars_Code),
    append(C_N,['_'|_],Chars_Code),
    number_chars(N,C_N),
    ( Args == [] ->
        Human = format('\'G.Const. ~p\' holds',[N])
    ;
        Human = format('\'G.Const. ~p\' holds (for ~p)',[N,@(Args)])
    ).
pr_pred_default(not(Auxiliar) :: Human) :-
    Auxiliar =.. [Aux|Args],
    %% For o_PRED_N
    atom_chars(Aux,['o','_'|C_Aux]), !,
    append(__C_Pred,['_'|C_Num],C_Aux),
    number_chars(N,C_Num),
    ( Args == [] ->
        Human = format('\'rule ~p\' holds',[N])
    ;
        Human = format('\'rule ~p\' holds (for ~p)',[N,@(Args)])
    ).
pr_pred_default(Forall  :: Human) :-
    Forall = forall(_,_), !,
    pr_pred_default_forall(Forall, Human).
pr_pred_default(Other              :: (H0, H1)) :-
    Other =.. [Name|Args],
    ( Args = [] ->
        H0 = format('\'~p\' holds',[Name])
    ;
        H0 = format('\'~p\' holds (for ~p)',[Name,@(Args)])
    ),
    pr_var_default(Args,H1).


pr_var_default(Args,H1) :-
    take_constraints(Args,Vars),
    pr_var_default_(Vars,H1).
pr_var_default_([], format('',[]) ).
pr_var_default_([V], format(', with ~p',[@(V:'')]) ) :- !.
pr_var_default_([V1,V2], ( HV1, format(', and with ~p',[@(V2:'')])) ) :- !,
    pr_var_default_([V1], HV1).
pr_var_default_([V|Vs], (HV,HVs) ) :-
    pr_var_default_([V],HV),
    pr_var_default_(Vs,HVs).

take_constraints([],[]).
take_constraints([V|As],[V|Vs]) :-
    V = (_ '| ' _), !,
    take_constraints(As,Vs).
take_constraints([_|As], Vs) :-
    take_constraints(As,Vs).


%% forall
pr_pred_default_forall(Forall, ( H0, H1 ) ) :-
    pr_pred_default_forall_(Forall, Vars, InForall),
    H0 = format('forall ~p, ',[@(Vars)]),
    pr_human_term(InForall :: H1, _ ).
pr_pred_default_forall_(forall(V,Rest), [V|Vs], InForall) :- !,
    pr_pred_default_forall_(Rest, Vs, InForall).
pr_pred_default_forall_(InForall, [], InForall).


%% To detect user/neg/aux predicates
user_predicate(is(_,_)) :- !.
user_predicate(findall(_,_,_)) :- !.
user_predicate(proved(A)) :- !,
    user_predicate(A).
user_predicate(chs(A)) :- !,
    user_predicate(A).
user_predicate(assume(A)) :- !,
    user_predicate(A).
user_predicate(A) :- !,
    \+ aux_predicate(A),
    A =.. [Name|Args],
    length(Args,La),
    pr_user_predicate(Name/La).
%%
user_neg_predicate(not(A)) :- !,
    user_predicate(A).
user_neg_predicate(A) :- !,
    A =.. [Name|_],
    atom_concat('-',_,Name).
%%
aux_predicate(-(o_,_)) :- !.
aux_predicate(A) :-
    A =.. [Name|_],
    atom_chars(Name,['o','_'|_]).

neg_aux_predicate(not(Pred)) :- aux_predicate(Pred).

truncate_(X,Y) :-
    current_option(decimals,D),
    Z is X * 10**D, ZA is truncate(Z), Y is ZA / 10**D.

%% PORTRAY - capture human output of the variables
:- if(current_prolog_flag(version_data, swi(_,_,_,_))).
:- multifile user:portray/1.
user:portray(Term) :-
    portray(Term).
:- else.
:- multifile portray/1.
:- endif.

portray(rat(A,B)) :-
    if_user_option( real,( C is A/B, truncate_(C,R), write(R) ) ), !.
portray(@(Var:_)) :- var(Var), !,
    print(Var).
portray(@(X:'')) :- !,
    human_portray_default(X).
portray(@(X:store)) :- !,
    human_portray_store(X).
portray(@(X:NX)) :- !,
    human_portray(X:NX).
portray(@(Args)) :-
    Args = [_|_], !,
    human_portray_args(Args).
portray('$'(X)) :- !,
    write(X).
portray(Constraint) :-
    Constraint =.. [Op,A,N/D],
    current_option(real,on),
    C is N/D, truncate_(C,R),
    pretty_clp(_,Op), !,
    format("~p ~p ~p",[A,Op,R]).
portray(Constraint) :-
    Constraint =.. [Op,A,B],
    pretty_clp(_,Op), !,
    format("~p ~p ~p",[A,Op,B]).

% W.o. description for the variable
human_portray_default(A '| ' B) :- !,
    format('~p ',[A]), human_portray_(B).
human_portray_default('$'(X)) :- !, write(X).
human_portray_default(X) :- write(X).

% Special case for constraint stores
human_portray_store((A '| ' B)) :-
    format('~p is ',[A]),
    human_portray_(B).

% W. NX description for he variable
human_portray((A '| ' B):NX) :- !,
    format('a ~p ~p ',[NX,A]),
    human_portray_(B).
human_portray('$'(X):NX) :- !,
    format('~p, a ~p,',[X,NX]).
human_portray(X:NX) :-
    format('the ~p ~p',[NX,X]).

% Human output for constraint
human_portray_({_ \= B}) :- !,
    format('not equal ~p',[B]).
human_portray_(Disequality) :-
    Disequality = {_ \= _ , _}, !,
    format('not equal ', []),
    print_d(Disequality).
human_portray_(CLPQ) :- !,
    print_c(CLPQ).

%% For CLP(\=)
print_d({_ \= A,Y,Z}) :- !,
    print(A), format(', ', []), print_d({Y,Z}).
print_d({_ \= A,Z}) :- !,
    print(A), format(', nor ', []), print_d({Z}).
print_d({_ \= A}) :- !,
    print(A).

%% For CLP(Q/R)
print_c({X,Y,Z}) :- !,
    print_c_(X), format(', ', []), print_c({Y,Z}).
print_c({X,Z}) :- !,
    print_c_(X), format(', and ', []), print_c({Z}).
print_c({X}) :-
    print_c_(X).
print_c_(Operation) :-
    Operation =.. [Op,_,B],
    human_op(Op,HOp),
    format('~p ~p',[HOp,B]).

human_op(\=, 'not equal').

human_op(#= ,'equal').
human_op(#<>,'not equal').
human_op(#< ,'less than').
human_op(#> ,'greater than').
human_op(#=<,'less or equal').
human_op(#>=,'greater or equal').
human_op(=,  '').
human_op(< ,'less than').
human_op(> ,'greater than').
human_op(=<,'less or equal').
human_op(>=,'greater or equal').


%% Human output of a list of arguments @(Args)
human_portray_args([V]) :- !,
    human_portray_arg(V).
human_portray_args([V1,V2]) :- !,
    human_portray_arg(V1),
    format(', and ', []),
    human_portray_arg(V2).
human_portray_args([V|As]) :-
    human_portray_arg(V),
    format(', ', []),
    human_portray_args(As).

human_portray_arg(A) :- var(A), !, print(A).
human_portray_arg(A '| ' _) :- !, print(A).
human_portray_arg('$'(A)) :- !, print(A).
human_portray_arg(A) :- print(A).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Print pretty term
%% (Also variables with attributes)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_mydict(D0,D1,A,PVar) :-
    (   lookup_mydict_(D0,A,PVar) ->
        D1 = D0
    ;
        length(D0,L),
        atom_number(AtomL,L),
        atom_concat('Var',AtomL,PVar),
        D1 = [(A=PVar)|D0]
    ).

lookup_mydict_([],_,_) :- !, fail.
lookup_mydict_([(V=PVar)|_],A,PVar) :- V == A, !.
lookup_mydict_([_|Rs],A,PVar) :- lookup_mydict_(Rs,A,PVar).

:- use_module(engine(attributes)).
pretty_term(D0,D1,A,PA) :-
    var(A), !,
    lookup_mydict(D0,D1,A,PVar),
    ( get_attribute(A,Att) ->
        pretty_portray_attribute(Att,A,PVar,PA)
    ;
        PA = '$'(PVar)
    ).
pretty_term(D0,D0,[],[]) :- !.
pretty_term(D0,D2,[A|As],[PA|PAs]) :- !,
    pretty_term(D0,D1,A,PA),
    pretty_term(D1,D2,As,PAs).
pretty_term(D0,D0,rat(A,B),C) :-
    (   current_option(real, on) ->
        C = rat(A,B)
    ;
        C = A/B
    ), !.
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
    Operand =.. ['| ', Var, _], !.
simple_operand(A,A).


:- use_module(clp_clpq).
pretty_portray_attribute(Att,A,PVar,PA) :-
    pretty_portray_attribute_(Att,A,PVar,PA),!.
pretty_portray_attribute(_Att,_,PVar,PVar).

pretty_portray_attribute_(att(_,false,att(clp_disequality_rt,neg(List),_)),_,PVar,PA) :-
    (  List == [] ->
        PA=PVar
    ;
        pretty_disequality(PVar,List,Const),
        PA =.. ['| ', PVar, {Const}]
    ).
pretty_portray_attribute_(_,A,PVar,PA) :-
    clpqr_dump_constraints(A, PVar, Constraints),
    (  Constraints == [] ->
        PA=PVar
    ;
        sort(Constraints,Sort),
        reverse(Sort,RConstraints),
        pretty_constraints(RConstraints,Const),
        PA =.. ['| ', PVar, {Const}]
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
pretty_clp_(= ,= ).
pretty_clp_(< ,< ).
pretty_clp_(> ,> ).
pretty_clp_(=<,=<).
pretty_clp_(>=,>=).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Set options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic current_option/2, counter/2.

set_options(Options) :-
    set_default_options,
    set_user_options(Options),
    set_default_tree_options,
    check_compatibilities.

set_default_options :-
    set(answers,-1),
    set(verbose,0).

set_default_tree_options :-
    ( current_option(print_tree,on) ->
        ( \+ current_option(short,on), \+ current_option(long,on) ->
            set(mid,on)
        ;
            true
        ),
        ( \+ current_option(pos,on) ->
            set(neg,on)
        ;
            true
        )
    ;
        true
    ).

check_compatibilities :-
    current_option(check_calls,on),
    current_option(human,on), !,
    format('ERROR: verboser and human output do not allowed together!\n\n',[]),
    s_help, abort,
    fail.
check_compatibilities.


set_user_options([]).
set_user_options([O | Os]) :-
    (
        set_user_option(O) ->
        set_user_options(Os)
    ;
        format('ERROR: The option ~w is not supported!\n\n',[O]),
        s_help, abort,
        fail
    ).

:- dynamic html_name/1.
set_user_option('--help_all')           :- help_all, abort.
set_user_option('-h')                   :- s_help, abort.
set_user_option('-?')                   :- s_help, abort.
set_user_option('--help')               :- s_help, abort.
set_user_option('-i')                   :- set(interactive, on).
set_user_option('--interactive')        :- set(interactive, on).
set_user_option('-a').
set_user_option('--auto').
set_user_option(Option) :- atom_chars(Option,['-','s'|Ns]),number_chars(N,Ns),set(answers,N).
set_user_option(Option) :- atom_chars(Option,['-','n'|Ns]),number_chars(N,Ns),set(answers,N).
set_user_option('-c')                   :- set(compiled, on).
set_user_option('--compiled')           :- set(compiled, on).

set_user_option('-d')                   :- assert(plain_dual(on)).
set_user_option('--plaindual')          :- assert(plain_dual(on)).

set_user_option('-r')                   :- set(real, on), set(decimals,5).
set_user_option(Option)                 :- atom_concat('-r=',Ns,Option),atom_number(Ns,D),set(real,on), set(decimals,D).

set_user_option('--code')               :- set(write_program, on), set(neg,on).
set_user_option('--tree')               :- set(process_stack, on), set(print_tree, on).
set_user_option('--tree*')              :- set(process_stack, on), set(print_tree, on), set(assume,on).

set_user_option('--plain')              .
set_user_option('--human')              :- set(human, on).

set_user_option('--long')               :- set(long,on).
set_user_option('--mid')                :- set(mid,on).
set_user_option('--short')              :- set(mid,on), set(short,on).

set_user_option('--neg')                :- set(neg,on).
set_user_option('--pos')                :- set(pos,on).

set_user_option('--html')               :- set(process_stack, on), set(html, on).
set_user_option(Option)                 :- atom_concat('--html=',File,Option),asserta(html_name(File)),set(process_stack, on), set(html, on).

set_user_option('-v')                   :- set(check_calls, on).
set_user_option('--verbose')            :- set(check_calls, on).
set_user_option('-f0')                  :- set(trace_failures, on).
set_user_option('-f')                   :- set(trace_failures, on), set(show_tree,on).
set_user_option('--tracefails')         :- set(trace_failures, on), set(show_tree,on).
set_user_option('--update')             :- scasp_update.
set_user_option('--version')            :- scasp_version.
% Development
set_user_option('-no')                  :- set(no_nmr, on).         %% skip the evaluation of nmr-checks (but compile them).
set_user_option('--no_nmr')             :- assert(no_nmr(on)), assert(no_olon(on)).     %% skip the compilation of nmr-checks.
set_user_option('--no_olon')            :- assert(no_olon(on)).  %% skip the compilation of olon-rules
set_user_option('-w')                   :- set(warning, on).
set_user_option('--warning')            :- set(warning, on).
set_user_option('--variant')            :- set(no_fail_loop, on).
%% Only with tabling
set_user_option('-m')                   :- set(minimal_model,on).
set_user_option('--minimal')            :- set(minimal_model,on).
set_user_option('--all_c_forall')       :- set(all_forall,on).
set_user_option('--prev_forall')        :- set(prev_forall,on).
set_user_option('--raw')                :- set(raw,on).





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

s_help :-
    display('Usage: scasp [options] InputFile(s)\n\n'),
    display('s(CASP) computes stable models of predicate normal logic programs with contraints\n'),
    display('  using a top-down evaluation algorihtm.\n'),
    display('Command-line switches are case-sensitive!\n\n'),
    display('General Options:\n\n'),
    display('  -h, -?, --help        Print this help message and terminate.\n'),
    display('  --help_all            Print extended help.\n'),
    display('  -i, --interactive     Run in interactive mode (REP loop).\n'),
    display('  -a, --auto            Run in batch mode (no user interaction).\n'),
    display('  -sN, -nN              Compute N answer sets, where N >= 0. N = 0 means ''all''.\n'),
    display('  -c, --compiled        Load compiled files (e.g. extracted using --code).\n'),
    display('  -d, --plaindual       Generate dual program with single-goal clauses\n'),
    display('                        (for propositional programs).\n'),
    display('  -r[=d]                Output rational numbers as real numbers.\n'),
    display('                        [d] determines precision. Defaults to d = 5.\n'),
    display('\n'),
    display('  --code                Print program with dual clauses and exit.\n'),
    display('  --tree                Print justification tree for each answer (if any).\n'),
    display('\n'),
    display('  --plain               Output code / justification tree as literals (default).\n'),
    display('  --human               Output code / justification tree in natural language.\n'),
    display('\n'),
    display('  --long                Output long version of justification.\n'),
    display('  --mid                 Output mid-sized version of justification (default) .\n'),
    display('  --short               Short version of justification.\n'),
    display('\n'),
    display('  --pos                 Only display the selected literals in the justification.\n'),
    display('  --neg                 Add the negated literals in the justification (default).\n'),
    display('\n'),
    display('  --html[=name]         Generate HTML file for the justification. [name]:\n'),
    display('                        use \'name.html\'. Default: first InputFile name.\n'),
    display('\n'),
    display('  -v, --verbose         Enable verbose progress messages.\n'),
    display('  -f, --tracefails      Trace user-predicate failures.\n'),
    display('  --update              Automatically update s(CASP).\n'),
    display('  --version             Output the current version of s(CASP)\n'),
    display('\n'),
    display('  --all_c_forall        Exhaustive evaluation of c_forall/2.\n'),
    display('  --prev_forall         Deprecated evaluation of forall/2.\n'),
    display('\n').

help_all :-
    help,
    display('  --no_olon             Do not compile olon rules (for debugging purposes).\n'),
    display('  --no_nmr              Do not compile NMR checks (for debugging purposes).\n'),
    display('  -w, --warning         Enable warning messages (failures in variant loops / disequality).\n'),
    display('  --variant             Do not fail in the presence of variant loops.\n'),
    display('\n'),
    display('  -m, --minimal         Collect only the minimal models (TABLING required).\n'),
    display('  --raw                 Sort the clauses as s(ASP) does (use with --code).\n'),
    display('\n').


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
    (   html_name(F) ->
        (   atom_concat(_,'.html',F) ->
            File = F
        ;
            atom_concat(F,',html',File)
        )
    ;
        loaded_file([S|_Sources]),
        create_file_name(S,File)
    ),
    open_output_file(Stream,File,Current),
    if(
        (
            load_html_head(Head),
            print(Head),
            (  current_option(human,on) ->
                print_html_human_query(Query),nl
                %% Skip output of the model in human mode
            ;
                print_html_query(Query),nl,
                format('<h3>Model:</h3>\n', []),
                print_model_(Model)
            ),
            br,br,nl,
            format('<h3> Justification: <button onclick="expand()">Expand All</button><button onclick="depth(+1)">+1</button><button onclick="depth(-1)">-1</button><button onclick="collapse()">Collapse All</button></h3>\n\n'),
            print_html_stack(StackOut),
            load_jquery_tree(Jquery_tree),
            print(Jquery_tree),nl,nl,
            load_html_tail(Tail),
            print(Tail),nl
        ),true,true),
    close_output_file(Stream,Current),
    write(' and END\n'),
    !.


create_file_name(Source,File) :-
    atom_chars(Source,C_S),
    reverse(C_S,RC_S),
    remove_ext(RC_S,RC_Name),
    reverse(RC_Name,C_Name),
    atom_chars(Name,C_Name),
    atom_concat(Name,'html',File).
remove_ext([C|Rs],S) :-
    C \= '.', !,
    remove_ext(Rs,S).
remove_ext(Rs,Rs).

:- use_module(library(terms_check)).
print_html_query([[true|PQ],_,Bindings,PVars]) :- !,
    print_html_query([PQ,_,Bindings,PVars]).
print_html_query([PQ,_,Bindings,PVars]) :-
    format('<h3>Query:</h3>\n'),
    tab_html(5),
    format('?-', []),tab_html(2),
    print_html_body(PQ),
    br,nl,br,nl,
    format('<h3>Answer:</h3>', []),
    ( Bindings = [] ->
        format('yes',[])
    ;
        print_html_unifier(Bindings,PVars)
    ),
    br,nl.

print_html_human_query([[true|PQ],[true|PAnswer],Bindings,PVars]) :- !,
    print_html_human_query([PQ,PAnswer,Bindings,PVars]).
print_html_human_query([PQ,PAnswer,Bindings,PVars]) :-
    format('<h3>Query:</h3>'),
    tab_html(5),
    format('I would like to know if'),br,nl,
    print_html_human_body(PQ),
    br,nl,
    format('<h3>Answer:</h3>'),nl,
    tab_html(5),
    format('Yes, I found that'),br,
    print_html_unifier(Bindings,PVars),
    print_html_human_body(PAnswer),
    br,nl.

print_html_unifier([],[]).
print_html_unifier([Binding|Bs],[PV|PVars]) :-
    ( PV == Binding ->
        true
    ;
        (   Binding =.. [_,PB,{PConst}], PV = $(PB) ->
            (   current_option(human,on) ->
                tab_html(15),format('when ~p',[@(Binding:store)]),br,nl
            ;
                tab_html(15),format("~p",[PConst]),br,nl
            )
        ;
            (   current_option(human,on) ->
                tab_html(15),format('when ~p is ~p',[PV,@(Binding:'')]),br,nl
            ;
                tab_html(15),format("~p = ~p",[PV,Binding]),br,nl
            )
        )
    ),
    print_html_unifier(Bs,PVars).

%% let's reuse sp_tab and pr_repeat from print_s/1.
print_html_stack(StackOut) :-
    retractall(sp_tab(_)),
    retractall(pr_repeat(_,_)),
    retractall(pr_print(_)),
    format('\n <ul class="tree">\n\n'),
    print_html_stack_(StackOut,5,5),
    format('\n </ul>\n\n').

print_html_stack_([],_,_) :-
    print_human('.'),
    retract(pr_print(Sp)),
    nl,tab(Sp), format('</li> '),
    close_ul(Sp,5).
print_html_stack_([[]|As],I,I0) :- !,
    (   sp_tab(I) ->
        retract(sp_tab(I)),
        I1 = I
    ;
        I1 is I - 4
    ),
    print_html_stack_(As,I1,I0).
print_html_stack_([A|As],I,I0) :- !,
    (  I0 > I ->
        retractall(pr_repeat(I0,_))
    ;
        true
    ),
    ( [A|As] == [o_nmr_check,[],[],[]] ->
        print_html_zero_nmr(A,I,I1)
    ;
        print_html_term(A,I,I1)
    ),
    print_html_stack_(As,I1,I).

print_html_term(A,I,I1) :-
    pr_human_term((A::Human),Type), !,
    (   current_option(mid,on), Type \= (pred), Type \= mid ->
        asserta(sp_tab(I)),
        I1 = I
    ;
        (   current_option(short,on), Type \= (pred) ->
            asserta(sp_tab(I)),
            I1 = I
        ;
            (   retract(pr_print(Sp)) ->
                (   Sp > I ->
                    print_human('.'),
                    nl,tab(Sp), print('</li> '),
                    close_ul(Sp,I)
                ;
                    Sp < I,
                    print_human(' :-'),
                    nl,tab(I), print('<ul>')
                ;
                    print_human(','),
                    nl,tab(Sp), print('</li> ')
                )
            ;
                true
            ),
            nl,tab(I),print('<li> '),
            nl,tab(I),call(Human),
            I1 is I + 4,
            asserta(pr_print(I))
        )
    ).

print_html_zero_nmr(_,I,I1) :-
    (   current_option(short,on) ->
        asserta(sp_tab(I)),
        I1 = I
    ;
        nl,tab(I),format('<li> '),
        nl,tab(I),
        (   current_option(human,on) ->
            format('There are no nmr to be checked',[])
        ;
            print(global_constraint)
        ),
        I1 is I + 4
    ).


close_ul(I0,I) :- I0 = I, !.
close_ul(I0,I) :-
    I1 is I0 - 4,
    nl,tab(I0), print('</ul> '),
    nl,tab(I1), print('</li> '),
    close_ul(I1,I).


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

print_html_human_body([Last]) :- !,
    pr_human_term(Last::Format,_),
    tab_html(15),
    call(Format),br,nl,
    nl.
print_html_human_body([L|Ls]) :-
    pr_human_term(L::Format,_),
    tab_html(15),
    call(Format),
    print(', and'),br,nl,
    print_html_human_body(Ls).

print_html_body([]) :- print('.').
print_html_body([X]):-
    print(X),print('.').
print_html_body([X,Y|Xs]):-
    print(X),print(','),tab_html(2),nl,
    print_html_body([Y|Xs]).

:- data(printingHTML/0).
open_output_file(Stream,File,Current) :-
    current_output(Current),
    open(File,append,_F),close(_F), %% if File does not exists open it
    open(File,write,Stream),
    set_output(Stream),
    asserta(printingHTML).
close_output_file(Stream,Current) :-
    set_output(Current),
    close(Stream),
    retractall(printingHTML).

br :- format('<br>').




:- pred print_human_program #"Output pretty print of the program +
dual rules + nmr-checks".

print_human_program :-
    pr_query(Query),
    pretty_term([],_,Query,PrettyQuery),
    findall(rule(Head,Body), pr_rule(Head,Body),Rules),
    pretty_term_rules(Rules,PrettyRules),
    filter(PrettyRules, UserRules, DualRules, NMRChecks),
    print_human_program_('% QUERY',PrettyQuery),
    nl,
    print_human_program_('% USER PREDICATES',UserRules),
    (  current_option(short,on) ->
        true
    ;
        current_option(mid,on),
        dual_reverse(DualRules,[_|R_DualRules]),
        nl,nl,
        print_human_program_('% DUAL RULES',R_DualRules)
    ;
        dual_reverse(DualRules,[_|R_DualRules]),
        nl,nl,
        print_human_program_('% DUAL RULES',R_DualRules),
        nmr_reverse(NMRChecks,R_NMRChecks),
        nl,nl,
        print_human_program_('% INTEGRITY CONSTRAINTS',R_NMRChecks)
    ),
    nl.

pretty_term_rules([],[]).
pretty_term_rules([R|Rs],[P|Ps]) :-
    pretty_term([],_,R,P),
    pretty_term_rules(Rs,Ps).


filter([],[],[],[]).
filter([R|Rs], Us, Ds, [R|Ns]) :-
    R = rule(not(Head),_),
    Head =.. [Pred|_],
    ( atom_concat(o_chk,_,Pred), ! ; atom_concat(o__chk,_,Pred), ! ),
    filter(Rs,Us,Ds,Ns).
filter([R|Rs], Us, Ds, [R|Ns]) :-
    R = rule(o_nmr_check,_), !,
    filter(Rs,Us,Ds,Ns).
filter([R|Rs], Us, Ds, Ns) :-
    R = rule(global_constraint,_), !,
    filter(Rs,Us,Ds,Ns).
filter([R|Rs], Us, [R|Ds], Ns) :-
    R = rule(not(_),_), !,
    filter(Rs,Us,Ds,Ns).
filter([R|Rs], [R|Us], Ds, Ns) :-
    filter(Rs,Us,Ds,Ns).


print_human_program_(Title,Rules) :-
    format('~p:',[Title]),
    nl,
    (  Title == '% QUERY' ->
        print_human_query(Rules)
    ;
        print_human_rules(Rules)
    ).


print_human_query([not(o_false)]) :- !,
    print('% Query not defined'), nl.
print_human_query([true,A|As]) :- !,
    print_human_query([A|As]).
print_human_query(Query) :-
    ( current_option(human,on) ->
        nl,
        print('I would like to know if'),
        print_human_body(Query)
    ;
        list_to_conj(Query,ConjPQ),
        format('?- ~p.\n',[ConjPQ])
    ).


print_human_rules([R]) :-
    print_human_rules_(R).
print_human_rules([R0,R1|Rs]) :-
    print_human_rules_(R0),
    (  rule_eq(R0,R1) ->  true ; nl ),
    print_human_rules([R1|Rs]).
print_human_rules_(R) :-
    R = rule(Head,Body),
    print_human_head(Head),
    ( Body == [] ->
        format('.\n')
    ;
        (  current_option(human,on) ->
            format(', if')
        ;
            format(' :-')
        ),
        print_human_body(Body)
    ).

rule_eq(rule(H,_),rule(H,_)) :- !.
rule_eq(rule(not(H),_),rule(not(H1),_)) :- !, rule_eq_(H,H1).
rule_eq(rule(-H,_),rule(-H1,_)) :- !, rule_eq_(H,H1).
rule_eq(rule(H,_),rule(H1,_)) :- !, rule_eq_(H,H1).

rule_eq_(H,H1) :-
    H =.. [Name|A], H1 =.. [Name|A1], length(A,L), length(A1,L).

print_human_head(Head) :-
    pr_human_term(Head::Format,_),
    call(Format).

print_human_body([Last]) :- !,
    print_human_body_(Last),
    format('.\n').
print_human_body([L|Ls]) :-
    print_human_body_(L),
    ( current_option(human,on) ->
        format(' and')
    ;
        format(',')
    ),
    print_human_body(Ls).

print_human_body_(L) :-
    pr_human_term(L::Format,_),
    nl,tab(5),
    call(Format).

print_human_body_forall(Forall,I) :-
    Forall = forall(_,InForall), !,
    pr_human_term(Forall::Format,_),
    nl,tab(I),
    call(Format),
    I1 is I + 3,
    print_human_body_forall(InForall,I1).

print_human_body_forall(InForall,I) :-
    pr_human_term(InForall::Format,_),
    nl,tab(I),
    call(Format).



:- pred dual_reverse/2 #"Auxiliary predicate to sort the DUAL rules".
dual_reverse(L,[_|L]) :- current_option(raw,on), !.

dual_reverse(L,R):-
    dual_reverse_(L,[],R).

dual_reverse_([], Ac, Ac).
dual_reverse_([A|As], Ac0, Ac) :-
    dual_pred(A, _), !,
    dual_eq([A|As], [], Eq, Rest),
    append(Eq, Ac0, Ac1),
    dual_reverse_(Rest, Ac1, Ac).
dual_reverse_([A|Rs], Ac0, Ac1) :-
    dual_reverse_(Rs, [A|Ac0], Ac1).

dual_pred(rule(not(-(o_, A)), _), L) :-
    A =.. [_|Args],
    length(Args, L).
dual_pred(rule(not(A), _), L) :-
    A =.. [Name|Args],
    length(Args, L),
    atom_chars(Name, ['o', '_'|_]).

dual_eq([A, B|As], Eq0, Eq, Rest) :-
    dual_pred(A, La),
    dual_pred(B, Lb), !,
    ( La = Lb ->
        append(Eq0,[A],Eq1),
        dual_eq([B|As], Eq1, Eq, Rest)
    ;
        La > Lb, %% B is forall del paquete Eq0 se pone primero
        dual_eq(As, [], Eq1, Rest),
        append([B|Eq0], [A], Eqm),
        append(Eqm, Eq1, Eq)
    ;
        La < Lb, %% Hay que hace un paquete para el proximo forall
        forall_eq([B|As], Forall, [F|RestForall]),
        append(Eq0,[A],Eq1),
        append(Eq1, [F|Forall], Eq2),
        dual_eq(RestForall, [], Eq3, Rest),
        append(Eq2,Eq3,Eq)
    ).
dual_eq([A|As], Eq0, Eq, As) :-
    append(Eq0,[A],Eq),
    dual_pred(A, _), !.
dual_eq(As, Eq, Eq, As).

forall_eq([A,B|As],[A|Eq],Rest) :-
    dual_pred(A,L),
    dual_pred(B,L),!,
    forall_eq([B|As],Eq,Rest).
forall_eq([B|As],[B],As).


:- pred nmr_reverse/2 #"Auxiliary predicate to sort the NMR checks".
nmr_reverse(L,L) :- current_option(raw,on), !.

nmr_reverse(L,[A|Rs]) :-
    nmr_check(A),
    append(Chks,[A],L),
    nmr_reverse_(Chks,[],Rs).

nmr_reverse_([],[],[]).
nmr_reverse_([A|As],Ac0,Ac) :-
    nmr_chk(A), !,
    nmr_eq([A|As],Eq,Rest),
    append(Eq,Ac0,Ac1),
    nmr_reverse_(Rest,Ac1,Ac).
nmr_reverse_([A|Rs],Ac0,Ac1) :-
    nmr_reverse_(Rs,[],AcRs),
    append([A|Ac0],AcRs,Ac1).

nmr_check(rule(o_nmr_check,_)).
nmr_chk(rule(not(A),_)) :-
    A =.. [Name|_],
    \+ atom_concat(o_chk,_,Name).

nmr_eq([A,B|As],[A|Eq],Rest) :-
    \+ \+ A = B, !,
    nmr_eq([B|As],Eq,Rest).
nmr_eq([A|As],[A],As).

