:- module(scasp_listing,
          [ scasp_portray_program/1     % :Options
          ]).
:- autoload(library(listing), [portray_clause/3]).

:- use_module(human).
:- use_module(compile).
:- use_module(output).
:- use_module(common).

:- meta_predicate
    scasp_portray_program(:).

:- create_prolog_flag(scasp_list_raw, false, []).


%!  scasp_portray_program(:Options)
%
%   Output pretty print of  the  program   +  dual  rules  + nmr-checks.
%   Options:
%
%     - human(Boolean)
%       If `true`, write in _human_ format.
%     - query(Boolean)
%       Print the query (default `true`)
%     - user(Boolean)
%       Print the user program (default `true`)
%     - duals(Boolean)
%       Print the duals (default `false`)
%     - constraints(Boolean)
%       Print the global constraints (default `false`)
%     - dcc(Boolean)
%       Print the DCC rules (default `false`)
%     - write_program(+Detail)
%       Set defaults for the above to handle the ``--code`` commandline
%       option.
%     - code_file(+Name)
%       Dump code to file Name instead of current output

:- det(scasp_portray_program/1).
scasp_portray_program(M:Options) :-
    (   option(write_program(Detail), Options)
    ->  program_details(Detail, DetailOptions),
        merge_options(Options, DetailOptions, WriteOptons)
    ;   WriteOptons = Options
    ),
    (   option(code_file(File), Options)
    ->  setup_call_cleanup(
            open(File, write, Out),
            with_output_to(Out,
                           scasp_portray_program(M, WriteOptons)),
            close(Out))
    ;   scasp_portray_program(M, WriteOptons)
    ).

:- det(program_details/2).
program_details(short, [query(true), user(true)]).
program_details(mid,   [query(true), user(true), duals(true)]).
program_details(long,  [query(true), user(true), duals(true),
                        constraints(true), dcc(true)]).

scasp_portray_program(M, Options) :-
    catch(scasp_query(M:Query, Bindings, Options),
          error(existence_error(scasp_query, _),_),
          Query = []),
    MOptions = [module(M)|Options],
    VOptions = [variable_names(Bindings)|MOptions],
    findall(rule(Head,Body), M:pr_rule(_Origin, Head, Body), Rules),
    filter(Rules, UserRules0, DualRules, NMRChecks0),
    remove_nmr_checks(NMRChecks0, UserRules0, NMRChecks, UserRules),
    findall(rule(DccH,DccB), M:pr_dcc_predicate(DccH,DccB),DCCs),
    print_program(query,       Query,       Printed, VOptions),
    print_program(user,        UserRules,   Printed, MOptions),
    print_program(duals,       DualRules,   Printed, MOptions),
    print_program(constraints, NMRChecks,   Printed, MOptions),
    print_program(dcc,	       DCCs,	    Printed, MOptions).

%!  filter(+Rules, -UserRules, -DualRules, -NMRChecks) is det.

filter([],[],[],[]).
filter([R|Rs], Us, Ds, [R|Ns]) :-
    R = rule(not(Head),_),
    chk_pred(Head),
    !,
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

chk_pred(Pred) :-
    functor(Pred, Name, _),
    (   sub_atom(Name, 0, _, _, o_chk)
    ;   sub_atom(Name, 0, _, _, o__chk)
    ),
    !.

%!  print_program(+Section, +Rules, ?Printed, +Options)

:- det(print_program/4).
print_program(_, [], _, _) :-
    !.
print_program(Section, Rules, Printed, Options) :-
    code_section_title(Section, Default, Title),
    Opt =.. [Section,true],
    option(Opt, Options, Default),
    !,
    sep_line(Printed),
    ansi_format(comment, "% ~w\n", [Title]),
    (   Section == query
    ->  print_query(Rules, Options)
    ;   order_rules(Section, Rules, Rules1),
        split_predicates(Rules1, Predicates),
        maplist(print_predicate(Options, Printed), Predicates)
    ).
print_program(_, _, _, _).

code_section_title(query,       true,  'Query').
code_section_title(user,        true,  'User Predicates').
code_section_title(duals,       false, 'Dual Rules').
code_section_title(constraints, false, 'Integrity Constraints').
code_section_title(dcc,         false, 'Dynamic consistency checks').

order_rules(duals, DualRules, R_DualRules) :-
    !,
    dual_reverse(DualRules,[_|R_DualRules]).
order_rules(constraints, NMRRules, R_NMRRules) :-
    !,
    nmr_reverse(NMRRules, R_NMRRules).
order_rules(_, Rules, Rules).

print_predicate(Options, Printed, Rules) :-
    maplist(prolog_rule, Rules, Clauses),
    (   option(human(true), Options)
    ->  human_predicate(Clauses, Options)
    ;   sep_line(Printed),
        maplist(portray_clause, Clauses)
    ).

sep_line(true) =>
    nl.
sep_line(Printed) =>
    Printed = true.

prolog_rule(rule(H, []), Rule) =>
    raise_negation(H, Rule).
prolog_rule(rule(H, B), Rule) =>
    raise_negation(H, Head),
    maplist(raise_negation, B, B1),
    comma_list(Body, B1),
    Rule = (Head :- Body).

prolog_query([not(o_false)], _) =>
    fail.
prolog_query(List, Query), is_list(List) =>
    delete(List, o_nmr_check, List1),
    delete(List1, true, List2),
    (   List2 == []
    ->  Query = true
    ;   comma_list(Query, List2)
    ).

print_query(Query, Options) :-
    option(human(true), Options),
    !,
    option(variable_names(Bindings), Options, []),
    ovar_set_bindings(Bindings),
    human_query(Query, Options).
print_query(Query, Options) :-
    prolog_query(Query, Prolog),
    format('?- ~@', [portray_clause(current_output, Prolog, Options)]).

split_predicates([], []).
split_predicates([H|T0], [[H|P]|T]) :-
    rules_same_pred(T0, H, P, T1),
    split_predicates(T1, T).

rules_same_pred([H|T0], P, [H|T], R) :-
    rule_eq(H, P),
    !,
    rules_same_pred(T0, P, T, R).
rules_same_pred(L, _, [], L).


%!  rule_eq(+Rule1, +Rule2) is semidet.
%
%   True when Rule1 and Rule2 belong to  the same predicate. Used to add
%   a blank line between two rule sets.

rule_eq(rule(H,_),rule(H1,_)) :-
    \+ H \= H1,
    !.
rule_eq(rule(not(H),_),rule(not(H1),_)) :- !, rule_eq_(H,H1).
rule_eq(rule(-H,_),rule(-H1,_)) :- !, rule_eq_(H,H1).
rule_eq(rule(H,_),rule(H1,_)) :- !, rule_eq_(H,H1).

rule_eq_(H, H1) :-
    same_functor(H, H1).

%!  dual_reverse(A, B) is det.
%
%   Auxiliary predicate to sort the DUAL rules

:- det(dual_reverse/2).
dual_reverse(L,[_|L]) :-
    current_prolog_flag(scasp_list_raw, true),
    !.
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
    functor(A, _, L).
dual_pred(rule(not(A), _), L) :-
    functor(A, Name, L),
    atom_chars(Name, ['o', '_'|_]).

dual_eq([A,B|As], Eq0, Eq, Rest) :-
    dual_pred(A, La),
    dual_pred(B, Lb), !,
    (   La =:= Lb
    ->  append(Eq0,[A],Eq1),
        dual_eq([B|As], Eq1, Eq, Rest)
    ;   La > Lb                         % B is forall del paquete Eq0 se pone primero
    ->  dual_eq(As, [], Eq1, Rest),
        append([B|Eq0], [A], Eqm),
        append(Eqm, Eq1, Eq)
    ;                                   % Hay que hace un paquete para el proximo forall
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


%!  remove_nmr_checks(+NMRChecks0, +UserRules0, -NMRChecks, -UserRules)

remove_nmr_checks([rule(o_nmr_check,[])], UserRules0, NMRChecks, UserRules) =>
    NMRChecks = [],
    delete(UserRules0, rule(global_constraints,[o_nmr_check]), UserRules).
remove_nmr_checks(NMRChecks0, UserRules0, NMRChecks, UserRules) =>
    NMRChecks = NMRChecks0,
    UserRules = UserRules0.


%!  nmr_reverse(+NMRChecks, -RevNNMRChecks)
%
%   Auxiliary predicate to sort the NMR checks

:- det(nmr_reverse/2).

nmr_reverse(L,L) :-
    current_prolog_flag(scasp_list_raw, true),
    !.
nmr_reverse(L,[A|Rs]) :-
    nmr_check(A),
    once(append(Chks,[A],L)),
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
    functor(A, Name, _),
    \+ atom_concat(o_chk,_,Name).
%   Using chk_pred(A) causes this to fail.

nmr_eq([A,B|As],[A|Eq],Rest) :-
    \+ A \= B, !,
    nmr_eq([B|As],Eq,Rest).
nmr_eq([A|As],[A],As).
