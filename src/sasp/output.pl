/*
* Copyright (c) 2016, University of Texas at Dallas
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the University of Texas at Dallas nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY OF TEXAS AT DALLAS BE LIABLE FOR
* ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

:- module(output,
          [ generate_pr_rules/1,
            pr_rule/2,
            pr_query/1,
            pr_user_predicate/1,
            pr_table_predicate/1,
            pr_show_predicate/1,
            pr_pred_predicate/1,
            revar/3                     % +Term -VarTerm, -VarNames
          ]).


/** <module> Output formatting and printing.

Predicates related to formatting and printing output. This includes predicates
that may be used for warning and error output.

@author Kyle Marple
@version 20170510
@license BSD-3
*/

:- use_module(library(lists)).
:- use_module(common).
:- use_module(program).
:- use_module(variables).

:- op(700, xfx, ::).

%!  format_term(+EntryIn:compound, -EntryOut:compound,
%!              -Constraints:list, +Vars:compound) is det
%
%   Format a term for printing.
%
%   @arg EntryIn The initial entry.
%   @arg EntryOut The formatted entry.
%   @arg Constraints Any constraints on variables in the entry.
%   @arg Vars Variable struct for filling in values.

format_term(X, X) :-
    is_var(X),				% constrained var
    !.
format_term(X, Xo) :-
    callable(X),
    !,
    format_predicate2(X, Xo).
format_term(X, X).		% anything else, just pass along

%!  format_term_list(+ListIn:compound, -ListOut:compound) is det
%
%   Format each term in a list.
%
%   @arg ListIn The initial list.
%   @arg ListOut The formatted list.

format_term_list([], []).
format_term_list([X|T], [X2|T2]) :-
    format_term(X, X2),
    format_term_list(T, T2).

%!  format_predicate2(+EntryIn:compound, -EntryOut:compound) is det
%
%   Given a term, remove the  arity,   strip  any  prefixes, and process
%   arguments.
%
%   @arg EntryIn The initial entry.
%   @arg EntryOut The formatted entry.

format_predicate2(Xi, Xo) :-
    Xi = [_|_], % list
    !,
    format_predicate3(Xi, Xo).
format_predicate2(Xi, Xo) :-
    predicate(Xi, X2, A),
    atom(X2), % compound term, predicate or atom
    !,
    split_functor(X2, X3, _), % strip arity
    strip_prefixes(X3, X4),
    format_predicate3(A, A2),
    (   X4 = not(Xn) % append args
    ->  Xn2 =.. [Xn|A2],
        Xo = not(Xn2)
    ;   Xo =.. [X4|A2]
    ).
format_predicate2(Xi, Xo) :-
    Xi =.. [X2|A], % compound term, but not a predicate or atom head
    !,
    format_predicate3(A, A2),
    Xo =.. [X2|A2].
format_predicate2(X, X).

%!  format_predicate3(+ArgsIn:list, -ArgsOut:list) is det
%
%   Process a list of predicate args or variable constraints.
%
%   @arg ArgsIn Input args.
%   @arg ArgsOut Output args.

format_predicate3([], []) :-
    !.
format_predicate3([X|T], [Y|T2]) :-
    !,
    format_predicate4(X, Y),
    format_predicate3(T, T2).
format_predicate3(X, Y) :-
    format_predicate4(X, Y).

%!  format_predicate4(+ArgsIn:list, -ArgsOut:list) is det
%
%   Process  a  single  predicate  arg  or  variable  constraint.  Where
%   possible, substitute previously used variables   for  variables with
%   the same value ID. This makes the final output more readable.
%
%   @arg ArgIn Input arg.
%   @arg ArgOut Output arg.

format_predicate4(X, X) :-
    is_var(X),
    !.
format_predicate4(Xi, Xo) :-
    format_predicate2(Xi, Xo).

%!  strip_prefixes(+FunctorIn:atom, -FunctorOut:atom) is det
%
%   Strip any reserved prefixes added during processing. If appropriate,
%   modify the output functor to restore   formatting represented by the
%   prefix. To ensure that prefixes  added   by  the user remain intact,
%   this predicate will return after removing a single copy of the dummy
%   prefix, if encountered. Otherwise,  all   reserved  prefixes will be
%   stripped. To prevent errors,  the  dual   rule  prefix,  if present,
%   should always be first.
%
%   @arg FunctorIn Input functor
%   @arg FunctorOut Output functor

strip_prefixes(Fi, Fo) :-
    atom_concat(c_, F1, Fi),		% 'c_': classical negation
    !,
    strip_prefixes(F1, F2),
    atom_concat(-, F2, Fo).
strip_prefixes(Fi, not(Fo)) :-
    atom_concat(n_, F1, Fi),		% 'n_': classical negation
    !,
    strip_prefixes(F1, Fo).
strip_prefixes(Fi, Fo) :-
    has_prefix(Fi, C),
    C \== 'd',				% non-dummy prefix
    !,
    sub_atom(Fi, 2, _, 0, F1),
    strip_prefixes(F1, Fo).
strip_prefixes(Fi, Fo) :-
    atom_concat(d_, Fo, Fi),		% 'd_': dummy prefix, remove and finish
    !.
strip_prefixes(Fi, Fo) :-		% '_c_' prefixes change to 'o_'
    atom_concat('_c_', Fc, Fi),
    !,
    atom_concat('o_-', Fc, Fo).
strip_prefixes(Fi, Fo) :-		% '_' prefixes change to 'o_'
    atom_concat('_', Fc, Fi),
    !,
    atom_concat('o_', Fc, Fo).
strip_prefixes(F, F).

:- dynamic pr_rule/2, pr_query/1, pr_user_predicate/1.
:- dynamic pr_table_predicate/1, pr_show_predicate/1, pr_pred_predicate/1.

%!  generate_pr_rules(+Sources)
%
%   Translate the sASP program from the   defined_* predicates into pr_*
%   predicates for sCASP.

:- det(generate_pr_rules/1).

generate_pr_rules(_Sources) :-
    clean_pr_program,
    findall(R, (defined_rule(_, H, B), c_rule(R, H, B)), Rs),
    format_term_list(Rs,Rs2),
    (   defined_nmr_check(NMR)
    ->  format_term_list(NMR, NMR2)
    ;   NMR2 = []
    ),
    (   defined_query(Q,_),
        format_term_list(Q,Q2),
        assert_pr_query(Q2)
    ->  true
    ;   true
    ),
    handle_table_directives,
    handle_show_directives,
    handle_pred_directives,
    assert_pr_rules(Rs2),
    assert_pr_rules([-('global_constraints', NMR2)]).

:- det((handle_table_directives/0,
        handle_show_directives/0,
        handle_pred_directives/0)).

handle_table_directives :-
    findall(T, defined_directive(table(T)), Ts),
    format_term_list(Ts, Ts2),
    assert_pr_table(Ts2).

handle_show_directives :-
    findall(S, defined_directive(show(S)), Ss),
    format_term_list(Ss, Ss2),
    assert_pr_show(Ss2).

handle_pred_directives :-
    findall(P, defined_directive(pred(P)), Ps),
    format_term_list(Ps, Ps2),
    assert_pr_pred(Ps2).


assert_pr_table([]).
assert_pr_table([[T|Ts]|Tss]) :-
    assert_pr_table([T|Ts]),
    assert_pr_table(Tss).
assert_pr_table([T|Ts]) :-
    assert(pr_table_predicate(T)),
    assert_pr_table(Ts).

assert_pr_show([]).
assert_pr_show([[T|Ts]|Tss]) :-
    assert_pr_show([T|Ts]),
    assert_pr_show(Tss).
assert_pr_show([not(Name)/Arity|Ts]) :- !,
    length(Args,Arity),
    T =.. [Name|Args],
    assert(pr_show_predicate(not(T))),
    assert_pr_show(Ts).
assert_pr_show([Name/Arity|Ts]) :-
    length(Args,Arity),
    T =.. [Name|Args],
    assert(pr_show_predicate(T)),
    assert_pr_show(Ts).

%!  process_pr_pred(+PredDecl).

assert_pr_pred([]).
assert_pr_pred([[T|Ts]|Tss]) :- !,
    assert_pr_pred([T|Ts]),
    assert_pr_pred(Tss).
assert_pr_pred([T|Ts]) :-
    process_pr_pred(T,PT),
    revar(PT,RT),
    assert(pr_pred_predicate(RT)),
    assert_pr_pred(Ts).

process_pr_pred(A::B,A::format(PB,List)) :-
    atom_chars(B,Chars),
    process_pr_pred_(Chars,PChars,List),
    atom_chars(PB,PChars).
process_pr_pred_([],[],[]).
process_pr_pred_([@,'('|Cs],[~,p|Ps],[@(V:NV)|Vs]) :- !,
    process_pr_pred_var(Cs,Rs,[],Var,NVar),
    atom_chars(V,Var),
    atom_chars(NV,NVar),
    process_pr_pred_(Rs,Ps,Vs).
process_pr_pred_([C|Cs],[C|Rs],Var) :-
    process_pr_pred_(Cs,Rs,Var).
process_pr_pred_var([':'|R0],Rs,VAc0,VAc1,NAc) :- !,
    reverse(VAc0,VAc1),
    process_pr_pred_name(R0,Rs,['\''],NAc).
process_pr_pred_var([')'|Rs],Rs,Ac0,Ac1,['\'','\'']) :- !,
    reverse(Ac0,Ac1).
process_pr_pred_var([V0|R0],Rs,Ac0,Ac1,NVar) :-
    process_pr_pred_var(R0,Rs,[V0|Ac0],Ac1,NVar).
process_pr_pred_name([')'|Rs],Rs,NAc0,NAc1) :- !,
    reverse(['\''|NAc0],NAc1).
process_pr_pred_name([NV0|R0],Rs,NAc0,NAc1) :-
    process_pr_pred_name(R0,Rs,[NV0|NAc0],NAc1).


assert_pr_rules([]).
assert_pr_rules([-(Head, Body)|Rs]) :-
    revar(-(Head,Body),-(H,B)),
    assert(pr_rule(H,B)),
    assert_pr_user_predicate([H]),
    assert_pr_rules(Rs).

assert_pr_query(Q) :-
    assert(pr_query(Q)).

assert_pr_user_predicate([]).
assert_pr_user_predicate([P|Ps]) :-
    functor(P, Name, La),
    (   pr_user_predicate(Name/La)
    ->  true
    ;   assert(pr_user_predicate(Name/La))
    ),
    assert_pr_user_predicate(Ps).


clean_pr_program :-
    retractall(pr_query(_)),
    retractall(pr_rule(_,_)),
    retractall(pr_user_predicate(_)),
    retractall(pr_table_predicate(_)),
    retractall(pr_show_predicate(_)),
    retractall(pr_pred_predicate(_)).

%!  revar(+Term, -VarTerm) is det.
%!  revar(+Term, -VarTerm, -Bindings) is det.
%
%   If Term is  a  term  that   contains  atoms  using  variable  syntax
%   ([A-Z].*), VarTerm is a copy of Term with all such atoms replaced by
%   variables.   In addition this performs the following rewrites:
%
%     - A term N/D is translated into rat(N,D)
%     - An atom N/D is translated into rat(N,D)
%     - A quoted atom is translated into its unquoted equivalent
%
%   @arg Bindings is a list `Name=Var` that contains the variable names.

revar(X,Y) :-
    empty_assoc(Dic),
    revar_(X,Y,Dic,_).

revar(X,Y,VarNames) :-
    empty_assoc(Dic0),
    revar_(X,Y,Dic0,Dic),
    assoc_to_list(Dic, Pairs),
    maplist(varname, Pairs, VarNames).

varname(Name-Var, Name=Var).

revar_(X,Y,Dic,Dic) :-
    var(X),
    !,
    Y=X.
revar_(X,Y,Dic0,Dic) :-
    is_var(X, Name),
    !,
    (   get_assoc(Name, Dic0, Y)
    ->  Dic = Dic0
    ;   put_assoc(Name, Dic0, Y, Dic)
    ).
revar_(X,Y,Dic,Dic) :-
    special_atom(X,Y),
    !.
revar_(X,Y,Dic0,Dic) :-
    X=..[F|As],
    revars(As,Bs,Dic0,Dic),
    Y=..[F|Bs].

special_atom(A/B,rat(A,B)) :-
    number(A),
    number(B),
    !.
special_atom(X,rat(A,B)) :-
    atom(X),
    atom_codes(X, Codes),
    append(C_A, [0'/|C_B], Codes),
    number_codes(A,C_A),
    number_codes(B,C_B),
    !.
special_atom(X,Y) :-
    atom(X),
    atom_chars(X,Codes),
    append(['\''|C_Y],['\''],Codes),
    atom_chars(Y,C_Y).

revars([],[],Dic,Dic).
revars([X|Xs],[Y|Ys],Dic0,Dic) :-
    revar_(X,Y,Dic0,Dic1),
    revars(Xs, Ys, Dic1, Dic).
