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

:- module(variables,
          [ is_var/1,
            is_var/2,
            body_vars/3
          ]).


/** <module> Variable storage and access

Predicates related to storing, accessing and modifying variables.

@author Kyle Marple
@version 20170515
@license BSD-3
*/

:- use_module(library(lists)).
:- use_module(options).

%! var_struct(?VarStruct:compound, ?Variables:list, ?Values:list,
%!            ?NameCnt:int, ?IDCnt:int) is det
%
% Convert a var struct to its components   or vice-versa. The purpose of
% the var struct is to allow emulation   of  Prolog unification in cases
% such as `X = Y, X = a`. That   is, unified variables should point to a
% common memory location rather than each-other.   This  is emulating by
% giving them the same value  ID  and   storing  the  actual values in a
% separate list with the corresponding IDs.   This shouldn't be accessed
% outside of this module.
%
% @arg VarStruct A variable struct pairing a variable list and a value list.
% @arg Variables A list of pairs linking variables with a value ID.
% @arg Values A list of pairs linking value IDs to values or constraint lists.
% @arg NameCnt The counter used to create unique variable names.
% @arg IDCnt The counter used to create unique variable value IDs.

var_struct(-(Var, Val, Cnt, Cnt2), Var, Val, Cnt, Cnt2).

%!  is_var(@Term) is semidet.
%!  is_var(@Term, Name) is semidet.
%
% Test an entry to see if it's   a variable (the first non-underscore is
% an upper-case letter.
%
% @arg Term is the term to be tested.

is_var($X) :-
    atom(X).

is_var($X, X) :-
    atom(X).

%!  body_vars(+Head:compound, +Body:list, -BodyVars:list) is det
%
%   Get the body variables (variables used in   the  body but not in the
%   head) for a clause.

body_vars(H, B, Bv) :-
    body_vars3(H, [], [], Hv), % get variables in head
    body_vars2(B, Hv, [], Bv).

%!  body_vars2(+Body:list, +HeadVars:list, +BodyVarsIn:list,
%!             -BodyVarsOut:list) is det
%
%   For each goal in a list, get the   variables that are not present in
%   the list of head variables. Note that this   can  be used to get all
%   variables in a list of goals by  calling   it  with an empty list of
%   head variables.
%
%   @arg Body The list of goals in the clause.
%   @arg HeadVars The list of variables in the head.
%   @arg BodyVarsIn Input body vars.
%   @arg BodyVarsOut Output body vars.

body_vars2([X | T], Hv, Bvi, Bvo) :-
    body_vars3(X, Hv, Bvi, Bv1),
    !,
    body_vars2(T, Hv, Bv1, Bvo).
body_vars2([], _, Bv, Bv) :-
    !.

%!  body_vars3(+Goal:compound, +HeadVars:list, +BodyVarsIn:list,
%!             -BodyVarsOut:list) is det
%
%   For a single goal, get the  variables   that  are not present in the
%   list of head variables. This predicate can  get all of the variables
%   in a goal if HeadVars is empty. The list of variables will be in the
%   order they are encountered.
%
%   @arg Goal The list of goals in the clause.
%   @arg HeadVars The list of variables in the head.
%   @arg BodyVarsIn Input body vars.
%   @arg BodyVarsOut Output body vars.

body_vars3(G, Hv, Bvi, Bvo) :-
    is_var(G), % variable
    \+ memberchk(G, Hv), % not a head variable
    \+ memberchk(G, Bvi), % not already encountered
    !,
    append(Bvi, [G], Bvo). % keep proper order.
body_vars3(G, Hv, Bvi, Bvo) :-
    G =.. [_ | A],
    A \= [], % goal is a compound term
    !,
    body_vars2(A, Hv, Bvi, Bvo). % check args for variables
body_vars3(_, _, Bv, Bv). % not a compound term or a new, non-head variable

%!  get_unique_vars2(+GoalsIn:list, -GoalsOut:list, +VarStructIn:int,
%!                   -VarStructOut:int, +VarsIn:compound, -VarsOut:compound)
%
%   Given a list of goals, replace  every   variable  with  a unique one
%   using the counter in the var  struct.   If  the variable has already
%   been replaced, use the same replacement value.
%
%   @arg GoalsIn Input goals.
%   @arg GoalsOut Output goals.
%   @arg VarStructIn Input var struct.
%   @arg VarStructOut Output var struct.
%   @arg VarsIn Input vars.
%   @arg VarsOut Output vars.

get_unique_vars2([Xi | Ti], [Xo | To], Ci, Co, Vi, Vo) :-
    get_unique_vars3(Xi, Xo, Ci, C1, Vi, V1),
    !,
    get_unique_vars2(Ti, To, C1, Co, V1, Vo).
get_unique_vars2([], [], C, C, V, V).

%!  get_unique_vars3(+Goal:compound, -GoalOut:compound, +VarStructIn:int,
%!                   -VarStructOut:int, +VarsIn:compound, -VarsOut:compound)
%
%   Given a goal, replace each variable with a unique one. If a variable
%   has already been replaced, use the same assignment.
%
%   @arg GoalIn Input goal.
%   @arg GoalOut Output goal.
%   @arg VarStructIn Input var struct.
%   @arg VarStructOut Output var struct.
%   @arg VarsIn Input vars.
%   @arg VarsOut Output vars.

get_unique_vars3(Gi, Go, Vs, Vs, V, V) :-
    is_var(Gi),
    member(-(Gi, Go), V), % already encountered, use the same value
    !.
get_unique_vars3(Gi, Go, Vsi, Vso, Vi, Vo) :-
    is_var(Gi), % not encountered, generate a value.
    !,
    generate_unique_var(Go, Vsi, Vso, Gi),
    Vo = [-(Gi, Go) | Vi],
    !.
get_unique_vars3(Gi, Go, Vsi, Vso, Vi, Vo) :-
    Gi =.. [F | A], % compound term, process args
    !,
    get_unique_vars2(A, A2, Vsi, Vso, Vi, Vo),
    Go =.. [F | A2],
    !.
get_unique_vars3(G, G, Vs, Vs, V, V) :-
    % not a variable or compound term; skip it.
    !.

%!  generate_unique_var(-Var:ground, +VarsIn:compound,
%!                      -VarsOut:compound, +Name:ruleName) is det
%
%   Using the counter in the variable struct, generate a unique variable
%   name and then update the counter by incrementing it.
%
%   @arg Var The newly generated variable.
%   @arg VarsIn Input vars.
%   @arg VarsOut Output vars.

generate_unique_var(Var, Vi, Vo, Name) :-
    var_struct(Vi, V1, V2, Ci, I), % get initial counter value.
    number_chars(Ci, Cc),
    (   user_option(html_justification, true)
    ->  atom_chars(Name,[F | Cname]),
        (   F \= '_'
        ->  atom_chars('<sub>', Ini),
            atom_chars('&nbsp;</sub>', Fin),
            append(Ini, Cc, Tmp),
            append(Tmp, Fin, NewCc),
            append([F |Cname],  NewCc, Cvar),
            atom_chars(Var, Cvar)
        ;   atom_chars('<sub>', Ini),
            atom_chars('&nbsp;</sub>', Fin),
            append(Ini, Cc, Tmp),
            append(Tmp, Fin, NewCc),
            atom_chars(Var, ['_', 'V' | NewCc])
        )
    ;   atom_chars(Var, ['V', 'a', 'r' | Cc])
    ),
    Co is Ci + 1,
    var_struct(Vo, V1, V2, Co, I), % repack the struct
    !.
