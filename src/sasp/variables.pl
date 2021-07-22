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
            new_var_struct/1,
            var_value/3,
            body_vars/3
          ]).


/** <module> Variable storage and access

Predicates related to storing, accessing and modifying variables.

@author Kyle Marple
@version 20170515
@license BSD-3
*/

:- use_module(library(lists)).
:- use_module(library(rbtrees)).
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

%! var_con(?Struct:compound, ?Constraints:list,
%!         ?Unbindable:int, ?LoopVar:int) is det
%
% Convert a var_con struct to its components and vice-versa.
%
% @arg Struct The structure.
% @arg Constraints The list of constraint values.
% @arg Unbindable 0, 1 or 2 indicating if binding (1) or further constraining
%      via disunification (2) the variable should trigger failure.
% @arg LoopVar -1, 0 or 1 indicating if the variable succeeded non-ground in a
%      positive loop. If -1, the variable is part of a forall and CANNOT be
%      made a loop variable (any attempt to do so must fail).

var_con(con(C, U, L), C, U, L).

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

%! new_var_struct(-VarStruct:compound) is det
%
%  Create an empty var struct.  The   search  spaces are empty Red-Black
%  trees and the counters are initialized  to   0.  The  `X` rbtree maps
%  variable names to a variable id. The second   maps  an id to a value,
%  which can be a term id(ID2) (dereferencing).
%
%  @arg VarStruct A variable structure.

new_var_struct(-(X, Y, 0, 0)) :-
    rb_empty(X),
    rb_empty(Y),
    !.

%!  var_value(+Variable:ground, +VarStruct:compound, -Value:compound) is det
%
%   Given a variable and a variable struct,  get the value for the given
%   variable, if present. Otherwise, return   an  empty list, indicating
%   that the variable is unbound. Value will   be  either a binding or a
%   list of values the variable cannot   take  (constraints). Any forall
%   variables (loopvar flag = -1) are  expected   to  be in the variable
%   struct. Thus any non-loop variables not present  may be given a loop
%   var flag of 0.
%
%   @arg Variable The variable.
%   @arg VarStruct The variable struct.
%   @arg Value The value of the variable.

var_value(V, Vs, Val) :-		% variable present in list
    is_var(V, Name),
    var_struct(Vs, V1, V2, _, _),
    rb_lookup(Name, ID, V1),		% get ID
    !,
    get_val_by_id(ID, V2, Val).
var_value(V, _, Val) :-			% variable not in list; completely unbound
    is_var(V, Name),			% fail if not a variable at all
    sub_atom(Name, 0, 1, _, C),
    (   C == ?                          % flagged (printing only)
    ->  var_con(Val, [], 0, 1)
    ;   var_con(Val, [], 0, 0)
    ).

%!  get_val_by_id(+ID:int, +ValStruct:compound, -Value:compound) is det
%
%   Given a variable value ID, get the  corresponding value. If it links
%   to another ID, check that one recursively.
%
%   @arg ID The ID.
%   @arg ValStruct The value struct from a variable struct.
%   @arg Value The value associated with the ID.

get_val_by_id(I, Vs, Vo) :-
    rb_lookup(I, Val, Vs),		% bind Val
    (   Val = id(I2)                    % check recursively
    ->  get_val_by_id(I2, Vs, Vo)
    ;   Vo = Val                        % value found
    ).

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
