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
