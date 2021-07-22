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
            body_vars/3,
            var_list/2                  % +N, -Vars
          ]).
:- use_module(library(assoc)).

/** <module> Variable storage and access

Predicates related to storing, accessing and modifying variables.

@author Kyle Marple
@version 20170515
@license BSD-3
*/

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

:- det(body_vars/3).

body_vars(H, B, Bv) :-
    empty_assoc(Empty),
    term_vars(H, _, [], Empty, Hv),
    term_vars(B, Bv, [], Hv, _).

term_vars(Var, Vars0, Vars, Seen0, Seen) :-
    is_var(Var, Name),
    !,
    (   get_assoc(Name, Seen0, _)
    ->  Vars = Vars0,
        Seen = Seen0
    ;   put_assoc(Name, Seen0, true, Seen),
        Vars0 = [Var|Vars]
    ).
term_vars(Term, Vars0, Vars, Seen0, Seen) :-
    compound(Term),
    !,
    functor(Term, _Name, Arity),
    term_vars(1, Arity, Term, Vars0, Vars, Seen0, Seen).
term_vars(_, Vars, Vars, Seen, Seen).

term_vars(I, Arity, Term, Vars0, Vars, Seen0, Seen) :-
    I =< Arity,
    !,
    arg(I, Term, Arg),
    term_vars(Arg, Vars0, Vars1, Seen0, Seen1),
    I2 is I+1,
    term_vars(I2, Arity, Term, Vars1, Vars, Seen1, Seen).
term_vars(_, _, _, Vars, Vars, Seen, Seen).

%!  var_list(+N:int, -Vars:list) is det
%
%   Get a list of N variables, each   of  which is different. Basically,
%   just append a counter to '_X'.  The   '_'  prefix ensures they don't
%   overlap with any existing variables in a rule.
%
%   @arg  N The size of the list to return.
%   @arg  Vars output list.

:- det(var_list/2).

var_list(0, []) :-
    !.
var_list(I, [H|T]) :-
    I2 is I-1,
    var_list(I2, T),
    mk_var(I2, H).

mk_var(I, $Name) :-
    atom_concat('_X', I, Name).
