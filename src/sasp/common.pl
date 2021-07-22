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

:- module(common,
          [ predicate/3,
            c_rule/3,
            rule/4,
            negate_functor/2,
            is_dual/1,
            split_functor/3,            % +Functor, -Name, -Arity
            join_functor/3,             % -Functor, +Name, +Arity
            create_unique_functor/3,
            write_verbose/2,
            var_list/2,
            operator/3
          ]).

/** <module> Common predicates used in multiple files

Common and utility predicates that may be called from multiple locations.

@author Kyle Marple
@version 20170127
@license BSD-3
*/

:- use_module(options).
:- use_module(variables).
:- use_module(program, [has_prefix/2]).

%!  predicate(?PredicateStruct:compound, ?Name:atom, ?Args:list) is det
%
%   Convert a predicate struct to its  components, or vice-versa. Ensure
%   this doesn't succeed for operators or not(_).
%
%   @arg  PredicateStruct Predicate sturct.
%   @arg  Name Predicate name, in name/arity format.
%   @arg  Args List of predicate args.

predicate(Predicate, Name, Args) :-
    Predicate =.. [Name | Args],
    \+ operator(Name, _, _),
    Name \= not,
    !.

%!  c_rule(?Rule:compound, ?Head:compound, ?Body:list) is det
%
%   Convert a rule structure into its head and body, or vice-versa. Note
%   that if an ID has been attached, it  will be paired with the head as
%   `Head = -(RealHead, ID)`. This can be taken advantage of if the head
%   and ID are simply being copied, but should be used with care.
%
%   @arg  Rule Rule struct.
%   @arg  Head Rule head.
%   @arg  Body Rule body.

c_rule(-(H, B), H, B).

%!  rule(?Rule:compound, ?Head:compound, ?ID:int, ?Body:list) is det
%
%   Convert a rule structure with an id into   its head, ID and body, or
%   vice-versa.
%
%   @arg  Rule Rule struct.
%   @arg  Head Rule head.
%   @arg  ID Rule ID.
%   @arg  Body Rule body.

rule(-(-(H, I), B), H, I, B).

%!  negate_functor(+Functor:atom, -NegFunctor:atom) is det
%
%   Given the functor of a predicate   (of  the form name/arity), return
%   the negation.
%
%   @arg  Functor The functor of a predicate.
%   @arg  NegFunctor The negated functor.

negate_functor(F, N) :-
    atom_concat(n_, N0, F),
    !,
    N = N0.
negate_functor(F, N) :-
    atom_concat(n_, F, N).

%!  is_dual(+Functor:atom) is semidet.
%
%   Succeed if a functor contains the   prefix  '_not_', indicating that
%   it's a dual.
%
%   @arg Functor The functor to test.

is_dual(X) :-
    has_prefix(X, n).

%!  split_functor(+Functor:atom, -Name:atom, -Arity:int) is det.
%
%   Given a predicate functor, return the components. Since the arity is
%   at the end, we have to be creative to remove it.
%
%   @arg Functor The predicate functor, of the form Name_Arity.
%   @arg Name The name with the arity stripped. A list of characters.
%   @arg Arity The arity of the predicate, or -1 if no arity is
%        attached.

split_functor(P, Name, Arity) :-
    sub_atom(P, Plen, _, Slen, '_'),
    sub_string(P, _, Slen, 0, NS),
    \+ sub_string(NS, _, _, _, "_"),
    number_string(Arity, NS),
    !,
    sub_atom(P, 0, Plen, _, Name).
split_functor(P, P, -1). % no arity attached

%!  join_functor(-Functor, +Name, +Arity) is det.

join_functor(Functor, Name, Arity) :-
    atomic_list_concat([Name, '_', Arity], Functor).


%!  create_unique_functor(+Head:ground, +Counter:int, -NewHead:ground) is det
%
%   Create a unique functor by  inserting   the  counter characters just
%   before the ``_Arity``.
%
%   @arg  Head A functor of the form head/arity to form the base of the unique
%        functor.
%   @arg  Counter Counter to ensure that the functor is unique. Don't reuse it
%        with the same base.
%   @arg  DualHead The functor for the dual of an individual clause.

create_unique_functor(Hi, C, Ho) :-
    split_functor(Hi, F, A), % Strip the arity
    atomic_list_concat([F, '_', C, '_', A], Ho).

%!  write_verbose(+Depth:int, +Format:string) is det.
%
%   Write a message to user_error if  the verbose or veryverbose options
%   are enabled (by passing -v or   -vv as command-line parameters). The
%   first argument is  an  integer  representing   the  "depth"  of  the
%   message. This is  used  to  indent   messages  for  readability  and
%   determine which messages to print for  each mode. Top-level messages
%   should use depth 0, with each  subsequent level increasing the depth
%   by 1. Verbose mode will print only those messages with a depth of 0,
%   while veryverbose mode will print all messages. Unlike write_error/1
%   and write_error/2, no prefix or terminal newline are appended.
%
%   @arg Depth The indentation level of the message.
%   @arg Format The item to print. This will probably be a quoted string, but
%        can be anything accepted by write/1.

write_verbose(0, Y) :-
    user_option(verbose, 1),
    write(user_error, Y),
    !.
write_verbose(X, Y) :-
    user_option(veryverbose, 1),
    write_indent(X),
    write(user_error, Y),
    !.
write_verbose(_, _) :-
    !.

%!  write_indent(+Depth:int)
%
%   Write an indentation of size Depth to user_error.
%
%   @arg Depth The number of indentations to print. Note that a single
%        indentation need not equate to a single space.

write_indent(Depth) :-
    tab(user_error, 2*Depth).

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

%!  operator(+Operator:ground, -Specifier:atom, -Priority:int) is det
%
%   ASP/Prolog operator table.  Original  table   from  the  ISO  Prolog
%   standard, with unsupported operators  removed.   NOTE:  Some  of the
%   operators below may not have been implemented yet.
%
%   @arg Operator An arithmetic operator.
%   @arg Specifier Defines associativity of operator.
%   @arg Priority Defines operator priority.

operator(',', xfy, 1000).
operator(=, xfx, 700).
operator(\=, xfx, 700).
operator(@<, xfx, 700).
operator(@>, xfx, 700).
operator(@>=, xfx, 700).
operator(@=<, xfx, 700).
%operator(=.., xfx, 700).
operator(is, xfx, 700).
operator(=:=, xfx, 700).
operator(=\=, xfx, 700).
operator(<, xfx, 700).
operator(=<, xfx, 700).
operator(>, xfx, 700).
operator(>=, xfx, 700).
operator(+, yfx, 500).
operator(-, yfx, 500).
operator(*, yfx, 400).
operator(/, yfx, 400).
operator(//, yfx, 400).
operator(rem, yfx, 400).
operator(mod, yfx, 400).
operator(<<, yfx, 400).
operator(>>, yfx, 400).
operator('**', xfx, 200).
operator(^, xfy, 200).
% constraint operator
operator(#=, xfx, 700).
operator(#<>, xfx, 700).
operator(#<, xfx, 700).
operator(#>, xfx, 700).
operator(#>=, xfx, 700).
operator(#=<, xfx, 700).
% clpq/r
operator(.=., xfx, 700).
operator(.<>., xfx, 700).
operator(.<., xfx, 700).
operator(.>., xfx, 700).
operator(.>=., xfx, 700).
operator(.=<., xfx, 700).
% operator for human output
operator(::, xfx, 700).



