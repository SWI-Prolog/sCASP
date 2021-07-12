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
            is_atom/3,
            is_compound/3,
            is_dual/1,
            split_functor/3,
            create_unique_functor/3,
            fatal_error/2,
            write_error/1,
            write_error/2,
            write_verbose/2,
            write_verbose/3,
            var_list/4,
            list_diff/3,
            list_intersection/3,
            operator/3
          ]).

/** <module> Common predicates used in multiple files

Common and utility predicates that may be called from multiple locations.

@author Kyle Marple
@version 20170127
@license BSD-3
*/

:- use_module(library(lists)).
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

%!  negate_functor(+Functor:compound, -NegFunctor:compound) is det
%
%   Given the functor of a predicate   (of  the form name/arity), return
%   the negation.
%
%   @arg  Functor The functor of a predicate.
%   @arg  NegFunctor The negated functor.

negate_functor(F, N) :-
    atom_chars(F, Fc),
    atom_chars('n_', Fn), % negation prefix
    append(Fn, N2, Fc), % F is negative, negation is the positive literal.
    atom_chars(N, N2),
    !.
negate_functor(F, N) :-
    atom_chars(F, Fc),
    atom_chars('n_', Fn), % negation prefix
    append(Fn, Fc, N2),
    atom_chars(N, N2),
    !.

%!  is_atom(+Goal:compound, +Vars:list, -Value:atom) is det
%
%   Given a goal, succeed if it is  an   atom  or a variable bound to an
%   atom. Check vars first, since internal  representation of a variable
%   is also an atom.
%
%   @arg  Goal The input goal.
%   @arg  Vars The list of variable values and constraints.
%   @arg  Value The atom.

is_atom(G, V, A) :-
    is_var(G),
    !,
    var_value(G, V, val(A)),
    A =.. [A | []]. % atom
is_atom(G, _, G) :-
    \+is_var(G),
    !,
    G =.. [G | []].

%!  is_compound(+Goal:compound, +Vars:list, -Value:compound) is det
%
%   Given a goal, succeed if it is a   compound term or a variable bound
%   to a compound term.
%
%   @arg  Goal The input goal.
%   @arg  Vars The list of variable values and constraints.
%   @arg  Value The compound term.

is_compound(G, V, A) :-
    is_var(G),
    !,
    var_value(G, V, val(A)),
    A =.. [_ | T],
    T \= []. % not an atom
is_compound(G, _, G) :-
    !,
    G =.. [_ | T],
    T \= []. % not an atom

%!  is_dual(+Functor:atom) is semidet.
%
%   Succeed if a functor contains the   prefix  '_not_', indicating that
%   it's a dual.
%
%   @arg Functor The functor to test.

is_dual(X) :-
    has_prefix(X, n).

%!  split_functor(+Functor:atom, -Name:list, -Arity:int) is det.
%
%   Given a predicate functor, return the components. Since the arity is
%   at the end, we have to be creative to remove it.
%
%   @arg Functor The predicate functor, of the form Name/Arity.
%   @arg Name The name with the arity stripped. A list of characters.
%   @arg Arity The arity of the predicate, or -1 if no arity is
%        attached.

split_functor(P, N, A) :-
    sub_atom(P, Plen, _, Slen, '_'),
    sub_string(P, _, Slen, 0, NS),
    \+ sub_string(NS, _, _, _, "_"),
    number_string(A, NS),
    !,
    sub_string(P, 0, Plen, _, Name),
    string_chars(Name, N).
split_functor(P, N, -1) :- % no arity attached
    atom_chars(P, N),
    !.

%!  create_unique_functor(+Head:ground, +Counter:int, -NewHead:ground) is det
%
%   Create a unique functor by  inserting   the  counter characters just
%   before the /arity.
%
%   @arg  Head A functor of the form head/arity to form the base of the unique
%        functor.
%   @arg  Counter Counter to ensure that the functor is unique. Don't reuse it
%        with the same base.
%   @arg  DualHead The functor for the dual of an individual clause.

create_unique_functor(Hi, C, Ho) :-
    split_functor(Hi, Fc, A), % Strip the arity
    number_chars(C, Cc),
    append(Fc, ['_'], Fcc), % Add simbol '_' before the counter
    append(Fcc, Cc, Fc2), % Add the counter
    number_chars(A, Ac),
    append(Fc2, ['_' | Ac], Fc3), % Add the arity back
    atom_chars(Ho, Fc3),
    !.

%!  fatal_error(+Format:string, +Arguments:list) is det
%
%   Call write_error/2 and then halt.
%
%   @arg  Format A quoted string that would be passed to writef/2. '~w' specifies
%        that the next element in Arguments should be printed.
%   @arg  Arguments A list of arguments that will be substituted, in order, for
%        '~w' in Format.

fatal_error(X, Y) :-
    write(user_error, 'FATAL '), % add to start of error message
    write_error(X, Y),
    halt(1).

%!  write_error(+Format:string) is det
%
%   Write a formatted error message to   the user_error stream. Since no
%   writef predicate allows writing to streams, first write to a string,
%   then write the string to the stream. NOTE: Syntax and parsing errors
%   are    handled    differently.     See    common_dcg:syntax_error/3,
%   tokenizer:eof_error and tokenizer:lex_error/2.
%
%   @arg  Format Anything that could be passed to write/1: a quoted string, a
%        term, etc.

write_error(X) :-
    split_string(X, "", "\n", [String]),
    print_message(error, format('~w',String)).

%!  write_error(+Format:string, +Arguments:list) is det
%
%   Similar to write_error/1, except that  arguments   are  filled in as
%   with writef/2.
%
%   @arg  Format A quoted string that would be passed to writef/2. '~w' specifies
%        that the next element in Arguments should be printed.
%   @arg  Arguments A list of arguments that will be substituted, in order, for
%        '~w' in Format.

write_error(X, Y) :-
    split_string(X, "", "\n", [Fmt]),
    print_message(error, format(Fmt,Y)).

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

%!  write_verbose(+Depth:int, +Format:string, +Arguments:list) is det
%
%   Similar to write_verbose/2, except that arguments   are filled in as
%   with writef/2.
%
%   @arg  Depth The indentation level of the message.
%   @arg  Format A quoted string in which '~w' will be replaced by the next item
%        in Arguments.
%   @arg  Arguments A list of arguments that will be substituted, in order, for
%        '~w' in Format.

write_verbose(0, Y, Z) :-
    user_option(verbose, 1),
    !,
    format(user_error, Y, Z).
write_verbose(X, Y, Z) :-
    user_option(veryverbose, 1),
    !,
    write_indent(X),
    format(user_error, Y, Z).
write_verbose(_, _, _).

%!  write_indent(+Depth:int)
%
%   Write an indentation of size Depth to user_error.
%
%   @arg Depth The number of indentations to print. Note that a single
%        indentation need not equate to a single space.

write_indent(Depth) :-
    tab(user_error, 2*Depth).

%!  var_list(+N:int, +Count:int, +ListIn:list, -ListOut:list) is det
%
%   Get a list of N variables, each   of  which is different. Basically,
%   just append a counter to '_X'.  The   '_'  prefix ensures they don't
%   overlap with any existing variables in a rule.
%
%   @arg  N The size of the list to return.
%   @arg  Count Counter to ensure that each variable has a unique name.
%   @arg  ListIn Input list.
%   @arg  ListOut output list.

var_list(N, C, Li, Lo) :-
    N > 0,
    !,
    number_chars(C, Cc), % get chars from counter
    append(['_', 'X'], Cc, Vc),
    atom_chars(V, Vc),
    C1 is C + 1,
    N1 is N - 1,
    var_list(N1, C1, [V | Li], Lo).
var_list(0, _, L, L2) :-
    reverse(L, L2), % restore order
    !.

%!  list_diff(+ListA:list, +ListB:list, -ListC:list) is det
%
%   Get all of the members of ListA that are not members of ListB.
%
%   @arg  ListA The first input list.
%   @arg  ListB The second input list.
%   @arg  ListC The output list.

list_diff([X | T], Y, Z) :-
    member(X, Y),
    !,
    list_diff(T, Y, Z).
list_diff([X | T], Y, [X | Z]) :-
    !,
    list_diff(T, Y, Z).
list_diff([], _, []) :-
    !.

%!  list_intersection(+ListA:list, +ListB:list, -ListC:list) is det
%
%   Get all of the members of ListA that are also members of ListB.
%
%   @arg ListA The first input list.
%   @arg ListB The second input list.
%   @arg ListC The output list.

list_intersection([X | T], Y, [X | Z]) :-
    member(X, Y),
    !,
    list_intersection(T, Y, Z).
list_intersection([_ | T], Y, Z) :-
    !,
    list_intersection(T, Y, Z).
list_intersection([], _, []) :-
    !.

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



