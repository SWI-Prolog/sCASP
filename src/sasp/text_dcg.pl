:- module(text_dcg, [
                    parse_program/4,
                    parse_query/2
                ]).

/** <module> DCG grammar for s(ASP) programs.

Parse tokens into a list of rules. Language is ungrounded ASP. Thanks to Feliks
Kluzniak for advice and examples on getting proper error messages from DCGs.

Input programs are normal logic programs with the following additions:

    * The following directives are supported:
    * =|#include file.asp.|=
    will include file.asp.
    * =|#compute N { Q }.|=
    will override default settings in automatic mode, computing N stable
    models using query Q.
    * =|#abducible X.|=
    will declare a predicate X to be an abducible, meaning that it can be
    either true or false as needed.
    * Atoms and predicates may begin with an underscore, indicating that they
    should be skipped when printing solutions.

@author Kyle Marple
@version 20170127
@license BSD-3
*/

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

:- use_module(library(lists)).
%:- use_module(library(writef)).
:- use_module(ciao_auxiliar).
:- use_module(common).
:- use_module(program).

%! parse_program(+Tokens:list, -Statements:list, -Directives:list, - Errors:int)
% Parse the list of tokens into a list of statements.
%
% @param Tokens The list of tokens from the input program.
% @param Statements List of statements constructed from tokens. List is in
%        program order.
% @param Directives Directives that will need to be processed.
% @param Errors The number of errors encountered during parsing.
parse_program(Toks, Stmts, Directives, Errs) :-
    write_verbose(1, 'Parsing input...\n'),
    once(asp_program(Stmts, Directives, 0, Errs, Toks, [])),
    !.
parse_program(_, _, _, _) :-
    write(user_error, 'One or more errors occurred during parsing!\n'),
    !,
    fail.

%! parse_query(+Tokens:list, -Query:list)
% Parse the list of tokens into a query.
%
% @param Tokens The list of tokens from the input program.
% @param Query List of query goals.
parse_query(Toks, Query) :-
    write_verbose(0, 'Parsing user query...\n'),
    once(user_query(Query, Toks, [])),
    !.

%! syntax_error(+Expected:callable, +TokensIn:list, -TokensOut:list)
% Print error messages for syntax errors. Gets next token and calls
% syntax_error2/3 to print the message.
%
% @param Expected Expected token.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
syntax_error(Expected) -->
    [(C, Pos)],
    {syntax_error2(C, Pos, Expected)},
    !.
syntax_error(Expected, [], []) :-
    syntax_msg(Expected, ExpMsg),
    swritef(Msg, 'ERROR: Unexpected end of file. ~w.\n', [ExpMsg]),
    write(user_error, Msg),
    !.

%! asp_program(-Statements:list, -Directives:list, +ErrorsIn:int, -ErrorsOut:int, +TokensIn:list, -TokensOut:list)
% A program is a list of statements
%
% @param Statements List of statements read.
% @param Directives Directives that will need to be processed.
% @param ErrorsIn Input error count.
% @param ErrorsOut Output error count.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
asp_program(S, D, Ein, Eout) -->
    statements(S, D, Ein, Eout).

%! user_query(-Goals:list, +TokensIn:list, -TokensOut:list) is det
% A user query is a list of goals followed by a terminal period.
%
% @param Goals The query goals entered by the user.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
user_query(X) -->
    body(X),
    terminal('.').
user_query(_) -->
    syntax_error(term),
    !,
    {fail}.

%! statements(-Statements:list, -Directives:list, +ErrorsIn:int, -ErrorsOut:int, +TokensIn:list, -TokensOut:list)
% Parse individual statements and directives, and handle error recovery.
%
% @param Statements List of statements read.
% @param Directives Directives that will need to be processed.
% @param ErrorsIn Input error count.
% @param ErrorsOut Output error count.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
statements(X, [D | T], Ein, Eout) -->
    {nb_setval(us_cnt, 0)}, % initialize underscore counter
    directive(D),
    !,
    statements(X, T, Ein, Eout).
statements([X | T], D, Ein, Eout) -->
    {nb_setval(us_cnt, 0)}, % initialize underscore counter
    statement(X),
    !,
    statements(T, D, Ein, Eout).
statements([], [], Errs, Errs) -->
    empty_list,
    !.
statements(X, D, Ein, Eout) --> % An error occurred, recover and keep going.
    parse_recover,
    !,
    incr(Ein, E2),
    statements(X, D, E2, Eout).

%! statement(-Statement:compound, +TokensIn:list, -TokensOut:list)
% A statement can be a query or a clause.
%
% @param Statement The struct returned for the statement.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
statement(c(1,X)) --> % represent as compute internally
    [('?-', _)], % query
    !,
    user_query(X).
statement(X) -->
    rule_clause(X).
statement(_) --> % HERE, don't treat empty list as an error, just fail.
    empty_list,
    !,
    {fail}.
statement(_) --> % invalid statement
    syntax_error(statement),
    !,
    {fail}.

%! directive(-Directive:compound, +TokensIn:list, -TokensOut:list)
% A directive can be be an include statement, an abducible statement or a
% compute statement. All are preceded by a '#'.
%
% @param Directive The struct returned for the directive.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
directive(X) -->
    [('#', _)],
    !,
    directive2(X).

%! include(-File:filepath, +TokensIn:list, -TokensOut:list)
% An include directive's body. Parenthesis are optional.
%
% @param File The file to include.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
include(Xo) -->
    [('(', _)],
    !,
    [(str(X), _)],
    {strip_quotes(X, Xo)},
    [(')', _)].
include(Xo) -->
    [(str(X), _)],
    {strip_quotes(X, Xo)}.

%! directive2(-Directive:compound, +TokensIn:list, -TokensOut:list)
% A directive can be be an include statement, an abducible statement or a
% compute statement.
%
% @param Directive The struct returned for the directive.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
directive2(include(Xo)) -->
    [('include', _)],
    include(Xo),
    terminal('.').
directive2(table(Xo)) -->
    [('table', _)],
      body(Xo),
      terminal('.').
directive2(show(Xo)) -->
    [('show', _)],
      body(Xo),
      terminal('.').
directive2(pred(Xo)) -->
    [('pred', _)],
    body(Xo),
    terminal('.').
directive2(X) -->
    [('compute', _)],
    compute(X).
directive2(abducible(X)) --> % convert abducible to positive loop
    [('abducible', _)], % temporary syntax, might need refinement
    asp_predicate(X),
    terminal('.').
directive2(_) --> % invalid statement
    syntax_error(directive),
    !,
    {fail}.

%! rule_clause(-Statement:compound, +TokensIn:list, -TokensOut:list)
% A clause can be a headless rule or a normal rule.
%
% @param Statement The struct returned for the statement.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
rule_clause(X) -->
    [(':-', _)],
    !,
    body(Y),
    !,
    terminal('.'),
    {predicate(G, '_false_0', [])}, % dummy head for headless rules
    {c_rule(X, G, Y)}.
rule_clause(X) -->
    head(Y),
    !,
    asp_rule(X, Y),
    !,
    terminal('.').

%! compute(-ComputeStatement:compound, +TokensIn:list, -TokensOut:list)
% Compute statement.
%
% @param ComputeStatement The compute statement struct returned.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
compute(c(X, Y)) -->
    terminal(int(X)),
    !,
    terminal('{'),
    !,
    body(Y),
    !,
    terminal('}'),
    !,
    terminal('.').

%! asp_rule(-Rule:compound, +Head:callable, +TokensIn:list, -TokensOut:list)
% Individual rules: normal rule or fact.
%
% @param Rule The rule struct returned.
% @param Head The rule head.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
asp_rule(X, Y) -->
    [(':-', _)],
    !,
    body(Z),
    {c_rule(X, Y, Z)}.
asp_rule(X, Y) --> % fact
    follow(['.']),
    {c_rule(X, Y, [])}.
asp_rule(_, _) -->
    syntax_error(rule),
    !,
    {fail}.

%! head(-Head:callable, +TokensIn:list, -TokensOut:list)
% A rule head may be a single predicate or a disjunction of predicates (not yet
% supported).
%
% @param Head The rule head.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.

%head(disjunct([X, Y | T])) -->
%        asp_predicate(X),
%        [('|', _)],
%        !,
%        asp_predicate(Y),
%        disjunction(T).
head(X) -->
    asp_predicate(X).

%%! disjunction(-Goals:list, +TokensIn:list, -TokensOut:list)
% A disjunction of literals. Note the first portion is handled by head/3.
%
% @param Goals List of goals in a disjunctive rule head.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
%disjunction([X | T]) -->
%        [('|', _)],
%        !,
%        asp_predicate(X),
%        disjunction(T).
%disjunction([]) -->
%        follow(['.', ':-']).
%disjunction(_) -->
%        syntax_error(rule),
%        !,
%        {fail}.

%! body(-Goals:list, +TokensIn:list, -TokensOut:list)
% The body of a rule is a list of predicates. Because commas are also operators,
% it's easier to read the body as an infix expression and then convert it to a
% list.
%
% @param Goals List of goals in the body of a rule.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
body(X) -->
    infix_expression(Y),
    {comma_to_list(Y, X)},
    !.
body(_) -->
    syntax_error(body),
    !,
    {fail}.

%! infix_expression(-Predicate:compound, +TokensIn:list, -TokensOut:list)
% An infix predicate consists of a term, an infix operator, and another term.
%
% @param Predicate An infix predicate, converted to prefix form.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
infix_expression(X) -->
    get_infix(X2),
    {infix_to_prefix(X, X2)}.

%! get_infix(-Expression:compound, +TokensIn:list, -TokensOut:list)
% Get an infix expression. Can be a single leaf.
%
% @param Expression An infix expression converted to prefix form.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
get_infix(X) -->
    get_infix2(A),
    [(B, _)],
    {operator(B, S, _)},
    {member(S, [xfx, yfx, xfy])},
    !,
    get_infix(C),
    !,
    {append(A, [B | C], X)}.
get_infix(X) -->
    get_infix2(X). % end

%! get_infix2(-Expression:compound, +TokensIn:list, -TokensOut:list)
% Leaves of an infix expression: parenthesized expressions or terms.
%
% @param Expression An infix expression converted to prefix form.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
get_infix2(X) -->
    [('(', _)],
    !,
    get_infix(Y), % parenthesis override priority
    [(')', _)],
    {append(['(' | Y], [')'], X)}.
get_infix2([X]) -->
    asp_term(X).

%! asp_predicate(-Predicate:compound, +TokensIn:list, -TokensOut:list)
% A predicate is an atom followed by a list of terms. If not an operator, it
% may be classically negated.
%
% @param Atom An atom constructed by concatenating the applicable tokens.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
asp_predicate(X) -->
    [('-', _)], % classical negation
    asp_atom(Y),
    !,
    {handle_prefixes(Y, Y2)},
    {atom_chars(Y2, Y3)},
    {c_name(Y4, ['c', '_' | Y3])}, % prefix for classical negation
    asp_predicate2(X, Y4).
asp_predicate(builtin_1(X)) -->
    [(builtin(Y), _)], % built-in, don't add prefixes
    !,
    asp_predicate2(X, Y).
asp_predicate(X) -->
    asp_atom(Y),
    !,
    {handle_prefixes(Y, Y2)},
    asp_predicate2(X, Y2).

%! asp_predicate2(-Predicate:compound, +Name:atom, +TokensIn:list, -TokensOut:list)
% If atom is a compound term, get a list of args. Add the arity to the predicate
% name for easy matching later on.
%
% @param Atom The final atom, concatenated with arguments.
% @param Name The initial atom, prior to reading any arguments.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
asp_predicate2(Z, X) -->
    [('(', _)],
    !,
    terms(Y),
    !,
    [(')', _)],
    !,
    {length(Y, C)}, % get arity)
    {number_chars(C, C2)}, % get char codes for digits
    {atom_chars(X, X2)},
    {append(X2, ['_' | C2], X3)},
    {c_name(X4, X3)},
    {predicate(Z, X4, Y)}.
asp_predicate2(X, Y) -->
    [],
    !,
    {atom_chars(Y, Y2)},
    {append(Y2, ['_', '0'], Y3)},
    {c_name(Y4, Y3)},
    {predicate(X, Y4, [])}.

%! terms(-Terms:list, +TokensIn:list, -TokensOut:list)
% A comma-separated list of terms.
%
% @param Terms List of tokens for terms.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
terms([X | T]) -->
    asp_term(X), % possibly asp_atom?
    !,
    terms2(T).

%! terms2(-Terms:list, +TokensIn:list, -TokensOut:list)
% Get terms after the first element.
%
% @param Terms List of tokens for terms.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
terms2(X) -->
    [(',', _)],
    !,
    terms(X).
terms2([]) -->
    follow([')']),
    !.
terms2(_) -->
    syntax_error(terms),
    !,
    {fail}.

%! asp_term(-Term:atom, +TokensIn:list, -TokensOut:list)
% A term can be a structure, an atom, a variable, an integer or a floating
% point number.
%
% @param Term An identifier, variable, integer or underscore.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
asp_term(X) -->
    asp_compound_term(X), % handles atoms as well
    !.
asp_term(X) -->
    asp_list(X),
    !.
asp_term(X) -->
    [(var(X), _)],
    !.
asp_term(X) -->
    [(int(X), _)],
    !.
asp_term(X) -->
    [(float(X), _)],
    !.
asp_term(X) -->
    [(rat(X), _)],
    !.
asp_term(V) -->
    [('_', _)], % replace underscore with unique variable
    {replace_underscore(V)},
    !.
asp_term(_) -->
    syntax_error(term),
    !,
    {fail}.

%! asp_compound_term(-Struct:atom, +TokensIn:list, -TokensOut:list)
% A structure can be a list or a compound term. Note that this handles atoms as
% well (compound terms with 0 args).
%
% @param Struct An identifier, variable, integer or underscore.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
asp_compound_term(not(X)) -->
    [('not', _)],
    !,
    asp_predicate(X).
asp_compound_term(X) -->
    asp_predicate(X).

%! asp_list(-List:list, +TokensIn:list, -TokensOut:list)
% A list. Identical to Prolog representation.
%
% @param List A list.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
asp_list([]) -->
    [('[', _)],
    [(']', _)],
    !.
asp_list(X) -->
    [('[', _)],
    !,
    infix_expression(Y),
    asp_list2(X, Y),
    [(']', _)].

%! asp_list2(-ListOut:list, +ListIn:compound, +TokensIn:list, -TokensOut:list)
% Get the tail of the list and format the entire list.
%
% @param ListOut A list.
% @param ListIn The head of the list as a compound term.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
asp_list2(X, Y) -->
    [('|', _)],
    infix_expression(Z),
    {\+(Z =.. [',', _, _])}, % only one term in tail
    {comma_to_list(Y, Y2)},
    {asp_list3(Y2, Z, X)}.
asp_list2(X, Y) -->
    {comma_to_list(Y, Y2)},
    {asp_list3(Y2, [], X)}.
asp_list2(_, _) -->
    syntax_error(list),
    !,
    {fail}.

%! asp_list3(+HeadIn:list, +TailIn:term, -ListOut:list)
% Convert a list of head terms and a tail to a single, recursively defined list.
%
% @param HeadIn A list of terms making up the head of the list.
% @param Tail A single tail element.
% @param ListOut The output list.
asp_list3([X | T], Y, [X | T2]) :-
    !,
    asp_list3(T, Y, T2).
asp_list3([], X, X).

%! asp_atom(-Atom:atom, +TokensIn:list, -TokensOut:list)
% An atom can be an ID or a quoted string.
%
% @param Atom An atom.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
asp_atom(X) -->
    [(id(X), _)].
asp_atom(X) -->
    [(str(X), _)].

%! infix_to_prefix(-Prefix:compound, +Infix:list) is det
% Convert an infix expression to a prefix expression.
%
% @param Prefix The prefix expression as a compound term.
% @param Infix The infix expression as a list of tokens.
infix_to_prefix(X, Y) :-
    reverse(['(' | Y], Y2), % add para and reverse
    infix_to_prefix2(Y2, [')'], [], [X]).

%! infix_to_prefix2(+Infix:list, +Stack:list, +PrefixIn:list, -PrefixOut:list) is det
% Convert an infix expression to a prefix expression.
%
% @param Infix The infix expression. Will be reversed.
% @param Stack Operator stack.
% @param PrefixIn Input prefix expression.
% @param PrefixOut The prefix expression as a compound term. Will be reversed.
infix_to_prefix2([X | T], S, Pi, Po) :-
    operator(X, _, Xp),
    !,
    infix_op_pop(Xp, S, S2, Pi, P1),
    infix_to_prefix2(T, [X | S2], P1, Po).
infix_to_prefix2(['(' | T], S, Pi, Po) :-
    !, % left parenthesis
    infix_para_pop(S, S2, Pi, P1),
    infix_to_prefix2(T, S2, P1, Po).
infix_to_prefix2([')' | T], S, Pi, Po) :-
    !, % right parenthesis
    infix_to_prefix2(T, [')' | S], Pi, Po).
infix_to_prefix2([X | T], S, Pi, Po) :-
    !, % operand
    infix_to_prefix2(T, S, [X | Pi], Po).
infix_to_prefix2([], [X | T], Pi, Po) :-
    !, % empty the stack
    infix_to_prefix2([], T, [X | Pi], Po).
infix_to_prefix2([], [], P, P).

%! infix_op_pop(+Priority:int, +StackIn:list, -StackOut:list, +PrefixIn:list, -PrefixOut:list) is det
% Pop operators from the stack and add to output until we reach one whose
% priority is greater than the input priority.
%
% @param Priority The priority to compare against.
% @param StackIn The input stack.
% @param StackOut The output stack.
% @param PrefixIn The input expression.
% @param PrefixOut The output expression.
infix_op_pop(P, [X | T], So, [A, B | Pi], Po) :-
    operator(X, _, P2),
    P2 =< P,
    !,
    Y =.. [X, A, B],
    infix_op_pop(P, T, So, [Y | Pi], Po).
infix_op_pop(P1, [X | T], [X | T], P, P) :-
    operator(X, _, P2),
    P2 > P1,
    !.
infix_op_pop(_, [')' | T], [')' | T], P, P) :-
    !.
infix_op_pop(_, [], [], P, P) :-
    !.

%! infix_para_pop(+StackIn:list, -StackOut:list, +PrefixIn:list, -PrefixOut:list) is det
% Pop operators from the stack and add to output until we reach a closing
% parenthesis.
%
% @param Priority The priority to compare against.
% @param StackIn The input stack.
% @param StackOut The output stack.
% @param PrefixIn The input expression.
% @param PrefixOut The output expression.
infix_para_pop([X | T], So, [A, B | Pi], Po) :-
    X \= ')',
    !,
    Y =.. [X, A, B],
    infix_para_pop(T, So, [Y | Pi], Po).
infix_para_pop([')' | T], T, P, P) :-
    !. % discard closing para

%! comma_to_list(+GoalsIn:compound, -GoalsOut:list) is det
% Given a single compound term representing a comma-separate list, convert it to
% a list by splitting on commas.
comma_to_list(','(A, B), C) :-
    !,
    comma_to_list(A, A2),
    comma_to_list(B, B2),
    append(A2, B2, C).
comma_to_list(A, [A]) :-
    !. % not comma separated goals.

%! handle_prefixes(+FunctorIn:ground, -FunctorOut:ground)
% If the predicate begins with a reserved prefix, add the dummy prefix to ensure
% that it won't be treated the same as predicates where the prefix is added
% internally. If no prefix, just return the original.
handle_prefixes(Fi, Fo) :-
    has_prefix(Fi, _), % has a reserved prefix; doesn't matter which one
    atom_chars(Fi, Fc),
    atom_chars(Fo, ['d', '_' | Fc]),
    !.
handle_prefixes(Fi, Fo) :-
    reserved_prefix(Fi), % The name *is* a reserved prefix.
    atom_chars(Fi, Fc),
    atom_chars(Fo, ['d', '_' | Fc]),
    !.
handle_prefixes(Fi, Fo) :-
    atom_chars(Fi, Fc),
    Fc = ['_' | _], % starts with an underscore
    atom_chars(Fo, ['d', '_' | Fc]),
    !.
handle_prefixes(F, F).

%! strip_quotes(+StringIn:atom, -StringOut:atom) is det
% Strip single and double quotes from each end of a string. Succeed if the first
% character is not a quote.
strip_quotes(Si, So) :-
    atom_chars(Si, [C | T]),
    member(C, ['\'', '\"']),
    !,
    reverse(T, T2), % reverse string
    T2 = [C | T3], % quote must match
    reverse(T3, T4), % restore correct order
    atom_chars(So, T4).
strip_quotes(S, S) :-
    !. % first char is not a quote

%! incr(+IntIn:int, -IntOut:int, +TokensIn:list, -TokensOut:list)
% DCG-ready increment.
%
% @param IntIn Initial integer.
% @param IntOut IntIn + 1.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
incr(I, I2, T, T) :-
    I2 is I + 1.

%! follow(+FollowList:list, +TokensIn:list, -TokensOut:list)
% Check the next token against a list of acceptable ones, but don't consume it.
% This just helps nail down where an error occurred.
%
% @param FollowList List of tokens that can follow the current one.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
follow(Xs, [T | Ts], [T | Ts]) :-
    T = (T2, _),
    member(T2, Xs).

%! terminal(+Expected:callable, +TokensIn:list, -TokensOut:list)
% Consume a terminal. Print an error message if incorrect token encountered.
%
% @param Expected Expected token.
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
terminal(Expected) -->
    [(Expected, _)],
    !.
terminal(Expected) -->
    syntax_error(Expected),
    !,
    {fail}.

%! empty_list(+TokensIn:list, -TokensOut:list)
% Token list is empty.
%
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
empty_list([], []).

%! replace_underscore(-Var:compound)
% Replace each underscore in a statement with a unique variable.
%
% @param Var The unique variable generated.
replace_underscore(V) :-
    b_getval(us_cnt, I), % get underscore counter
    I2 is I + 1,
    number_chars(I2, Ic),
    atom_chars(V, ['_', 'V' | Ic]),
    b_setval(us_cnt, I2), % update underscore counter
    !.

%! syntax_error2(+Token:callable, +Position:compound, +Expected:callable)
% Construct and print the actual message. Called only by syntax_error/3.
%
% @param Token Token actually encountered.
% @param Position Token position.
% @param Expected Token expected.
syntax_error2(Token, (Source, Line, Col), Expected) :-
    syntax_msg(Expected, ExpMsg),
    visible_token(Token, Vtok),
    !,
    format(user_error, 'ERROR: ~w:~w:~w: Syntax error at \"~w\". ~w.\n', [Source, Line, Col, Vtok, ExpMsg]).
syntax_error2(Token, (Source, Line, Col), _) :-
    visible_token(Token, Vtok),
    !,
    format(user_error, 'ERROR: ~w:~w:~w: Syntax error at \"~w\".\n', [Source, Line, Col, Vtok]).

%! syntax_msg(+Type:atom, -Message:string)
% Define error messages for various tokens and rules.
%
% @param Type Identifier associated with message.
% @param Message Message string.
syntax_msg(statement, 'Invalid start of statement. Expected \"compute\", \":-\" or identifier').
syntax_msg(rule_type, 'Invalid rule type. Expected valid integer: 1, 2, 3, 5, 6 or 8').
syntax_msg(term, 'Invalid term. Expected integer, identifier or \"_\"').
% syntax_msg(atom, ''). % default is fine.
syntax_msg(terms, 'Invalid operator in list of terms. Expected \",\" or \")\"').
syntax_msg(negated_lit, 'Invalid token after negation! Expected an atom').
syntax_msg(double_negation, 'Double negation is not allowed').
syntax_msg(rule, 'Invalid token in rule. Expected \":-\" or \".\"').
syntax_msg(body, 'Invalid token in rule body. Expected \",\" or \".\"').
syntax_msg('{', 'Expected \"{"').
syntax_msg('}', 'Expected \"}"').
syntax_msg('[', 'Expected \"["').
syntax_msg(']', 'Expected \"]"').
syntax_msg('(', 'Expected \"("').
syntax_msg(')', 'Expected \")"').
syntax_msg(bplus, 'Expected \"B+\"').
syntax_msg(bminus, 'Expected \"B-\"').
syntax_msg(literal, 'Expected literal').
syntax_msg(atom, 'Expected atom').
syntax_msg(basic_literal, 'Expected basic literal').
syntax_msg(special_lit, 'Expected \"{\" or \"[\"').
syntax_msg(weight_lit, 'Expected \",\" or \"]\"').
syntax_msg(constraint_lit, 'Expected \",\" or \"}\"').
syntax_msg(choice_rule, 'Expected \",\" or \"}\"').
syntax_msg(compute, 'Expected \",\" or \"}\"').
syntax_msg(int(X), 'Expected integer') :-
    var(X).
syntax_msg(int(X), Msg) :-
    integer(X),
    swritef(Msg, 'Expected integer ~w', [X]).
syntax_msg(X, Msg) :-
    visible_token(X, Vtok),
    swritef(Msg, 'Expected \"~w\"', [Vtok]).
syntax_msg(X, Msg) :-
    swritef(Msg, 'Expected \"~w\"', [X]).

%! visible_token(+Type:atom, -String:string)
% For tokens replaced during scanning, get the proper character or string to
% print.
%
% @param Type The token as it will appear in the list of tokens.
% @param String The string to print.
visible_token(id(X), X).
visible_token(int(X), X).
visible_token(float(X), X).
visible_token(rat(X), X).
visible_token(var(X), X).
visible_token(str(X), X).
visible_token(T, T).

%! parse_recover(+TokensIn:list, -TokensOut:list)
% Go to the next period (statement terminator), and consume it. Stop if the list
% becomes empty. If this predicate is called, parsing has already failed. The
% purpose of this predicate is to recover, if possible, so that any other errors
% in the program can also be caught.
%
% @param TokensIn Input list of tokens.
% @param TokensOut Output list of tokens.
parse_recover -->
    [('.', _)],
    !.
parse_recover -->
    empty_list,
    !.
parse_recover -->
    [(_, _)],
    parse_recover.





