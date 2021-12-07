:- module(scasp_ops,
          [ op(700, xfx, [#= , #<>, #< , #> , #=<, #>= ]),
            op(950, xfx, ::),
            op(700, xfx, '| '),
            op(700, xfx, [~>, <~]),
            op(900, fy,  not),
            op(700, xfx, '\u2209'),

            scasp_push_operators/0,
            scasp_push_operators/1,             % +Module
            scasp_pop_operators/0
          ]).
:- use_module(library(apply)).
:- use_module(library(operators)).

scasp_push_operators :-
    prolog_load_context(module, Context),
    scasp_push_operators(Context).

scasp_push_operators(Context) :-
    findall(op(Pri, Ass, Op), scasp_op(Pri, Ass, Op), Ops),
    push_operators(Context:Ops).

scasp_pop_operators :-
    pop_operators.

term_expansion(scasp_op(Pri, Ass, List), Ops) :-
    maplist(mkop(Pri, Ass), List, Ops).

mkop(Pri, Ass, Op, scasp_op(Pri, Ass, Op)).

scasp_op(700, xfx, [#= , #<>, #< , #> , #=<, #>= ]).
scasp_op(950, xfx, [::]).
scasp_op(700, xfx, ['| ']).
scasp_op(700, xfx, [~>, <~]).
scasp_op(900, fy,  [not]).
scasp_op(700, xfx, ['\u2209']).

% from input.pl
scasp_op(1200, fx,  #).
scasp_op(350, fx,  [include, compute, abducible]).
scasp_op(1150, fx, [table, show, pred]).
