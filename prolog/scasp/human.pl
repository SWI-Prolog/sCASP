:- module(scasp_just_human,
          [ human_justification_tree/1,
            human_justification_tree/2
          ]).

human_justification_tree(Tree) :-
    human_justification_tree(Tree, []).

human_justification_tree(Tree, Options) :-
    print_message(information, scasp_justification(Tree, [depth(1)|Options])).

%!  human_output(+FilterChildren, +Options)

human_output([A,B|Rs], Options) -->
    human_output_(A, Options),
    connector(and, Options),
    human_output([B|Rs], Options).
human_output([A], Options) -->
    { option(depth(0), Options) },
    !,
    human_output_(A, Options),
    full_stop(Options).
human_output([A], Options) -->
    human_output_(A, Options).

human_output_(Term-[], Options) -->
    indent(Options),
    term(Term, Options).
human_output_(Term-Child, Options) -->
    indent(Options),
    term(Term, Options),
    connector(implies, Options),
    { incr_indent(Options, Options1) },
    human_output(Child, Options1).

term(not(Term), Options) -->
    !,
    connector(not, Options),
    term(Term, Options).
term(-Term, Options) -->
    !,
    connector(-, Options),
    term(Term, Options).
term(Term, _Options) -->
    [ '~p'-[Term] ].

connector(and, _Options) -->
    [ ' and '-[], nl ].
connector(not, _Options) -->
    [ 'not '-[] ].
connector(-, _Options) -->
    [ '-'-[] ].
connector(implies, _Options) -->
    [ ' because '-[], nl ].

full_stop(_Options) -->
    [ '.'-[], nl ].

indent(Options) -->
    { option(depth(D), Options),
      Spaces is D*3,
      format(string(S), '~t~*|', [Spaces])
    },
    [ '~w'-[S] ].

incr_indent(Options0, [depth(D)|Options1]) :-
    select_option(depth(D0), Options0, Options1),
    D is D0+1.

:- multifile prolog:message//1.

prolog:message(scasp_justification(Tree, Options)) -->
    human_output(Tree, Options).
