:- module(scasp_modules,
          [ scasp_encoded_module_term/2, % ?MTerm, ?QTerm
            encoded_module_term/2,       % ?TermIn, ?TermOut
            qualify_body/3,              % +BodyIn, +Module, -BodyOut
            unqualify_model_term/3,      % +Module, +TermIn, -TermOut
            model_term_module/2,         % :Term, -Module
            implementation/2             % :GoalIn, :GoalOut
          ]).
:- use_module(predicates, [scasp_builtin/1]).
:- use_module(library(error), [instantiation_error/1, type_error/2]).
:- use_module(library(lists), [append/3]).

/** <module> Encode modules

The sCASP program representation and  solver   are  not module aware. We
solve this by encoding the module in  the functor names. The alternative
is to make the solver and program representation work with M:Term terms.

This current approach  implies  overhead   in  program  preparation  and
returning results, but avoids overhead in   the compilation and solving.
As notably the solver is the slow part   it is likely that interning the
module into the names is both simpler and faster.
*/

:- meta_predicate
    model_term_module(:, -).

%!  encoded_module_name(?QTerm, ?QName) is det.
%
%   Encode a module qualification into a name. Also deals with names
%   that embed classical negation (-Name).

encoded_module_name(M:NName, QName),
    atom(M), atom(NName), atom_concat(-,Name, NName) =>
    atomic_list_concat([-,M,:,Name], QName).
encoded_module_name(M:Name, QName), atom(M), atom(Name) =>
    atomic_list_concat([M,:,Name], QName).
encoded_module_name(MName, QName), atom(QName) =>
    prefix(Prefix),
    atom_concat(Prefix, QName1, QName),
    !,
    (   split_module_name(QName1, M0, Name0)
    ->  MName = M:Name,
        (   atom_concat(-, M, M0)
        ->  atom_concat(-, Name0, Name1)
        ;   M = M0,
            Name1 = Name0
        ),
        atom_concat(Prefix, Name1, Name)
    ;   (var(MName) ; atom(MName))
    ->  atom_concat(Prefix, QName1, MName)
    ).

prefix(o_).
prefix('').

%!  split_module_name(+QName, -Module, -Name) is semidet.
%
%   Undo the `Module:Name` encoding of encoded_module_name/2.  Split at
%   the *last* colon: a module name may well contain one - the Logical
%   English examples have a module called
%   `family-scasp+http://tests.com` - while the encoded predicate names
%   never do, as they are built from the functor, its arity and one of
%   the reserved prefixes.

split_module_name(QName, Module, Name) :-
    atomic_list_concat(Parts, :, QName),
    Parts = [_,_|_],
    append(ModuleParts, [Name], Parts),
    atomic_list_concat(ModuleParts, :, Module).


%!  encoded_module_term(?TermIn, ?TermOut) is det.

encoded_module_term(M:Term0, Term), atom(M) =>
    Term0 =.. [Name|Args],
    encoded_module_name(M:Name, QName),
    Term =.. [QName|Args].
encoded_module_term(MTerm, Term), callable(Term) =>
    functor(Term, QName, _),
    (   encoded_module_name(M:Name, QName)
    ->  Term =.. [_|Args],
        Term1 =.. [Name|Args],
        MTerm = M:Term1
    ;   MTerm = Term
    ).

%!  scasp_encoded_module_term(?MTerm, ?QTerm) is det.
%
%   Map an explicit Prolog module qualification into one that is encoded
%   in the functor name.

scasp_encoded_module_term(-Term, QTerm) =>
    QTerm = -Term1,
    scasp_encoded_module_term(Term, Term1).
scasp_encoded_module_term(not(Term), QTerm) =>
    QTerm = not(Term1),
    scasp_encoded_module_term(Term, Term1).
scasp_encoded_module_term(Term, -QTerm) =>
    Term = -Term1,
    scasp_encoded_module_term(Term1, QTerm).
scasp_encoded_module_term(Term, not(QTerm)) =>
    Term = not(Term1),
    scasp_encoded_module_term(Term1, QTerm).
scasp_encoded_module_term(MTerm, QTerm) =>
    encoded_module_term(MTerm, QTerm).

%!  qualify_body(+BodyIn, +Module, -BodyOut) is det.
%
%   Include the original Prolog module  into   the  program  for running
%   modular programs.
%
%   Note that forall/2, findall/3, not/1 and  (-)/1 are matched here as
%   s(CASP) control constructs.  A program that  *defines* one of them as
%   an ordinary predicate loses to the construct: the clause head is read
%   as the construct and its arguments  as  goals, which then reaches the
%   errors below rather than being reported as the name clash it is.
%
%   @error instantiation_error if a goal in the body is unbound.
%   @error type_error(callable, G) if a goal in the body is not callable.

qualify_body(-(Head), M, Q) =>
    Q = -QHead,
    qualify_body(Head, M, QHead).
qualify_body(not(Head), M, Q) =>
    Q = not(QHead),
    qualify_body(Head, M, QHead).
qualify_body(forall(Var, Goal), M, Q) =>
    Q = forall(Var, QGoal),
    qualify_body(Goal, M, QGoal).
qualify_body(findall(Templ, Head, List), M, Q) =>
    Q = findall(Templ, QHead, List),
    qualify_body(Head, M, QHead).
qualify_body((A,B), M, Q) =>
    Q = (QA,QB),
    qualify_body(A, M, QA),
    qualify_body(B, M, QB).
qualify_body((A:-B), M, Q) =>
    Q = (QA:-QB),
    qualify_body(A, M, QA),
    qualify_body(B, M, QB).
qualify_body(G, M, Q), callable(G) =>
    (   scasp_builtin(G)
    ->  Q = G
    ;   current_predicate(M:pr_prolog_predicate/2),
        M:pr_prolog_predicate(G, Type)
    ->  Q = prolog(M:G, Type)
    ;   implementation(M:G, Callee),
        encoded_module_term(Callee, Q)
    ).
qualify_body(G, _M, _Q), var(G) =>
    instantiation_error(G).
qualify_body(G, _M, _Q) =>
    type_error(callable, G).

%!  unqualify_model_term(+Module, +TermIn, -TermOut)

unqualify_model_term(M, goal_origin(Term0, O), Term)  =>
    unqualify_model_term(M, Term0, Term1),
    Term = goal_origin(Term1, O).
unqualify_model_term(M, Term0, Term),
    functor(Term0, Name, 1), model_wrapper(Name) =>
    Term0 =.. [Name,Arg0],
    Term  =.. [Name,Arg],
    unqualify_model_term(M, Arg0, Arg).
unqualify_model_term(M, findall(Templ, Goal0, List), Findall) =>
    Findall = findall(Templ, Goal, List),
    unqualify_model_term(M, Goal0, Goal).
unqualify_model_term(M, Term0, Term) =>
    (   encoded_module_term(Q:Term1, Term0)
    ->  (   Q == M
        ->  Term = Term1
        ;   Term = Q:Term1
        )
    ;   Term0 = Term
    ).

model_wrapper(-).
model_wrapper(not).
model_wrapper(chs).
model_wrapper(proved).
model_wrapper(assume).
model_wrapper(abduced).

%!  model_term_module(:Term, -Module) is det.

:- det(model_term_module/2).
model_term_module(M0:Term, M) :-
    model_term_module(Term, M0, M).

model_term_module(Term, M0, M),
    functor(Term, Name, 1), model_wrapper(Name) =>
    arg(1, Term, A),
    model_term_module(A, M0, M).
model_term_module(M:_Term, _, Q) =>
    Q = M.
model_term_module(Term, M0, M) =>
    (   encoded_module_term(Q:_, Term)
    ->  M = Q
    ;   M = M0
    ).

%!  implementation(:GoalIn, :GoalOut) is det.
%
%   True when GoalOut is the  true   location  of  the predicate GoalIn,
%   i.e., the location from where the predicate was imported.

implementation(M0:Head, M:Head) :-
    predicate_property(M0:Head, imported_from(M1)),
    !,
    M = M1.
implementation(Head, Head).

