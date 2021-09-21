:- module(scasp_modules,
          [ scasp_encoded_module_term/2, % ?MTerm, ?QTerm
            encoded_module_term/2,       % ?TermIn,?TermOut
            unqualify_model_term/3       % +Module,+TermIn,-TermOut
          ]).

/** <module> Encode modules

The sCASP program representation and  solver   are  not module aware. We
solve this by encoding the module in  the functor names. The alternative
is to make the solver and program representation work with M:Term terms.

This current approach  implies  overhead   in  program  preparation  and
returning results, but avoids overhead in   the compilation and solving.
As notably the solver is the slow part   it is likely that interning the
module into the names is both simpler and faster.
*/

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
    (   sub_atom(QName, B, _, A, :)
    ->  MName = M:Name,
        sub_atom(QName, _, A, 0, Name0),
        sub_atom(QName, 0, B, _, M0),
        (   atom_concat(-, M, M0)
        ->  atom_concat(-, Name0, Name)
        ;   M = M0,
            Name = Name0
        )
    ;   MName = QName
    ).

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

%!  unqualify_model_term(+Module, +TermIn, -TermOut)

unqualify_model_term(M, Term0, Term),
    functor(Term0, Name, 1), model_wrapper(Name) =>
    Term0 =.. [Name,Arg0],
    Term  =.. [Name,Arg],
    unqualify_model_term(M, Arg0, Arg).
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
