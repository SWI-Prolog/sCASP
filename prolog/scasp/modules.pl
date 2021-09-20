:- module(scasp_modules,
          [ scasp_encoded_module_term/2, % ?MTerm, ?QTerm
            encoded_module_term/2        % ?TermIn,?TermOut
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
%   Encode a module qualification into a name.

encoded_module_name(M:Name, QName), atom(M), atom(Name) =>
    atomic_list_concat([M,:,Name], QName).
encoded_module_name(MName, QName), atom(QName) =>
    (   sub_atom(QName, B, _, A, :)
    ->  MName = M:Name,
        sub_atom(QName, 0, B, _, M),
        sub_atom(QName, _, A, 0, Name)
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
