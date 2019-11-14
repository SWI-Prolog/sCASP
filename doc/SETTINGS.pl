:- module(_, [], [doccfg]).

%! \title Configuration for s(CASP) manual
%  \author Joaquin Arias

filepath := '../src'.

output_name := scasp.
doc_structure :=
    'scasp_man' -[
            'user_installation',
            'user_usage'
    ].

doc_mainopts := no_biblio|no_bugs|no_patches.
doc_compopts := no_biblio|no_bugs|no_patches.

