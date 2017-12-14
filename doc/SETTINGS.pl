:- module(_, [], [lpdoclib(doccfg)]).

%! \title Configuration for TCLP(ASP) manual
%  \author Joaquin Arias

:- include(ciao_docsrc(common/'LPDOCCOMMON')).

filepath := '../src'.
filepath := '.'.

filepath := ~ciaofilepath_common.

output_name := tclp_asp.

doc_structure :=
	'tclp_asp_man' -[
	    'user_part' -[
		'user_installation',
		'user_usage'
	    ],
	    'reference_part' -[
				  'tclp_asp',
				  'tclp_asp_io',
				  'clp_call_stack',
				  'clp_disequality_rt',
				  'clp_clpq'
	    ]
	].

%papertype := afourthesis.

%index := lib, pred.

doc_mainopts := no_biblio|no_bugs|no_patches.
doc_compopts := no_biblio|no_bugs|no_patches.
