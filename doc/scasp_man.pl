:- module(scasp_man, [], [assertions]).

:- doc(filetype, application).

:- doc(title, "s(CASP)").

:- doc(subtitle, "A stable model semantic solver for constraint logic programs"
    ).

:- doc(author, "Joaquin Arias").
:- doc(copyright, "").

:- doc(summary, "This is the manual of @apl{s(CASP)} is a bundle for
the Ciao System which allows the evaluation of constraint logic
programs under the stable model semantics. ").

:- doc(module, " 

The s(CASP) system is a top-down interpreter for ASP programs with
constraints.

This work was presented at ICLP'18 (Arias et al. 2018), also available
at @href{https://arxiv.org/abs/1804.11162}.

@section{Details}

s(CASP) (available at
@href{https://gitlab.software.imdea.org/joaquin.arias/sCASP} by
Joaquin Arias (visit
@href{https://software.imdea.org/~joaquin.arias}, is based on s(ASP)
(available at @href{https://sourceforge.net/projects/sasp-system}) by
University of Texas at Dallas.

s(CASP) is an implementation of the stable model semantics of
constraint logic programming. Unlike similar systems, it does not
employ any form of grounding. This allows s(CASP) to execute programs
that are not finitely groundable, including those which make use of
lists and terms.

@section{Overview of this document} 

This document is divided in two parts:

@begin{itemize} 

@item @bf{Part I - Installation}. It explains how to download and
install this this bundle.


@item @bf{Part II - Usage}. s(CASP) Usage details how to use this
bundle and its options. Also, several execution modes are shown.

@end{itemize}
    ").
